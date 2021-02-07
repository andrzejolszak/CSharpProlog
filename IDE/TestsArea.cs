using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using Serilog;
using WeifenLuo.WinFormsUI.Docking;
using static Prolog.PrologEngine;

namespace Prolog
{
    public partial class TestsArea : DockContent
    {
        private const int GroupIcon = 0;
        private const int TestIcon = 1;
        private const int RunningIcon = 2;
        private const int PassedIcon = 3;
        private const int FailedIcon = 4;

        private readonly PrologEngine pe;
        private readonly SourceArea sourceArea;
        private readonly List<TreeNode> testNodes = new List<TreeNode>();

        public TestsArea(PrologEngine pe, SourceArea sourceArea)
        {
            InitializeComponent();

            this.pe = pe;
            this.sourceArea = sourceArea;

            RefreshTests();
        }

        public void RefreshTests()
        {
            treeView1.Nodes.Clear();
            testNodes.Clear();

            List<PredicateDescr> predDescrs = pe.PredTable.Predicates.Values
                .Where(x => !x.IsPredefined)
                .Where(x => x.Arity == 0)
                .Where(x => x.ClauseList.Head.TestGroup != null)
                .OrderBy(x => x.ToString())
                .ToList();

            foreach (IGrouping<string, PredicateDescr> testGroup in predDescrs.GroupBy(x => x.ClauseList.Head.TestGroup)
                .OrderBy(x => x.Key).ToArray())
            {
                TreeNode[] tests = testGroup
                    .Select(x => x.Functor)
                    .OrderBy(x => x)
                    .Select(x => new TreeNode(x, TestIcon, TestIcon)).ToArray();

                TreeNode tg = new TreeNode(testGroup.Key + " - " + tests.Count() + " test(s)", GroupIcon, GroupIcon,
                    tests);

                treeView1.Nodes.Add(tg);
                testNodes.AddRange(tests);
            }

            treeView1.ExpandAll();

            // Autorun
            if (checkBox1.Checked && testNodes.Count > 0)
            {
                Task.Factory.StartNew(() =>
                {
                    Thread.Sleep(500);
                    BeginInvoke(new Action(() => RunTests(testNodes)));
                });
            }
        }

        private void runAllTestsClick(object sender, EventArgs evtArgs)
        {
            RunTests(testNodes);
        }

        private void RunTests(IEnumerable<TreeNode> targetTestNodes)
        {
            foreach (TreeNode n in targetTestNodes)
            {
                n.ImageIndex = n.SelectedImageIndex = RunningIcon;
                n.Parent.ImageIndex = n.Parent.SelectedImageIndex = RunningIcon;
                n.Parent.Expand();
            }

            Application.DoEvents();

            string source = sourceArea.sourceEditor.Editor.Text;

            BasicIo oldIo = IO.BasicIO;
            IO.BasicIO = new SilentIO();
            ParallelLoopResult res =
                Parallel.ForEach(targetTestNodes.Select(control => (control.Text, control)).OrderBy(x => x.Text).ToArray(),
                    new ParallelOptions { MaxDegreeOfParallelism = 4 }, x =>
                      {
                          bool suceeded = false;
                          string callStackText = "N/A";

                          try
                          {
                              PrologEngine e = new PrologEngine(new ExecutionDetails());

                              e.ConsultFromString(source);

                              // Timeout after 2s
                              e.MaxExecutionMs = 2_000;
                              SolutionSet ss = e.GetAllSolutions(x.Text, 0);

                              suceeded = !ss.HasError && ss.Success;
                              callStackText = suceeded ? string.Empty : (ss.ErrMsg + Environment.NewLine + e.ExecutionDetails.CallHistoryStringWithLinesLast10);
                          }
                          catch(Exception ex)
                          {
                              suceeded = false;
                              callStackText = "Exception during test run:" + Environment.NewLine + ex.Message;
                              Log.Error(ex, x.Text + ": " + callStackText);
                          }

                          BeginInvoke(new Action(() =>
                          {
                              x.control.ImageIndex = x.control.SelectedImageIndex = suceeded ? PassedIcon : FailedIcon;
                              x.control.ToolTipText = callStackText;
                              x.control.ToolTipText = x.control.ToolTipText.Trim(' ', '\n', '\t', '\r');
                          }));

                          Application.DoEvents();

                      });

            BeginInvoke(new Action(() =>
            {
                HashSet<TreeNode> parents = new HashSet<TreeNode>();
                foreach (TreeNode n in targetTestNodes)
                {
                    parents.Add(n.Parent);

                    if (n.Parent.ImageIndex == RunningIcon)
                    {
                        n.Parent.ImageIndex = n.Parent.SelectedImageIndex = n.ImageIndex;
                    }
                    else if (n.ImageIndex == FailedIcon || n.ImageIndex == RunningIcon)
                    {
                        n.Parent.ImageIndex = n.Parent.SelectedImageIndex = FailedIcon;
                    }
                }

                foreach (TreeNode p in parents)
                {
                    if (p.SelectedImageIndex == FailedIcon)
                    {
                        p.Expand();
                    }
                    else
                    {
                        p.Collapse();
                    }
                }
            }));

            Application.DoEvents();

            IO.BasicIO = oldIo;
        }

        private void runFailedTests(object sender, EventArgs e)
        {
            RunTests(testNodes.Where(x => x.SelectedImageIndex == FailedIcon));
        }

        private void treeView1_NodeMouseDoubleClick(object sender, TreeNodeMouseClickEventArgs e)
        {
            int pos = sourceArea.sourceEditor.Editor.Text.IndexOf(e.Node.Text + " :-");

            if (pos == -1)
            {
                pos = sourceArea.sourceEditor.Editor.Text.IndexOf(e.Node.Text + ":-");
            }

            if (pos != -1)
            {
                sourceArea.sourceEditor.Editor.GotoPosition(pos);
                sourceArea.Focus();
            }
        }
    }
}