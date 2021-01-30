using System;
using System.Collections.Generic;
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
            for (int i = 0; i < treeView1.Nodes.Count; i++)
            {
                TreeNode node = treeView1.Nodes[i];
                if (targetTestNodes.Any(x => x.Parent == node))
                {
                    node.ImageIndex = node.SelectedImageIndex = RunningIcon;
                }
            }

            BasicIo oldIo = IO.BasicIO;
            IO.BasicIO = new SilentIO();
            ParallelLoopResult res =
                Parallel.ForEach(targetTestNodes.OrderBy(x => x.Name).ToArray(),
                    new ParallelOptions { MaxDegreeOfParallelism = 8 }, x =>
                      {
                          bool suceeded = false;
                          string callStackText = "N/A";
                          string testName = null;

                          try
                          {
                              string source = null;

                              Invoke(new Action(() =>
                              {
                                  x.ImageIndex = x.SelectedImageIndex = RunningIcon;
                                  testName = x.Text;
                                  source = sourceArea.sourceEditor.Editor.Text;
                              }));

                              PrologEngine e = new PrologEngine(new ExecutionDetails());
                              e.ConsultFromString(source);

                              SolutionSet ss = e.GetAllSolutions(testName, 0);

                              suceeded = !ss.HasError && ss.Success;
                              callStackText = e.ExecutionDetails.CurrentTermHistoryString;
                          }
                          catch(Exception ex)
                          {
                              suceeded = false;
                              callStackText = "Exception during test run: \n" + ex.Message;
                              Log.Error(ex, testName + ": " + callStackText);
                          }

                          BeginInvoke(new Action(() =>
                            {
                                x.ImageIndex = x.SelectedImageIndex = suceeded ? PassedIcon : FailedIcon;
                                x.ToolTipText = suceeded ? string.Empty : callStackText;
                                x.ToolTipText = x.ToolTipText.Trim(' ', '\n', '\t', '\r');

                                lock (x.Parent)
                                {
                                    if (suceeded)
                                    {
                                        if (x.Parent.ImageIndex == RunningIcon)
                                        {
                                            x.Parent.ImageIndex = x.Parent.SelectedImageIndex = PassedIcon;
                                            x.Parent.Collapse();
                                        }
                                    }
                                    else
                                    {
                                        x.Parent.ImageIndex = x.Parent.SelectedImageIndex = FailedIcon;
                                        x.Parent.Expand();
                                    }
                                }
                            }));
                      });

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