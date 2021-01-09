using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Policy;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using static Prolog.PrologEngine;

namespace Prolog
{
    public partial class TestsArea : WeifenLuo.WinFormsUI.Docking.DockContent
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
                .Where(x => x.ClauseList.Term.TestGroup != null)
                .OrderBy(x => x.ToString())
                .ToList();

            foreach (var testGroup in predDescrs.GroupBy(x => x.ClauseList.Term.TestGroup).OrderBy(x => x.Key).ToArray())
            {
                var tests = testGroup
                    .Select(x => x.ToString())
                    .OrderBy(x => x)
                    .Select(x => new TreeNode(x, TestIcon, TestIcon)).ToArray();

                TreeNode tg = new TreeNode(testGroup.Key + " - " + tests.Count() + " test(s)", GroupIcon, GroupIcon, tests);

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

            Task.Factory.StartNew(() => 
            Parallel.ForEach(targetTestNodes.OrderBy(x => x.Name).ToArray(), new ParallelOptions { MaxDegreeOfParallelism = 8 }, x =>
            {
                string testName = null;
                string source = null;
                Invoke(new Action(() => 
                {
                    x.ImageIndex = x.SelectedImageIndex = RunningIcon;
                    testName = x.Text;
                    source = sourceArea.sourceEditor.Editor.Text;
                }));

                // Run in dedicated app domains to isolate statics
                AppDomain domain = AppDomain.CreateDomain(x.Text);

                domain.SetData("source", source);
                domain.SetData("test", testName);

                try
                {
                    //domain.DoCallBack(() => {
                        string src = (string)AppDomain.CurrentDomain.GetData("source");
                        string test = (string)AppDomain.CurrentDomain.GetData("test");

                        PrologEngine e = new PrologEngine(persistentCommandHistory: false);
                        e.ConsultFromString(src);

                        List<string> callStack = new List<string>();
                        e.OnCurrentTermChanged += y =>
                        {
                            callStack.Add(y.ToString());
                            if (callStack.Count > 3)
                            {
                                callStack.RemoveAt(0);
                            }
                        };

                        SolutionSet ss = e.GetAllSolutions(null, test, 0);

                        bool suceeded = !ss.HasError && ss.Success;
                        string callStackText = callStack.Aggregate((z, y) => z + "\n" + y);

                        AppDomain.CurrentDomain.SetData("result", suceeded);
                        AppDomain.CurrentDomain.SetData("callstack", callStackText);
                    //});

                    bool result = suceeded;
                    string callStackMsg = callStackText;

                    //bool result = (bool)domain.GetData("result");
                    //string callStackMsg = (string)domain.GetData("callstack");

                    BeginInvoke(new Action(() =>
                    {
                        x.ImageIndex = x.SelectedImageIndex = result ? PassedIcon : FailedIcon;
                        x.ToolTipText = result ? string.Empty : callStackMsg;
                        x.ToolTipText = x.ToolTipText.Trim(new char[] { ' ', '\n', '\t', '\r' });

                        lock (x.Parent)
                        {
                            if (result)
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
                }
                catch (Exception ex)
                {
                    throw new Exception(ex.StackTrace);
                }
                finally
                {
                    AppDomain.Unload(domain);
                }
            }));
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
