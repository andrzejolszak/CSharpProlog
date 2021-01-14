using System;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using ScintillaNET;
using WeifenLuo.WinFormsUI.Docking;
using static Prolog.PrologEngine;

namespace Prolog
{
    public partial class MainForm : Form
    {
        private const string DockPanelConfigFile = "guiLayout.xml";
        private readonly DebuggerArea debuggerArea;

        private readonly DockPanel dockPanel;
        private readonly InspectorArea inspectorArea;
        private readonly OutputArea outputArea;
        private readonly PrologEngine pe;
        private readonly QueryArea queryArea;
        private readonly SourceArea sourceArea;
        private readonly StaticAnalysisArea staticAnalysisArea;
        private readonly TestsArea testsArea;

        public MainForm()
        {
            IsMdiContainer = true;
            Scintilla.SetDestroyHandleBehavior(true);

            InitializeComponent();

            Text = "CSProlog Editor";

            outputArea = new OutputArea();

            IO.BasicIO = outputArea.GuiIO;
            pe = new PrologEngine(false);

            sourceArea = new SourceArea(pe, outputArea, outputArea.tbAnswer, outputArea.HandleProgressChanged);
            debuggerArea = new DebuggerArea(pe, sourceArea);
            queryArea = new QueryArea(pe, sourceArea, outputArea, outputArea.tbAnswer,
                outputArea.HandleProgressChanged);
            testsArea = new TestsArea(pe, sourceArea);
            inspectorArea = new InspectorArea(pe, sourceArea);
            staticAnalysisArea = new StaticAnalysisArea(pe, sourceArea);

            sourceArea.SourceConsultedSuccess += () =>
            {
                outputArea.BringToFront();

                sourceArea.sourceEditor.RefreshAutocompletionItems();
                sourceArea.sourceEditor.RefreshReferenceArrows();
                queryArea.queryEditor.RefreshAutocompletionItems();
                staticAnalysisArea.HandleConsulted(true);
                testsArea.RefreshTests();
                inspectorArea.RefresthUIState(pe, "Prolog Engine");

                sourceArea.Focus();
            };

            sourceArea.SourceConsultedError += () =>
            {
                outputArea.BringToFront();

                inspectorArea.RefresthUIState(pe, "Prolog Engine");
                staticAnalysisArea.HandleConsulted(false);

                sourceArea.Focus();
            };

            sourceArea.sourceEditor.InspectTermRequested += x =>
            {
                inspectorArea.RefresthUIState(x, $"Source Term: '{x}'");
                inspectorArea.BringToFront();
            };

            pe.FoundAllSolutions += () =>
            {
                outputArea.BeginInvoke(new Action(() =>
                {
                    outputArea.BringToFront();
                }));
            };

            dockPanel = new DockPanel();
            dockPanel.Theme = new VS2015LightTheme();
            dockPanel.Dock = DockStyle.Fill;
            dockPanel.AllowEndUserDocking = true;
            Controls.Add(dockPanel);
            dockPanel.BringToFront();

            Shown += HandleShown;
        }

        private void HandleShown(object sender, EventArgs e)
        {
            foreach (string file in Directory.GetFiles(@".\Examples\")
                .Where(x => x.EndsWith(".pl"))
                .Select(x => x.Replace(@".\Examples\", string.Empty))
                .OrderBy(x => x))
            {
                examplesToolStripMenuItem.DropDownItems.Add(file, null, ExampleSelected);
            }

            try
            {
                dockPanel.LoadFromXml(DockPanelConfigFile, GetContentFromPersistString);
            }
            catch
            {
                sourceArea.Show(dockPanel, DockState.Document);
                queryArea.Show(sourceArea.Pane, DockAlignment.Bottom, 0.4);
                staticAnalysisArea.Show(queryArea.Pane, DockAlignment.Bottom, 0.65);
                outputArea.Show(staticAnalysisArea.Pane, staticAnalysisArea);
                inspectorArea.Show(sourceArea.Pane, DockAlignment.Right, 0.27);
                testsArea.Show(inspectorArea.Pane, inspectorArea);
                debuggerArea.Show(inspectorArea.Pane, testsArea);
            }
        }

        private void ExampleSelected(object sender, EventArgs e)
        {
            if (!sourceArea.AskCanLoseCurrentSource())
            {
                return;
            }

            string filename = @".\Examples\" + ((ToolStripItem)sender).Text;
            sourceArea.OpenFile(filename, File.Open(filename, FileMode.Open));
        }

        private IDockContent GetContentFromPersistString(string persistString)
        {
            if (persistString == typeof(SourceArea).ToString())
            {
                return sourceArea;
            }

            if (persistString == typeof(QueryArea).ToString())
            {
                return queryArea;
            }

            if (persistString == typeof(OutputArea).ToString())
            {
                return outputArea;
            }

            if (persistString == typeof(TestsArea).ToString())
            {
                return testsArea;
            }

            if (persistString == typeof(InspectorArea).ToString())
            {
                return inspectorArea;
            }

            if (persistString == typeof(StaticAnalysisArea).ToString())
            {
                return staticAnalysisArea;
            }

            if (persistString == typeof(DebuggerArea).ToString())
            {
                return debuggerArea;
            }

            return null;
        }

        private void exitToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void newToolStripMenuItem_Click(object sender, EventArgs e)
        {
            sourceArea.newToolStripMenuItem_Click(sender, e);
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            sourceArea.openToolStripMenuItem_Click(sender, e);
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            sourceArea.saveToolStripMenuItem_Click(sender, e);
        }

        private void saveAsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            sourceArea.saveAsToolStripMenuItem_Click(sender, e);
        }

        private void MainForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            dockPanel.SaveAsXml(DockPanelConfigFile);
        }

        private void openGUIToolStripMenuItem_Click(object sender, EventArgs e)
        {
        }

        private void menuStrip1_ItemClicked(object sender, ToolStripItemClickedEventArgs e)
        {
        }
    }

    public static class Extensions
    {
        private static readonly string CRLF = Environment.NewLine;

        public static void Write(this TextBox tb, string s)
        {
            tb.AppendText(s);
        }

        public static void WriteLine(this TextBox tb)
        {
            tb.AppendText(CRLF);
        }

        public static void WriteLine(this TextBox tb, string s)
        {
            tb.AppendText(s);
            tb.AppendText(CRLF);
        }

        // BackgroundWorker
        public static void DoGuiAction(this BackgroundWorker bgw, GuiAction a)
        {
            if (bgw != null)
            {
                bgw.ReportProgress((int)a, null);
            }
        }

        public static void DoGuiAction(this BackgroundWorker bgw, GuiAction a, string s)
        {
            if (bgw != null)
            {
                bgw.ReportProgress((int)a, s);
            }
        }
    }
}