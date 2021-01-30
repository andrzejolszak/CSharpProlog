using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using WeifenLuo.WinFormsUI.Docking;
using static Prolog.PrologEngine;

namespace Prolog
{
    public partial class InspectorArea : DockContent
    {
        private readonly StateBrowser.WinForms.StateBrowser _stateBrowser;
        private readonly PrologEngine pe;
        private readonly SourceArea sourceArea;

        public InspectorArea(PrologEngine pe, SourceArea sourceArea)
        {
            InitializeComponent();

            this.pe = pe;
            this.sourceArea = sourceArea;

            _stateBrowser = new StateBrowser.WinForms.StateBrowser();
            _stateBrowser.Context = "Prolog Engine";
            _stateBrowser.Name = "_stateBrowserControl";
            _stateBrowser.Dock = DockStyle.Fill;
            _stateBrowser.ObjectToBrowse = pe;

            panel1.Controls.Add(_stateBrowser);
            this.sourceArea.sourceEditor.Editor.UpdateUI +=
                (sender, args) => label1.Text = this.sourceArea.sourceEditor.Editor.CurrentPosition.ToString();
            _stateBrowser.AddInspectMenuItemForType<PredicateDescr>("Select in Editor",
                x =>
                {
                    this.sourceArea.sourceEditor.Editor.ClearSelections();
                    this.sourceArea.sourceEditor.Editor.AddSelection(x.ClauseList.Head.Symbol.StartAdjusted,
                        x.ClauseListEnd.Head.Symbol.FinalAdjusted);
                });
        }

        public void RefresthUIState(object targetObj, string context)
        {
            _stateBrowser.Context = context;
            _stateBrowser.ObjectToBrowse = targetObj;
            _stateBrowser.RefreshState();

            _stateBrowser.TreeView.BeginUpdate();
            _stateBrowser.TreeView.CollapseAll();
            _stateBrowser.TreeView.Nodes[0].Expand();
            _stateBrowser.TreeView.EndUpdate();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            int pos = sourceArea.sourceEditor.Editor.CurrentPosition;
            List<PredicateDescr> clauses = pe.PredTable.Predicates.Values.Where(
                x => x.ClauseList.Head.Symbol.StartAdjusted <= pos && x.ClauseListEnd.Head.Symbol.FinalAdjusted >= pos &&
                     !x.IsPredefined).ToList();

            RefresthUIState(new Dictionary<string, object> { ["Clauses"] = clauses.ToDictionary(x => x.ToString()) },
                "Current caret position");
            foreach (PredicateDescr t in clauses)
            {
                sourceArea.sourceEditor.Editor.AddSelection(t.ClauseList.Head.Symbol.StartAdjusted,
                    t.ClauseListEnd.Head.Symbol.FinalAdjusted);
            }
        }
    }
}