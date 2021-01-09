using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using static Prolog.PrologEngine;

namespace Prolog
{
    public partial class InspectorArea : WeifenLuo.WinFormsUI.Docking.DockContent
    {
        private readonly PrologEngine pe;
        private readonly SourceArea sourceArea;
        private readonly StateBrowser.WinForms.StateBrowser _stateBrowser;

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
                    this.sourceArea.sourceEditor.Editor.AddSelection(x.ClauseList.Term.Symbol.Start, x.ClauseListEnd.Term.Symbol.Final);
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
                    x => x.ClauseList.Term.Symbol.Start <= pos && x.ClauseListEnd.Term.Symbol.Final >= pos && !x.IsPredefined).ToList();

            RefresthUIState(new Dictionary<string, object> { ["Clauses"] = clauses.ToDictionary(x => x.ToString()) }, "Current caret position");
            foreach (var t in clauses)
            {
                this.sourceArea.sourceEditor.Editor.AddSelection(t.ClauseList.Term.Symbol.Start, t.ClauseListEnd.Term.Symbol.Final);
            }
        }
    }
}
