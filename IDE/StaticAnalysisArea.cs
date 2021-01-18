using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using Serilog;
using WeifenLuo.WinFormsUI.Docking;
using static Prolog.PrologEngine;

namespace Prolog
{
    public partial class StaticAnalysisArea : DockContent
    {
        private readonly PrologEngine _pe;
        private readonly SourceArea _sourceArea;
        private readonly HashSet<int> _warningTypesToIgnore = new HashSet<int>();

        public StaticAnalysisArea(PrologEngine pe, SourceArea sourceArea)
        {
            InitializeComponent();

            _pe = pe;
            _sourceArea = sourceArea;

            CloseButton = false;
            CloseButtonVisible = false;

            Text = "Static Analysis";

            _sourceArea.sourceEditor.Editor.TextChanged += SourceTextChanged;
        }

        private void SourceTextChanged(object sender, EventArgs e)
        {
            if (dataGridView1.Rows.Count > 0)
            {
                dataGridView1.Rows.Clear();
                _sourceArea.sourceEditor.ClearIndicators();
            }
        }

        private void analyzeBtn_Click(object sender, EventArgs e)
        {
            Analyze();
        }

        internal void HandleConsulted(bool success)
        {
            if (success && checkBox1.Checked)
            {
                Analyze();
            }
            else
            {
                Text = "Static Analysis";
            }
        }

        private void Analyze()
        {
            dataGridView1.Rows.Clear();
            _sourceArea.sourceEditor.ClearIndicators();

            int warningCount = 0;

            List<PredicateDescr> userPreds = _pe.PredTable.Predicates.Values
                .Where(x => !x.IsPredefined && x.DefinitionFile != null &&
                            !x.DefinitionFile.Contains(Path.DirectorySeparatorChar))
                .ToList();
            foreach (PredicateDescr pred in userPreds)
            {
                TermNode nextNode = pred.ClauseList.NextNode;
                while (nextNode != null)
                {
                    // Name defined - 1
                    if (!_pe.PredTable.Predicates.Values.Any(x => x.Functor == nextNode.Term.FunctorToString))
                    {
                        warningCount = AddWarning(warningCount, nextNode,
                            $"Predicate with name '{nextNode.Term.FunctorToString}' not defined in KB. Is it dynamically asserted?",
                            1);
                    }
                    // Name & arity defined - 2
                    else if (!_pe.PredTable.Predicates.Values.Any(x =>
                        x.Functor == nextNode.Term.FunctorToString && x.Arity == nextNode.Term.Arity))
                    {
                        string closeMatches = _pe.PredTable.Predicates.Values
                            .Where(x => x.Functor == nextNode.Term.FunctorToString).Select(x => x.Name)
                            .Aggregate((x, y) => x + ", " + y);
                        warningCount = AddWarning(warningCount, nextNode,
                            $"Predicate '{nextNode.Term.Name}' not defined in KB. Possible close matches are: {closeMatches}.",
                            2);
                    }

                    // Next node
                    nextNode = nextNode.NextNode;
                }
            }

            if (warningCount == 0)
            {
                Text = "Static Analysis - ok";
            }
            else
            {
                Text = $"Static Analysis - {warningCount} Warnings";
                BringToFront();
            }
        }

        private int AddWarning(int warningCount, TermNode nextNode, string msg, int type)
        {
            if (_warningTypesToIgnore.Contains(type))
            {
                return warningCount;
            }

            int startPos = nextNode.Term.Symbol.StartAdjusted;
            int endPos = nextNode.Term.Symbol.FinalAdjusted;
            int line = _sourceArea.sourceEditor.Editor.LineFromPosition(startPos);

            dataGridView1.Rows.Add(line, "Warning", msg, type);
            _sourceArea.sourceEditor.IndicatorFillRange(PrologEditor.StaticWarningIndicator, startPos,
                endPos - startPos, msg);
            warningCount++;
            return warningCount;
        }

        private void dataGridView1_CellMouseDoubleClick(object sender, DataGridViewCellMouseEventArgs e)
        {
            try
            {
                int line = (int)dataGridView1.Rows[e.RowIndex].Cells[0].Value;
                _sourceArea.sourceEditor.Editor.ScrollRange(line, line);
                _sourceArea.sourceEditor.Editor.GotoPosition(_sourceArea.sourceEditor.Editor.Lines[line].Position);
                _sourceArea.Focus();
            }
            catch (Exception ex)
            {
                // Silent catch - don't handle UI edge cases
                Log.Error(ex, "GUI edge case: " + ex.Message);
            }
        }

        private void dataGridView1_CellMouseClick(object sender, DataGridViewCellMouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                try
                {
                    ContextMenuStrip m = new ContextMenuStrip();
                    int type = (int)dataGridView1.Rows[e.RowIndex].Cells["Type"].Value;

                    ToolStripMenuItem hide = new ToolStripMenuItem($"Hide Warning Type={type}");
                    hide.Click += (x, y) =>
                    {
                        _warningTypesToIgnore.Add(type);
                        Analyze();
                    };
                    m.Items.Add(hide);

                    ToolStripMenuItem show = new ToolStripMenuItem($"Show Hidden {_warningTypesToIgnore.Count} Types");
                    show.Click += (x, y) =>
                    {
                        _warningTypesToIgnore.Clear();
                        Analyze();
                    };
                    m.Items.Add(show);

                    m.Show(dataGridView1, new Point(e.X, e.Y));
                }
                catch (Exception ex)
                {
                    // Silent catch - don't handle UI edge cases
                    Log.Error(ex, "GUI edge case: " + ex.Message);
                }
            }
        }
    }
}