using System;
using System.Threading;
using System.Windows.Forms;
using ScintillaNET;
using System.Collections;
using static Prolog.PrologEngine;
using System.Collections.Generic;
using System.Linq;

namespace Prolog
{
    public enum DebuggerUserCommand
    {
        Continue, Stop, StepOver, StepInto, StepOut
    }

    public partial class DebuggerArea : WeifenLuo.WinFormsUI.Docking.DockContent
    {
        private const int GroupIcon = 0;
        private const int TestIcon = 1;

        private readonly PrologEngine pe;
        private readonly SourceArea sourceArea;
        private readonly PrologEditor sourceEditor;

        private readonly AutoResetEvent _debugStepWaitHandle;
        private DebuggerUserCommand? _lastDebuggerUserCommand;
        private int? _lastLevel;

        public DebuggerArea(PrologEngine pe, SourceArea sourceArea)
        {
            InitializeComponent();

            this.pe = pe;
            this.sourceArea = sourceArea;
            sourceEditor = sourceArea.sourceEditor;

            _debugStepWaitHandle = new AutoResetEvent(false);
            pe.DebugEventBlocking += HandleDebugEvent;
            pe.FoundAllSolutions += () =>
            {
                BeginInvoke(new Action(() =>
                {
                    _lastDebuggerUserCommand = null;
                    _lastLevel = null;
                }));
            };

            sourceEditor.Editor.KeyDown += sourceKeyDown;
        }


        private bool HandleDebugEvent(TermNode goalNode, TermNode currentClause, bool isReturn, VarStack varStack, Stack<CallReturn> callStack)
        {
            if (this._lastLevel.HasValue && this._lastDebuggerUserCommand == DebuggerUserCommand.StepOver && callStack.Count > this._lastLevel)
            {
                return false;
            }

            if (this._lastLevel.HasValue && this._lastDebuggerUserCommand == DebuggerUserCommand.StepOut && callStack.Count >= this._lastLevel)
            {
                return false;
            }

            if (goalNode?.PredDescr?.IsPredefined == true && currentClause != null)
            {
                return false;
            }

            bool shouldBreak = false;

            this.Invoke(new Action(() => shouldBreak = HasBreakpoint(currentClause?.Term)));

            if (!shouldBreak &&
                (this._lastDebuggerUserCommand == DebuggerUserCommand.Continue
                || this._lastDebuggerUserCommand == null))
            {
                return false;
            }

            double totalCpuSeconds = pe.ProcessorTime();
            // Limited timer resolution - ignore when smaller than 100ms
            string procTimeString = totalCpuSeconds < 0.1 ? "" : $" ({totalCpuSeconds:f3} s)";

            this.Invoke(new Action(() =>
            {
                this.sourceEditor.Editor.ReadOnly = true;
                this.debuggerPanel.Show();
                this.sourceEditor.ClearIndicators();
                bool movedToPosition = false;

                // Mark the goal
                if (goalNode?.Term?.Symbol != null && goalNode.Term.Symbol.FinalAdjusted > 0)
                {
                    this.sourceEditor.IndicatorFillRange(
                        PrologEditor.DebugLineIndicator,
                        goalNode.Term.Symbol.StartAdjusted,
                        goalNode.Term.Symbol.FinalAdjusted - goalNode.Term.Symbol.StartAdjusted,
                        goalNode.Term.ToDisplayString() + procTimeString);

                    this.sourceEditor.Editor.GotoPosition(goalNode.Term.Symbol.StartAdjusted);
                    movedToPosition = true;
                }

                // Open calltip
                string clauseTip = (currentClause?.Term?.ToDisplayString() ?? goalNode.ToString() ?? string.Empty) + procTimeString;
                this.sourceEditor.Editor.CallTipSetPosition(true);
                this.sourceEditor.Editor.CallTipShow(currentClause?.Term?.Symbol?.StartAdjusted ?? goalNode?.Term?.Symbol?.StartAdjusted ?? this.sourceEditor.Editor.CurrentPosition, clauseTip);
                this.sourceEditor.InitialCallTipDisplay = true;

                // Mark the clause
                if (currentClause?.Term?.Symbol != null && currentClause.Term.Symbol.FinalAdjusted > 0)
                {
                    this.sourceEditor.IndicatorFillRange(
                        PrologEditor.DebugLineIndicator,
                        currentClause.Term.Symbol.StartAdjusted,
                        (currentClause.GetLastNode().Term?.Symbol?.FinalAdjusted ?? currentClause.Term.Symbol.FinalAdjusted) - currentClause.Term.Symbol.StartAdjusted,
                        clauseTip);

                    if (!movedToPosition)
                    {
                        this.sourceEditor.Editor.GotoPosition(currentClause.Term.Symbol.StartAdjusted);
                    }
                }

                RefreshNodes(varStack, callStack, currentClause);
            }));

            // Await user input
            this._debugStepWaitHandle.WaitOne();

            this.Invoke(new Action(() =>
            {
                this.sourceEditor.Editor.ReadOnly = false;
                this.debuggerPanel.Hide();
            }));

            if (this._lastDebuggerUserCommand == DebuggerUserCommand.Stop)
            {
                this.Invoke(new Action(() =>
                {
                    this.sourceEditor.ClearIndicators();
                    this._lastDebuggerUserCommand = null;
                    this._lastLevel = null;
                }));

                throw new AbortQueryException();
            }

            this._lastLevel = callStack.Count;
            return false;
        }

        private bool HasBreakpoint(BaseTerm term)
        {
            if (term == null)
            {
                return false;
            }

            const uint mask = (1 << PrologEditor.BREAKPOINT_MARKER);

            Line line1 = sourceEditor.Editor.Lines[sourceEditor.Editor.LineFromPosition(term.Symbol.Start + 1)];
            Line line2 = sourceEditor.Editor.Lines[sourceEditor.Editor.LineFromPosition(term.Symbol.Final - 1)];
            return (line1.MarkerGet() & mask) > 0 || (line2.MarkerGet() & mask) > 0;
        }

        public void RefreshNodes(VarStack vatStack, Stack<CallReturn> callStack, TermNode curreNode)
        {
            callStackView.Nodes.Clear();
            TreeNode parent = new TreeNode("Query", GroupIcon, GroupIcon);
            callStackView.Nodes.Add(parent);
            foreach (var call in callStack.ToArray().Reverse())
            {
                TreeNode current = new TreeNode(call.SavedGoal.ToString(), TestIcon, TestIcon);
                parent.Nodes.Add(current);
                parent = current;
            }

            callStackView.ExpandAll();

            variablesView.Nodes.Clear();
            foreach (var variable in vatStack.ToArray())
            {
                BaseTerm term = variable as BaseTerm;
                if (term != null)
                {
                    TreeNode current = new TreeNode(term.ToString(), TestIcon, TestIcon);
                    variablesView.Nodes.Add(current);
                }
            }
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

        private void debuggerContinueButton_Click(object sender, EventArgs e)
        {
            _lastDebuggerUserCommand = DebuggerUserCommand.Continue;
            _debugStepWaitHandle.Set();
        }

        private void debuggerStepIntoButton_Click(object sender, EventArgs e)
        {
            _lastDebuggerUserCommand = DebuggerUserCommand.StepInto;
            _debugStepWaitHandle.Set();
        }

        private void debuggerStopButton_Click(object sender, EventArgs e)
        {
            _lastDebuggerUserCommand = DebuggerUserCommand.Stop;
            _debugStepWaitHandle.Set();
        }

        private void debuggerStepOverButton_Click(object sender, EventArgs e)
        {
            _lastDebuggerUserCommand = DebuggerUserCommand.StepOver;
            _debugStepWaitHandle.Set();
        }

        private void deuggerStepOutButton_Click(object sender, EventArgs e)
        {
            _lastDebuggerUserCommand = DebuggerUserCommand.StepOut;
            _debugStepWaitHandle.Set();
        }

        private void sourceKeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.F11 && debuggerPanel.Visible)
            {
                e.SuppressKeyPress = true;
                debuggerStepIntoButton_Click(null, null);
            }
            else if (e.KeyCode == Keys.F10 && debuggerPanel.Visible)
            {
                e.SuppressKeyPress = true;
                debuggerStepOverButton_Click(null, null);
            }
            else if (e.KeyCode == Keys.F12 && debuggerPanel.Visible)
            {
                e.SuppressKeyPress = true;
                deuggerStepOutButton_Click(null, null);
            }
            else if (e.KeyCode == Keys.F5 && debuggerPanel.Visible)
            {
                e.SuppressKeyPress = true;
                debuggerContinueButton_Click(null, null);
            }
        }
    }
}
