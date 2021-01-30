using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Windows.Forms;
using ScintillaNET;
using WeifenLuo.WinFormsUI.Docking;
using static Prolog.PrologEngine;

namespace Prolog
{
    public enum DebuggerUserCommand
    {
        Continue, Stop, StepOver, StepInto, StepOut
    }

    public partial class DebuggerArea : DockContent
    {
        private const int GroupIcon = 0;
        private const int TestIcon = 1;

        private readonly AutoResetEvent _debugStepWaitHandle;

        private readonly PrologEngine pe;
        private readonly SourceArea sourceArea;
        private readonly PrologEditor sourceEditor;
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

        private bool HandleDebugEvent(TermNode goalNode, TermNode currentClause, bool isReturn, VarStack varStack,
            Stack<CallReturn> callStack)
        {
            if (_lastLevel.HasValue && _lastDebuggerUserCommand == DebuggerUserCommand.StepOver &&
                callStack.Count > _lastLevel)
            {
                return false;
            }

            if (_lastLevel.HasValue && _lastDebuggerUserCommand == DebuggerUserCommand.StepOut &&
                callStack.Count >= _lastLevel)
            {
                return false;
            }

            if (goalNode?.PredDescr?.IsPredefined == true && currentClause != null)
            {
                return false;
            }

            bool shouldBreak = false;

            Invoke(new Action(() => shouldBreak = HasBreakpoint(currentClause?.Head)));

            if (!shouldBreak &&
                (_lastDebuggerUserCommand == DebuggerUserCommand.Continue
                 || _lastDebuggerUserCommand == null))
            {
                return false;
            }

            double totalCpuSeconds = pe.ProcessorTime();
            // Limited timer resolution - ignore when smaller than 100ms
            string procTimeString = totalCpuSeconds < 0.1 ? "" : $" ({totalCpuSeconds:f3} s)";

            Invoke(new Action(() =>
            {
                sourceEditor.Editor.ReadOnly = true;
                debuggerPanel.Show();
                sourceEditor.ClearIndicators();
                bool movedToPosition = false;

                // Mark the goal
                if (goalNode?.Head?.Symbol != null && goalNode.Head.Symbol.FinalAdjusted > 0)
                {
                    sourceEditor.IndicatorFillRange(
                        PrologEditor.DebugLineIndicator,
                        goalNode.Head.Symbol.StartAdjusted,
                        goalNode.Head.Symbol.FinalAdjusted - goalNode.Head.Symbol.StartAdjusted,
                        goalNode.Head.ToDisplayString() + procTimeString);

                    sourceEditor.Editor.GotoPosition(goalNode.Head.Symbol.StartAdjusted);
                    movedToPosition = true;
                }

                // Open calltip
                string clauseTip = (currentClause?.Head?.ToDisplayString() ?? goalNode.ToString() ?? string.Empty) +
                                   procTimeString;
                sourceEditor.Editor.CallTipSetPosition(true);
                sourceEditor.Editor.CallTipShow(
                    currentClause?.Head?.Symbol?.StartAdjusted ?? goalNode?.Head?.Symbol?.StartAdjusted ??
                    sourceEditor.Editor.CurrentPosition, clauseTip);
                sourceEditor.InitialCallTipDisplay = true;

                // Mark the clause
                if (currentClause?.Head?.Symbol != null && currentClause.Head.Symbol.FinalAdjusted > 0)
                {
                    sourceEditor.IndicatorFillRange(
                        PrologEditor.DebugLineIndicator,
                        currentClause.Head.Symbol.StartAdjusted,
                        (currentClause.GetLastNode().Head?.Symbol?.FinalAdjusted ??
                         currentClause.Head.Symbol.FinalAdjusted) - currentClause.Head.Symbol.StartAdjusted,
                        clauseTip);

                    if (!movedToPosition)
                    {
                        sourceEditor.Editor.GotoPosition(currentClause.Head.Symbol.StartAdjusted);
                    }
                }

                RefreshNodes(varStack, callStack, currentClause);
            }));

            // Await user input
            _debugStepWaitHandle.WaitOne();

            Invoke(new Action(() =>
            {
                sourceEditor.Editor.ReadOnly = false;
                debuggerPanel.Hide();
            }));

            if (_lastDebuggerUserCommand == DebuggerUserCommand.Stop)
            {
                Invoke(new Action(() =>
                {
                    sourceEditor.ClearIndicators();
                    _lastDebuggerUserCommand = null;
                    _lastLevel = null;
                }));

                throw new AbortQueryException();
            }

            _lastLevel = callStack.Count;
            return false;
        }

        private bool HasBreakpoint(BaseTerm term)
        {
            if (term == null)
            {
                return false;
            }

            const uint mask = 1 << PrologEditor.BREAKPOINT_MARKER;

            Line line1 = sourceEditor.Editor.Lines[sourceEditor.Editor.LineFromPosition(term.Symbol.StartAdjusted + 1)];
            Line line2 = sourceEditor.Editor.Lines[sourceEditor.Editor.LineFromPosition(term.Symbol.FinalAdjusted - 1)];
            return (line1.MarkerGet() & mask) > 0 || (line2.MarkerGet() & mask) > 0;
        }

        public void RefreshNodes(VarStack vatStack, Stack<CallReturn> callStack, TermNode curreNode)
        {
            callStackView.Nodes.Clear();
            TreeNode parent = new TreeNode("Query", GroupIcon, GroupIcon);
            callStackView.Nodes.Add(parent);
            foreach (CallReturn call in callStack.ToArray().Reverse())
            {
                TreeNode current = new TreeNode(call.SavedGoal.ToString(), TestIcon, TestIcon);
                parent.Nodes.Add(current);
                parent = current;
            }

            callStackView.ExpandAll();

            variablesView.Nodes.Clear();
            foreach (object variable in vatStack.ToArray())
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