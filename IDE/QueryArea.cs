﻿using ScintillaNET;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Threading;
using System.Windows.Forms;
using static Prolog.PrologEngine;

namespace Prolog
{
    public partial class QueryArea : WeifenLuo.WinFormsUI.Docking.DockContent
    {
        public PrologEditor queryEditor;

        private bool _findAllSolutions;
        private readonly List<string> queryCallStack = new List<string>();
        private readonly TextBox tbAnswer;
        private readonly PrologEngine pe;
        private readonly OutputArea winIO;
        private bool? stop;
        private ManualResetEvent semaMoreStop;
        private readonly Action<ProgressChangedEventArgs> progressChangedHandler;
        private readonly SourceArea sourceArea;

        public QueryArea(PrologEngine pe, SourceArea sourceArea, OutputArea winIO, TextBox tbAnswer, Action<ProgressChangedEventArgs> progressChangedHandler)
        {
            InitializeComponent();

            Text = "Query";

            CloseButton = false;
            CloseButtonVisible = false;

            this.pe = pe;
            this.sourceArea = sourceArea;
            this.winIO = winIO;
            this.tbAnswer = tbAnswer;
            this.progressChangedHandler = progressChangedHandler;

            stop = null;

            queryEditor = new PrologEditor(pe);
            Scintilla scintillaQuery = queryEditor.Editor;

            scintillaQuery.Anchor = (AnchorStyles.Top | AnchorStyles.Left)
                                    | AnchorStyles.Right | AnchorStyles.Bottom;
            scintillaQuery.Location = new Point(0, 0);
            scintillaQuery.Size = queryPanel.Size;
            scintillaQuery.TabIndex = 20;
            scintillaQuery.TabStop = true;
            scintillaQuery.ScrollWidth = 200;

            scintillaQuery.KeyDown += queryKeyDown;

            queryPanel.Controls.Add(scintillaQuery);

            bgwExecuteQuery.DoGuiAction(GuiAction.BtnsOff);

            btnMore.Enabled = btnStop.Enabled = false;
        }

        private void queryKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Shift && e.KeyCode == Keys.Enter)
            {
                e.SuppressKeyPress = true;
                SetDebug(false);
                ExecuteQuery(true);
            }
            else if (e.Control && e.KeyCode == Keys.F1)
            {
                //TODO: this.sourceEditor.Editor.Focus();
                e.SuppressKeyPress = true;
            }
        }

        private void btnXeqQuery_Click(object sender, EventArgs e)
        {
            SetDebug(false);
            ExecuteQuery(false);
        }

        private void SetDebug(bool val)
        {
            // TODO: pe.EventDebug = val;
        }

        private void ExecuteQuery(bool findAllSolutions)
        {
            if (bgwExecuteQuery.IsBusy && !bgwExecuteQuery.CancellationPending) return;

            btnXeqQuery.BackColor = Color.LightBlue;

            tbAnswer.Clear();
            btnCancelQuery.Enabled = true;
            btnMore.Enabled = btnStop.Enabled = false;
            _findAllSolutions = findAllSolutions;

            queryCallStack.Clear();
            pe.OnCurrentTermChanged -= CollectCallStack;

            if (checkBox1.Checked)
            {
                pe.OnCurrentTermChanged += CollectCallStack;
            }

            winIO.GuiIO.bgw = bgwExecuteQuery;
            bgwExecuteQuery.RunWorkerAsync(queryEditor.Editor.Text);
        }

        private void CollectCallStack(TermNode termNode)
        {
            queryCallStack.Add(termNode.ToString());
            if (queryCallStack.Count > 3)
            {
                queryCallStack.RemoveAt(0);
            }
        }

        private void bgwExecuteQuery_DoWork(object sender, DoWorkEventArgs e)
        {
            if (_findAllSolutions)
            {
                SolutionSet solutions = pe.GetAllSolutions(null, e.Argument as string, 0);

                if (solutions.HasError)
                {
                    winIO.GuiIO.WriteLine("Errors: \r\n" + solutions.ErrMsg);
                }
                else if (solutions.Success)
                {
                    winIO.GuiIO.WriteLine("yes\r\n" + solutions);
                }
                else
                {
                    winIO.GuiIO.WriteLine("no\r\n" + solutions);
                }
                /* TODO: migrate
                foreach (PrologEngine.RuntimeException error in solutions.ErrMsg.Errors)
                {
                    PrologEngine.BaseParser.Symbol symbol = error.Term?.Symbol ??
                                    (error.VarStack?.Peek() as PrologEngine.BaseTerm)?.Symbol;
                    winIO.Write($"* Line {symbol?.LineNo}: {error.Message}");
                    this.Invoke(new Action(() => this.sourceArea.sourceEditor.Editor.GotoPosition(symbol?.StartAdjusted ?? this.sourceArea.sourceEditor.Editor.CurrentPosition)));
                }
                */

                winIO.GuiIO.Write(solutions.ErrMsg);

                if (!solutions.Success && queryCallStack.Count > 0)
                {
                    winIO.GuiIO.Write(queryCallStack.Aggregate((x, y) => x + "---" + y));
                }
            }
            else
            {
             pe.Query = e.Argument as string;
             semaMoreStop = new ManualResetEvent(false);

             foreach (PrologEngine.ISolution s in pe.SolutionIterator)
             {
                 winIO.GuiIO.WriteLine("{0}{1}", s, (s.IsLast ? null : ";"));

                 if (s.IsLast && !s.Solved && queryCallStack.Count > 0)
                 {
                     winIO.GuiIO.Write(queryCallStack.Aggregate((x, y) => x + "---" + y));
                 }

                 if (s.IsLast) break;

                 bool stop;
                 btnMore.Enabled = btnStop.Enabled = true;
                 WaitForMoreOrStopPressed(out stop);
                 semaMoreStop.Reset();

                 if (stop) break;
             }
            }
        }


        private void WaitForMoreOrStopPressed(out bool halt)
        {
            bgwExecuteQuery.DoGuiAction(GuiAction.BtnsOn);

            try
            {
                semaMoreStop.WaitOne();
            }
            finally
            {
                halt = stop ?? false;
                stop = null;
                bgwExecuteQuery.DoGuiAction(GuiAction.BtnsOff);
            }
        }


        private void btnMore_Click(object sender, EventArgs e)
        {
            stop = false;
            semaMoreStop.Set();
        }


        private void btnStop_Click(object sender, EventArgs e)
        {
            stop = true;
            semaMoreStop.Set();
        }

        // click event for (now invisible) Cancel-button, which does not work as expected.
        // (execution does not get interrupted, have to sort out why this does not work)
        private void btnCancelQuery_Click(object sender, EventArgs e)
        {
            bgwExecuteQuery.CancelAsync();
            btnXeqQuery.Enabled = true;

            while (bgwExecuteQuery.CancellationPending)
            {
                Application.DoEvents();
                Thread.Sleep(10);
            }
        }


        private void bgwExecuteQuery_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            btnCancelQuery.Enabled = false;
            btnMore.Enabled = btnStop.Enabled = false;
            btnXeqQuery.Enabled = true;

            btnXeqQuery.BackColor = BackColor;
        }


        private void bgwExecuteQuery_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            if (InvokeRequired)
            {
                Invoke(new Action(() => progressChangedHandler(e)));
            }
            else
            {
                progressChangedHandler(e);
            }
        }

        private void debugBtn_Click(object sender, EventArgs e)
        {
            if (!sourceArea.sourceEditor.IsDirty)
            {
                sourceArea.Focus();
                SetDebug(true);
                ExecuteQuery(true);
            }
            else
            {
                tbAnswer.AppendText("Please consult the current editor contents.");
            }
        }
    }
}
