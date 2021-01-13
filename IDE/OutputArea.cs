using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;
using WeifenLuo.WinFormsUI.Docking;

namespace Prolog
{
    public enum GuiAction
    {
        None, ReadStart, ReadEnd, ReadLn, ReadCh, Write, WriteLn, NewLn, Clear, Reset, BtnsOn, BtnsOff
    }

    public partial class OutputArea : DockContent
    {
        private readonly Queue<int> charBuffer;

        private readonly ManualResetEvent semaGetInput;
        public WinIO GuiIO;
        private GuiAction readMode; // for distinguishing between various ways of reading input

        public OutputArea()
        {
            InitializeComponent();

            CloseButton = false;
            CloseButtonVisible = false;

            Text = "Output/Input";

            readMode = GuiAction.None;
            semaGetInput = new ManualResetEvent(false);
            charBuffer = new Queue<int>();
            GuiIO = new WinIO(semaGetInput, tbInput, charBuffer);
        }

        private void tbInput_KeyDown(object sender, KeyEventArgs e)
        {
            if (cbNewLines.Checked && e.KeyCode == Keys.Enter)
            {
                if (readMode == GuiAction.ReadCh)
                {
                    foreach (char c in tbInput.Text)
                    {
                        charBuffer.Enqueue(c);
                    }

                    foreach (char c in Environment.NewLine)
                    {
                        charBuffer.Enqueue(c);
                    }
                }

                e.Handled = true;
                e.SuppressKeyPress = true;
                semaGetInput.Set();
            }
        }

        private void btnEnter_Click(object sender, EventArgs e)
        {
            if (readMode == GuiAction.ReadCh)
            {
                foreach (char c in Environment.NewLine)
                {
                    charBuffer.Enqueue(c);
                }
            }

            semaGetInput.Set();
        }

        private void cbNewLines_CheckedChanged(object sender, EventArgs e)
        {
            btnEnter.Visible = !cbNewLines.Checked;
        }

        private void btnClearA_Click(object sender, EventArgs e)
        {
            tbAnswer.Clear();
        }

        public void HandleProgressChanged(ProgressChangedEventArgs e)
        {
            switch ((GuiAction)e.ProgressPercentage)
            {
                case GuiAction.ReadStart:
                    tbInput.Text = null;
                    tbInput.Enabled = true;
                    tbInput.BackColor = Color.LightYellow;
                    tbInput.Focus();
                    break;

                case GuiAction.ReadEnd:
                    tbInput.Enabled = false;
                    tbInput.BackColor = Color.White;
                    break;

                case GuiAction.ReadLn:
                    readMode = GuiAction.ReadLn;
                    break;

                case GuiAction.ReadCh:
                    readMode = GuiAction.ReadCh;
                    break;

                case GuiAction.Write:
                    tbAnswer.Write(e.UserState as string);
                    break;

                case GuiAction.WriteLn:
                    tbAnswer.WriteLine(e.UserState as string);
                    break;

                case GuiAction.NewLn:
                    tbAnswer.WriteLine();
                    break;

                case GuiAction.Clear:
                    tbAnswer.Clear();
                    break;

                case GuiAction.Reset:
                    tbInput.Clear();
                    break;

                case GuiAction.BtnsOn:
                    break;

                case GuiAction.BtnsOff:
                    break;
            }
        }

        public class WinIO : BasicIo
        {
            public BackgroundWorker bgw;
            private readonly Queue<int> charBuffer;
            private readonly ManualResetEvent semaGetInput;
            private readonly TextBox tbInput;

            public WinIO(ManualResetEvent semaGetInput,
                TextBox tbInput, Queue<int> charBuffer)
            {
                this.semaGetInput = semaGetInput;
                this.tbInput = tbInput;
                this.charBuffer = charBuffer;
            }

            public override string ReadLine()
            {
                try
                {
                    bgw.DoGuiAction(GuiAction.ReadStart);
                    bgw.DoGuiAction(GuiAction.ReadLn);
                    semaGetInput.WaitOne(); // wait until text has been entered in tbInput

                    return tbInput.Text;
                }
                finally
                {
                    semaGetInput.Reset();
                    bgw.DoGuiAction(GuiAction.ReadEnd);
                }
            }

            public override int ReadChar()
            {
                if (charBuffer.Count == 0)
                {
                    try
                    {
                        bgw.DoGuiAction(GuiAction.ReadStart);
                        bgw.DoGuiAction(GuiAction.ReadCh);
                        semaGetInput.WaitOne(); // wait until charBuffer is not empty
                    }
                    finally
                    {
                        semaGetInput.Reset();
                        bgw.DoGuiAction(GuiAction.ReadEnd);
                    }
                }

                return charBuffer.Dequeue();
            }

            public override void Write(string s)
            {
                bgw.DoGuiAction(GuiAction.Write, s);
            }

            public override void WriteLine(string s)
            {
                bgw.DoGuiAction(GuiAction.WriteLn, s);
            }

            public override void WriteLine()
            {
                bgw.DoGuiAction(GuiAction.NewLn);
            }

            public override void Clear()
            {
                bgw.DoGuiAction(GuiAction.Clear);
            }

            public override void Reset()
            {
                bgw.DoGuiAction(GuiAction.Reset);
            }
        }
    }
}