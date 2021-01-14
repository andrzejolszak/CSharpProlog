using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Threading.Tasks;
using System.Windows.Forms;
using ScintillaNET;
using WeifenLuo.WinFormsUI.Docking;
using static Prolog.PrologEngine;
using static Prolog.PrologEngine.BaseParser;

namespace Prolog
{
    public partial class SourceArea : DockContent
    {
        private readonly Timer _stoppedTypingTimer;
        private readonly string namePrefix = "";
        private readonly PrologEngine pe;
        private readonly Action<ProgressChangedEventArgs> progressChangedHandler;
        private readonly TextBox tbAnswer;
        private readonly OutputArea winIO;
        private string _currentFile = "Untitled";
        public PrologEditor sourceEditor;
        private PrologEngine stoppedTypingEngine;
        private Task stoppedTypingParseTask;
        private bool wasModified;

        public SourceArea(PrologEngine pe, OutputArea winIO, TextBox tbAnswer,
            Action<ProgressChangedEventArgs> progressChangedHandler)
        {
            InitializeComponent();

            this.pe = pe;
            this.winIO = winIO;
            this.progressChangedHandler = progressChangedHandler;
            this.tbAnswer = tbAnswer;

            CloseButton = false;
            CloseButtonVisible = false;

            sourceEditor = new PrologEditor(pe);
            Scintilla scintillaSource = sourceEditor.Editor;

            scintillaSource.Anchor = AnchorStyles.Top | AnchorStyles.Left
                                                      | AnchorStyles.Right | AnchorStyles.Bottom;
            scintillaSource.Location = new Point(0, 0);
            scintillaSource.Size = sourcePanel.Size;
            scintillaSource.TabIndex = 19;
            scintillaSource.TabStop = true;
            scintillaSource.ScrollWidth = 400;

            _stoppedTypingTimer = new Timer();
            _stoppedTypingTimer.Interval = 2000;
            _stoppedTypingTimer.Tick += OnStoppedTyping;

            scintillaSource.KeyDown += sourceKeyDown;
            scintillaSource.TextChanged += textChanged;

            sourcePanel.Controls.Add(scintillaSource);

            sourceEditor.Map.Anchor = AnchorStyles.Top | AnchorStyles.Left
                                                       | AnchorStyles.Right | AnchorStyles.Bottom;
            sourceEditor.Map.Height = scintillaSource.Height;
            mapPanel.Controls.Add(sourceEditor.Map);

            RefreshTitle();

            pe.FoundAllSolutions += () =>
            {
                BeginInvoke(new Action(() =>
                {
                    sourceEditor.ClearIndicators();
                }));
            };

            sourceEditor.DrawDependencyArrows = true;
        }

        public event Action SourceConsultedSuccess;

        public event Action SourceConsultedError;

        private void ResetBackgroundWorker()
        {
            if (bgwLoadSource != null)
            {
                bgwLoadSource.CancelAsync();
                bgwLoadSource.DoWork -= bgwLoadSource_DoWork;
                bgwLoadSource.ProgressChanged -= bgwLoadSource_ProgressChanged;
                bgwLoadSource.RunWorkerCompleted -= bgwLoadSource_RunWorkerCompleted;
                bgwLoadSource.Dispose();
            }

            bgwLoadSource = new BackgroundWorker();
            bgwLoadSource.WorkerReportsProgress = true;
            bgwLoadSource.WorkerSupportsCancellation = true;
            bgwLoadSource.DoWork += bgwLoadSource_DoWork;
            bgwLoadSource.ProgressChanged += bgwLoadSource_ProgressChanged;
            bgwLoadSource.RunWorkerCompleted += bgwLoadSource_RunWorkerCompleted;
        }

        private void OnStoppedTyping(object sender, EventArgs e)
        {
            _stoppedTypingTimer.Stop();

            if (sourceEditor.Editor.Lines.Count > 10000)
            {
                // Don't do background consult on large files
                return;
            }

            if (stoppedTypingParseTask == null || stoppedTypingParseTask.IsFaulted)
            {
                ResetBackgroundWorker();
                winIO.GuiIO.bgw = bgwLoadSource;
                IO.BasicIO = winIO.GuiIO;
                stoppedTypingEngine = new PrologEngine(false);
                string text = sourceEditor.Editor.Text;

                stoppedTypingParseTask = new Task(() =>
                {
                    try
                    {
                        stoppedTypingEngine.ConsultFromString(text);
                        Invoke(new Action(() =>
                        {
                            tbAnswer.Clear();
                            sourceEditor.RefreshAutocompletionItems(stoppedTypingEngine);
                            sourceEditor.RefreshReferenceArrows(stoppedTypingEngine);
                        }));
                    }
                    catch (PrologException ex)
                    {
                        Invoke(new Action(() =>
                        {
                            sourceEditor.RefreshReferenceArrows(null);
                            sourceEditor.ClearIndicators();
                            tbAnswer.Clear();

                            Symbol symbol = ex.GetBestEffortSymbol;
                            if (symbol != null)
                            {
                                sourceEditor.IndicatorFillRange(PrologEditor.SquiggleIndicator,
                                    symbol.Start, symbol.Final - symbol.Start, ex.Message);
                            }

                            tbAnswer.Write($"^ Error at line {symbol?.LineNo}: {ex.Message}");
                        }));
                    }

                    stoppedTypingParseTask = null;
                });

                stoppedTypingParseTask.Start();
            }
        }

        private void textChanged(object sender, EventArgs e)
        {
            wasModified = true;
            RefreshTitle();
            tbAnswer.Clear();
            _stoppedTypingTimer.Stop();
            _stoppedTypingTimer.Start();
        }

        private void sourceKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control && e.KeyCode == Keys.Space)
            {
                _stoppedTypingTimer.Stop();
            }
            else if (e.Shift && e.KeyCode == Keys.Enter)
            {
                e.SuppressKeyPress = true;
                consultButton_Click(sender, null);
            }
            else if (e.Control && e.KeyCode == Keys.F1)
            {
                e.SuppressKeyPress = true;
                // this.queryEditor.Editor.Focus();
            }
        }

        private void RefreshTitle()
        {
            Text = namePrefix + _currentFile + (wasModified ? "*" : "") + (sourceEditor.IsDirty ? "" : " [consulted]");
        }

        private void consultButton_Click(object sender, EventArgs e)
        {
            if (bgwLoadSource.IsBusy)
            {
                return;
            }

            _stoppedTypingTimer.Stop();

            consultButton.BackColor = Color.LightBlue;
            tbAnswer.Clear();
            pe.Reset();

            winIO.GuiIO.bgw = bgwLoadSource;
            bgwLoadSource.RunWorkerAsync(sourceEditor.Editor.Text);
        }

        private void bgwLoadSource_ProgressChanged(object sender, ProgressChangedEventArgs e)
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

        private void bgwLoadSource_DoWork(object sender, DoWorkEventArgs e)
        {
            pe.ConsultFromString((string)e.Argument);
        }

        private void bgwLoadSource_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            consultButton.BackColor = BackColor;
            if (e.Error != null)
            {
                sourceEditor.ClearIndicators();
                tbAnswer.Clear();

                Symbol symbol = (e.Error as PrologException)?.GetBestEffortSymbol;
                if (symbol != null)
                {
                    sourceEditor.IndicatorFillRange(PrologEditor.SquiggleIndicator,
                        symbol.Start, symbol.Final - symbol.Start, e.Error.Message);
                    sourceEditor.Editor.GotoPosition(symbol.Start);
                }

                tbAnswer.Write($"^ Error at line {symbol?.LineNo}: {e.Error.Message}");
                
                SourceConsultedError?.Invoke();
            }
            else
            {
                tbAnswer.WriteLine("OK!");

                sourceEditor.MarkAsClean();
                RefreshTitle();

                SourceConsultedSuccess?.Invoke();
            }
        }

        public bool AskCanLoseCurrentSource()
        {
            if (wasModified)
            {
                DialogResult result = MessageBox.Show(this, "Changes to the current document will be lost.\nContinue?",
                    "Please Confirm", MessageBoxButtons.OKCancel, MessageBoxIcon.Question);
                return result == DialogResult.OK;
            }

            return true;
        }

        public void newToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (!AskCanLoseCurrentSource())
            {
                return;
            }

            pe.Reset();
            sourceEditor.Editor.ClearAll();
            _currentFile = "Untitled";
            wasModified = false;
            RefreshTitle();
        }

        public void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (!AskCanLoseCurrentSource())
            {
                return;
            }

            // Create an instance of the open file dialog box.
            OpenFileDialog openFileDialog1 = new OpenFileDialog();

            // Set filter options and filter index.
            openFileDialog1.Filter = "Prolog Files (.pl)|*.pl|All Files (*.*)|*.*";
            openFileDialog1.FilterIndex = 1;

            openFileDialog1.Multiselect = true;

            // Call the ShowDialog method to show the dialog box.
            DialogResult result = openFileDialog1.ShowDialog();

            // Process input if the user clicked OK.
            if (result == DialogResult.OK)
            {
                // Open the selected file to read.
                Stream fileStream = openFileDialog1.OpenFile();

                OpenFile(openFileDialog1.FileName, fileStream);
            }
        }

        public void OpenFile(string fileName, Stream fileStream)
        {
            using (StreamReader reader = new StreamReader(fileStream))
            {
                sourceEditor.Editor.Text = reader.ReadToEnd();
            }

            fileStream.Close();

            _currentFile = fileName;
            wasModified = false;
            RefreshTitle();
        }

        public void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_currentFile == "Untitled")
            {
                saveAsToolStripMenuItem_Click(sender, e);
                return;
            }

            FileStream fs = new FileStream(_currentFile, FileMode.Truncate);
            StreamWriter sw = new StreamWriter(fs);
            sw.Write(sourceEditor.Editor.Text);
            sw.Close();
            fs.Close();

            wasModified = false;
            RefreshTitle();
        }

        public void saveAsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            SaveFileDialog saveFileDialog1 = new SaveFileDialog();
            saveFileDialog1.Filter = "Prolog Files (.pl)|*.pl|All Files (*.*)|*.*";
            saveFileDialog1.Title = "Save an Prolog File";
            saveFileDialog1.ShowDialog();

            // If the file name is not an empty string open it for saving.
            if (saveFileDialog1.FileName != "")
            {
                FileStream fs = (FileStream)saveFileDialog1.OpenFile();
                StreamWriter sw = new StreamWriter(fs);
                sw.Write(sourceEditor.Editor.Text);
                sw.Close();
                fs.Close();

                _currentFile = saveFileDialog1.FileName;
                wasModified = false;
                RefreshTitle();
            }
        }

        public void MainForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (!AskCanLoseCurrentSource())
            {
                e.Cancel = true;
            }
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            sourceEditor.DrawDependencyArrows = checkBox1.Checked;
        }
    }
}