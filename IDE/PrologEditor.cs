//using ScintillaNET_FindReplaceDialog;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Linq;
using System.Resources;
using System.Windows.Forms;
using AutocompleteMenuNS;
using ScintillaNET;
using static Prolog.PrologEngine;

namespace Prolog
{
    public class PrologEditor
    {
        public const int BREAKPOINT_MARGIN = 1; // Conventionally the symbol margin
        public const int BREAKPOINT_MARKER = 3; // Arbitrary. Any valid index would work.

        public static readonly int SquiggleIndicator = 9;
        public static readonly int StaticWarningIndicator = 10;
        public static readonly int DebugLineIndicator = 11;
        public static readonly int HighlightIndicator = 8;
        private readonly AutocompleteMenu _autocompleteMenu;
        private readonly Pen _dependencyArrowPen;
        private readonly Pen _dependencyArrowPenBlowup;

        private readonly Pen _dependencyArrowPenHighlight;

        //private readonly FindReplace _findReplaceDialog;
        private readonly ImageList _imageList1;

        private readonly List<Tuple<int, int, string>> _indicatorDescriptions = new List<Tuple<int, int, string>>();
        private readonly ToolStripMenuItem _inspectTermContextMenuItem;
        private readonly Dictionary<GraphicsPath, Pen> _paths = new Dictionary<GraphicsPath, Pen>();
        private readonly Dictionary<GraphicsPath, int> _pathsToStartPos = new Dictionary<GraphicsPath, int>();
        private readonly PrologEngine prologEngine;

        private Point _mouseLocation = new Point(0, 0);
        private Dictionary<string, PredicateDescr> _referenceArrowsPredTable;
        private string callTip;

        public Scintilla Editor;
        private int lastCaretPos;

        private int lastDrawnPosition;
        private int lastFirstVisibleLine;
        private bool lastWasNull;
        public Panel Map;

        private int? openParenPosition;

        private int positionToMoveTo = -1;

        public PrologEditor(PrologEngine prologEngine)
        {
            Editor = new Scintilla();
            Editor.Painted += EditorPainted;
            this.prologEngine = prologEngine;

            // Configuring the default style with properties
            // we have common to every lexer style saves time.
            Editor.StyleResetDefault();
            Editor.Styles[Style.Default].Font = "Consolas";
            Editor.Styles[Style.Default].Size = 10;
            Editor.Styles[Style.Default].ForeColor = Color.Black;

            Editor.StyleClearAll();

            Editor.Styles[Style.LineNumber].ForeColor = Color.DarkSlateBlue;
            Editor.Styles[Style.LineNumber].Size = 9;

            Editor.Margins[0].Width = 45;
            Editor.Margins[0].Cursor = MarginCursor.Arrow;

            /*
            // Some properties we like
            Editor.SetProperty("tab.timmy.whinge.level", "1");
            Editor.SetProperty("fold", "1");

            // Use margin 2 for fold markers
            Editor.Margins[2].Type = MarginType.Symbol;
            Editor.Margins[2].Mask = Marker.MaskFolders;
            Editor.Margins[2].Sensitive = true;
            Editor.Margins[2].Width = 20;*/

            Margin breakPointMargin = Editor.Margins[BREAKPOINT_MARGIN];
            breakPointMargin.Width = 16;
            breakPointMargin.Sensitive = true;
            breakPointMargin.Type = MarginType.Symbol;
            breakPointMargin.Mask = Marker.MaskAll;
            breakPointMargin.Cursor = MarginCursor.ReverseArrow;

            Marker marker = Editor.Markers[BREAKPOINT_MARKER];
            marker.Symbol = MarkerSymbol.Circle;
            marker.SetBackColor(Color.IndianRed);
            marker.SetForeColor(Color.DarkRed);

            Editor.MarginClick += OnMarginClick;

            // Buffer margin
            Editor.Margins[4].Type = MarginType.Text;
            Editor.Margins[4].Mask = Marker.MaskFolders;
            Editor.Margins[4].Sensitive = false;
            Editor.Margins[4].Width = 8;
            Editor.Margins[4].Cursor = MarginCursor.Arrow;

            /*
            // Reset folder markers
            for (int i = Marker.FolderEnd; i <= Marker.FolderOpen; i++)
            {
                Editor.Markers[i].SetForeColor(SystemColors.ControlLightLight);
                Editor.Markers[i].SetBackColor(SystemColors.ControlDark);
            }

            // Style the folder markers
            Editor.Markers[Marker.Folder].Symbol = MarkerSymbol.BoxPlus;
            Editor.Markers[Marker.Folder].SetBackColor(SystemColors.ControlText);
            Editor.Markers[Marker.FolderOpen].Symbol = MarkerSymbol.BoxMinus;
            Editor.Markers[Marker.FolderEnd].Symbol = MarkerSymbol.BoxPlusConnected;
            Editor.Markers[Marker.FolderEnd].SetBackColor(SystemColors.ControlText);
            Editor.Markers[Marker.FolderMidTail].Symbol = MarkerSymbol.TCorner;
            Editor.Markers[Marker.FolderOpenMid].Symbol = MarkerSymbol.BoxMinusConnected;
            Editor.Markers[Marker.FolderSub].Symbol = MarkerSymbol.VLine;
            Editor.Markers[Marker.FolderTail].Symbol = MarkerSymbol.LCorner;
            */

            // Enable automatic folding
            //Editor.AutomaticFold = (AutomaticFold.Show | AutomaticFold.Click | AutomaticFold.Change);

            Editor.Styles[Style.BraceLight].BackColor = Color.LightGray;
            Editor.Styles[Style.BraceLight].ForeColor = Color.BlueViolet;
            Editor.Styles[Style.BraceBad].ForeColor = Color.Red;

            Editor.Styles[VisualPrologStyle.Default].ForeColor = Color.Silver;
            Editor.Styles[VisualPrologStyle.Identifier].ForeColor = Color.FromArgb(0, 0, 0); // Black
            Editor.Styles[VisualPrologStyle.CommentLine].ForeColor = Color.FromArgb(0, 128, 0); // Green
            Editor.Styles[VisualPrologStyle.CommentBlock].ForeColor = Color.FromArgb(0, 128, 0); // Green
            Editor.Styles[VisualPrologStyle.Number].ForeColor = Color.Olive;
            Editor.Styles[VisualPrologStyle.Character].ForeColor = Color.Blue;
            Editor.Styles[VisualPrologStyle.Anonymous].ForeColor = Color.Blue;
            Editor.Styles[VisualPrologStyle.String].ForeColor = Color.FromArgb(163, 21, 21); // Red
            Editor.Styles[VisualPrologStyle.Character].ForeColor = Color.FromArgb(163, 21, 21); // Red
            Editor.Styles[VisualPrologStyle.Verbatim].ForeColor = Color.FromArgb(163, 21, 21); // Red
            Editor.Styles[VisualPrologStyle.StringEolOpen].BackColor = Color.Pink;
            Editor.Styles[VisualPrologStyle.Operator].ForeColor = Color.Purple;
            Editor.Styles[VisualPrologStyle.KeyMajor].ForeColor = Color.Magenta;
            Editor.Styles[VisualPrologStyle.KeyDirective].ForeColor = Color.Magenta;
            Editor.Styles[VisualPrologStyle.KeyMinor].ForeColor = Color.Cyan;
            Editor.Styles[VisualPrologStyle.Variable].ForeColor = Color.Blue;
            Editor.Styles[VisualPrologStyle.StringEscape].ForeColor = Color.FromArgb(163, 21, 21); // Red
            Editor.Styles[VisualPrologStyle.StringEscapeError].ForeColor = Color.FromArgb(163, 21, 21); // Red
            Editor.Lexer = (Lexer)107; // VisualProlog

            Editor.Indicators[SquiggleIndicator].Style = IndicatorStyle.Squiggle;
            Editor.Indicators[SquiggleIndicator].ForeColor = Color.Red;

            Editor.Indicators[StaticWarningIndicator].Style = IndicatorStyle.Dots;
            Editor.Indicators[StaticWarningIndicator].ForeColor = Color.DarkSalmon;

            Editor.Indicators[DebugLineIndicator].Style = IndicatorStyle.StraightBox;
            Editor.Indicators[DebugLineIndicator].Alpha = 200;
            Editor.Indicators[DebugLineIndicator].Under = true;
            Editor.Indicators[DebugLineIndicator].ForeColor = Color.DarkOrange;

            Editor.AssignCmdKey(Keys.Alt | Keys.Shift | Keys.Up, Command.MoveSelectedLinesUp);
            Editor.AssignCmdKey(Keys.Alt | Keys.Shift | Keys.Down, Command.MoveSelectedLinesDown);
            Editor.AssignCmdKey(Keys.Control | Keys.D, Command.LineDuplicate);
            Editor.AssignCmdKey(Keys.Control | Keys.E, Command.LineDelete);
            Editor.AssignCmdKey(Keys.Control | Keys.Shift | Keys.C, Command.Cancel);

            // Set the keywords
            Editor.SetKeywords(0, string.Join(" ",
                prologEngine.PredTable.Predicates.Values.Select(x => x.ToString())));
            Editor.SetKeywords(1, string.Join(" ",
                prologEngine.OpTable.Values.Where(x => !x.Name.Contains("^")).Select(x => x.Name)
            ));

            //_findReplaceDialog = new FindReplace(Editor);

            _imageList1 = new ImageList();

            _imageList1.TransparentColor = Color.Transparent;
            _imageList1.Images.Add(Image.FromFile(".\\Resources\\Method_636.ico")); // built-in: 0
            _imageList1.Images.Add(Image.FromFile(".\\Resources\\MethodInstance.png")); // user-defined: 1
            _imageList1.Images.Add(Image.FromFile(".\\Resources\\Atom.png")); // user-defined atoms: 2

            _autocompleteMenu = new AutocompleteMenu();
            _autocompleteMenu.TargetControlWrapper = new ScintillaWrapper(Editor);
            _autocompleteMenu.MaximumSize = new Size(366, 300);
            _autocompleteMenu.ImageList = _imageList1;
            _autocompleteMenu.ToolTipDuration = 30000;
            _autocompleteMenu.AutoPopup = false;

            _autocompleteMenu.Selected += AutocompletionItemSelected;
            Editor.UpdateUI += OnUiUpdate;

            Editor.KeyUp += onKeyUp;
            Editor.DoubleClick += TextDoubleClick;
            Editor.MouseClick += TextMouseClick;
            Editor.KeyDown += onKeyDown;
            Editor.TextChanged += onTextChanged;

            RefreshAutocompletionItems();
            RefreshReferenceArrows();

            Map = new Panel();
            Map.Width = 25;
            Map.BackColor = Color.LightGray;
            Map.Height = Editor.Height - 4;
            Map.Paint += PaintMap;
            Map.MouseDown += HandleMapMouseDown;
            Map.MouseMove += HandleMapMouseMove;
            Map.MouseEnter += MouseEnterLeave;
            Map.MouseLeave += MouseEnterLeave;

            Editor.ContextMenuStrip = new ContextMenuStrip();

            ToolStripMenuItem undo = new ToolStripMenuItem("Undo");
            undo.Click += (x, y) => Editor.Undo();
            undo.ShortcutKeys = Keys.Control | Keys.Z;
            Editor.ContextMenuStrip.Items.Add(undo);

            ToolStripMenuItem redo = new ToolStripMenuItem("Redo");
            redo.Click += (x, y) => Editor.Redo();
            redo.ShortcutKeys = Keys.Control | Keys.Y;
            Editor.ContextMenuStrip.Items.Add(redo);

            Editor.ContextMenuStrip.Items.Add("-");

            ToolStripMenuItem cut = new ToolStripMenuItem("Cut");
            cut.Click += (x, y) => Editor.Cut();
            cut.ShortcutKeys = Keys.Control | Keys.X;
            Editor.ContextMenuStrip.Items.Add(cut);

            ToolStripMenuItem copy = new ToolStripMenuItem("Copy");
            copy.Click += (x, y) => Editor.Copy();
            copy.ShortcutKeys = Keys.Control | Keys.C;
            Editor.ContextMenuStrip.Items.Add(copy);

            ToolStripMenuItem paste = new ToolStripMenuItem("Paste");
            paste.Click += (x, y) => Editor.Paste();
            paste.ShortcutKeys = Keys.Control | Keys.V;
            Editor.ContextMenuStrip.Items.Add(paste);

            ToolStripMenuItem selectAll = new ToolStripMenuItem("Select All");
            selectAll.Click += (x, y) => Editor.SelectAll();
            selectAll.ShortcutKeys = Keys.Control | Keys.A;
            Editor.ContextMenuStrip.Items.Add(selectAll);

            Editor.ContextMenuStrip.Items.Add("-");

            _inspectTermContextMenuItem = new ToolStripMenuItem("Inspect term");
            _inspectTermContextMenuItem.Click += InspectTerm;
            Editor.ContextMenuStrip.Items.Add(_inspectTermContextMenuItem);
            Editor.ContextMenuStrip.Opening += BeforeContextMenuPopup;
            Editor.IndicatorClick += HandleIndicatorClick;
            Editor.MouseMove += Editor_MouseMove;
            Editor.MouseDown += Editor_MouseDown;
            Editor.MouseUp += Editor_MouseUp;

            _dependencyArrowPen = new Pen(Color.FromArgb(255, 10, 50, 255), 1);
            _dependencyArrowPen.CustomEndCap = new AdjustableArrowCap(3, 3, true);
            _dependencyArrowPen.StartCap = LineCap.RoundAnchor;
            _dependencyArrowPenBlowup = (Pen)_dependencyArrowPen.Clone();
            _dependencyArrowPenBlowup.Width = 10;
            _dependencyArrowPenHighlight = (Pen)_dependencyArrowPen.Clone();
            _dependencyArrowPenHighlight.Color = Color.FromArgb(255, 70, 180, 255);
            _dependencyArrowPenHighlight.Width = 3;
        }

        public bool IsDirty { get; private set; }

        public bool DrawDependencyArrows { get; set; }
        public bool InitialCallTipDisplay { get; set; }

        public event Action<BaseTerm> InspectTermRequested;

        private void HandleIndicatorClick(object sender, IndicatorClickEventArgs e)
        {
            Tuple<int, int, string> res =
                _indicatorDescriptions.FirstOrDefault(x => x.Item1 <= e.Position && x.Item2 >= e.Position);

            if (res != null)
            {
                Editor.CallTipSetPosition(true);
                Editor.CallTipShow(e.Position, res.Item3);
                InitialCallTipDisplay = true;
            }
        }

        private void BeforeContextMenuPopup(object sender, EventArgs e)
        {
            BaseTerm predDescr = GetCurrentPositionPredicate();

            if (predDescr != null)
            {
                Editor.SetSelection(predDescr.Symbol.Final, predDescr.Symbol.Start);
                _inspectTermContextMenuItem.Text = $"Inspect Term '{predDescr}'";
                _inspectTermContextMenuItem.Visible = true;
            }
            else
            {
                _inspectTermContextMenuItem.Visible = false;
            }
        }

        internal void MarkAsClean()
        {
            IsDirty = false;
        }

        private void InspectTerm(object sender, EventArgs e)
        {
            BaseTerm predDescr = GetCurrentPositionPredicate(Editor.CurrentPosition - 1);

            if (predDescr != null)
            {
                InspectTermRequested?.Invoke(predDescr);
            }
        }

        private BaseTerm GetCurrentPositionPredicate(int? pos = null)
        {
            if (IsDirty)
            {
                return null;
            }

            int currentPosition = pos ?? Editor.CurrentPosition;
            List<PredicateDescr> predDescrs =
                prologEngine.PredTable.Predicates.Values
                    .Where(x => !x.IsPredefined)
                    .ToList();

            BaseTerm res = FindDepthFirst(predDescrs, currentPosition);
            return res;
        }

        private BaseTerm FindDepthFirst(IEnumerable<PredicateDescr> predDescrs, int pos)
        {
            foreach (TermNode pd in predDescrs.Select(x => x.ClauseList))
            {
                if (pd == null)
                {
                    continue;
                }

                {
                    if (pd.Term != null)
                    {
                        BaseTerm argRes = FindDepthFirstArgs(pd.Term.Args, pos);
                        if (argRes != null)
                        {
                            return argRes;
                        }
                    }

                    if (pd.NextNode != null)
                    {
                        BaseTerm nextNodeRes = FindDepthFirst(new[] { pd.NextNode.PredDescr }, pos);
                        if (nextNodeRes != null)
                        {
                            return nextNodeRes;
                        }
                    }
                }

                if (pd.Term.Symbol.StartAdjusted < pos && pd.Term.Symbol.FinalAdjusted > pos)
                {
                    return pd.Term;
                }
            }

            return predDescrs.FirstOrDefault().ClauseList.Term;
        }

        private BaseTerm FindDepthFirstArgs(IEnumerable<BaseTerm> args, int pos)
        {
            if (args == null)
            {
                return null;
            }

            foreach (BaseTerm pd in args)
            {
                {
                    BaseTerm argRes = FindDepthFirstArgs(pd.Args, pos);
                    if (argRes != null)
                    {
                        return argRes;
                    }
                }

                if (pd.Symbol.StartAdjusted < pos && pd.Symbol.FinalAdjusted > pos)
                {
                    return pd;
                }
            }

            return null;
        }

        private void MouseEnterLeave(object sender, EventArgs e)
        {
            Map.Invalidate();
        }

        private void HandleMapMouseMove(object sender, MouseEventArgs e)
        {
            HandleMapMouseDown(sender, e);
            if (e.Button != MouseButtons.Left)
            {
                Map.Invalidate();
            }
        }

        private void HandleMapMouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                float linesOnScreen = Editor.LinesOnScreen;
                float allLines = Editor.Lines.Count;

                float mouseRelativeScroll = e.Y / (float)Map.Height;
                int firstLine = (int)((mouseRelativeScroll * allLines) - (linesOnScreen / 2.0));
                int lastLine = (int)((mouseRelativeScroll * allLines) + (linesOnScreen / 2.0));

                Editor.ScrollRange(Editor.Lines[lastLine].Position, Editor.Lines[lastLine].Position);
                Editor.ScrollRange(Editor.Lines[firstLine].Position, Editor.Lines[firstLine].Position);
            }
        }

        private void PaintMap(object sender, PaintEventArgs e)
        {
            Point mousePos = Map.PointToClient(Cursor.Position);

            float firstLineOnScreen = Editor.FirstVisibleLine;
            float linesOnScreen = Editor.LinesOnScreen;
            float allLines = Editor.Lines.Count;
            float caretLine = Editor.CurrentLine;
            float mapHeight = Map.Height;

            // Current range
            float rangeY = mapHeight * firstLineOnScreen / allLines;
            float rangeHeight = mapHeight * linesOnScreen / allLines;
            bool highlight = mousePos.Y >= rangeY && mousePos.Y <= rangeY + rangeHeight && mousePos.X > 0 &&
                             mousePos.X <= 25;
            Brush rangeBrush = new SolidBrush(highlight ? Color.DeepSkyBlue : Color.LightSkyBlue);
            e.Graphics.FillRectangle(rangeBrush,
                0, rangeY,
                20, rangeHeight);

            // Caret
            float caretInRange = mapHeight * caretLine / allLines;
            e.Graphics.FillRectangle(new SolidBrush(Color.DarkSlateBlue),
                0, caretInRange,
                25, 4);

            // Indicators
            DrawIndicator(e, allLines, mapHeight, Editor.Indicators[HighlightIndicator], Color.MediumOrchid, 4, false);
            //DrawIndicator(e, allLines, mapHeight, _findReplaceDialog.Indicator, Color.MediumOrchid, 4, false);
            DrawIndicator(e, allLines, mapHeight, Editor.Indicators[SquiggleIndicator], Color.OrangeRed, 12, true);
            DrawIndicator(e, allLines, mapHeight, Editor.Indicators[StaticWarningIndicator], Color.Salmon, 12, false);
            DrawIndicator(e, allLines, mapHeight, Editor.Indicators[DebugLineIndicator], Color.DarkOrange, 0, false);

            // Breakpoints
            foreach (Line line in Editor.Lines)
            {
                const uint mask = 1 << BREAKPOINT_MARKER;
                if ((line.MarkerGet() & mask) > 0)
                {
                    float lineInRange = mapHeight * line.Index / allLines;
                    e.Graphics.FillEllipse(new SolidBrush(Color.IndianRed),
                        6, lineInRange - 3,
                        10, 10);
                }
            }
        }

        private void DrawIndicator(PaintEventArgs e, float allLines, float mapHeight, Indicator indicator, Color color,
            int markXOffset, bool thickBar)
        {
            int textLength = Editor.TextLength;
            int bitmapFlag = 1 << indicator.Index;

            int startPos = 0;
            int endPos = 0;

            do
            {
                startPos = indicator.Start(endPos);
                endPos = indicator.End(startPos);

                uint bitmap = Editor.IndicatorAllOnFor(startPos);
                bool filled = (bitmapFlag & bitmap) == bitmapFlag;
                if (filled)
                {
                    float indicatorY = mapHeight * Editor.LineFromPosition(startPos) / allLines;
                    e.Graphics.FillRectangle(new SolidBrush(color),
                        0 + markXOffset, indicatorY - (thickBar ? 2 : 0),
                        8, thickBar ? 8 : 4);
                }
            } while (endPos != 0 && endPos < textLength);
        }

        private void onTextChanged(object sender, EventArgs e)
        {
            ClearIndicators();
            IsDirty = true;
            RefreshReferenceArrows(null);
        }

        public void RefreshAutocompletionItems()
        {
            RefreshAutocompletionItems(prologEngine);
        }

        public void RefreshAutocompletionItems(PrologEngine pe)
        {
            ResourceManager helpRm = CsPrologHelp.ResourceManager;

            List<PredicateDescr> predDescrs = pe.PredTable.Predicates.Values
                .OrderBy(x => x.Name)
                .ToList();

            _autocompleteMenu.SetAutocompleteItems(new DynamicCollection(predDescrs, pe.UserAtoms, helpRm));
        }

        public void RefreshReferenceArrows()
        {
            RefreshReferenceArrows(prologEngine);
        }

        public void RefreshReferenceArrows(PrologEngine stoppedTypingEngine)
        {
            lock (this)
            {
                _referenceArrowsPredTable = stoppedTypingEngine?.PredTable.Predicates;
                Editor.Invalidate();
            }
        }

        private static bool IsBrace(int c)
        {
            switch (c)
            {
                case '(':
                case ')':
                case '[':
                case ']':
                case '{':
                case '}':
                    return true;
            }

            return false;
        }

        private void OnMarginClick(object sender, MarginClickEventArgs e)
        {
            if (e.Margin == BREAKPOINT_MARGIN)
            {
                // Do we have a marker for this line?
                const uint mask = 1 << BREAKPOINT_MARKER;
                Line line = Editor.Lines[Editor.LineFromPosition(e.Position)];
                if ((line.MarkerGet() & mask) > 0)
                {
                    // Remove existing bookmark
                    line.MarkerDelete(BREAKPOINT_MARKER);
                }
                else
                {
                    // Add bookmark
                    line.MarkerAdd(BREAKPOINT_MARKER);
                }
            }
        }

        private void OnUiUpdate(object sender, UpdateUIEventArgs e)
        {
            Map.Invalidate();

            if (openParenPosition.HasValue && Editor.CurrentPosition >= openParenPosition.Value)
            {
                if (Editor.LineFromPosition(openParenPosition.Value) == Editor.CurrentLine
                    && Editor.Text.IndexOf(')', Editor.CurrentPosition) ==
                    Editor.Text.IndexOf(')', openParenPosition.Value))
                {
                    string leftSide = Editor.Text.Substring(openParenPosition.Value,
                        Editor.CurrentPosition - openParenPosition.Value);
                    int paramIdx = leftSide.Count(x => x == ',');

                    int paramCount = 0;
                    int currIdx = callTip.IndexOf('(') + 1;
                    while (paramCount < paramIdx)
                    {
                        currIdx = callTip.IndexOf(',', currIdx) + 1;
                        paramCount++;
                    }

                    int nextIdx = callTip.IndexOf(",", currIdx) > 0
                        ? callTip.IndexOf(",", currIdx)
                        : callTip.IndexOf(")", currIdx);

                    Editor.CallTipSetHlt(currIdx, nextIdx);

                    return;
                }
            }

            openParenPosition = null;

            // Brace matching
            // Has the caret changed position?
            int caretPos = Editor.CurrentPosition;
            if (lastCaretPos != caretPos)
            {
                if (!InitialCallTipDisplay)
                {
                    Editor.CallTipCancel();
                }

                InitialCallTipDisplay = false;

                lastCaretPos = caretPos;
                int bracePos1 = -1;
                int bracePos2 = -1;

                // Is there a brace to the left or right?
                if (caretPos > 0 && IsBrace(Editor.GetCharAt(caretPos - 1)))
                {
                    bracePos1 = caretPos - 1;
                }
                else if (IsBrace(Editor.GetCharAt(caretPos)))
                {
                    bracePos1 = caretPos;
                }

                if (bracePos1 >= 0)
                {
                    // Find the matching brace
                    bracePos2 = Editor.BraceMatch(bracePos1);
                    if (bracePos2 == Scintilla.InvalidPosition)
                    {
                        Editor.BraceBadLight(bracePos1);
                    }
                    else
                    {
                        Editor.BraceHighlight(bracePos1, bracePos2);
                    }
                }
                else
                {
                    // Turn off brace matching
                    Editor.BraceHighlight(Scintilla.InvalidPosition, Scintilla.InvalidPosition);
                }
            }
        }

        private void Editor_MouseMove(object sender, MouseEventArgs e)
        {
            _mouseLocation = e.Location;

            bool shouldInvalidate = false;
            foreach (GraphicsPath path in _paths.Keys.ToArray())
            {
                if (path.IsOutlineVisible(_mouseLocation, _dependencyArrowPenBlowup))
                {
                    Pen curr = _paths[path];
                    if (curr != _dependencyArrowPenHighlight)
                    {
                        _paths[path] = _dependencyArrowPenHighlight;
                        shouldInvalidate = true;
                    }
                }
                else
                {
                    Pen curr = _paths[path];
                    if (curr != _dependencyArrowPen)
                    {
                        _paths[path] = _dependencyArrowPen;
                        shouldInvalidate = true;
                    }
                }
            }

            if (shouldInvalidate)
            {
                Editor.Invalidate();
            }
        }

        private void Editor_MouseUp(object sender, MouseEventArgs e)
        {
            if (positionToMoveTo != -1)
            {
                Editor.ScrollRange(positionToMoveTo, positionToMoveTo);
                Editor.SetSelection(positionToMoveTo, positionToMoveTo);
                positionToMoveTo = -1;
                Editor.Invalidate();
            }
        }

        private void Editor_MouseDown(object sender, MouseEventArgs e)
        {
            foreach (GraphicsPath path in _paths.Keys)
            {
                if (path.IsOutlineVisible(_mouseLocation, _dependencyArrowPenBlowup))
                {
                    positionToMoveTo = _pathsToStartPos[path];
                    return;
                }
            }
        }

        private void EditorPainted(object sender, EventArgs e)
        {
            lock (this)
            {
                if (!DrawDependencyArrows)
                {
                    return;
                }

                if (_referenceArrowsPredTable == null)
                {
                    if (!lastWasNull)
                    {
                        Editor.Invalidate();
                    }

                    lastWasNull = true;

                    return;
                }

                lastWasNull = false;

                if (sender == Editor && positionToMoveTo == -1)
                {
                    BuildPathsToPaint();

                    using (Graphics g = Editor.CreateGraphics())
                    {
                        g.SmoothingMode = SmoothingMode.AntiAlias;

                        foreach (KeyValuePair<GraphicsPath, Pen> pathPen in _paths)
                        {
                            g.DrawPath(pathPen.Value, pathPen.Key);
                        }
                    }
                }
            }
        }

        private void BuildPathsToPaint()
        {
            int pos = Editor.CurrentPosition;
            int firstVisibleLine = Editor.FirstVisibleLine;

            if (pos != lastDrawnPosition || firstVisibleLine != lastFirstVisibleLine)
            {
                lastDrawnPosition = pos;
                lastFirstVisibleLine = firstVisibleLine;

                _paths.Clear();
                _pathsToStartPos.Clear();

                List<PredicateDescr> clauses = _referenceArrowsPredTable.Values
                    .Where(
                        x => !x.IsPredefined
                             && x.ClauseList.Term.Symbol.Start <= pos
                             && x.ClauseList.Term.Symbol.Final >= pos)
                    .ToList();

                PredicateDescr clause = clauses.FirstOrDefault();

                if (clause != null)
                {
                    int clauseHeadStart = clause.ClauseList.Term.Symbol.Start;
                    int? clauseHeadEnd = clause.ClauseList.Term.Symbol.Final;

                    List<PredicateDescr> res;
                    if (clauseHeadEnd.HasValue &&
                        prologEngine.PredTable.CrossRefTable.ReverseDirectRefIndex.TryGetValue(clause, out res))
                    {
                        for (int i = 0; i < res.Count; i++)
                        {
                            PredicateDescr pd = res[i];

                            if (pd == clause)
                            {
                                continue;
                            }

                            int refClauseStart = pd.ClauseList.Term.Symbol.Start;
                            int? refClauseEnd = pd.ClauseList.Term.Symbol.Final;
                            if (refClauseEnd.HasValue)
                            {
                                GraphicsPath path = new GraphicsPath();
                                _paths.Add(path, _dependencyArrowPen);
                                _pathsToStartPos.Add(path, refClauseStart);

                                int startX = Editor.PointXFromPosition(refClauseStart) +
                                             ((Editor.PointXFromPosition(refClauseEnd.Value) -
                                               Editor.PointXFromPosition(refClauseStart)) / 2);
                                int startY = Editor.PointYFromPosition(refClauseStart) +
                                             ((Editor.PointYFromPosition(refClauseEnd.Value) -
                                               Editor.PointYFromPosition(refClauseStart)) / 2) + 7;
                                int endX = Editor.PointXFromPosition(clauseHeadStart) +
                                           ((Editor.PointXFromPosition(clauseHeadEnd.Value) -
                                             Editor.PointXFromPosition(clauseHeadStart)) / 2);
                                int endY = Editor.PointYFromPosition(clauseHeadStart) +
                                           ((Editor.PointYFromPosition(clauseHeadEnd.Value) -
                                             Editor.PointYFromPosition(clauseHeadStart)) / 2) + 7;
                                path.AddCurve(new[]
                                {
                                    new Point(startX, startY),
                                    new Point(((startX + endX) / 2) - 10, ((startY + endY) / 2) - 3),
                                    new Point(endX, endY)
                                });
                            }
                        }
                    }
                }

                Editor.Invalidate();
            }
        }

        private void AutocompletionItemSelected(object sender, SelectedEventArgs e)
        {
            if (e.Item.Text.EndsWith(")") && e.Item.Text.Contains("(") && !e.Item.Text.EndsWith("()"))
            {
                Editor.CallTipSetPosition(true);
                callTip = e.Item.ToolTipTitle;
                Editor.CallTipShow(Editor.CurrentPosition, callTip.Replace("^", ""));

                int initSelectStart = e.Item.Text.IndexOf("(") + 1;
                int initSelectEnd = e.Item.Text.IndexOf(",") > 0 ? e.Item.Text.IndexOf(",") : e.Item.Text.IndexOf(")");

                Editor.CallTipSetHlt(initSelectStart, initSelectEnd);

                Editor.SelectionStart = Editor.CurrentPosition;
                Editor.SelectionEnd = Editor.CurrentPosition + initSelectEnd - initSelectStart - 1;

                openParenPosition = Editor.CurrentPosition - (Editor.SelectionEnd - Editor.SelectionStart);
            }
        }

        private void onKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control && e.KeyCode == Keys.F)
            {
                //_findReplaceDialog.ShowIncrementalSearch();
                //e.SuppressKeyPress = true;
            }
            else if (e.Control && e.Shift && e.KeyCode == Keys.C)
            {
                int startLine = Editor.LineFromPosition(Editor.SelectionStart);
                int endLine = Editor.LineFromPosition(Editor.SelectionEnd);

                int firstLineStartPos = Editor.Lines[startLine].Position;
                bool comment = Editor.GetCharAt(firstLineStartPos) != '%';

                Editor.BeginUndoAction();
                for (int i = startLine; i <= endLine; i++)
                {
                    int lineStartPos = Editor.Lines[i].Position;
                    if (comment)
                    {
                        Editor.InsertText(lineStartPos, "%");
                    }
                    else if (Editor.GetCharAt(lineStartPos) == '%')
                    {
                        Editor.DeleteRange(lineStartPos, 1);
                    }
                }

                Editor.EndUndoAction();
            }
        }

        private void TextMouseClick(object sender, MouseEventArgs e)
        {
            onKeyUp(sender, null);
        }

        private void TextDoubleClick(object sender, DoubleClickEventArgs e)
        {
            onKeyUp(sender, null);
        }

        private void onKeyUp(object sender, KeyEventArgs e)
        {
            Scintilla target = (Scintilla)sender;

            // if (target.Indicators.Any(x => x.Style == IndicatorStyle.Squiggle))
            // {
            //     return;
            // }

            if (!string.IsNullOrWhiteSpace(target.SelectedText))
            {
                HighlightWord(target);
            }
            else
            {
                // Clean everything apart from error squiggles
                int textLength = Editor.TextLength;
                int bitmapFlag = 1 << Editor.Indicators[SquiggleIndicator].Index;

                int startPos = 0;
                int endPos = 0;

                do
                {
                    startPos = Editor.Indicators[SquiggleIndicator].Start(endPos);
                    endPos = Editor.Indicators[SquiggleIndicator].End(startPos);

                    uint bitmap = Editor.IndicatorAllOnFor(startPos);
                    bool filled = (bitmapFlag & bitmap) == bitmapFlag;
                    if (!filled)
                    {
                        target.IndicatorClearRange(startPos, endPos);
                    }
                } while (endPos != 0 && endPos < textLength);
            }
        }

        public void IndicatorFillRange(int indicatorType, int startPos, int length, string description)
        {
            _indicatorDescriptions.Add(Tuple.Create(startPos, startPos + length + 1,
                description.Trim(' ', '\n', '\t', '\r')));
            Editor.IndicatorCurrent = indicatorType;
            Editor.IndicatorFillRange(startPos, length);
        }

        public void ClearIndicators()
        {
            _indicatorDescriptions.Clear();
            Editor.IndicatorClearRange(0, Editor.TextLength);
        }

        private static void HighlightWord(Scintilla editor)
        {
            // Remove all uses of our indicator
            editor.IndicatorCurrent = HighlightIndicator;
            editor.IndicatorClearRange(0, editor.TextLength);

            // Update indicator appearance
            editor.Indicators[HighlightIndicator].Style = IndicatorStyle.StraightBox;
            editor.Indicators[HighlightIndicator].Under = true;
            editor.Indicators[HighlightIndicator].ForeColor = Color.Green;
            editor.Indicators[HighlightIndicator].OutlineAlpha = 50;
            editor.Indicators[HighlightIndicator].Alpha = 30;

            // Search the document
            editor.TargetStart = 0;
            editor.TargetEnd = editor.TextLength;
            editor.SearchFlags = SearchFlags.None;
            while (editor.SearchInTarget(editor.SelectedText) != -1)
            {
                // Mark the search results with the current indicator
                editor.IndicatorFillRange(editor.TargetStart, editor.TargetEnd - editor.TargetStart);

                // Search the remainder of the document
                editor.TargetStart = editor.TargetEnd;
                editor.TargetEnd = editor.TextLength;
            }
        }

        public static class VisualPrologStyle
        {
            public const int SCE_VISUALPROLOG_DEFAULT = 0;
            public const int SCE_VISUALPROLOG_KEY_MAJOR = 1;
            public const int SCE_VISUALPROLOG_KEY_MINOR = 2;
            public const int SCE_VISUALPROLOG_KEY_DIRECTIVE = 3;
            public const int SCE_VISUALPROLOG_COMMENT_BLOCK = 4;
            public const int SCE_VISUALPROLOG_COMMENT_LINE = 5;
            public const int SCE_VISUALPROLOG_COMMENT_KEY = 6;
            public const int SCE_VISUALPROLOG_COMMENT_KEY_ERROR = 7;
            public const int SCE_VISUALPROLOG_IDENTIFIER = 8;
            public const int SCE_VISUALPROLOG_VARIABLE = 9;
            public const int SCE_VISUALPROLOG_ANONYMOUS = 10;
            public const int SCE_VISUALPROLOG_NUMBER = 11;
            public const int SCE_VISUALPROLOG_OPERATOR = 12;
            public const int SCE_VISUALPROLOG_CHARACTER = 13;
            public const int SCE_VISUALPROLOG_CHARACTER_TOO_MANY = 14;
            public const int SCE_VISUALPROLOG_CHARACTER_ESCAPE_ERROR = 15;
            public const int SCE_VISUALPROLOG_STRING = 16;
            public const int SCE_VISUALPROLOG_STRING_ESCAPE = 17;
            public const int SCE_VISUALPROLOG_STRING_ESCAPE_ERROR = 18;
            public const int SCE_VISUALPROLOG_STRING_EOL_OPEN = 19;
            public const int SCE_VISUALPROLOG_STRING_VERBATIM = 20;
            public const int SCE_VISUALPROLOG_STRING_VERBATIM_SPECIAL = 21;
            public const int SCE_VISUALPROLOG_STRING_VERBATIM_EOL = 22;

            /// <summary>
            ///     Default (whitespace) style index.
            /// </summary>
            public const int Default = SCE_VISUALPROLOG_DEFAULT;

            public const int KeyMajor = SCE_VISUALPROLOG_KEY_MAJOR;
            public const int KeyMinor = SCE_VISUALPROLOG_KEY_MINOR;
            public const int KeyDirective = SCE_VISUALPROLOG_KEY_DIRECTIVE;
            public const int CommentBlock = SCE_VISUALPROLOG_COMMENT_BLOCK;
            public const int CommentLine = SCE_VISUALPROLOG_COMMENT_LINE;
            public const int CommentKey = SCE_VISUALPROLOG_COMMENT_KEY;
            public const int CommentKeyError = SCE_VISUALPROLOG_COMMENT_KEY_ERROR;
            public const int Identifier = SCE_VISUALPROLOG_IDENTIFIER;
            public const int Variable = SCE_VISUALPROLOG_VARIABLE;
            public const int Anonymous = SCE_VISUALPROLOG_ANONYMOUS;
            public const int Number = SCE_VISUALPROLOG_NUMBER;
            public const int Operator = SCE_VISUALPROLOG_OPERATOR;
            public const int Character = SCE_VISUALPROLOG_CHARACTER;
            public const int CharacterTooMany = SCE_VISUALPROLOG_CHARACTER_TOO_MANY;
            public const int CharacterEscapeError = SCE_VISUALPROLOG_CHARACTER_ESCAPE_ERROR;
            public const int String = SCE_VISUALPROLOG_STRING;
            public const int StringEscape = SCE_VISUALPROLOG_STRING_ESCAPE;
            public const int StringEscapeError = SCE_VISUALPROLOG_STRING_ESCAPE_ERROR;
            public const int StringEolOpen = SCE_VISUALPROLOG_STRING_EOL_OPEN;
            public const int Verbatim = SCE_VISUALPROLOG_STRING_VERBATIM;
            public const int VerbatimSpecial = SCE_VISUALPROLOG_STRING_VERBATIM_SPECIAL;
            public const int VerbatimEol = SCE_VISUALPROLOG_STRING_VERBATIM_EOL;
        }

        public class DynamicCollection : IEnumerable<AutocompleteItem>
        {
            private readonly List<AutocompleteItem> _items;

            private readonly List<MulticolumnAutocompleteItem> _staticMulticolumn =
                new List<MulticolumnAutocompleteItem>();

            private readonly List<SnippetAutocompleteItem> _staticSnippets = new List<SnippetAutocompleteItem>();

            public DynamicCollection(List<PredicateDescr> predicateDescriptors, List<AtomTerm> atoms,
                ResourceManager helpRm)
            {
                _items = new List<AutocompleteItem>();

                // Misc
                _items.Add(new SnippetAutocompleteItem(":- begin_tests(^GroupName)")
                {
                    ImageIndex = 0,
                    ToolTipTitle = ":- begin_tests(GroupName)",
                    ToolTipText = @"
Directive that begins a unit test group.
Tests should be parameterless and should evaluate to test result.
E.g.
 :- begin_tests(arithm).
    testSimpleAdd :- 1 + 2 =:= 3.
 :- end_tests(arithm).

[builtin]"
                });

                _items.Add(new SnippetAutocompleteItem(":- end_tests(^GroupName)")
                {
                    ImageIndex = 0,
                    ToolTipTitle = ":- end_tests(GroupName)",
                    ToolTipText = @"
Directive that ends a unit test group.
Tests should be parameterless and should evaluate to test result.
E.g.
 :- begin_tests(arithm).
 testSimpleAdd :- 1 + 2 =:= 3.
:- end_tests(arithm).

[builtin]"
                });

                _items.Add(new SnippetAutocompleteItem(":- ensure_loaded(^FileNameAtom)")
                {
                    ImageIndex = 0,
                    ToolTipTitle = ":- ensure_loaded(FileNameAtom)",
                    ToolTipText = @"
Directive that ensures that the file is loaded exactly once.
All predicates are imported regardless of any module declarations.

[builtin]"
                });

                List<string> functorNames = predicateDescriptors.Where(x => x.Name.Contains("_"))
                    .Select(x => x.Name.Substring(0, x.Name.LastIndexOf("_")).ToLower().Replace(" ", "_")).ToList();

                HashSet<string> functorSet = new HashSet<string>(functorNames);
                IEnumerable<string> atomsSet =
                    atoms.Select(x => x.ToString()).Except(functorSet).Distinct().OrderBy(x => x);

                // User atoms
                _items = _items.Concat(
                        atomsSet
                            .Select(x =>
                            {
                                SnippetAutocompleteItem item = new SnippetAutocompleteItem(x)
                                {
                                    ImageIndex = 2,
                                    //   MenuText = x,
                                    ToolTipTitle = x,
                                    ToolTipText = "atom term"
                                };

                                return item;
                            }))
                    .ToList();

                // All predicates
                _items = _items.Concat(predicateDescriptors.Select(x =>
                    {
                        BaseTerm t = x.TermListEnd?.Term;
                        string file = x.DefinitionFile.Contains(Path.DirectorySeparatorChar) ?
                            x.DefinitionFile.Substring(x.DefinitionFile.LastIndexOf(Path.DirectorySeparatorChar) + 1)
                            : (x.IsPredefined ? "builtin" : "current");
                        SnippetAutocompleteItem item = new SnippetAutocompleteItem(PredicateHeadString(x))
                        {
                            ImageIndex = x.IsPredefined ? 0 : 1,
                            //   MenuText = x,
                            ToolTipTitle = t?.CommentHeader ?? x.ToString() + ((t?.TestGroup == null) ? string.Empty : (" - test " + t?.TestGroup)),
                            ToolTipText = helpRm.GetString(x.Name.ToLower().Replace(" ", "_")) ?? (t?.CommentBody) + $"\n[{file}]"
                        };

                        if (string.IsNullOrWhiteSpace(item.ToolTipTitle))
                        {
                            item.ToolTipTitle = item.Text.Replace("^", "");
                        }

                        item.ToolTipText = item.ToolTipText.Trim(' ', '\n', '\t', '\r');

                        if (string.IsNullOrWhiteSpace(item.ToolTipText))
                        {
                            item.ToolTipText = " ";
                        }

                        return item;
                    }))
                    .ToList();
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return BuildList().GetEnumerator();
            }

            public IEnumerator<AutocompleteItem> GetEnumerator()
            {
                return BuildList().GetEnumerator();
            }

            private IEnumerable<AutocompleteItem> BuildList()
            {
                //return autocomplete items
                foreach (AutocompleteItem item in _items.Concat(_staticSnippets).Concat(_staticMulticolumn))
                {
                    yield return item;
                }
            }

            private static string PredicateHeadString(PredicateDescr predicateDesc)
            {
                string name = predicateDesc.Name;

                if (predicateDesc.ClauseList.Head.Arity == 0)
                {
                    return name;
                }

                string args = string.Join(", ", predicateDesc.ClauseList.Head.Args.Select(x => x.Name));
                return name + "(^" + args + ")";
            }
        }
    }
}