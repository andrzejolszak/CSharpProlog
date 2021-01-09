#define showToken

namespace Prolog
{
    using System;
    using System.IO;
    using System.Text;
    using System.Xml;
    using System.Collections;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Globalization;

    /* _______________________________________________________________________________________________
      |                                                                                               |
      |  C#Prolog -- Copyright (C) 2007-2014 John Pool -- j.pool@ision.nl                                  |
      |                                                                                               |
      |  This library is free software; you can redistribute it and/or modify it under the terms of   |
      |  the GNU General Public License as published by the Free Software Foundation; either version  |
      |  2 of the License, or any later version.                                                      |
      |                                                                                               |
      |  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;    |
      |  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    |
      |  See the GNU General Public License for details, or enter 'license.' at the command prompt.   |
      |_______________________________________________________________________________________________|
    */

    // PrologParser Generator version 4.0 -- Date/Time: 22-12-2010 8:42:54

    public partial class PrologEngine
    {
        public static readonly CultureInfo CIC = CultureInfo.InvariantCulture;

        
        public class ConsultException : Exception
        {
            public ConsultException(string msg, BaseTerm term = null, BaseParser.Symbol symbol = null) : base(msg)
            {
                this.Term = term;
                this.Symbol = symbol;
            }

            public BaseParser.Symbol Symbol { get; private set; }
            public BaseTerm Term { get; private set; }
        }

        public class RuntimeException : Exception
        {
            public RuntimeException(string msg, BaseTerm term = null, VarStack varStack = null) : base(msg)
            {
                this.Term = term;
                this.VarStack = varStack;
            }

            public VarStack VarStack { get; set; }
            public BaseTerm Term { get; private set; }
        }
        
                public class TerminalSet
        {
            private bool[] x;

            public TerminalSet(int terminalCount, params int[] ta)
            {
                x = new bool[terminalCount];

                for (int i = 0; i < ta.Length; i++) x[ta[i]] = true;
            }

            public TerminalSet(int terminalCount, bool[] y)
            {
                x = (bool[]) y.Clone();
            }


            public TerminalSet Union(int terminalCount, params int[] ta)
            {
                TerminalSet union = new TerminalSet(terminalCount, x);  // create an identical set

                for (int i = 0; i < ta.Length; i++) union[ta[i]] = true;

                return union;
            }


            public bool this[int i]
            {
                set { x[i] = value; }
            }


            public bool Contains(int terminal)
            {
                return x[terminal];
            }


            public bool IsEmpty()
            {
                for (int i = 0; i < x.Length; i++) if (x[i]) return false;

                return true;
            }


            public void ToIntArray(out int[] a)
            {
                int count = 0;

                for (int i = 0; i < x.Length; i++) if (x[i]) count++;

                a = new int[count];
                count = 0;

                for (int i = 0; i < x.Length; i++) if (x[i]) a[count++] = i;
            }
        }
        
                public class BaseParser
        {
            public BaseParser()
            {
                cdh = new ConditionalDefinitionHandler(ppChar);
            }

                        protected class ConditionalDefinitionHandler
            {
                // An ifdef..endif statement is made up by one or more more blocks separated by else(if)s.
                // At most one of these blocks wil actually be processed; the others contain 'dead' code.
                private enum IfStatStatus
                {
                    Pristine, // No block in the ifdef..endif statement has been processed yet.
                    Active,   // The current block is being processed.
                    Done      // A previous block in the ifdef..endif statement has already been processed.
                }

                private const int NONE = -1;
                private List<string> definedSymbols;
                private Stack<IfStatStatus> nestedBlockStatus; // for keeping track of conditional definitions nesting
                private IfStatStatus ifStatStatus;
                private bool codeIsActive => (ifStatStatus == IfStatStatus.Active);
                private bool codeIsInactive => (ifStatStatus != IfStatStatus.Active);
                private int prevTerminalId;
                private int prevSymLineNo;
                private char ppChar;
                public bool IsExpectingId { get; private set; }
                public bool CodeIsInactive => codeIsInactive;

                public ConditionalDefinitionHandler(char ppChar)
                {
                    this.ppChar = ppChar;
                    definedSymbols = new List<string>();
                    Initialize();
                }


                public void Initialize()
                {
                    nestedBlockStatus = new Stack<IfStatStatus>();
                    ifStatStatus = IfStatStatus.Active; // default for outermost scope
                    IsExpectingId = false;
                    prevTerminalId = NONE;
                    nestedBlockStatus.Push(ifStatStatus);
                }


                public void HandleSymbol(Symbol symbol)
                {
                    if (symbol.TerminalId == Undefined)
                        IO.ErrorConsult( "Unknown conditional definition symbol: {0}", symbol);

                    if (IsExpectingId)
                    {
                        if (symbol.Class == SymbolClass.Id && symbol.LineNo == prevSymLineNo)
                        {
                            symbol.Class = SymbolClass.Meta; // in order to get rid of it in NextSymbol loop
                            string s = symbol.ToString();

                            if (prevTerminalId == ppDefine && codeIsActive)
                                Define(s);
                            else if (prevTerminalId == ppUndefine && codeIsActive)
                                Undefine(s);
                            else if (prevTerminalId == ppIf)
                            {
                                if (codeIsInactive)
                                    EnterScope(ifStatStatus, symbol); // do nothing; keep non-active status
                                else if (IsDefined(s)) //
                                    EnterScope(IfStatStatus.Active, symbol);
                                else
                                    EnterScope(IfStatStatus.Pristine, symbol); // wait for potential else(if) block
                            }
                            else if (prevTerminalId == ppIfNot)
                            {
                                if (codeIsInactive)
                                    EnterScope(ifStatStatus, symbol); // do nothing; keep non-active status
                                else if (IsDefined(s)) //
                                    EnterScope(IfStatStatus.Pristine, symbol); // wait for potential else(if) block
                                else
                                    EnterScope(IfStatStatus.Active, symbol);
                            }
                            else if (prevTerminalId == ppElseIf)
                            {
                                if (codeIsActive)
                                {
                                    ExitScope(symbol);
                                    EnterScope(IfStatStatus.Done, symbol);
                                }
                                else if (ifStatStatus == IfStatStatus.Pristine && IsDefined(s))
                                {
                                    ExitScope(symbol);
                                    EnterScope(IfStatStatus.Active, symbol);
                                }
                            } //else leave status untouched
                        }
                        else
                            IO.ErrorConsult(
                              $"Identifier missing after {ppChar}if, {ppChar}ifdef {ppChar}ifndef, {ppChar}elseif, {ppChar}define or {ppChar}undefine", symbol);

                        IsExpectingId = false;
                    }
                    else if (symbol.TerminalId == EndOfInput && nestedBlockStatus.Count > 1)
                        IO.ErrorConsult($"Unexpected end of input -- {ppChar}else or {ppChar}endif expected", symbol);
                    else
                    {
                        switch (symbol.TerminalId)
                        {
                            case ppDefine:
                            case ppUndefine:
                            case ppIf:
                            case ppIfNot:
                                IsExpectingId = true;
                                break;
                            case ppElseIf:
                                if (prevTerminalId == ppIf || prevTerminalId == ppEndIf || prevTerminalId == ppElseIf)
                                    IsExpectingId = true;
                                else
                                    IO.ErrorConsult($"Unexpected {ppChar}elseif-directive", symbol);
                                break;
                            case ppElse:
                                if (prevTerminalId == ppIf || prevTerminalId == ppEndIf || prevTerminalId == ppElseIf)
                                {
                                    if (codeIsActive) // code is active ...
                                    {
                                        ExitScope(symbol);
                                        EnterScope(IfStatStatus.Done, symbol); // ... deactivate it
                                    }
                                    else if (ifStatStatus == IfStatStatus.Pristine) // code is inactive ...
                                    {
                                        ExitScope(symbol);         // ... activate it (sets codeIsInactive !!)
                                        EnterScope(IfStatStatus.Active, symbol); // ...
                                    }
                                }
                                else
                                    IO.ErrorConsult($"Unexpected {ppChar}else-directive", symbol);
                                break;
                            case ppEndIf:
                                ExitScope(symbol);
                                break;
                        }

                        prevTerminalId = symbol.TerminalId;
                        prevSymLineNo = symbol.LineNo;
                    }
                }


                private void Define(string sym)
                {
                    if (!definedSymbols.Contains(sym))
                        definedSymbols.Add(sym);
                }


                private void Undefine(string sym)
                {
                    if (definedSymbols.Contains(sym))
                        definedSymbols.Remove(sym);
                }


                private bool IsDefined(string sym)
                {
                    return definedSymbols.Contains(sym);
                }


                private void EnterScope(IfStatStatus mode, Symbol symbol)
                {
                    nestedBlockStatus.Push(ifStatStatus = mode);
                }


                private void ExitScope(Symbol symbol)
                {
                    if (nestedBlockStatus.Count == 1)
                        IO.ErrorConsult( "Unexpected {0}-symbol", symbol);

                    nestedBlockStatus.Pop();
                    ifStatStatus = nestedBlockStatus.Peek();
                }
            }
            
            
            private int traceIndentLevel;
            // The const values below must be in strict accordance with the
            // order in procedure EnterPredefinedSymbols in uCommons.pas
            protected const int Undefined = 0;
            protected const int Comma = 1;
            protected const int LeftParen = 2;
            protected const int RightParen = 3;
            protected const int Identifier = 4;
            protected const int IntLiteral = 5;
            protected const int ppDefine = 6;
            protected const int ppUndefine = 7;
            protected const int ppIf = 8;
            protected const int ppIfNot = 9;
            protected const int ppElse = 10;
            protected const int ppElseIf = 11;
            protected const int ppEndIf = 12;
            protected const int RealLiteral = 13;
            protected const int ImagLiteral = 14;
            protected const int StringLiteral = 15;
            protected const int CharLiteral = 16;
            protected const int CommentStart = 17;
            protected const int CommentSingle = 18;
            protected const int EndOfLine = 19;
            protected const int ANYSYM = 20;
            protected const int EndOfInput = 21;
            // end of Terminal symbol const values
            protected virtual char ppChar => '#';

            protected const char SQUOTE = '\'';
            protected const char DQUOTE = '"';
            protected const char BSLASH = '\\';
            protected const int UNDEF = -1;

            protected BaseTrie terminalTable;
            protected ConditionalDefinitionHandler cdh;
            protected char ch;
            protected char prevCh;
            protected bool endText;
            protected Symbol symbol;
            protected int maxRuntime = 0; // maximum runtime in miliseconds; 0 means unlimited
            protected int actRuntime;     // actual runtime for Parse () in miliseconds
            protected Buffer inStream = null;
            protected Buffer outStream = null; // standard output
            protected string streamInPrefix;
            protected bool syntaxErrorStat;
            protected string errorMessage;
            protected int streamInLen;
            protected StreamPointer streamInPtr;
            protected bool stringMode; // iff a string is being read (i.e. ignore comments)
            protected bool parseAnyText;
            protected int prevTerminal;
            protected int anyTextStart;
            protected int anyTextFinal;
            protected int anyTextFPlus;
            protected int clipBegin;
            protected int clipEnd;
            protected bool seeEndOfLine;
            protected bool SeeEndOfLine { set { seeEndOfLine = value; } get { return seeEndOfLine; } }
            protected bool formatMode;
            /*
                Preprocessor symbols are retained in nested parser calls, i.e. the symbols
                defined in the outer Call are visible in the inner Call. At the end of the
                inner parse, the state is reset to the one at the start of the inner parse.
            */
            protected int eoLineCount;
            protected int lineCount = 0;
            protected bool showErrTrace = true;
            protected int streamInPreLen; // length of streamInPrefix
            protected positionMarker errMark;

            public int ClipBegin => clipBegin;
            public int ClipEnd => clipEnd;
            public bool FormatMode { get { return formatMode; } set { formatMode = value; } }
            public int LineCount => lineCount;
            public bool ShowErrTrace { get { return showErrTrace; } set { showErrTrace = value; } }
            public int MaxRuntime { get { return maxRuntime; } set { maxRuntime = value; } }

            public string Prefix
            {
                get
                {
                    return streamInPrefix;
                }
                set
                {
                    streamInPrefix = value;
                    streamInPreLen = streamInPrefix.Length;
                }
            }


            public string SyntaxError
            {
                set
                {
                    if (parseAnyText) return;

                    errorMessage = symbol.Context + value + Environment.NewLine;
                    syntaxErrorStat = true;

                    throw new ConsultException(errorMessage, symbol: symbol);
                }
                get
                {
                    return errorMessage;
                }
            }


            public string ErrorMessage
            {
                set
                {
                    if (parseAnyText) return;

                    throw new ConsultException($"{symbol.Context}{value}{Environment.NewLine}", symbol: symbol);
                }
                get
                {
                    return errorMessage;
                }
            }


            public void Warning(string msg)
            {
                if (parseAnyText) return;

                positionMarker currPos;

                InputStreamMark(out currPos);

                if (errMark.Start != UNDEF) InputStreamRedo(errMark, 0);

                IO.WriteLine("{0}{1}{2}", symbol.Context, msg, Environment.NewLine);

                if (errMark.Start != UNDEF) InputStreamRedo(currPos, 0);
            }


            public void Warning(string msg, params Object[] o)
            {
                Warning(String.Format(msg, o));
            }


            public void SemanticError(string msg)
            {
                if (parseAnyText) return;

                if (errMark.Start != UNDEF) InputStreamRedo(errMark, 0);

                throw new ConsultException($"{symbol.Context}{msg}{Environment.NewLine}");
            }


            public void SemanticError(string msg, params Object[] o)
            {
                SemanticError(String.Format(msg, o));
            }


            protected void Error(string msg)
            {
                if (parseAnyText) return;

                if (errMark.Start != UNDEF) InputStreamRedo(errMark, 0);

                throw new ConsultException($"{symbol.Context}{msg}{Environment.NewLine}", symbol: symbol);
            }


            protected void Error(string msg, params Object[] o)
            {
                Error(String.Format(msg, o));
            }


            public void Fatal(string msg)
            {
                if (parseAnyText) return;

                if (errMark.Start != UNDEF) InputStreamRedo(errMark, 0);

                throw new InvalidOperationException($"{symbol.Context}{msg}{Environment.NewLine}");
            }


            public void Fatal(string msg, params Object[] o)
            {
                Fatal(String.Format(msg, o));
            }


            public string StreamIn
            {
                get
                {
                    return inStream.ToString();
                }
                set
                {
                    inStream = new StringReadBuffer(value);
                    streamInLen = inStream.Length;
                    Parse();
                }
            }


            public void LoadFromFile(string fileName)
            {
                LoadFromStream(null, fileName);
            }

            public void LoadFromStream(Stream stream, string streamName)
            {
                try
                {
                    inStream = new FileReadBuffer(stream, streamName);
                    streamInLen = inStream.Length;
                }
                catch
                {
                    Prefix = "";
                    throw new Exception("*** Unable to read file \"" + streamName + "\"");
                }

                Parse();
            }


            public void SetInputStream(TextReader tr)
            {
                inStream = new StringReadBuffer(((StreamReader)tr).ReadToEnd());
                streamInLen = inStream.Length;
            }


            public string StreamInClip(int n, int m)
            {
                return GetStreamInString(n, m);
            }

            public string StreamInClip()
            {
                return GetStreamInString(clipBegin, clipEnd);
            }

            public bool AtEndOfInput => (inStream == null);

            
            
                        protected struct StreamPointer
            {
                public int Position;
                public int LineNo;
                public int LineStart;
                public bool EndLine;
                public bool FOnLine;  /// if the coming symbol is the first one on the current line
            }
            

                        protected struct positionMarker
            {
                public OpDescrTriplet Payload;
                public StreamPointer Pointer;
                public int Terminal;
                public SymbolClass Class;
                public int Start;
                public int Final;
                public int FinalPlus;
                public int PrevFinal;
                public int LineNo;
                public int AbsSeqNo;
                public int RelSeqNo;
                public int LineStart;
                public bool Processed;
                public bool IsSet; // initialized to false
            }
            
            
                        public enum SymbolClass { None, Id, Text, Number, Meta, Group, Comment }

            public class Symbol
            {
                private BaseParser parser;
                public OpDescrTriplet Payload;
                public SymbolClass Class;
                public int TerminalId;
                public int Start;     // position in input stream
                public int Final;     // first position after symbol
                public int FinalPlus; // first position of next symbol
                public int PrevFinal; // final position of previous symbol
                public int LineNo;
                public int LineStart; // position of first char of line in input stream
                public int AbsSeqNo;  // sequence number of symbol // -- absolute value, invariant under MARK/REDO
                public int RelSeqNo;  // sequence number of symbol in current line // -- relative value, invariant under MARK/REDO
                public bool IsFollowedByLayoutChar; // true iff the symbol is followed by a layout character

                public Symbol(BaseParser p)
                {
                    parser = p;
                    IsProcessed = false;
                    Payload = default(OpDescrTriplet);
                    Class = SymbolClass.None;
                    TerminalId = PrologParser.Undefined;
                    Start = UNDEF;
                    Final = UNDEF;
                    FinalPlus = UNDEF;
                    PrevFinal = UNDEF;
                    LineNo = UNDEF;
                    LineStart = UNDEF;
                    AbsSeqNo = UNDEF;
                    RelSeqNo = UNDEF;
                }

                public Symbol Clone()
                {
                    Symbol symbol = new Symbol(null);
                    symbol.TerminalId = this.TerminalId;
                    symbol.Class = this.Class;
                    symbol.Payload = this.Payload;
                    symbol.Start = this.Start;
                    symbol.Final = this.Final;
                    symbol.FinalPlus = this.FinalPlus;
                    symbol.PrevFinal = this.PrevFinal;
                    symbol.LineNo = this.LineNo;
                    symbol.AbsSeqNo = this.AbsSeqNo;
                    symbol.RelSeqNo = this.RelSeqNo;
                    symbol.LineStart = this.LineStart;
                    symbol.SetProcessed(this.IsProcessed);

                    return symbol;
                }

                public int StartAdjusted => Start - 10;
                public int FinalAdjusted => Final - 10;

                public bool IsNumber => true;

                public override string ToString()
                {
                    if (this.Start == Final) return "";

                    if (this.TerminalId == Undefined && Start > Final) return "<Undefined>";

                    if (this.TerminalId == EndOfLine) return "<EndOfLine>";

                    if (this.TerminalId == EndOfInput) return "<EndOfInput>";

                    return parser?.StreamInClip(Start, Final);
                }


                public int ColNo => (Start >= LineStart) ? Start - LineStart + 1 : UNDEF;


                public string ToName()
                {
                    return parser?.terminalTable.Name(TerminalId);
                }


                public string ToUnquoted()
                {
                    string s = this.ToString();
                    int len = s.Length;
                    char c;

                    if (len < 2) return s;

                    if ((c = s[0]) == SQUOTE && s[len - 1] == SQUOTE ||
                        (c = s[0]) == DQUOTE && s[len - 1] == DQUOTE)
                        return s.Substring(1, len - 2).Replace(c + c.ToString(), c.ToString());
                    else
                        return s;
                }


                public int ToInt()
                {
                    try
                    {
                        return Convert.ToInt32(ToString());
                    }
                    catch
                    {
                        throw new ConsultException($"*** Unable to convert '{this}' to an integer value", symbol: this);
                    }
                }


                public int ToShort()
                {
                    try
                    {
                        return Convert.ToInt16(ToString());
                    }
                    catch
                    {
                        throw new ConsultException($"*** Unable to convert '{this}' to a short value", symbol: this);
                    }
                }


                public double ToDouble()
                {
                    try
                    {
                        return Double.Parse(ToString(), CIC);
                    }
                    catch
                    {
                        throw new ConsultException($"*** Unable to convert '{this}' to a real (double) value", symbol: this);
                    }
                }


                public decimal ToDecimal()
                {
                    try
                    {
                        return Decimal.Parse(ToString(), NumberStyles.Float, CIC);
                    }
                    catch
                    {
                        throw new ConsultException($"*** Unable to convert '{this}' to a decimal value", symbol: this);
                    }
                }

                public string Dump()
                {
                    return $"Start={Start} Final={Final} LineStart={LineStart}";
                }


                public string Context
                {
                    get
                    {
                        if (parser?.inStream == null)
                        {
                            return string.Empty;
                        }

                        StringBuilder sb = new StringBuilder(
                            $"{Environment.NewLine}*** {parser.inStream.Name}: line {LineNo}");
                        
                        sb.Append(" position " + (this.Start >= this.LineStart ? ColNo.ToString() : parser.inStream.PositionOnLine.ToString()));
                        
                        if (this.Start >= this.Final) return sb + Environment.NewLine + " " + Environment.NewLine;

                        if (parser.inStream.Name != "")
                            sb.Append(Environment.NewLine + InputLine + Environment.NewLine);

                        return sb.ToString();
                    }
                }


                public bool IsMemberOf(params int[] ts)
                {
                    int lo, hi, mid;
                    int n = ts.Length;

                    if (n <= 8) // linear
                    {
                        for (lo = 0; lo < n; lo++) if (TerminalId <= ts[lo]) break;

                        return (lo >= n) ? false : ts[lo] == TerminalId;
                    }
                    else // binary
                    {
                        lo = -1;
                        hi = n;

                        while (hi != lo + 1)
                        {
                            mid = (lo + hi) >> 1;

                            if (TerminalId < ts[mid])
                                hi = mid;
                            else
                                lo = mid;
                        }

                        return (lo < 0) ? false : ts[lo] == TerminalId;
                    }
                }


                public bool IsProcessed { get; private set; }


                public void SetProcessed(bool status)
                {
                    IsProcessed = status;
                }


                public void SetProcessed()
                {
                    SetProcessed(true);
                }


                public string TextPlus()
                {
                    if (TerminalId == Undefined && this.Start > this.FinalPlus)
                        return "<Undefined>";
                    else
                        return parser?.StreamInClip(this.Start, this.FinalPlus);
                }


                public string IntroText()
                {
                    if (TerminalId == Undefined && this.Start > this.FinalPlus)
                        return "<Undefined>";
                    else
                        return parser?.StreamInClip(this.PrevFinal, this.Start);
                }


                public string InputLine
                {
                    get
                    {
                        if (parser?.inStream == null)
                        {
                            return string.Empty;
                        }

                        char c;
                        int i = this.LineStart;
                        // find end of line
                        while (i < parser.streamInLen)
                        {
                            parser.GetStreamInChar(i, out c);
                            if (c == '\n') break;
                            i++;
                        }
                        return parser.StreamInClip(LineStart, i);
                    }
                }
            }
            

                        public class TerminalDescr : IComparable
            {
                public OpDescrTriplet Payload { get; set; }

                public int IVal { get; set; }

                public string Image { get; set; }

                public SymbolClass Class { get; set; }

                public TerminalDescr(int iVal, OpDescrTriplet payload, string image)
                {
                    this.IVal = iVal;
                    this.Payload = payload;
                    this.Image = image;
                    this.Class = 0;
                }

                public TerminalDescr(int iVal, OpDescrTriplet payload, string image, SymbolClass @class)
                {
                    this.IVal = iVal;
                    this.Payload = payload;
                    this.Image = image;
                    this.Class = @class;
                }

                public int CompareTo(object o)
                {
                    return IVal.CompareTo((int)o);
                }

                public override string ToString()
                {
                    if (Payload == null)
                        return $"{IVal} ({Image})";
                    else
                        return $"{IVal}:{Payload} ({Image}) [{Class}]";
                }
            }
            
                        public class TrieNode : IComparable
            {
                public char KeyChar { get; set; }

                public TerminalDescr TermRec { get; set; }

                public List<object> SubTrie { get; set; }

                public TrieNode(char k, List<object> s, TerminalDescr t)
                {
                    KeyChar = k;
                    SubTrie = s;
                    TermRec = t;
                }


                public int CompareTo(object o)
                {
                    return KeyChar.CompareTo((char)o);
                }


                public override string ToString()
                {
                    StringBuilder sb = new StringBuilder();

                    //ToString (this, sb, 0);        // tree representation
                    ToString(this, "", sb, 0);    // flat representation

                    return sb.ToString();
                }


                private void ToString(TrieNode node, StringBuilder sb, int indent)
                {
                    if (indent == 0)
                        sb.Append("<root>" + Environment.NewLine);
                    else
                    {
                        sb.AppendFormat("{0}{1} -- ", Spaces(indent), node.KeyChar);

                        if (node.TermRec == null)
                            sb.Append(Environment.NewLine);
                        else
                            sb.Append(String.Format(node.TermRec.ToString()));
                    }
                    if (node.SubTrie != null)
                        foreach (TrieNode subTrie in node.SubTrie) ToString(subTrie, sb, indent + 1);
                }


                public static void ToArrayList(TrieNode node, bool atRoot, ref List<object> a)
                {
                    if (!atRoot) // skip root
                        if (node.TermRec != null) a.Add(node.TermRec.Payload);

                    if (node.SubTrie != null)
                        foreach (TrieNode subTrie in node.SubTrie) ToArrayList(subTrie, false, ref a);
                }


                private void ToString(TrieNode node, string prefix, StringBuilder sb, int indent)
                {
                    if (indent != 0) // skip root
                        if (node.TermRec != null) sb.AppendFormat("{0} {1}\r\n", prefix, node.TermRec);

                    if (node.SubTrie != null)
                        foreach (TrieNode subTrie in node.SubTrie) ToString(subTrie, prefix, sb, indent + 1);
                }
            }
            
                        public enum DupMode { dupIgnore, dupAccept, dupError };

            public class BaseTrie
            {
                public static readonly int UNDEF = -1;
                private TrieNode root = new TrieNode('\x0', null, null);
                private int terminalCount;
                private List<object> indices = new List<object>();
                private Dictionary<int, string> names = new Dictionary<int, string>();
                private List<object> curr;
                private DupMode dupMode = DupMode.dupError;
                private bool caseSensitive = false; // every term to lowercase
                private List<object> currSub;
                public bool AtLeaf => (currSub == null);

                public BaseTrie(int terminalCount, bool cs)
                {
                    this.terminalCount = terminalCount;
                    caseSensitive = cs;
                }


                public void AddOrReplace(int iVal, string name, string image)
                {
                    Remove(image);
                    Add(iVal, name, image);
                }


                public void Add(int iVal, string name, params string[] images)
                {
                    Add(iVal, 0, name, images);
                }


                public void Add(int iVal, SymbolClass @class, string name, params string[] images)
                {
                    names[iVal] = name;

                    foreach (string key in images) Add(key, iVal, default(OpDescrTriplet), @class);
                }


                public void Add(string key, int iVal, OpDescrTriplet payload)
                {
                    Add(key, iVal, payload, 0);
                }


                public void Add(string key, int iVal, OpDescrTriplet payload, SymbolClass @class)
                {
                    if (key == null || key == "")
                        throw new Exception("*** Trie.Add: Attempt to insert a null- or empty key");

                    if (!caseSensitive) key = key.ToLower();

                    if (root.SubTrie == null) root.SubTrie = new List<object>();

                    curr = root.SubTrie;
                    int imax = key.Length - 1;
                    TrieNode node;
                    TrieNode next;
                    int i = 0;

                    while (i <= imax)
                    {
                        int k = (curr.Count == 0) ? -1 : curr.BinarySearch(key[i], null); // null req'd for MONO?

                        if (k >= 0) // found
                        {
                            node = (TrieNode)curr[k];

                            if (i == imax) // at end of key
                            {
                                if (node.TermRec == null)
                                    AddToIndices(node.TermRec = new TerminalDescr(iVal, payload, key, @class));
                                else if (dupMode == DupMode.dupAccept)
                                {
                                    TerminalDescr trec = node.TermRec;
                                    trec.IVal = iVal;
                                    trec.Payload = payload;
                                    trec.Image = key;
                                    trec.Class = @class;
                                }
                                else if (dupMode == DupMode.dupError)
                                    throw new Exception($"*** Attempt to insert duplicate key '{key}'");

                                return;
                            }

                            if (node.SubTrie == null) node.SubTrie = new List<object>();
                            curr = node.SubTrie;
                            i++;
                        }
                        else // char not found => append chain of TrieNodes for rest of key
                        {
                            node = new TrieNode(key[i], null, null);
                            curr.Insert(~k, node);
                            while (true)
                            {
                                if (i == imax) // at end of key
                                {
                                    AddToIndices(node.TermRec = new TerminalDescr(iVal, payload, key, @class));

                                    return;
                                }
                                node.SubTrie = new List<object>();
                                node.SubTrie.Add(next = new TrieNode(key[++i], null, null));
                                node = next;
                            }
                        }
                    }
                }


                private void AddToIndices(TerminalDescr td)
                {
                    int k = indices.BinarySearch(td.IVal);
                    indices.Insert((k < 0) ? ~k : k, td);
                }


                public bool Find(string key, out TerminalDescr td)
                {
                    if (key == null || key == "")
                        throw new Exception("*** Trie.Add: Attempt to search for a null- or empty key");

                    int imax = key.Length - 1;
                    int i = 0;
                    int k;

                    td = null;
                    curr = root.SubTrie;

                    while (curr != null)
                    {
                        if ((k = curr.BinarySearch(key[i])) < 0)
                        {
                            curr = null;

                            return false;
                        }

                        TrieNode match = (TrieNode)curr[k];

                        if (i++ == imax) return ((td = match.TermRec) != null);

                        curr = match.SubTrie;
                    }
                    return false;
                }


                public int this[string key]
                {
                    get
                    {
                        TerminalDescr td;

                        return (Find(key, out td) ? td.IVal : UNDEF);
                    }
                    set
                    {
                        TerminalDescr td;

                        if (Find(key, out td))
                            td.IVal = value;
                        else
                            throw new Exception("*** Trie indexer: key [" + key + "] not found");
                    }
                }


                public void FindCharInSubtreeReset()
                {
                    currSub = root.SubTrie;
                }


                public bool FindCharInSubtree(char c, out TerminalDescr td)
                {
                    int k;

                    k = (currSub == null) ? -1 : k = currSub.BinarySearch(c);

                    if (k >= 0)
                    {
                        TrieNode match = (TrieNode)currSub[k];
                        td = match.TermRec;
                        currSub = match.SubTrie;

                        return true;
                    }
                    else
                    {
                        td = null;

                        return false;
                    }
                }


                public string Name(int i)
                {
                    return (string)names[i];
                }


                public List<object> TerminalsOf(int i)
                {
                    int k = indices.BinarySearch(i);

                    if (k < 0) return null;

                    List<object> result = new List<object>();
                    int k0 = k;

                    while (true)
                    {
                        result.Add(indices[k++]);
                        if (k == indices.Count || ((TerminalDescr)indices[k]).CompareTo(i) != 0) break;
                    }

                    k = k0 - 1;

                    while (k > 0 && ((TerminalDescr)indices[k]).CompareTo(i) == 0)
                        result.Add(indices[k--]);

                    return result;
                }


                public int IndexOf(string key)
                {
                    TerminalDescr td;

                    return Find(key, out td) ? td.IVal : -1;
                }


                public string TerminalImageSet(TerminalSet ts)
                {
                    StringBuilder result = new StringBuilder();
                    bool isFirst = true;
                    int[] ii;

                    ts.ToIntArray(out ii);

                    foreach (int i in ii)
                    {
                        List<object> a = TerminalsOf(i);
                        bool isImage = false;

                        if (a != null)
                            foreach (TerminalDescr td in a)
                            {
                                isImage = true;
                                if (isFirst) isFirst = false; else result.Append(", ");
                                result.Append(td.Image);
                            }

                        if (!isImage)
                        {
                            if (isFirst) isFirst = false; else result.Append(", ");
                            result.Append("<" + (string)names[i] + ">");
                        }
                    }
                    return result.ToString();
                }

                /*
                        // public bool Remove (string key)

                        Remove (keyChar)

                        Stel je zit in een CacheTrieNode met KeyChar, Val, SubTrie. Huidige letter is keyChar[i]

                        De volgende properties zijn gedefinieerd:

                        ・CMatch   = (KeyChar == keyChar[i])
                        ・IsTerm   = (CachedTerm != null)
                        ・IsLeaf   = (SubTrie == null)
                        ・IsKeyEnd = (i == imax)

                        De vraag is nu bij welke combinaties van de bovenstaande properties een
                        CacheTrieNode gedelete mag worden, dus dat een referentie ernaar in de CacheTrieNode
                        hoger in de tree op null gezet mag worden.

                        Het proces begint met het vinden van de keyChar(node). Als die er niet is,
                        return met false.

                        Vandaaruit (IsKeyEnd) terug naar de root (door terugkeer uit recursie).
                        In een node wordt aangegeven of de parentnode hem mag deleten door in een
                        node de outputparam mayDelete terug te geven aan de ancestor (= aanroeper).

                        Een node mag weg als IsLeaf. Als de node de laatste is (IsEndKey) mag dit
                        zonder meer, als hij niet de laatste is, mag het alleen wanneer hij geen
                        terminal is (!IsTerm, d.w.z. een eindpunt van een kortere keyChar (subkey)).
                */

                public bool Remove(string key)
                {
                    bool result;
                    bool dummy;

                    if (!caseSensitive) key = key.ToLower();

                    RemoveEx(root, key, 0, key.Length - 1, out result, out dummy);

                    return result;
                }


                public void RemoveEx(TrieNode curr, string key, int i, int imax, out bool result, out bool mayDelete)
                {
                    result = false;
                    mayDelete = false;

                    if (curr.SubTrie == null) return;

                    int k = curr.SubTrie.BinarySearch(key[i]);

                    if (k < 0) return;

                    TrieNode match = (TrieNode)curr.SubTrie[k];

                    if (i == imax) // last char of key -- now we work our way back to the root
                    {
                        if (match.TermRec != null)
                        {
                            indices.Remove(match.TermRec);
                            match.TermRec = null;
                            mayDelete = (curr.SubTrie == null);
                            result = true;
                        }
                    }
                    else
                        RemoveEx(match, key, i + 1, imax, out result, out mayDelete);

                    if (!result || !mayDelete) return;

                    if (mayDelete) curr.SubTrie.RemoveAt(k);

                    mayDelete = (curr.SubTrie == null && curr.TermRec != null);
                }


                public void Remove(int i) // remove all entries with index i
                {
                    names.Remove(i);

                    List<object> a = TerminalsOf(i);

                    if (a != null) foreach (TerminalDescr td in a) Remove(td.Image);
                }

                public List<object> ToArrayList()
                {
                    List<object> a = new List<object>();

                    TrieNode.ToArrayList(root, true, ref a);

                    return a;
                }


                public override string ToString()
                {
                    return root.ToString();
                }
            }
            
                        protected virtual void ScanNumber()
            {
                bool isReal;
                StreamPointer savPosition;

                do { NextCh(); } while (Char.IsDigit(ch));

                symbol.TerminalId = IntLiteral;
                isReal = true; // assumed until proven conversily

                if (ch == '.') // fractional part?
                {
                    // save dot position
                    savPosition = streamInPtr;
                    NextCh();

                    if (Char.IsDigit(ch))
                    {
                        symbol.TerminalId = RealLiteral;

                        do { NextCh(); } while (Char.IsDigit(ch));
                    }
                    else // not a digit after period
                    {
                        InitCh(savPosition); // 'unread' dot
                        isReal = false;       // ... and remember this
                    }
                }

                if (ch == 'i')
                {
                    symbol.TerminalId = ImagLiteral;
                    NextCh();
                }

                if (isReal) // integer or real, possibly with scale factor
                {
                    savPosition = streamInPtr;

                    if (ch == 'e' || ch == 'E')
                    { // scale factor
                        NextCh();

                        if (ch == '+' || ch == '-') NextCh();

                        if (Char.IsDigit(ch))
                        {
                            do
                            {
                                NextCh();
                            }
                            while (Char.IsDigit(ch));

                            if (ch == 'i')
                            {
                                symbol.TerminalId = ImagLiteral;
                                NextCh();
                            }
                            else
                                symbol.TerminalId = RealLiteral;
                        }
                        else if (!stringMode) // Error in real syntax
                            InitCh(savPosition);
                    }
                }

                symbol.Class = SymbolClass.Number;
                symbol.Final = streamInPtr.Position;
            }


            protected virtual bool ScanFraction()
            {
                StreamPointer savPosition = streamInPtr; // Position of '.'

                do NextCh(); while (Char.IsDigit(ch));

                bool result = (streamInPtr.Position > savPosition.Position + 1); // a fraction

                if (result)
                {
                    if (ch == 'i')
                    {
                        symbol.TerminalId = ImagLiteral;
                        NextCh();
                    }
                    else
                        symbol.TerminalId = RealLiteral;
                }
                else
                    InitCh(savPosition);

                return result;
            }
            

                        protected virtual void ScanString()
            {
                bool multiLine = true;

                do
                {
                    NextCh();

                    if (!streamInPtr.EndLine)
                    {
                        if (true)
                        {
                            if (ch == DQUOTE)
                            {
                                symbol.TerminalId = StringLiteral;
                                NextCh();
                            }
                            else if (ch == BSLASH)
                                NextCh();
                        }
                        else if (ch == DQUOTE)
                        {
                            NextCh();

                            if ((streamInPtr.EndLine && !multiLine) ||
                                 ch != DQUOTE) symbol.TerminalId = StringLiteral;
                        }
                    }
                } while ((!streamInPtr.EndLine || multiLine) && !endText && symbol.TerminalId != StringLiteral);

                symbol.Class = SymbolClass.Text;
                symbol.Final = streamInPtr.Position;

                if (streamInPtr.EndLine && symbol.TerminalId != StringLiteral)
                    SyntaxError = "Unterminated string: " + symbol;
            }
            

                        protected virtual void ScanIdOrTerminalOrCommentStart()
            {
                TerminalDescr tRec;
                StreamPointer iPtr = streamInPtr;
                StreamPointer tPtr = iPtr;
                int fCnt; // count of calls to FindCharAndSubtree
                int tLen; // length of longest Terminal sofar
                int iLen; // length of longest Identifier sofar

                iLen = (ch.IsIdStartChar()) ? 1 : 0; // length of longest Identifier sofar }
                tLen = 0;
                fCnt = 0;
                terminalTable.FindCharInSubtreeReset();

                while ((fCnt++ >= 0) && terminalTable.FindCharInSubtree(Char.ToLower(ch), out tRec))
                {
                    if (tRec != null)
                    {
                        symbol.TerminalId = tRec.IVal;
                        symbol.Payload = tRec.Payload;
                        symbol.Class = tRec.Class;
                        tLen = fCnt;
                        tPtr = streamInPtr; // next char to be processed
                        if (terminalTable.AtLeaf) break; // terminal cannot be extended
                    }
                    NextCh();

                    if (iLen == fCnt && ch.IsBaseIdChar())
                    {
                        iLen++;
                        iPtr = streamInPtr;
                    }
                } // fCnt++ by last (i.e. failing) call
                  /*
                        At this point: (in the Prolog case, read 'Identifier and Atom made up
                        from specials' for 'Identifier'):
                        - tLen has length of Trie terminal (if any, 0 otherwise);
                        - iLen has length of Identifier (if any, 0 otherwise);
                        - fCnt is the number of characters inspected (for Id AND terminal)
                        - The character pointer is on the last character inspected (for both)
                        Iff iLen = fCnt then the entire sequence read sofar is an Identifier.
                        Now try extending the Identifier, only meaningful if iLen = fCnt.
                        Do not do this for an Atom made up from specials if a Terminal was recognized !!
                  */
                if (iLen == fCnt)
                {
                    while (true)
                    {
                        NextCh();
                        if (ch.IsBaseIdChar())
                        {
                            iLen++;
                            iPtr = streamInPtr;
                        }
                        else
                            break;
                    }
                }

                if (iLen > tLen) // tLen = 0 iff Terminal == Undefined
                {
                    symbol.TerminalId = Identifier;
                    symbol.Class = SymbolClass.Id;
                    InitCh(iPtr);
                }
                else if (symbol.TerminalId == Undefined)
                    InitCh(iPtr);
                else // we have a terminal != Identifier
                {
                    if (iLen == tLen) symbol.Class = SymbolClass.Id;
                    InitCh(tPtr);
                }

                NextCh();
            }
            

                        protected virtual void NextSymbol()
            {
                NextSymbol(null);
            }

            protected virtual void NextSymbol(string _Proc)
            {
                if (symbol.AbsSeqNo != 0 && streamInPtr.FOnLine) streamInPtr.FOnLine = false;

                symbol.PrevFinal = symbol.Final;

                if (symbol.TerminalId == EndOfInput)
                    SyntaxError = "*** Trying to read beyond end of input";

                prevTerminal = symbol.TerminalId;
                symbol.Class = SymbolClass.None;
                symbol.Payload = default(OpDescrTriplet);
                bool Break = false;

                do
                {
                                        do
                    {
                        while (Char.IsWhiteSpace(ch)) NextCh();

                        symbol.Start = streamInPtr.Position;
                        symbol.LineNo = streamInPtr.LineNo;
                        symbol.LineStart = streamInPtr.LineStart;
                        symbol.TerminalId = Undefined;
                        symbol.Class = SymbolClass.None;

                        if (endText)
                        {
                            symbol.TerminalId = EndOfInput;
                            cdh.HandleSymbol(symbol); // check for missing #endif missing
                        }
                        else if (streamInPtr.EndLine)
                            symbol.TerminalId = EndOfLine;
                        else if (Char.IsDigit(ch))
                            ScanNumber();
                        else if (ch == DQUOTE)
                            ScanString();
                        else if (ch == '.')
                        {
                            if (!ScanFraction()) ScanIdOrTerminalOrCommentStart();
                        }
                        else
                            ScanIdOrTerminalOrCommentStart();

                        symbol.Final = symbol.FinalPlus = streamInPtr.Position;

                        if (symbol.Class == SymbolClass.Comment) break;

                        if (cdh.IsExpectingId || symbol.Class == SymbolClass.Meta)
                            cdh.HandleSymbol(symbol); // if expecting: symbol must be an identifier

                    } while (cdh.CodeIsInactive || symbol.Class == SymbolClass.Meta);
                    

                    if (symbol.TerminalId == EndOfLine)
                    {
                        eoLineCount++;
                        NextCh();
                        Break = seeEndOfLine;
                    }
                    else
                    {
                        eoLineCount = 0;

                        switch (symbol.TerminalId)
                        {
                            case Identifier:
                                Break = true;
                                break;
                            case EndOfInput:
                                Break = true;
                                break;
                            case CommentStart:
                                if (stringMode)
                                    Break = true;

                                if (!DoComment("*/", true, streamInPtr.FOnLine))
                                    ErrorMessage = "Unterminated comment starting at line " + symbol.LineNo;
                                break;
                            case CommentSingle:
                                if (stringMode) Break = true; else Break = false;
                                DoComment("\n", false, streamInPtr.FOnLine);
                                eoLineCount = 1;

                                if (seeEndOfLine)
                                {
                                    symbol.TerminalId = EndOfLine;
                                    Break = true;
                                }

                                break;
                            default:
                                if (seeEndOfLine && symbol.TerminalId != EndOfLine)
                                    streamInPtr.FOnLine = false;

                                Break = true;
                                break;
                        }
                    }
                } while (!Break); // skip symbols while in preprocessing ignore mode

                symbol.AbsSeqNo++;
                symbol.RelSeqNo++;
            }

            protected virtual bool GetSymbol(TerminalSet followers, bool done, bool genXCPN)
            {
                throw new Exception("GetSymbol must be overridden");
            }

            protected void EOL(TerminalSet followers, bool GenXCPN)
            {
                GetSymbol(new TerminalSet(EndOfLine), true, GenXCPN);

                while (symbol.TerminalId == EndOfLine)
                {
                    NextSymbol(null);
                }
                symbol.SetProcessed(false);
            }

            protected void ANYTEXT(TerminalSet followers)
            {
                StreamPointer anyStart;
                StreamPointer follower;

                if (symbol.IsProcessed)
                    anyTextStart = streamInPtr.Position;
                else
                    anyTextStart = symbol.Start;

                if (followers.Contains(symbol.TerminalId) && !symbol.IsProcessed)
                {
                    anyTextFinal = anyTextStart; // empty, nullstring
                    anyTextFPlus = anyTextStart;

                    return;
                }

                parseAnyText = true;
                anyStart = streamInPtr;

                do
                {
                    // Follower will eventually be a symbol from Followers
                    follower = streamInPtr;
                    NextSymbol(null);
                    symbol.FinalPlus = symbol.Start; // up to next symbol
                    anyTextFPlus = symbol.Start;
                }

                while (symbol.TerminalId != EndOfInput && !followers.Contains(symbol.TerminalId));

                // "unread" last symbol
                InitCh(follower);
                // set AnyText "symbol" values
                symbol.Start = anyTextStart;
                symbol.LineNo = anyStart.LineNo;
                symbol.LineStart = anyStart.LineStart;
                symbol.Final = streamInPtr.Position;
                symbol.TerminalId = Undefined;
                symbol.SetProcessed(true);
                anyTextFinal = streamInPtr.Position;
                parseAnyText = false;
            }


            protected void InitPositionMarkers(positionMarker[] ma)
            {
                for (int i = 0; i < 4; i++) ma[i].Start = UNDEF;
            }


            protected void ReportParserProcEntry(string procName)
            {
                traceIndentLevel++;
                Console.WriteLine("{0}=> {1} {2}", new string(' ', 2 * traceIndentLevel), procName, symbol.ToString());
            }


            protected void ReportParserProcExit(string procName)
            {
                Console.WriteLine("{0}<= {1} {2}", new string(' ', 2 * traceIndentLevel), procName, symbol.ToString());
                traceIndentLevel--;
            }


            protected void InputStreamMark(out positionMarker m)
            {
                m.Pointer = streamInPtr;
                m.Terminal = symbol.TerminalId;
                m.Class = symbol.Class;
                m.Payload = symbol.Payload;
                m.Start = symbol.Start;
                m.Final = symbol.Final;
                m.FinalPlus = symbol.FinalPlus;
                m.PrevFinal = symbol.PrevFinal;
                m.LineNo = symbol.LineNo;
                m.AbsSeqNo = symbol.AbsSeqNo;
                m.RelSeqNo = symbol.RelSeqNo;
                m.LineStart = symbol.LineStart;
                m.Processed = symbol.IsProcessed;
                m.IsSet = true;
            }


            protected void InputStreamRedo(positionMarker m, int n)
            {
                if (!m.IsSet) throw new Exception("REDO Error: positionMarker " + n + " is not set");

                InitCh(m.Pointer);
                symbol.TerminalId = m.Terminal;
                symbol.Class = m.Class;
                symbol.Payload = m.Payload;
                symbol.Start = m.Start;
                symbol.Final = m.Final;
                symbol.FinalPlus = m.FinalPlus;
                symbol.PrevFinal = m.PrevFinal;
                symbol.LineNo = m.LineNo;
                symbol.AbsSeqNo = m.AbsSeqNo;
                symbol.RelSeqNo = m.RelSeqNo;
                symbol.LineStart = m.LineStart;
                symbol.SetProcessed(m.Processed);
            }
            
                        public virtual void RootCall()
            {
            }


            public virtual void Delegates()
            {
            }

            public void InitParse()
            {
                stringMode = false;
                parseAnyText = false;
                anyTextStart = 0;
                anyTextFinal = 0;
                anyTextFPlus = 0;
                seeEndOfLine = false;
                eoLineCount = 1;
                // For the very first symbol pretend that an EndOfLine preceded it (for formatting purposes)
                // This also means that leading blank lines will be formatted
                symbol.SetProcessed(true);
                symbol.AbsSeqNo = 0;
                symbol.RelSeqNo = 0;
                symbol.TerminalId = Undefined;
                streamInPtr.Position = UNDEF;
                streamInPtr.LineNo = 0;
                streamInPtr.LineStart = UNDEF;
                streamInPtr.EndLine = true;
                streamInPtr.FOnLine = true;
                InputStreamMark(out errMark); // just to make the compiler happy when there is no ERRMARK+
                errMark.Start = UNDEF;
                errorMessage = null;
                syntaxErrorStat = false;
                endText = false;
                prevCh = ' ';
                traceIndentLevel = -1;
                streamInLen += streamInPreLen;
                inStream.UpdateCache(0);
                cdh.Initialize();
                NextCh();
                Delegates();
            }

            protected void Parse()
            {
                ParseEx();
            }


            private void ParseEx()
            {
                InitParse();

                try
                {
                    RootCall();
                    lineCount = LineNo;

                    if (symbol.IsProcessed) NextSymbol(null);

                    if (symbol.TerminalId != EndOfInput)
                        throw new ConsultException($"Unexpected symbol {symbol} after end of input", symbol: symbol);
                }
                catch (ConsultException e)
                {
                    throw e;
                }
                catch (Exception e) // other errors
                {
                    errorMessage =
                        $"*** Line {LineNo}: {e.Message}{(showErrTrace ? Environment.NewLine + e.StackTrace : null)}";

                    throw new ConsultException(errorMessage, symbol: symbol);
                }
                finally
                {
                    ExitParse();
                }
            }


            public void ExitParse()
            {
                if (inStream != null) inStream.Close();

                inStream = null;
                Prefix = "";
                symbol.LineNo = UNDEF;
            }
            
                        public void ClipStart()
            {
                clipBegin = symbol.Start;
            }


            public void ClipStart(int clipBegin)
            {
                clipBegin = symbol.Start;
            }


            public void ClipFinal()
            {
                clipEnd = symbol.Start;
            }


            public void ClipFinal(int clipBegin)
            {
                clipEnd = symbol.Start;
            }


            public void ClipFinalPlus() // includes current symbol
            {
                clipEnd = symbol.Final;
            }


            public void ClipFinalPlus(int clipEnd)
            {
                clipEnd = symbol.Final;
            }


            public void ClipFinalTrim()  /// exclude any text (i.e. comment) after last symbol
            {
                clipEnd = symbol.PrevFinal;
            }


            public void ClipFinalTrim(int clipEnd)
            {
                clipEnd = symbol.PrevFinal;
            }


            public void GetStreamInChar(int n, out char c)
            {
                if (n < streamInPreLen)
                    c = streamInPrefix[n];
                else
                {
                    n -= streamInPreLen;
                    c = inStream[n];
                }
            }


            private string GetStreamInString(int n, int m)
            {
                string p;

                if (n < streamInPreLen) // start in Prefix
                {
                    if (m < streamInPreLen) // end in Prefix
                    {
                        return streamInPrefix.Substring(n, m - n);
                    }
                    else // end beyond Prefix
                    {
                        p = streamInPrefix.Substring(n, streamInPreLen - n - 1); // part in Prefix
                                                                                 // Overlap. Number of chars taken from prefix is streamInPreLen - n,
                                                                                 // so decrease final position m accordingly. Set n to 0.
                        n = 0;
                        m -= streamInPreLen - n;
                    }
                }
                else
                {
                    n -= streamInPreLen;
                    m -= streamInPreLen;
                    p = "";
                }

                return p + inStream.Substring(n, m - n);
            }


            public int LineNo => symbol.LineNo;


            public int ColNo => symbol.ColNo;


            protected void NextCh()
            {
                if (endText) return;

                if (streamInPtr.Position == streamInLen - 1)
                {
                    endText = true;
                    streamInPtr.EndLine = true;
                    ch = '\0';
                    streamInPtr.Position++;
                }
                else
                {
                    if (streamInPtr.EndLine)
                    {
                        streamInPtr.LineNo++;
                        streamInPtr.LineStart = streamInPtr.Position + 1;
                        symbol.RelSeqNo = 0;
                        streamInPtr.EndLine = false;
                        streamInPtr.FOnLine = true;
                    }

                    prevCh = ch;
                    streamInPtr.Position++;
                    GetStreamInChar(streamInPtr.Position, out ch);
                    streamInPtr.EndLine = (ch == '\n');
                }
            }


            protected void InitCh(StreamPointer c)
            {
                streamInPtr = c;
                GetStreamInChar(streamInPtr.Position, out ch);

                if (streamInPtr.Position <= 0)
                    prevCh = ' ';
                else
                    GetStreamInChar(streamInPtr.Position - 1, out prevCh);

                endText = (streamInPtr.Position > streamInLen - 1);

                if (endText) ch = '\x0';
            }


            public string ReadLine()
            {
                if (endText) return null;

                int start = streamInPtr.Position;
                int final = start;

                while (!streamInPtr.EndLine) NextCh();

                final = streamInPtr.Position;
                NextCh();

                return StreamInClip(start, final).TrimEnd('\r');
            }


            public int ReadChar()
            {
                if (endText) return -1;

                int result = ch;

                NextCh();

                return result;
            }


            protected bool SkipOverChars(string p)
            {
                if (p.Length == 0) return false;

                do
                {
                    if (ch == p[0])
                    {
                        int i = 0;

                        do
                        {
                            NextCh();

                            if (++i == p.Length) return true;

                        } while (ch == p[i]);
                    }
                    else
                        NextCh();
                } while (!endText);

                return false;
            }

            protected bool DoComment(string p, bool multiLine, bool firstOnLine)
            {
                bool result = SkipOverChars(p);

                symbol.Final = streamInPtr.Position;

                return result;
            }
                    }
        
                public class Buffer
        {
            private Stack<object> indentStack = new Stack<object>();
            protected char indentChar = '\u0020';
            protected int indentDelta = 2;
            protected string name;
            protected bool quietMode = false;
            protected bool firstSymbolOnLine = true;
            protected int positionOnLine = 0;
            protected int rightMargin = -1; // i.e. not set
            public int indentLength = 0;

            public int RightMargin { get { return rightMargin; } set { rightMargin = value; } }

            public virtual char this[int i] => '\0';


            public string Name => name;


            public virtual string[] Lines => null;


            public virtual string Substring(int n, int len)
            {
                return null; // gets overridden
            }


            public virtual int Length => 0;


            public virtual void UpdateCache(int p)
            {
            }


            public void Indent()
            {
                indentStack.Push(indentLength); // save current
                indentLength += indentDelta;
            }


            public void Undent()
            {
                indentLength = (indentStack.Count == 0) ? 0 : (int)indentStack.Pop();
            }


            public void Indent(int i)
            {
                indentStack.Push(indentLength); // save current
                indentLength = i;
            }


            public virtual void Write(string s, params object[] pa)
            {
            }


            public virtual void WriteLine(string s, params object[] pa)
            {
            }


            public virtual void WriteChar(char c)
            {
            }


            public virtual void NewLine()
            {
            }


            public bool AtLeftMargin // at beginning of line
            {
                get
                {
                    if (quietMode) return false;

                    return (positionOnLine == 0);
                }
            }


            public virtual void Clear()
            {
            }


            public virtual void SaveToFile(string fileName)
            {
            }


            public virtual void Close()
            {
            }


            public bool QuietMode
            {
                get { return quietMode; }
                set { quietMode = value; }
            }


            public virtual bool FirstSymbolOnLine => firstSymbolOnLine;


            public virtual int PositionOnLine => positionOnLine;
        }

                public class StringBuffer : Buffer
        {
        }


                public class StringReadBuffer : StringBuffer
        {
            private string buffer;

            public StringReadBuffer(string s)
            {
                buffer = s;
                name = "input string";
            }


            public override char this[int i] => buffer[i];


            public override int Length => buffer?.Length ?? 0;


            public override string Substring(int n, int len)
            {
                try
                {
                    return buffer.Substring(n, len);
                }
                catch
                {
                    return null;
                }
            }
        }
        

                public class FileBuffer : Buffer
        {
            protected Stream fs;
        }

                public class FileReadBuffer : FileBuffer
        {
            private const int CACHESIZE = 256 * 1024;
            private byte[] cache = new byte[CACHESIZE];
            private int cacheOfs; // number of chars in fs before first char of cache
            private int cacheLen; // cache length (normally CACHESIZE, less at eof)
            private bool little_endian;
            private StringBuilder sb;

            public FileReadBuffer(string fileName)
              : this(null, fileName)
            {
            }

            public FileReadBuffer(Stream stream, string streamName)
            {
                name = streamName;

                try
                {
                    if (stream == null) stream = new FileStream(streamName, FileMode.Open, FileAccess.Read, FileShare.Read);
                    fs = stream;
                    sb = new StringBuilder();
                    cacheOfs = 0;
                    cacheLen = 0;
                }
                catch
                {
                    throw new ConsultException($"*** Could not open file '{streamName}' for reading");
                }

                if (fs.Length >= 2) // try to work out type of file (primitive approach)
                {
                    fs.Read(cache, 0, 2);
                    little_endian = (cache[0] == '\xFF' && cache[1] == '\xFE');
                    fs.Position = 0; // rewind
                }
            }


            ~FileReadBuffer()
            {
                if (fs != null) Close();
            }


            public override void Close()
            {
                fs.Dispose();
            }


            public override void UpdateCache(int p)
            {
                int i;
                cacheOfs = CACHESIZE * (p / CACHESIZE);

                if (cacheOfs > fs.Length)
                    throw new Exception($"*** Attempt to read beyond end of FileReadBuffer '{name}'");
                else
                    fs.Position = cacheOfs;

                cacheLen = fs.Read(cache, 0, CACHESIZE); // cacheLen is actual number of bytes read

                if (cacheLen < CACHESIZE)
                {
                    for (i = cacheLen; i < CACHESIZE; cache[i++] = 32) ;
                    //cacheLen += 2;
                }
            }


            public override char this[int i]
            {
                get
                {
                    if (little_endian) i = 2 * i + 2;
                    if ((i < cacheOfs) || (i >= cacheOfs + cacheLen)) UpdateCache(i);
                    return (char)cache[i % CACHESIZE];  // no test on cacheLen
                }
            }


            public override string Substring(int n, int len)
            {
                sb.Length = 0;
                for (int i = n; i < n + len; i++) sb.Append(this[i]);

                return sb.ToString();
            }


            public override int Length => Convert.ToInt32(((little_endian) ? (fs.Length / 2 - 1) : fs.Length));
        }
        

        // FileWriteBuffer

                public class FileWriteBuffer : FileBuffer
        {
            private StreamWriter sw;
            private bool isTemp;

            public FileWriteBuffer(string fileName)
            {
                name = fileName;

                try
                {
                    fs = new FileStream(fileName, FileMode.Create, FileAccess.Write);
                    sw = new StreamWriter(fs);
                    isTemp = false;
                }
                catch
                {
                    throw new ConsultException($"*** Could not create file '{fileName}'");
                }
            }


            public FileWriteBuffer()
            {
                try
                {
                    name = Path.GetTempFileName();
                    fs = new FileStream(name, FileMode.Create);
                    sw = new StreamWriter(fs);
                    isTemp = true;
                }
                catch
                {
                    throw new Exception("*** FileWriteBuffer constructor could not create temporary file");
                }
            }


            ~FileWriteBuffer()
            {
                Close();
            }


            public override void SaveToFile(string fileName)
            {
                FileStream f = new FileStream(fileName, FileMode.Create);
                byte[] b = new byte[fs.Length];
                fs.Read(b, 0, b.Length);
                f.Write(b, 0, b.Length);
                f.Dispose();
            }


            public override string Substring(int n, int len)
            {
                byte[] b = new byte[len];
                fs.Position = n;
                fs.Read(b, 0, len);
                return new ASCIIEncoding().GetString(b);
            }


            public override string ToString()
            {
                byte[] b = new byte[fs.Length];
                fs.Read(b, 0, b.Length);
                ASCIIEncoding enc = new ASCIIEncoding();
                return enc.GetString(b);
            }


            public override void Close()
            {
                try { sw.Dispose(); }
                catch { return; }

                if (isTemp)
                {
                    try
                    {
                        File.Delete(name);
                    }
                    catch
                    {
                        throw new Exception("*** FileWriteBuffer Close() could not delete temporary file");
                    }
                }
            }


            public void Append(FileWriteBuffer f)  // f is assumed open for reading
            {
                byte[] b = new byte[f.fs.Length];
                f.fs.Read(b, 0, b.Length);
                fs.Position = fs.Length;
                fs.Write(b, 0, b.Length);
                firstSymbolOnLine = false;
            }


            public new int Length
            {
                get { return Convert.ToInt32(fs.Length); }
                set { fs.SetLength(value); }
            }


            public override void Clear()
            {
                fs.SetLength(0);
                firstSymbolOnLine = true;
            }


            public override void Write(string s, params object[] pa)
            {
                if (quietMode || s == "") return;

                if (s == null) s = "";

                if (firstSymbolOnLine)
                {
                    sw.Write(new String(indentChar, indentLength));
                    firstSymbolOnLine = false;
                    positionOnLine = indentLength;
                }

                string xs = (pa.Length == 0) ? s : string.Format(s, pa); // expanded s

                if (rightMargin > 0 && positionOnLine + xs.Length > rightMargin)
                {
                    sw.Write("{0}{1}{2}", Environment.NewLine, new String(indentChar, indentLength), xs);
                    positionOnLine = indentLength + xs.Length;
                }
                else
                {
                    sw.Write(xs);
                    positionOnLine += xs.Length;
                }
            }


            public override void WriteLine(string s, params object[] pa)
            {
                if (quietMode) return;

                if (s == null) s = "";

                if (firstSymbolOnLine) sw.Write(new String(indentChar, indentLength));

                string xs = (pa.Length == 0) ? s : string.Format(s, pa); // expanded s

                if (rightMargin > 0 && positionOnLine + xs.Length > rightMargin)
                    sw.WriteLine("{0}{1}{0}{2}", Environment.NewLine, new String(indentChar, indentLength), xs);
                else
                    sw.WriteLine(xs, pa);

                firstSymbolOnLine = true;
                positionOnLine = 0;
            }


            public override void WriteChar(char c)
            {
                if (quietMode) return;

                if (rightMargin > 0 && positionOnLine + 1 > rightMargin)
                {
                    sw.Write("{0}{1}{2}", Environment.NewLine, new String(indentChar, indentLength), c);
                    positionOnLine = indentLength + 1;
                }
                else
                {
                    sw.Write(c);
                    positionOnLine++;
                }
            }


            public override void NewLine()
            {
                if (quietMode) return;

                sw.Write(Environment.NewLine);
                firstSymbolOnLine = true;
                positionOnLine = 0;
            }
        }
        
                    }

    
    internal static class BaseParserExtensions
    {
        public static bool IsIdStartChar(this char c)
        {
            return (Char.IsLetter(c) || c == '_');
        }

        public static bool IsBaseIdChar(this char c)
        {
            return (Char.IsLetter(c) || char.IsDigit(c) || c == '_');
        }
    }
    }

