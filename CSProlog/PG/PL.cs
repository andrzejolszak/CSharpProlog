//#define LL1_tracing
namespace Prolog
{
    using System;
    using System.Collections.Generic;

    /* _______________________________________________________________________________________________
      |                                                                                               |
      |  C#Prolog -- Copyright (C) 2007 John Pool -- j.pool@ision.nl                                  |
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

    // Parser Generator version 4.0 -- Date/Time: 03-Apr-14 21:12:01


    public partial class PrologEngine
    {
                public partial class PrologParser : BaseParser
        {
            private string _currentTestGroup;

            protected override char ppChar => '!';

            
            /* The following constants are defined in BaseParser.cs:
            const int Undefined = 0;
            const int Comma = 1;
            const int LeftParen = 2;
            const int RightParen = 3;
            const int Identifier = 4;
            const int IntLiteral = 5;
            const int ppDefine = 6;
            const int ppUndefine = 7;
            const int ppIf = 8;
            const int ppIfNot = 9;
            const int ppElse = 10;
            const int ppElseIf = 11;
            const int ppEndIf = 12;
            const int RealLiteral = 13;
            const int ImagLiteral = 14;
            const int StringLiteral = 15;
            const int CharLiteral = 16;
            const int CommentStart = 17;
            const int CommentSingle = 18;
            const int EndOfLine = 19;
            const int ANYSYM = 20;
            const int EndOfInput = 21;
            */
            private const int Operator = 22;
            private const int Atom = 23;
            private const int VerbatimStringStart = 24;
            private const int Dot = 25;
            private const int Anonymous = 26;
            private const int CutSym = 27;
            private const int ImpliesSym = 28;
            private const int PromptSym = 29;
            private const int DCGArrowSym = 30;
            private const int BuiltinCSharp = 31;
            private const int LSqBracket = 32;
            private const int RSqBracket = 33;
            private const int LCuBracket = 34;
            private const int RCuBracket = 35;
            private const int VBar = 36;
            private const int OpSym = 37;
            private const int WrapSym = 38;
            private const int BuiltinSym = 39;
            private const int ProgramSym = 40;
            private const int ReadingSym = 41;
            private const int EnsureLoaded = 42;
            private const int Discontiguous = 43;
            private const int StringStyle = 44;
            private const int AllDiscontiguous = 45;
            private const int Module = 46;
            private const int Dynamic = 47;
            private const int ListPatternOpen = 48;
            private const int ListPatternClose = 49;
            private const int EllipsisSym = 50;
            private const int SubtreeSym = 51;
            private const int NegateSym = 52;
            private const int PlusSym = 53;
            private const int TimesSym = 54;
            private const int QuestionMark = 55;
            private const int QuestionMarks = 56;
            private const int TrySym = 57;
            private const int CatchSym = 58;
            private const int WrapOpen = 59;
            private const int WrapClose = 60;
            private const int AltListOpen = 61;
            private const int AltListClose = 62;
            private const int Slash = 63;
            private const int VerbatimStringLiteral = 64;
            private const int BeginTests = 65;
            private const int EndTests = 66;
            // Total number of terminals:
            public const int terminalCount = 67;

            public static void FillTerminalTable(BaseTrie terminalTable)
            {
                terminalTable.Add(Undefined, SymbolClass.None, "Undefined");
                terminalTable.Add(Comma, SymbolClass.None, "Comma", ",");
                terminalTable.Add(LeftParen, SymbolClass.Group, "LeftParen", "(");
                terminalTable.Add(RightParen, SymbolClass.Group, "RightParen", ")");
                terminalTable.Add(Identifier, SymbolClass.Id, "Identifier");
                terminalTable.Add(IntLiteral, SymbolClass.Number, "IntLiteral");
                terminalTable.Add(ppDefine, SymbolClass.Meta, "ppDefine", "!define");
                terminalTable.Add(ppUndefine, SymbolClass.Meta, "ppUndefine", "!undefine");
                terminalTable.Add(ppIf, SymbolClass.Meta, "ppIf", "!if");
                terminalTable.Add(ppIfNot, SymbolClass.Meta, "ppIfNot", "!ifnot");
                terminalTable.Add(ppElse, SymbolClass.Meta, "ppElse", "!else");
                terminalTable.Add(ppElseIf, SymbolClass.Meta, "ppElseIf", "!elseif");
                terminalTable.Add(ppEndIf, SymbolClass.Meta, "ppEndIf", "!endif");
                terminalTable.Add(RealLiteral, SymbolClass.Number, "RealLiteral");
                terminalTable.Add(ImagLiteral, SymbolClass.Number, "ImagLiteral");
                terminalTable.Add(StringLiteral, SymbolClass.Text, "StringLiteral");
                terminalTable.Add(CharLiteral, SymbolClass.Text, "CharLiteral");
                terminalTable.Add(CommentStart, SymbolClass.Comment, "CommentStart", "/*");
                terminalTable.Add(CommentSingle, SymbolClass.Comment, "CommentSingle", "%");
                terminalTable.Add(EndOfLine, SymbolClass.None, "EndOfLine");
                terminalTable.Add(ANYSYM, SymbolClass.None, "ANYSYM");
                terminalTable.Add(EndOfInput, SymbolClass.None, "EndOfInput");
                terminalTable.Add(Operator, SymbolClass.None, "Operator");
                terminalTable.Add(Atom, SymbolClass.None, "Atom");
                terminalTable.Add(VerbatimStringStart, SymbolClass.None, "VerbatimStringStart", @"@""");
                terminalTable.Add(Dot, SymbolClass.None, "Dot");
                terminalTable.Add(Anonymous, SymbolClass.None, "Anonymous", "_");
                terminalTable.Add(CutSym, SymbolClass.None, "CutSym", "!");
                terminalTable.Add(ImpliesSym, SymbolClass.None, "ImpliesSym", ":-");
                terminalTable.Add(PromptSym, SymbolClass.None, "PromptSym", "?-");
                terminalTable.Add(DCGArrowSym, SymbolClass.None, "DCGArrowSym", "-->");
                terminalTable.Add(BuiltinCSharp, SymbolClass.None, "BuiltinCSharp", ":==");
                terminalTable.Add(LSqBracket, SymbolClass.Group, "LSqBracket", "[");
                terminalTable.Add(RSqBracket, SymbolClass.Group, "RSqBracket", "]");
                terminalTable.Add(LCuBracket, SymbolClass.Group, "LCuBracket", "{");
                terminalTable.Add(RCuBracket, SymbolClass.Group, "RCuBracket", "}");
                terminalTable.Add(VBar, SymbolClass.None, "VBar", "|");
                terminalTable.Add(OpSym, SymbolClass.None, "OpSym", "op");
                terminalTable.Add(WrapSym, SymbolClass.None, "WrapSym", "wrap");
                terminalTable.Add(BuiltinSym, SymbolClass.None, "BuiltinSym", "&builtin");
                terminalTable.Add(ProgramSym, SymbolClass.None, "ProgramSym", "&program");
                terminalTable.Add(ReadingSym, SymbolClass.None, "ReadingSym", "&reading");
                terminalTable.Add(EnsureLoaded, SymbolClass.None, "EnsureLoaded", "ensure_loaded");
                terminalTable.Add(Discontiguous, SymbolClass.None, "Discontiguous", "discontiguous");
                terminalTable.Add(StringStyle, SymbolClass.None, "StringStyle", "stringstyle");
                terminalTable.Add(AllDiscontiguous, SymbolClass.None, "AllDiscontiguous", "alldiscontiguous");
                terminalTable.Add(Module, SymbolClass.None, "Module", "module");
                terminalTable.Add(Dynamic, SymbolClass.None, "Dynamic", "dynamic");
                terminalTable.Add(ListPatternOpen, SymbolClass.Group, "ListPatternOpen", "[!");
                terminalTable.Add(ListPatternClose, SymbolClass.Group, "ListPatternClose", "!]");
                terminalTable.Add(EllipsisSym, SymbolClass.None, "EllipsisSym", "..");
                terminalTable.Add(SubtreeSym, SymbolClass.None, "SubtreeSym", @"\");
                terminalTable.Add(NegateSym, SymbolClass.None, "NegateSym", "~");
                terminalTable.Add(PlusSym, SymbolClass.None, "PlusSym", "+");
                terminalTable.Add(TimesSym, SymbolClass.None, "TimesSym", "*");
                terminalTable.Add(QuestionMark, SymbolClass.None, "QuestionMark", "?");
                terminalTable.Add(QuestionMarks, SymbolClass.None, "QuestionMarks", "??");
                terminalTable.Add(TrySym, SymbolClass.None, "TrySym", "TRY");
                terminalTable.Add(CatchSym, SymbolClass.None, "CatchSym", "CATCH");
                terminalTable.Add(WrapOpen, SymbolClass.None, "WrapOpen");
                terminalTable.Add(WrapClose, SymbolClass.None, "WrapClose");
                terminalTable.Add(AltListOpen, SymbolClass.None, "AltListOpen");
                terminalTable.Add(AltListClose, SymbolClass.None, "AltListClose");
                terminalTable.Add(Slash, SymbolClass.None, "Slash");
                terminalTable.Add(VerbatimStringLiteral, SymbolClass.None, "VerbatimStringLiteral");
                terminalTable.Add(BeginTests, SymbolClass.None, "BeginTests", "begin_tests");
                terminalTable.Add(EndTests, SymbolClass.None, "BeginTests", "end_tests");
            }

            
                        public PrologParser(PrologEngine engine)
            {
                this.engine = engine;
                predTable = engine.PredTable;
                terminalTable = engine.terminalTable;
                opTable = engine.OpTable;
                symbol = new Symbol(this);
                streamInPrefix = "";
                streamInPreLen = 0;
                AddReservedOperators();
            }
            
            protected override bool GetSymbol(TerminalSet followers, bool done, bool genXCPN)
            {
                string s;

                if (symbol.IsProcessed) NextSymbol();

                symbol.SetProcessed(done);
                if (parseAnyText || followers.IsEmpty()) return true;

                if (syntaxErrorStat) return false;

                if (symbol.TerminalId == ANYSYM || followers.Contains(symbol.TerminalId)) return true;

                switch (symbol.TerminalId)
                {
                    case EndOfLine:
                        if (seeEndOfLine) s = "<EndOfLine>"; else goto default;
                        s = "<EndOfLine>";
                        break;
                    case EndOfInput:
                        s = "<EndOfInput>";
                        break;
                    default:
                        s = $"\"{symbol}\"";
                        break;
                }

                s =
                    $"*** Unexpected symbol: {s}{Environment.NewLine}*** Expected one of: {terminalTable.TerminalImageSet(followers)}";
                if (genXCPN)
                    SyntaxError = s;
                else
                    errorMessage = s;

                return true;
            }
            
                        public override void RootCall()
            {
                PrologCode(new TerminalSet(terminalCount, EndOfInput));
            }


            public override void Delegates()
            {

            }


                        private void PrologCode(TerminalSet _TS)
            {
                SetCommaAsSeparator(false); // Comma-role only if comma is separating arguments
                terminalTable[OP] = OpSym;
                terminalTable[WRAP] = WrapSym;
                InQueryMode = false;
                try
                {
                    SeeEndOfLine = false;
                    terminalTable[ELLIPSIS] = Operator;
                    terminalTable[SUBTREE] = Operator;
                    terminalTable[STRINGSTYLE] = Atom;
                    if (terminalTable[NEGATE] == NegateSym) terminalTable[NEGATE] = Atom;
                    GetSymbol(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                         Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, OpSym, WrapSym,
                                                         BuiltinSym, ProgramSym, ReadingSym, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                                         AltListOpen, AltListClose, VerbatimStringLiteral), false, true);
                    if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                           Anonymous, CutSym, LSqBracket, LCuBracket, OpSym, WrapSym, BuiltinSym, ProgramSym, ReadingSym,
                                           ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose, VerbatimStringLiteral))
                    {
                        GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                                   Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, OpSym, WrapSym,
                                                                   BuiltinSym, ProgramSym, ReadingSym, ListPatternOpen, TrySym, WrapOpen,
                                                                   WrapClose, AltListOpen, AltListClose, VerbatimStringLiteral), false, true);
                        if (symbol.TerminalId == BuiltinSym)
                        {
                            symbol.SetProcessed();
                            Initialize();
                            Predefineds(_TS);
                        }
                        else if (symbol.TerminalId == ProgramSym)
                        {
                            symbol.SetProcessed();
                            Initialize();
                            Program(_TS);
                        }
                        else if (symbol.TerminalId == ReadingSym)
                        {
                            symbol.SetProcessed();
                            PrologTerm(new TerminalSet(terminalCount, Dot), out readTerm);
                            GetSymbol(new TerminalSet(terminalCount, Dot), true, true);
                        }
                        else
                        {
                            engine.EraseVariables();
                            InQueryMode = true;
                            GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral,
                                                                       StringLiteral, Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket,
                                                                       OpSym, WrapSym, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                                                       AltListOpen, AltListClose, VerbatimStringLiteral), false, true);
                            if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                                   Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                                   AltListOpen, AltListClose, VerbatimStringLiteral))
                            {
                                terminalTable[OP] = Atom;
                                terminalTable[WRAP] = Atom;
                                SetReservedOperators(true);
                                Query(new TerminalSet(terminalCount, Dot), out queryNode);
                            }
                            else if (symbol.TerminalId == OpSym)
                            {
                                OpDefinition(new TerminalSet(terminalCount, Dot), true);
                                queryNode = null;
                            }
                            else
                            {
                                WrapDefinition(new TerminalSet(terminalCount, Dot));
                                queryNode = null;
                            }
                            GetSymbol(new TerminalSet(terminalCount, Dot), true, true);
                        }
                    }
                }
                finally { Terminate(); }
            }
            

                        private void Program(TerminalSet _TS)
            {
                bool firstReport = true;
                while (true)
                {
                    GetSymbol(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                         Operator, Atom, Anonymous, CutSym, ImpliesSym, PromptSym, LSqBracket, LCuBracket,
                                                         ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                         VerbatimStringLiteral), false, true);
                    if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                           Anonymous, CutSym, ImpliesSym, PromptSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym,
                                           WrapOpen, WrapClose, AltListOpen, AltListClose, VerbatimStringLiteral))
                    {
                        ClauseNode(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                              Operator, Atom, Anonymous, CutSym, ImpliesSym, PromptSym, LSqBracket,
                                                              LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen,
                                                              AltListClose, VerbatimStringLiteral), ref firstReport);
                    }
                    else
                        break;
                }
            }
            

                        private void ClauseNode(TerminalSet _TS, ref bool firstReport)
            {
                BaseTerm head;
                TermNode body = null;
                ClauseNode c;
                engine.EraseVariables();
                int lineNo = symbol.LineNo;
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                           Operator, Atom, Anonymous, CutSym, ImpliesSym, PromptSym, LSqBracket,
                                                           LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen,
                                                           AltListClose, VerbatimStringLiteral), false, true);
                if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                       Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                       AltListOpen, AltListClose, VerbatimStringLiteral))
                {
                    PrologTerm(new TerminalSet(terminalCount, Dot, ImpliesSym, DCGArrowSym), out head);
                    if (!head.IsCallable)
                        IO.ErrorConsult( "\r\nIllegal predicate head: {0}", head);
                    if (engine.PredTable.Predefined.Contains(head.Key))
                        IO.ErrorConsult( "\r\nPredefined predicate or operator '{0}' cannot be redefined.", head);
                    GetSymbol(new TerminalSet(terminalCount, Dot, ImpliesSym, DCGArrowSym), false, true);
                    if (symbol.IsMemberOf(ImpliesSym, DCGArrowSym))
                    {
                        GetSymbol(new TerminalSet(terminalCount, ImpliesSym, DCGArrowSym), false, true);
                        if (symbol.TerminalId == ImpliesSym)
                        {
                            symbol.SetProcessed();
                            Query(new TerminalSet(terminalCount, Dot), out body);
                        }
                        else
                        {
                            symbol.SetProcessed();
                            BaseTerm t;
                            readingDcgClause = true;
                            PrologTerm(new TerminalSet(terminalCount, Dot), out t);
                            readingDcgClause = false;
                            body = t.ToDCG(ref head);
                        }
                    }
                    c = new ClauseNode(head, body);
                    engine.ReportSingletons(c, lineNo - 1, ref firstReport);
                    predTable.AddClause(c);
                }
                else if (symbol.TerminalId == PromptSym)
                {
                    symbol.SetProcessed();
                    bool m = InQueryMode;
                    bool o = isReservedOperatorSetting;
                    try
                    {
                        InQueryMode = true;
                        SetReservedOperators(true);
                        Query(new TerminalSet(terminalCount, Dot), out queryNode);
                        IO.ErrorConsult( "'?-' querymode in file not yet supported", queryNode.Term);
                    }
                    finally
                    {
                        InQueryMode = m;
                        SetReservedOperators(o);
                    }
                }
                else
                {
                    symbol.SetProcessed();
                    terminalTable[STRINGSTYLE] = StringStyle;
                    terminalTable.Add(Module, "Module", "module");
                    terminalTable.Add(Discontiguous, "Discontiguous", "discontiguous");
                    terminalTable.Add(Dynamic, "Dynamic", "dynamic");
                    try
                    {
                        GetSymbol(new TerminalSet(terminalCount, Atom, LSqBracket, OpSym, WrapSym, EnsureLoaded, Discontiguous, StringStyle,
                                                                   AllDiscontiguous, Module, Dynamic, BeginTests, EndTests), false, true);
                        if (symbol.TerminalId == OpSym)
                        {
                            OpDefinition(new TerminalSet(terminalCount, Dot), true);
                        }
                        else if (symbol.TerminalId == Dynamic)
                        {
                            DynamicDirective(new TerminalSet(terminalCount, Dot));
                        }
                        else if (symbol.TerminalId == WrapSym)
                        {
                            WrapDefinition(new TerminalSet(terminalCount, Dot));
                        }
                        else if (symbol.TerminalId == EnsureLoaded)
                        {
                            symbol.SetProcessed();
                            GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                            GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                            if (symbol.TerminalId == Atom)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            string fileName = Utils.ExtendedFileName(symbol.ToString().ToLower(), ".pl");
                            if (engine.ConsultedFiles[fileName] == null)
                            {
                                predTable.Consult(fileName);
                                engine.ConsultedFiles[fileName] = true;
                            }
                            GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                        }
                        else if (symbol.TerminalId == Discontiguous)
                        {
                            symbol.SetProcessed();
                            BaseTerm t;
                            PrologTerm(new TerminalSet(terminalCount, Dot), out t);
                            predTable.SetDiscontiguous(t);
                        }
                        else if (symbol.TerminalId == StringStyle)
                        {
                            symbol.SetProcessed();
                            BaseTerm t;
                            PrologTerm(new TerminalSet(terminalCount, Dot), out t);
                            engine.SetStringStyle(t);
                        }
                        else if (symbol.TerminalId == AllDiscontiguous)
                        {
                            symbol.SetProcessed();
                            predTable.SetDiscontiguous(true);
                        }
                        else if (symbol.TerminalId == Module)
                        {
                            symbol.SetProcessed();
                            GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                            try
                            {
                                SetCommaAsSeparator(true);
                                GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                                if (symbol.TerminalId == Atom)
                                {
                                    symbol.SetProcessed();
                                }
                                else
                                {
                                    symbol.SetProcessed();
                                }
                                predTable.SetModuleName(symbol.ToString(), symbol);
                                IO.Warning("line {0} -- :- 'module' directive not implemented -- ignored", symbol.LineNo);
                                GetSymbol(new TerminalSet(terminalCount, Comma), true, true);
                            }
                            finally
                            {
                                SetCommaAsSeparator(false);
                            }
                            BaseTerm t;
                            PrologTerm(new TerminalSet(terminalCount, RightParen), out t);
                            GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                        }
                        else if (symbol.TerminalId == BeginTests)
                        {
                            symbol.SetProcessed();
                            GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                            GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                            if (symbol.TerminalId == Atom)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }

                            this._currentTestGroup = symbol.ToString();

                            GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                        }
                        else if (symbol.TerminalId == EndTests)
                        {
                            symbol.SetProcessed();
                            GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                            GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                            if (symbol.TerminalId == Atom)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }

                            this._currentTestGroup = null;

                            GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                        }
                        else if (symbol.TerminalId == LSqBracket)
                        {
                            symbol.SetProcessed();
                            int lines = 0;
                            int files = 0;
                            try
                            {
                                while (true)
                                {
                                    GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                                    if (symbol.TerminalId == Atom)
                                    {
                                        symbol.SetProcessed();
                                    }
                                    else
                                    {
                                        symbol.SetProcessed();
                                    }
                                    string fileName = Utils.FileNameFromSymbol(symbol.ToString(), ".pl");
                                    SetCommaAsSeparator(false);
                                    lines += predTable.Consult(fileName);
                                    files++;
                                    SetCommaAsSeparator(true);
                                    GetSymbol(new TerminalSet(terminalCount, Comma, RSqBracket), false, true);
                                    if (symbol.TerminalId == Comma)
                                    {
                                        symbol.SetProcessed();
                                    }
                                    else
                                        break;
                                }
                                if (files > 1) IO.Message("Grand total is {0} lines", lines);
                            }
                            finally
                            {
                                SetCommaAsSeparator(false);
                            }
                            GetSymbol(new TerminalSet(terminalCount, RSqBracket), true, true);
                        }
                        else
                        {
                            SimpleDirective(new TerminalSet(terminalCount, Dot));
                        }
                    }
                    finally
                    {
                        terminalTable.Remove("module");
                        terminalTable.Remove("discontiguous");
                        terminalTable.Remove("dynamic");
                        terminalTable[ELLIPSIS] = Atom;
                        terminalTable[STRINGSTYLE] = Atom;
                        terminalTable[SLASH] = Operator;
                        terminalTable[SUBTREE] = Operator;
                    }
                }
                GetSymbol(new TerminalSet(terminalCount, Dot), true, true);
            }
            

                        private void DynamicDirective(TerminalSet _TS)
            {
                GetSymbol(new TerminalSet(terminalCount, Dynamic), true, true);
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Operator, Atom), false, true);
                if (symbol.IsMemberOf(Operator, Atom))
                {
                    GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                    if (symbol.TerminalId == Atom)
                    {
                        symbol.SetProcessed();
                    }
                    else
                    {
                        symbol.SetProcessed();
                    }
                }
                else
                {
                    symbol.SetProcessed();
                    GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                    if (symbol.TerminalId == Atom)
                    {
                        symbol.SetProcessed();
                    }
                    else
                    {
                        symbol.SetProcessed();
                    }
                    GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                }
                string name = symbol.ToString();
                int saveSlash = terminalTable[SLASH];
                int arity;
                try
                {
                    terminalTable[SLASH] = Slash;
                    GetSymbol(new TerminalSet(terminalCount, Slash), true, true);
                    GetSymbol(new TerminalSet(terminalCount, IntLiteral), true, true);
                    arity = symbol.ToInt();
                    IO.Warning("line {0} -- :- 'dynamic' directive not implemented -- ignored", symbol.LineNo);
                }
                finally
                {
                    terminalTable[SLASH] = saveSlash;
                }
            }
            

                        private void SimpleDirective(TerminalSet _TS)
            {
                GetSymbol(new TerminalSet(terminalCount, Atom), true, true);
                string directive = symbol.ToString();
                bool spaceAfter = symbol.IsFollowedByLayoutChar;
                string argument = null;
                int arity = -1;
                int saveSlash = terminalTable[SLASH];
                Symbol originalSymbol = symbol.Clone();
                try
                {
                    terminalTable[SLASH] = Slash;
                    GetSymbol(_TS.Union(terminalCount, LeftParen), false, true);
                    if (symbol.TerminalId == LeftParen)
                    {
                        symbol.SetProcessed();
                        GetSymbol(new TerminalSet(terminalCount, IntLiteral, StringLiteral, Operator, Atom), false, true);
                        if (symbol.IsMemberOf(Operator, Atom))
                        {
                            if (spaceAfter)
                                IO.ErrorConsult($"Illegal space between directive '{directive}' and left parenthesis", symbol);
                            GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                            if (symbol.TerminalId == Atom)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            argument = symbol.ToString();
                            GetSymbol(new TerminalSet(terminalCount, RightParen, Slash), false, true);
                            if (symbol.TerminalId == Slash)
                            {
                                symbol.SetProcessed();
                                GetSymbol(new TerminalSet(terminalCount, IntLiteral), true, true);
                                arity = symbol.ToInt();
                            }
                        }
                        else
                        {
                            GetSymbol(new TerminalSet(terminalCount, IntLiteral, StringLiteral), false, true);
                            if (symbol.TerminalId == StringLiteral)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            argument = symbol.ToString().Dequoted();
                        }
                        GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                    }
                    predTable.HandleSimpleDirective(this, originalSymbol, directive, argument, arity);
                }
                finally
                {
                    terminalTable[SLASH] = saveSlash;
                }
            }
            

                        private void OpDefinition(TerminalSet _TS, bool user)
            {
                string name;
                string assoc;
                try
                {
                    SetCommaAsSeparator(true);
                    GetSymbol(new TerminalSet(terminalCount, OpSym), true, true);
                    int prec;
                    GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                    GetSymbol(new TerminalSet(terminalCount, IntLiteral, CutSym), false, true);
                    if (symbol.TerminalId == IntLiteral)
                    {
                        symbol.SetProcessed();
                        prec = symbol.ToInt();
                    }
                    else
                    {
                        symbol.SetProcessed();
                        prec = -1;
                    }
                    GetSymbol(new TerminalSet(terminalCount, Comma), true, true);
                    GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                    if (symbol.TerminalId == Atom)
                    {
                        symbol.SetProcessed();
                    }
                    else
                    {
                        symbol.SetProcessed();
                    }
                    assoc = symbol.ToString();
                    GetSymbol(new TerminalSet(terminalCount, Comma), true, true);
                    GetSymbol(new TerminalSet(terminalCount, LeftParen, Operator, Atom, LSqBracket, OpSym, WrapSym, EnsureLoaded,
                                                               Discontiguous, StringStyle, AllDiscontiguous, Module, Dynamic, WrapOpen,
                                                               WrapClose, BeginTests, EndTests), false, true);
                    if (symbol.TerminalId == LSqBracket)
                    {
                        symbol.SetProcessed();
                        while (true)
                        {
                            PotentialOpName(new TerminalSet(terminalCount, Comma, RSqBracket), out name);
                            if (prec == -1)
                                RemovePrologOperator(assoc, name, user);
                            else
                                AddPrologOperator(prec, assoc, name, user);
                            GetSymbol(new TerminalSet(terminalCount, Comma, RSqBracket), false, true);
                            if (symbol.TerminalId == Comma)
                            {
                                symbol.SetProcessed();
                            }
                            else
                                break;
                        }
                        GetSymbol(new TerminalSet(terminalCount, RSqBracket), true, true);
                    }
                    else
                    {
                        PotentialOpName(new TerminalSet(terminalCount, RightParen), out name);
                        if (prec == -1)
                            RemovePrologOperator(assoc, name, user);
                        else
                            AddPrologOperator(prec, assoc, name, user);
                    }
                    GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                }
                finally
                {
                    SetCommaAsSeparator(false);
                }
            }
            

                        private void WrapDefinition(TerminalSet _TS)
            {
                // wrapClose is set to the reverse of wrapOpen if only one argument is supplied.
                string wrapOpen;
                string wrapClose = null;
                bool useAsList = false;
                try
                {
                    SetCommaAsSeparator(true);
                    GetSymbol(new TerminalSet(terminalCount, WrapSym), true, true);
                    GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                    PotentialOpName(new TerminalSet(terminalCount, Comma, RightParen), out wrapOpen);
                    GetSymbol(new TerminalSet(terminalCount, Comma, RightParen), false, true);
                    if (symbol.TerminalId == Comma)
                    {
                        symbol.SetProcessed();
                        GetSymbol(new TerminalSet(terminalCount, LeftParen, RightParen, Operator, Atom, VBar, OpSym, WrapSym, EnsureLoaded,
                                                                   Discontiguous, StringStyle, AllDiscontiguous, Module, Dynamic, WrapOpen,
                                                                   WrapClose, BeginTests, EndTests), false, true);
                        if (symbol.IsMemberOf(LeftParen, Operator, Atom, VBar, OpSym, WrapSym, EnsureLoaded, Discontiguous, StringStyle,
                                               AllDiscontiguous, Module, Dynamic, WrapOpen, WrapClose, BeginTests, EndTests))
                        {
                            GetSymbol(new TerminalSet(terminalCount, LeftParen, Operator, Atom, VBar, OpSym, WrapSym, EnsureLoaded,
                                                                       Discontiguous, StringStyle, AllDiscontiguous, Module, Dynamic, WrapOpen,
                                                                       WrapClose, BeginTests, EndTests), false, true);
                            if (symbol.TerminalId == VBar)
                            {
                                symbol.SetProcessed();
                                useAsList = true;
                                GetSymbol(new TerminalSet(terminalCount, Comma, RightParen), false, true);
                                if (symbol.TerminalId == Comma)
                                {
                                    symbol.SetProcessed();
                                    PotentialOpName(new TerminalSet(terminalCount, RightParen), out wrapClose);
                                    wrapClose = symbol.ToString();
                                }
                            }
                            else
                            {
                                PotentialOpName(new TerminalSet(terminalCount, RightParen), out wrapClose);
                                wrapClose = symbol.ToString();
                            }
                        }
                    }
                    if (wrapClose == null) wrapClose = wrapOpen.Mirror();
                    GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                    AddBracketPair(wrapOpen, wrapClose, useAsList);
                }
                finally
                {
                    SetCommaAsSeparator(false);
                }
            }
            

                        private void PotentialOpName(TerminalSet _TS, out string name)
            {
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Operator, Atom, OpSym, WrapSym, EnsureLoaded, Discontiguous,
                                                           StringStyle, AllDiscontiguous, Module, Dynamic, WrapOpen, WrapClose, BeginTests, EndTests), false,
                           true);
                if (symbol.IsMemberOf(Operator, Atom, OpSym, WrapSym, EnsureLoaded, Discontiguous, StringStyle, AllDiscontiguous,
                                       Module, Dynamic, WrapOpen, WrapClose, BeginTests, EndTests))
                {
                    GetSymbol(new TerminalSet(terminalCount, Operator, Atom, OpSym, WrapSym, EnsureLoaded, Discontiguous, StringStyle,
                                                               AllDiscontiguous, Module, Dynamic, WrapOpen, WrapClose, BeginTests, EndTests), false, true);
                    if (symbol.TerminalId == Atom)
                    {
                        symbol.SetProcessed();
                    }
                    else if (symbol.TerminalId == Operator)
                    {
                        symbol.SetProcessed();
                    }
                    else if (symbol.TerminalId == WrapOpen)
                    {
                        symbol.SetProcessed();
                    }
                    else if (symbol.TerminalId == WrapClose)
                    {
                        symbol.SetProcessed();
                    }
                    else
                    {
                        ReservedWord(_TS);
                    }
                    name = symbol.ToString();
                }
                else
                {
                    symbol.SetProcessed();
                    PotentialOpName(new TerminalSet(terminalCount, RightParen), out name);
                    GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                }
            }
            

                        private void ReservedWord(TerminalSet _TS)
            {
                GetSymbol(new TerminalSet(terminalCount, OpSym, WrapSym, EnsureLoaded, Discontiguous, StringStyle, AllDiscontiguous,
                                                           Module, Dynamic, BeginTests, EndTests), false, true);
                if (symbol.TerminalId == OpSym)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == WrapSym)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == EnsureLoaded)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == Discontiguous)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == StringStyle)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == AllDiscontiguous)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == Module)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == BeginTests)
                {
                    symbol.SetProcessed();
                }
                else if (symbol.TerminalId == EndTests)
                {
                    symbol.SetProcessed();
                }
                else
                {
                    symbol.SetProcessed();
                }
            }
            

                        private void Predefineds(TerminalSet _TS)
            {
                do
                {
                    Predefined(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                          Operator, Atom, Anonymous, CutSym, ImpliesSym, LSqBracket, LCuBracket,
                                                          ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                          VerbatimStringLiteral));
                    GetSymbol(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                         Operator, Atom, Anonymous, CutSym, ImpliesSym, LSqBracket, LCuBracket,
                                                         ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                         VerbatimStringLiteral), false, true);
                } while (!(_TS.Contains(symbol.TerminalId)));
            }
            

                        private void Predefined(TerminalSet _TS)
            {
                BaseTerm head;
                bool opt = true;
                TermNode body = null;
                engine.EraseVariables();
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                           Operator, Atom, Anonymous, CutSym, ImpliesSym, LSqBracket, LCuBracket,
                                                           ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                           VerbatimStringLiteral), false, true);
                if (symbol.TerminalId == ImpliesSym)
                {
                    symbol.SetProcessed();
                    GetSymbol(new TerminalSet(terminalCount, Atom, OpSym, WrapSym), false, true);
                    if (symbol.TerminalId == OpSym)
                    {
                        OpDefinition(new TerminalSet(terminalCount, Dot), false);
                    }
                    else if (symbol.TerminalId == WrapSym)
                    {
                        WrapDefinition(new TerminalSet(terminalCount, Dot));
                    }
                    else
                    {
                        SimpleDirective(new TerminalSet(terminalCount, Dot));
                    }
                }
                else
                {
                    PrologTerm(new TerminalSet(terminalCount, Dot, ImpliesSym, DCGArrowSym, BuiltinCSharp), out head);
                    GetSymbol(new TerminalSet(terminalCount, Dot, ImpliesSym, DCGArrowSym, BuiltinCSharp), false, true);
                    if (symbol.IsMemberOf(ImpliesSym, DCGArrowSym, BuiltinCSharp))
                    {
                        GetSymbol(new TerminalSet(terminalCount, ImpliesSym, DCGArrowSym, BuiltinCSharp), false, true);
                        if (symbol.TerminalId == BuiltinCSharp)
                        {
                            symbol.SetProcessed();
                            GetSymbol(new TerminalSet(terminalCount, Operator, Atom), false, true);
                            if (symbol.TerminalId == Atom)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            predTable.AddPredefined(new ClauseNode(head, new TermNode(symbol.ToString())));
                            opt = false;
                        }
                        else if (symbol.TerminalId == ImpliesSym)
                        {
                            symbol.SetProcessed();
                            Query(new TerminalSet(terminalCount, Dot), out body);
                            predTable.AddPredefined(new ClauseNode(head, body));
                            opt = false;
                        }
                        else
                        {
                            symbol.SetProcessed();
                            BaseTerm term;
                            readingDcgClause = true;
                            PrologTerm(new TerminalSet(terminalCount, Dot), out term);
                            readingDcgClause = false;
                            body = term.ToDCG(ref head);
                            predTable.AddPredefined(new ClauseNode(head, body));
                            opt = false;
                        }
                    }
                    if (opt) predTable.AddPredefined(new ClauseNode(head, null));
                }
                GetSymbol(new TerminalSet(terminalCount, Dot), true, true);
            }
            

                        private void Query(TerminalSet _TS, out TermNode body)
            {
                BaseTerm t = null;
                PrologTerm(_TS, out t);
                body = t.ToGoalList();
            }
            

                        private void PrologTerm(TerminalSet _TS, out BaseTerm t)
            {
                bool saveStatus = SetCommaAsSeparator(false);
                PrologTermEx(_TS, out t);
                SetCommaAsSeparator(saveStatus);
            }
            

                        private void PrologTermEx(TerminalSet _TS, out BaseTerm t)
            {
                string functor;
                bool spaceAfter;
                TokenSeqToTerm tokenSeqToTerm = new TokenSeqToTerm(opTable);
                do
                {
                    BaseTerm[] args = null;
                    GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                               Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                               TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                               VerbatimStringLiteral), false, true);
                    if (symbol.TerminalId == Operator)
                    {
                        symbol.SetProcessed();
                        spaceAfter = symbol.IsFollowedByLayoutChar;
                        Symbol originalSymbol = symbol.Clone();
                        bool commaAsSeparator = !spaceAfter && tokenSeqToTerm.PrevTokenWasOperator;
                        GetSymbol(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                             Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                             TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose, VerbatimStringLiteral),
                                   false, true);
                        if (symbol.TerminalId == LeftParen)
                        {
                            symbol.SetProcessed();
                            ArgumentList(new TerminalSet(terminalCount, RightParen), out args, commaAsSeparator);
                            GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                        }

                        originalSymbol.FinalPlus = symbol.FinalPlus;
                        originalSymbol.Final = symbol.Final;

                        if (args == null)
                            tokenSeqToTerm.Add(originalSymbol, originalSymbol.Payload); // single operator
                        else if (commaAsSeparator)
                        {
                            tokenSeqToTerm.AddOperatorFunctor(originalSymbol, originalSymbol.Payload, args);
                                // operator as functor with >= 1 args
                        }
                        else
                        {
                            tokenSeqToTerm.Add(originalSymbol, originalSymbol.Payload);
                            tokenSeqToTerm.Add(args[0]);
                        }

                        this._lastCommentBlock.Clear();
                    }
                    else
                    {
                        GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral,
                                                                   StringLiteral, Atom, Anonymous, CutSym, LSqBracket, LCuBracket,
                                                                   ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                                   VerbatimStringLiteral), false, true);
                        if (symbol.TerminalId == Atom)
                        {
                            symbol.SetProcessed();
                            Symbol originalSymbol = symbol.Clone();
                            functor = symbol.ToString();
                            spaceAfter = symbol.IsFollowedByLayoutChar;
                            GetSymbol(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                                 Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                                 TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                                 VerbatimStringLiteral), false, true);
                            if (symbol.TerminalId == LeftParen)
                            {
                                symbol.SetProcessed();
                                ArgumentList(new TerminalSet(terminalCount, RightParen), out args, true);
                                GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                            }
                            string wholeComment = this._lastCommentBlock.ToString();
                            int firstNewLineIdx = wholeComment.IndexOf("\n");
                            originalSymbol.FinalPlus = symbol.FinalPlus;
                            originalSymbol.Final = symbol.Final;
                            BaseTerm addedTerm = tokenSeqToTerm.AddFunctorTerm(originalSymbol, functor, 
                                wholeComment.Substring(0, firstNewLineIdx + 1), 
                                wholeComment.Substring(firstNewLineIdx + 1), _currentTestGroup, spaceAfter, args);
                            this._lastCommentBlock.Clear();

                            if (addedTerm is AtomTerm && this.engine.PredTable.ConsultFileName != null)
                            {
                                engine.UserAtoms.Add(addedTerm as AtomTerm);
                            }
                        }
                        else if (symbol.TerminalId == LeftParen)
                        {
                            symbol.SetProcessed();
                            bool saveStatus = SetCommaAsSeparator(false);
                            PrologTermEx(new TerminalSet(terminalCount, RightParen), out t);
                            SetCommaAsSeparator(saveStatus);
                            tokenSeqToTerm.Add(t);
                            GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                        }
                        else if (symbol.TerminalId == Identifier)
                        {
                            GetIdentifier(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral,
                                                                     StringLiteral, Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket,
                                                                     ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                                     VerbatimStringLiteral), out t);
                            tokenSeqToTerm.Add(t);
                        }
                        else if (symbol.TerminalId == Anonymous)
                        {
                            symbol.SetProcessed();
                            tokenSeqToTerm.Add(new AnonymousVariable(symbol));
                        }
                        else if (symbol.TerminalId == CutSym)
                        {
                            symbol.SetProcessed();
                            tokenSeqToTerm.Add(new Cut(symbol, 0));
                        }
                        else if (symbol.TerminalId == AltListOpen)
                        {
                            AltList(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                               Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                               TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose, VerbatimStringLiteral),
                                    out t);
                            tokenSeqToTerm.Add(t);
                        }
                        else if (symbol.TerminalId == LSqBracket)
                        {
                            List(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                            Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                            TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose, VerbatimStringLiteral),
                                 out t);
                            tokenSeqToTerm.Add(t);
                        }
                        else if (symbol.IsMemberOf(IntLiteral, RealLiteral))
                        {
                            GetSymbol(new TerminalSet(terminalCount, IntLiteral, RealLiteral), false, true);
                            if (symbol.TerminalId == IntLiteral)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            tokenSeqToTerm.Add(new DecimalTerm(symbol, symbol.ToDecimal()));
                        }
                        else if (symbol.TerminalId == StringLiteral)
                        {
                            symbol.SetProcessed();
                            string s = symbol.ToUnquoted();
                            s = true ? s.Unescaped() : s.Replace("\"\"", "\"");
                            tokenSeqToTerm.Add(engine.NewIsoOrCsStringTerm(symbol, s));
                        }
                        else if (symbol.TerminalId == VerbatimStringLiteral)
                        {
                            symbol.SetProcessed();
                            string s = symbol.ToUnquoted();
                            s = s.Replace("\"\"", "\"");
                            tokenSeqToTerm.Add(engine.NewIsoOrCsStringTerm(symbol, s));
                        }
                        else if (symbol.TerminalId == LCuBracket)
                        {
                            DCGBracketList(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral,
                                                                      StringLiteral, Operator, Atom, Anonymous, CutSym, LSqBracket,
                                                                      LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen,
                                                                      AltListClose, VerbatimStringLiteral), out t);
                            tokenSeqToTerm.Add(t);
                        }
                        else if (symbol.TerminalId == WrapOpen)
                        {
                            symbol.SetProcessed();
                            string wrapOpen = symbol.ToString();
                            GetSymbol(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                                 Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                                 TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                                 VerbatimStringLiteral), false, true);
                            if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                                   Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                                   AltListOpen, AltListClose, VerbatimStringLiteral))
                            {
                                string wrapClose = engine.WrapTable.FindCloseBracket(wrapOpen);
                                bool saveStatus = SetCommaAsSeparator(false);
                                ArgumentList(new TerminalSet(terminalCount, WrapClose), out args, true);
                                SetCommaAsSeparator(saveStatus);
                                GetSymbol(new TerminalSet(terminalCount, WrapClose), true, true);
                                if (symbol.ToString() != wrapClose)
                                    IO.ErrorConsult($"Illegal wrapper close token: got '{symbol}' expected '{wrapClose}'", symbol);
                                tokenSeqToTerm.Add(new WrapperTerm(symbol, wrapOpen, wrapClose, args));
                            }
                            if (args == null) tokenSeqToTerm.Add(new AtomTerm(symbol, wrapOpen.ToAtom()));
                        }
                        else if (symbol.IsMemberOf(WrapClose, AltListClose))
                        {
                            GetSymbol(new TerminalSet(terminalCount, WrapClose, AltListClose), false, true);
                            if (symbol.TerminalId == WrapClose)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            string orphanCloseBracket = symbol.ToString();
                            tokenSeqToTerm.Add(new AtomTerm(symbol, orphanCloseBracket.ToAtom()));
                        }
                        else if (symbol.TerminalId == ListPatternOpen)
                        {
                            symbol.SetProcessed();
                            ListPatternMembers(new TerminalSet(terminalCount, ListPatternClose), out t);
                            GetSymbol(new TerminalSet(terminalCount, ListPatternClose), true, true);
                            tokenSeqToTerm.Add(t);
                        }
                        else
                        {
                            TryCatchClause(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral,
                                                                      StringLiteral, Operator, Atom, Anonymous, CutSym, LSqBracket,
                                                                      LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen,
                                                                      AltListClose, VerbatimStringLiteral), tokenSeqToTerm, out t);
                        }
                    }
                    GetSymbol(_TS.Union(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                         Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                         TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose, VerbatimStringLiteral),
                               false, true);
                } while (!(_TS.Contains(symbol.TerminalId)));
                tokenSeqToTerm.ConstructPrefixTerm(symbol, out t);
            }
            

                        private void GetIdentifier(TerminalSet _TS, out BaseTerm t)
            {
                GetSymbol(new TerminalSet(terminalCount, Identifier), true, true);
                string id = symbol.ToString();
                t = engine.GetVariable(id);
                if (t == null)
                {
                    t = new NamedVariable(symbol, id);

                    engine.SetVariable(t, id);
                }
                else
                    engine.RegisterVarNonSingleton(id);
            }
            

                        private void ArgumentList(TerminalSet _TS, out BaseTerm[] args, bool commaIsSeparator)
            {
                bool b = isReservedOperatorSetting;
                List<BaseTerm> argList = new List<BaseTerm>();
                BaseTerm a;
                bool saveStatus = SetCommaAsSeparator(commaIsSeparator);
                SetReservedOperators(true);
                while (true)
                {
                    PrologTermEx(_TS.Union(terminalCount, Comma), out a);
                    argList.Add(a);
                    GetSymbol(_TS.Union(terminalCount, Comma), false, true);
                    if (symbol.TerminalId == Comma)
                    {
                        symbol.SetProcessed();
                    }
                    else
                        break;
                }
                SetCommaAsSeparator(saveStatus);
                SetReservedOperators(b);
                args = argList.ToArray();
            }
            

                        private void ListPatternMembers(TerminalSet _TS, out BaseTerm t)
            {
                bool b = isReservedOperatorSetting;
                List<SearchTerm> searchTerms;
                bool saveStatus = SetCommaAsSeparator(true);
                int saveEllipsis = terminalTable[ELLIPSIS];
                int saveNegate = terminalTable[NEGATE];
                int saveSubtree = terminalTable[SUBTREE];
                SetReservedOperators(true);
                bool isRangeVar;
                bool lastWasRange = false;
                List<ListPatternElem> rangeTerms = new List<ListPatternElem>();
                try
                {
                    bool isSearchTerm = false;
                    BaseTerm RangeVar = null;
                    BaseTerm minLenTerm;
                    BaseTerm maxLenTerm;
                    BaseTerm altListName = null;
                    minLenTerm = maxLenTerm = new DecimalTerm(symbol, 0);
                    searchTerms = null;
                    bool isSingleVar;
                    bool isNegSearch = false;
                    while (true)
                    {
                        terminalTable[ELLIPSIS] = EllipsisSym;
                        terminalTable[NEGATE] = NegateSym;
                        terminalTable[SUBTREE] = SubtreeSym;
                        GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                                   Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                                   EllipsisSym, NegateSym, TrySym, WrapOpen, WrapClose, AltListOpen,
                                                                   AltListClose, VerbatimStringLiteral), false, true);
                        if (symbol.IsMemberOf(LCuBracket, EllipsisSym))
                        {
                            if (lastWasRange)
                            {
                                rangeTerms.Add(new ListPatternElem(symbol, minLenTerm, maxLenTerm, RangeVar, null, null, false, false));
                                RangeVar = null;
                            }
                            Range(_TS.Union(terminalCount, Comma), out minLenTerm, out maxLenTerm);
                            lastWasRange = true;
                        }
                        else
                        {
                            isRangeVar = false;
                            AlternativeTerms(_TS.Union(terminalCount, Comma, LCuBracket, EllipsisSym),
                                             saveEllipsis, saveNegate, saveSubtree, out searchTerms, out altListName, out isSingleVar, out isNegSearch
                                             );
                            isSearchTerm = true;
                            GetSymbol(_TS.Union(terminalCount, Comma, LCuBracket, EllipsisSym), false, true);
                            if (symbol.IsMemberOf(LCuBracket, EllipsisSym))
                            {
                                if (!isSingleVar) IO.ErrorConsult("Range specifier may be preceded by a variable only", symbol);
                                if (lastWasRange)
                                    rangeTerms.Add(new ListPatternElem(symbol, minLenTerm, maxLenTerm, RangeVar, null, null, false, false));
                                Range(_TS.Union(terminalCount, Comma), out minLenTerm, out maxLenTerm);
                                isRangeVar = true;
                                lastWasRange = true;
                                isSearchTerm = false;
                            }
                            if (isRangeVar)
                                RangeVar = searchTerms[0].term;
                            else
                                lastWasRange = false;
                        }
                        if (isSearchTerm)
                        {
                            rangeTerms.Add(new ListPatternElem(symbol, minLenTerm, maxLenTerm, RangeVar, altListName, searchTerms, isNegSearch, false));
                            isSearchTerm = false;
                            RangeVar = null;
                            altListName = null;
                            searchTerms = null;
                            minLenTerm = maxLenTerm = new DecimalTerm(symbol, 0);
                        }
                        GetSymbol(_TS.Union(terminalCount, Comma), false, true);
                        if (symbol.TerminalId == Comma)
                        {
                            symbol.SetProcessed();
                        }
                        else
                            break;
                    }
                    if (lastWasRange) rangeTerms.Add(new ListPatternElem(symbol, minLenTerm, maxLenTerm, RangeVar, null, null, false, false));
                    t = new ListPatternTerm(symbol, rangeTerms.ToArray());
                }
                finally
                {
                    SetCommaAsSeparator(saveStatus);
                    terminalTable[ELLIPSIS] = saveEllipsis;
                    terminalTable[NEGATE] = saveNegate;
                    terminalTable[SUBTREE] = saveSubtree;
                    SetReservedOperators(b);
                }
            }
            

                        private void AlternativeTerms(TerminalSet _TS,
                                          int saveEllipsis, int saveNegate, int saveSubtree, out List<SearchTerm> searchTerms,
                                          out BaseTerm altListName, out bool isSingleVar, out bool isNegSearch)
            {
                searchTerms = new List<SearchTerm>();
                BaseTerm t;
                altListName = null;
                bool first = true;
                DownRepFactor downRepFactor = null;
                isNegSearch = false;
                while (true)
                {
                    GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                               Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                               NegateSym, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                               VerbatimStringLiteral), false, true);
                    if (symbol.TerminalId == NegateSym)
                    {
                        if (isNegSearch) IO.ErrorConsult("Only one '~' allowed (which will apply to the entire alternatives list)", symbol);
                        GetSymbol(new TerminalSet(terminalCount, NegateSym), true, true);
                        isNegSearch = true;
                    }
                    //terminalTable [ELLIPSIS] = saveEllipsis;
                    //terminalTable [NEGATE]   = saveNegate;
                    //terminalTable [SUBTREE]  = saveSubtree;
                    PrologTermEx(_TS.Union(terminalCount, CutSym, VBar), out t);
                    //terminalTable [ELLIPSIS] = EllipsisSym;
                    //terminalTable [NEGATE]   = NegateSym;
                    //terminalTable [SUBTREE]  = SubtreeSym;
                    if (!first) searchTerms.Add(new SearchTerm(downRepFactor, t));
                    //if (t is AnonymousVariable)
                    //  IO.Warning ("Anonymous variable in alternatives list makes it match anything");
                    GetSymbol(_TS.Union(terminalCount, CutSym, VBar), false, true);
                    if (symbol.IsMemberOf(CutSym, VBar))
                    {
                        GetSymbol(new TerminalSet(terminalCount, CutSym, VBar), false, true);
                        if (symbol.TerminalId == VBar)
                        {
                            symbol.SetProcessed();
                            if (first) searchTerms.Add(new SearchTerm(downRepFactor, t));
                        }
                        else
                        {
                            symbol.SetProcessed();
                            if (first)
                            {
                                if (t is Variable)
                                {
                                    if (isNegSearch)
                                        IO.ErrorConsult("'~' not allowed before alternatives list name", t);
                                    else
                                        altListName = t;
                                }
                                else
                                    IO.ErrorConsult("Variable expected before !", t);
                                first = false;
                            }
                            else
                                IO.ErrorConsult("Only one ! allowed for alternatives list", t);
                        }
                    }
                    else
                        break;
                }
                if (first) searchTerms.Add(new SearchTerm(downRepFactor, t));
                isSingleVar = (searchTerms.Count == 1 && searchTerms[0].term is Variable);
            }
            

                        private void Range(TerminalSet _TS, out BaseTerm minLenTerm, out BaseTerm maxLenTerm)
            {
                try
                {
                    savePlusSym = terminalTable[PLUSSYM];
                    saveTimesSym = terminalTable[TIMESSYM];
                    saveQuestionMark = terminalTable[QUESTIONMARK];
                    terminalTable[PLUSSYM] = PlusSym;
                    terminalTable[TIMESSYM] = TimesSym;
                    terminalTable[QUESTIONMARK] = QuestionMark;
                    GetSymbol(new TerminalSet(terminalCount, LCuBracket, EllipsisSym), false, true);
                    if (symbol.TerminalId == LCuBracket)
                    {
                        int minLen = 0;
                        int maxLen = 0;
                        minLenTerm = maxLenTerm = DecimalTerm.ZERO;
                        GetSymbol(new TerminalSet(terminalCount, LCuBracket), true, true);
                        GetSymbol(_TS.Union(terminalCount, Comma, Identifier, IntLiteral, PlusSym, TimesSym, QuestionMark), false, true);
                        if (symbol.IsMemberOf(Comma, Identifier, IntLiteral, RCuBracket))
                        {
                            GetSymbol(new TerminalSet(terminalCount, Comma, Identifier, IntLiteral, RCuBracket), false, true);
                            if (symbol.IsMemberOf(Identifier, IntLiteral))
                            {
                                GetSymbol(new TerminalSet(terminalCount, Identifier, IntLiteral), false, true);
                                if (symbol.TerminalId == IntLiteral)
                                {
                                    symbol.SetProcessed();
                                    minLen = maxLen = symbol.ToInt();
                                    minLenTerm = maxLenTerm = new DecimalTerm(symbol, minLen);
                                }
                                else
                                {
                                    GetIdentifier(new TerminalSet(terminalCount, Comma, RCuBracket), out minLenTerm);
                                    maxLenTerm = minLenTerm;
                                }
                            }
                            GetSymbol(new TerminalSet(terminalCount, Comma, RCuBracket), false, true);
                            if (symbol.TerminalId == Comma)
                            {
                                symbol.SetProcessed();
                                maxLen = Infinite;
                                maxLenTerm = new DecimalTerm(symbol, Infinite);
                                GetSymbol(new TerminalSet(terminalCount, Identifier, IntLiteral, RCuBracket), false, true);
                                if (symbol.IsMemberOf(Identifier, IntLiteral))
                                {
                                    GetSymbol(new TerminalSet(terminalCount, Identifier, IntLiteral), false, true);
                                    if (symbol.TerminalId == IntLiteral)
                                    {
                                        symbol.SetProcessed();
                                        if (minLen > (maxLen = symbol.ToInt()))
                                            IO.ErrorConsult($"Range lower bound {minLen} not allowed to be greater than range upper bound {maxLen}", symbol);
                                        maxLenTerm = new DecimalTerm(symbol, maxLen);
                                    }
                                    else
                                    {
                                        GetIdentifier(new TerminalSet(terminalCount, RCuBracket), out maxLenTerm);
                                    }
                                }
                            }
                        }
                        else if (symbol.TerminalId == TimesSym)
                        {
                            symbol.SetProcessed();
                            minLenTerm = new DecimalTerm(symbol, 0);
                            maxLenTerm = new DecimalTerm(symbol, Infinite);
                        }
                        else if (symbol.TerminalId == PlusSym)
                        {
                            symbol.SetProcessed();
                            minLenTerm = new DecimalTerm(symbol, 1);
                            maxLenTerm = new DecimalTerm(symbol, Infinite);
                        }
                        else if (symbol.TerminalId == QuestionMark)
                        {
                            symbol.SetProcessed();
                            minLenTerm = new DecimalTerm(symbol, 0);
                            maxLenTerm = new DecimalTerm(symbol, 1);
                        }
                        GetSymbol(new TerminalSet(terminalCount, RCuBracket), true, true);
                    }
                    else
                    {
                        symbol.SetProcessed();
                        minLenTerm = new DecimalTerm(symbol, 0);
                        maxLenTerm = new DecimalTerm(symbol, Infinite);
                    }
                }
                finally
                {
                    terminalTable[PLUSSYM] = savePlusSym;
                    terminalTable[TIMESSYM] = saveTimesSym;
                    terminalTable[QUESTIONMARK] = saveQuestionMark;
                }
            }
            

                        private void TryCatchClause(TerminalSet _TS, TokenSeqToTerm tokenSeqToTerm, out BaseTerm t)
            {
                GetSymbol(new TerminalSet(terminalCount, TrySym), true, true);
                bool nullClass = false;
                tokenSeqToTerm.Add(new TryOpenTerm(symbol));
                tokenSeqToTerm.Add(symbol, CommaOpTriplet);
                GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                PrologTermEx(new TerminalSet(terminalCount, RightParen), out t);
                GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                tokenSeqToTerm.Add(t);
                tokenSeqToTerm.Add(symbol, CommaOpTriplet);
                List<string> ecNames = new List<string>();
                int catchSeqNo = 0;
                do
                {
                    GetSymbol(new TerminalSet(terminalCount, CatchSym), true, true);
                    if (nullClass)
                        IO.ErrorConsult("No CATCH-clause allowed after CATCH-clause without exception class", symbol);
                    string exceptionClass = null;
                    BaseTerm msgVar = null;
                    GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, Atom), false, true);
                    if (symbol.IsMemberOf(Identifier, IntLiteral, Atom))
                    {
                        GetSymbol(new TerminalSet(terminalCount, Identifier, IntLiteral, Atom), false, true);
                        if (symbol.IsMemberOf(IntLiteral, Atom))
                        {
                            bool saveStatus = SetCommaAsSeparator(true);
                            GetSymbol(new TerminalSet(terminalCount, IntLiteral, Atom), false, true);
                            if (symbol.TerminalId == Atom)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            if (ecNames.Contains(exceptionClass = symbol.ToString()))
                                IO.ErrorConsult($"Duplicate exception class name '{exceptionClass}'", symbol);
                            else
                                ecNames.Add(exceptionClass);
                            GetSymbol(new TerminalSet(terminalCount, Comma, LeftParen, Identifier), false, true);
                            if (symbol.IsMemberOf(Comma, Identifier))
                            {
                                GetSymbol(new TerminalSet(terminalCount, Comma, Identifier), false, true);
                                if (symbol.TerminalId == Comma)
                                {
                                    symbol.SetProcessed();
                                }
                                GetIdentifier(new TerminalSet(terminalCount, LeftParen), out msgVar);
                            }
                            SetCommaAsSeparator(saveStatus);
                        }
                        else
                        {
                            GetIdentifier(new TerminalSet(terminalCount, LeftParen), out msgVar);
                        }
                    }
                    nullClass = nullClass || (exceptionClass == null);
                    if (msgVar == null) msgVar = new AnonymousVariable(symbol);
                    tokenSeqToTerm.Add(new CatchOpenTerm(symbol, exceptionClass, msgVar, catchSeqNo++));
                    tokenSeqToTerm.Add(symbol, CommaOpTriplet);
                    t = null;
                    GetSymbol(new TerminalSet(terminalCount, LeftParen), true, true);
                    GetSymbol(new TerminalSet(terminalCount, LeftParen, RightParen, Identifier, IntLiteral, RealLiteral, ImagLiteral,
                                                               StringLiteral, Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket,
                                                               ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                               VerbatimStringLiteral), false, true);
                    if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                           Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                           AltListOpen, AltListClose, VerbatimStringLiteral))
                    {
                        PrologTermEx(new TerminalSet(terminalCount, RightParen), out t);
                    }
                    GetSymbol(new TerminalSet(terminalCount, RightParen), true, true);
                    if (t != null)
                    {
                        tokenSeqToTerm.Add(t);
                        tokenSeqToTerm.Add(symbol, CommaOpTriplet);
                    }
                    GetSymbol(_TS.Union(terminalCount, CatchSym), false, true);
                } while (!(_TS.Contains(symbol.TerminalId)));
                tokenSeqToTerm.Add(TC_CLOSE);
            }
            

                        private void OptionalPrologTerm(TerminalSet _TS, out BaseTerm t)
            {
                t = null;
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                           EndOfInput, Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket,
                                                           ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                           VerbatimStringLiteral), false, true);
                if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                       Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                       AltListOpen, AltListClose, VerbatimStringLiteral))
                {
                    PrologTerm(new TerminalSet(terminalCount, Dot), out t);
                    GetSymbol(new TerminalSet(terminalCount, Dot), true, true);
                }
                else
                {
                    symbol.SetProcessed();
                }
            }
            

                        private void List(TerminalSet _TS, out BaseTerm term)
            {
                BaseTerm afterBar = null;
                terminalTable[OP] = Atom;
                terminalTable[WRAP] = Atom;
                BaseTerm[] elements = null;
                GetSymbol(new TerminalSet(terminalCount, LSqBracket), true, true);
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                           Operator, Atom, Anonymous, CutSym, LSqBracket, RSqBracket, LCuBracket,
                                                           ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                           VerbatimStringLiteral), false, true);
                if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                       Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                       AltListOpen, AltListClose, VerbatimStringLiteral))
                {
                    ArgumentList(new TerminalSet(terminalCount, RSqBracket, VBar), out elements, true);
                    GetSymbol(new TerminalSet(terminalCount, RSqBracket, VBar), false, true);
                    if (symbol.TerminalId == VBar)
                    {
                        symbol.SetProcessed();
                        PrologTerm(new TerminalSet(terminalCount, RSqBracket), out afterBar);
                    }
                }
                terminalTable[OP] = OpSym;
                terminalTable[WRAP] = WrapSym;
                GetSymbol(new TerminalSet(terminalCount, RSqBracket), true, true);
                term = (afterBar == null) ? new ListTerm(symbol) : afterBar;
                if (elements != null) term = ListTerm.ListFromArray(elements, term);
            }
            

                        private void AltList(TerminalSet _TS, out BaseTerm term)
            {
                BaseTerm afterBar = null;
                terminalTable[OP] = Atom;
                terminalTable[WRAP] = Atom;
                BaseTerm[] elements = null;
                GetSymbol(new TerminalSet(terminalCount, AltListOpen), true, true);
                string altListOpen = symbol.ToString();
                string altListClose = engine.AltListTable.FindCloseBracket(altListOpen);
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                           Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen,
                                                           TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                           VerbatimStringLiteral), false, true);
                if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                       Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                       AltListOpen, AltListClose, VerbatimStringLiteral))
                {
                    ArgumentList(new TerminalSet(terminalCount, VBar, AltListClose), out elements, true);
                    GetSymbol(new TerminalSet(terminalCount, VBar, AltListClose), false, true);
                    if (symbol.TerminalId == VBar)
                    {
                        symbol.SetProcessed();
                        PrologTerm(new TerminalSet(terminalCount, AltListClose), out afterBar);
                    }
                }
                terminalTable[OP] = OpSym;
                terminalTable[WRAP] = WrapSym;
                GetSymbol(new TerminalSet(terminalCount, AltListClose), true, true);
                if (symbol.ToString() != altListClose)
                    IO.ErrorConsult($"Illegal alternative list close token: got '{symbol}' expected '{altListClose}'", symbol);
                term = (afterBar == null) ? new AltListTerm(symbol, altListOpen, altListClose) : afterBar;
                if (elements != null)
                    term = AltListTerm.ListFromArray(symbol, altListOpen, altListClose, elements, term);
            }
            

                        private void DCGBracketList(TerminalSet _TS, out BaseTerm term)
            {
                terminalTable[OP] = Atom;
                terminalTable[WRAP] = Atom;
                BaseTerm[] elements = null;
                GetSymbol(new TerminalSet(terminalCount, LCuBracket), true, true);
                GetSymbol(new TerminalSet(terminalCount, LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral,
                                                           Operator, Atom, Anonymous, CutSym, LSqBracket, LCuBracket, RCuBracket,
                                                           ListPatternOpen, TrySym, WrapOpen, WrapClose, AltListOpen, AltListClose,
                                                           VerbatimStringLiteral), false, true);
                if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, ImagLiteral, StringLiteral, Operator, Atom,
                                       Anonymous, CutSym, LSqBracket, LCuBracket, ListPatternOpen, TrySym, WrapOpen, WrapClose,
                                       AltListOpen, AltListClose, VerbatimStringLiteral))
                {
                    ArgumentList(new TerminalSet(terminalCount, RCuBracket), out elements, true);
                }
                GetSymbol(new TerminalSet(terminalCount, RCuBracket), true, true);
                term = BaseTerm.NULLCURL;
                if (elements != null)
                    if (readingDcgClause)
                        for (int i = elements.Length - 1; i >= 0; i--)
                            term = new DcgTerm(elements[i].Symbol, elements[i], term);
                    else
                        term = new CompoundTerm(symbol, CURL, elements);
            }
            


                    }
            }
}
