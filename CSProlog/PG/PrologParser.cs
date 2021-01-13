//#define showToken

using System;
using System.Text;

namespace Prolog
{
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

    public partial class PrologEngine
    {
        public partial class PrologParser : BaseParser
        {
            private const char USCORE = '_';

            public const string IMPLIES = ":-";
            public const string DCGIMPL = "-->";
            public const string ARROW = "->";
            public const string DOT = ".";
            public const string CUT = "!";
            public const string OP = "op";
            public const string WRAP = "wrap";
            public const string STRINGSTYLE = "stringstyle";
            public const string CURL = "{}";
            public const string EQ = "=";
            public const string COLON = ":";
            public const string COMMA = ",";
            public const string QCOMMA = "','";
            public const string SEMI = ";";
            public const string SLASH = "/";
            public const string LISTPATOPEN = "[!";
            public const string LISTPATCLOSE = "!]";
            public const string TREEPATOPEN = "{!";
            public const string TREEPATCLOSE = "!}";
            public const string ELLIPSIS = "..";
            public const string SUBTREE = "\\";
            public const string NEGATE = "~";
            public const string PLUSSYM = "+";
            public const string TIMESSYM = "*";
            public const string QUESTIONMARK = "?";

            public static readonly int Infinite = int.MaxValue;
            private readonly StringBuilder _lastCommentBlock = new StringBuilder();

            private readonly PrologEngine engine;
            private readonly OperatorTable opTable;
            private readonly PredicateTable predTable;
            private char extraUnquotedAtomChar = '_';

            private bool isReservedOperatorSetting;
            private TermNode queryNode;
            private bool readingDcgClause; // determines how to interpret {t1, ... , tn}

            private BaseTerm readTerm; // result of read (X)

            private int savePlusSym;
            private int saveQuestionMark;
            private int saveTimesSym;

            public TermNode QueryNode => queryNode;
            public BaseTerm ReadTerm => readTerm;
            public bool InQueryMode { get; private set; }

            private void Initialize()
            {
                SetReservedOperators(false);
                TerminalTable[PLUSSYM] = Operator;
                TerminalTable[TIMESSYM] = Operator;
                TerminalTable[QUESTIONMARK] = Atom;
                TerminalTable.Remove("module"); // will be temporarily set after an initial ':-' only
                TerminalTable.Remove("dynamic"); // ...
                TerminalTable.Remove("discontiguous");
                TerminalTable.Remove("alldiscontiguous");
                //terminalTable.Remove ("stringstyle");

                //if (!ConfigSettings.VerbatimStringsAllowed)
                //    terminalTable.Remove(@"@""");
            }

            private void Terminate()
            {
                InQueryMode = true;
            }

            public OperatorDescr AddPrologOperator(int prec, string type, string name, bool user)
            {
                AssocType assoc = AssocType.None;

                try
                {
                    assoc = (AssocType)Enum.Parse(typeof(AssocType), type);
                }
                catch
                {
                    IO.ErrorConsult($"Illegal operator type '{type}'", symbol);
                }

                if (prec < 0 || prec > 1200)
                {
                    IO.ErrorConsult($"Illegal precedence value {prec} for operator '{name}'", symbol);
                }

                TerminalDescr td;
                OpDescrTriplet triplet;

                if (TerminalTable.Find(name, out td))

                {
                    // some operator symbols (:-, -->, +, ...) already exist prior to their op/3 definition
                    if (td.Payload == null)
                    {
                        td.Payload = opTable.Add(prec, type, name, user);
                        td.IVal = Operator;
                    }
                    else // no need to add it -- just change its properties
                    {
                        td.Payload.Assign(name, prec, assoc, user);
                        td.IVal = Operator;
                    }

                    triplet = td.Payload;
                }
                else // new operator
                {
                    triplet = opTable.Add(prec, type, name, user);
                    TerminalTable.Add(name, Operator, triplet);
                }

                return triplet[assoc];
            }

            public void RemovePrologOperator(string type, string name, bool user)
            {
                AssocType assoc = AssocType.None;

                try
                {
                    assoc = (AssocType)Enum.Parse(typeof(AssocType), type);
                }
                catch
                {
                    IO.ErrorConsult($"Illegal operator type '{type}'", symbol);
                }

                TerminalDescr td;

                if (TerminalTable.Find(name, out td) && td.Payload != null)
                {
                    if (!user)
                    {
                        IO.ErrorConsult($"Undefine of operator ({type}, {name}) not allowed", symbol);
                    }
                    else
                    {
                        td.Payload.Unassign(name, assoc);

                        foreach (OperatorDescr od in td.Payload)
                        {
                            if (od.IsDefined)
                            {
                                return;
                            }
                        }

                        TerminalTable.Remove(name); // name no longer used
                    }
                }
                else // unknown operator
                {
                    IO.ErrorConsult($"Operator not found: ({type}, {name})", symbol);
                }
            }

            private void SetReservedOperators(bool asOpr)
            {
                if (asOpr) // parsed as Operator
                {
                    TerminalTable[IMPLIES] = Operator;
                    TerminalTable[DCGIMPL] = Operator;
                }
                else // parsed 'normally'
                {
                    TerminalTable[IMPLIES] = ImpliesSym;
                    TerminalTable[DCGIMPL] = DCGArrowSym;
                    TerminalTable[OP] = OpSym;
                }

                isReservedOperatorSetting = asOpr;
            }

            private bool SetCommaAsSeparator(bool mode)
            {
                bool result = TerminalTable[COMMA] == Comma; // returnvalue is *current* status

                TerminalTable[COMMA] = mode ? Comma : Operator; // 'Comma' is separator

                return result;
            }

            public void AddBracketPair(string openBracket, string closeBracket, bool useAsList)
            {
                if (openBracket == closeBracket)
                {
                    IO.ErrorConsult("Wrapper open and wrapper close token must be different", symbol);
                }

                if (useAsList)
                {
                    engine.AltListTable.Add(ref openBracket, ref closeBracket); // possible quotes are removed
                    TerminalTable.AddOrReplace(AltListOpen, "AltListOpen", openBracket);
                    TerminalTable.AddOrReplace(AltListClose, "AltListClose", closeBracket);
                }
                else
                {
                    engine.WrapTable.Add(ref openBracket, ref closeBracket);
                    TerminalTable.AddOrReplace(WrapOpen, "WrapOpen", openBracket);
                    TerminalTable.AddOrReplace(WrapClose, "WrapClose", closeBracket);
                }
            }

            protected void ScanVerbatimString()
            {
                do
                {
                    if (ch == DQUOTE)
                    {
                        NextCh();

                        if (ch != DQUOTE)
                        {
                            symbol.TerminalId = VerbatimStringLiteral;

                            break;
                        }
                    }

                    NextCh();
                } while (!endText);

                symbol.Start++;
                symbol.Class = SymbolClass.Text;
                symbol.Final = streamInPtr.Position;

                if (symbol.TerminalId != VerbatimStringLiteral)
                {
                    SyntaxError =
                        $"Unterminated verbatim string: {symbol}\r\n(remember to use \"\" instead of \\\" for an embedded \")";
                }
            }

            protected override void ScanIdOrTerminalOrCommentStart()
            {
                TerminalDescr tRec;
                bool special = ch.IsSpecialAtomChar();
                bool firstLow = Char.IsLower(ch);
                StreamPointer iPtr = streamInPtr;
                StreamPointer tPtr = iPtr;
                int aLen = ch.IsIdStartChar() || special ? 1 : 0; // length of longest Atom sofar
                int tLen = 0; // length of longest Terminal sofar
                int fCnt = 0; // count of calls to FindCharAndSubtree
                bool isDot = ch == '.'; // remains valid only if symbol length is 1
                TerminalTable.FindCharInSubtreeReset();

                while (fCnt++ >= 0 && TerminalTable.FindCharInSubtree(ch, out tRec))
                {
                    if (tRec != null)
                    {
                        symbol.TerminalId = tRec.IVal;
                        symbol.Payload = tRec.Payload;
                        symbol.Class = tRec.Class;
                        tLen = fCnt;
                        tPtr = streamInPtr; // next char to be processed

                        if (symbol.TerminalId == CommentStart || symbol.TerminalId == CommentSingle)
                        {
                            return;
                        }

                        if (TerminalTable.AtLeaf)
                        {
                            break; // terminal cannot be extended
                        }
                    }

                    NextCh();

                    if (aLen == fCnt &&
                        ((special && ch.IsSpecialAtomChar()) ||
                         (!special && ch.IsIdAtomContinueChar(extraUnquotedAtomChar))
                        )
                    )
                    {
                        aLen++;
                        iPtr = streamInPtr;
                    }
                } // fCnt++ by last (i.e. failing) Call

                // At this point: (in the Prolog case, read 'Identifier and Atom made up
                // from specials' for 'Identifier'):
                // - tLen has length of BaseTrie terminal (if any, 0 otherwise);
                // - aLen has length of Identifier (if any, 0 otherwise);
                // - fCnt is the number of characters inspected (for Id AND terminal)
                // - The character pointer is on the last character inspected (for both)
                // Iff aLen = fCnt then the entire sequence read sofar is an Identifier.
                // Now try extending the Identifier, only meaningful if iLen = fCnt.
                // Do not do this for an Atom made up from specials if a Terminal was recognized !!

                if (aLen == fCnt)
                {
                    while (true)
                    {
                        NextCh();

                        if (special ? ch.IsSpecialAtomChar() : ch.IsIdAtomContinueChar(extraUnquotedAtomChar))
                        {
                            aLen++;
                            iPtr = streamInPtr;
                        }
                        else
                        {
                            break;
                        }
                    }
                }

                if (aLen > tLen) // tLen = 0 iff Terminal == Undefined
                {
                    if (firstLow || special)
                    {
                        symbol.TerminalId = Atom;
                    }
                    else
                    {
                        symbol.TerminalId = Identifier;
                    }

                    symbol.Class = SymbolClass.Id;
                    InitCh(iPtr);
                }
                else if (symbol.TerminalId == Undefined)
                {
                    InitCh(iPtr);
                }
                else // we have a terminal != Identifier
                {
                    if (aLen == tLen)
                    {
                        symbol.Class = SymbolClass.Id;
                    }

                    InitCh(tPtr);
                }

                NextCh();

                // a bit hacky: find erroneous conditional define symbols
                // such as e.g. !ifxxx (space omitted between !if and xxx)

                if (symbol.Class == SymbolClass.Meta)
                {
                    int pos = streamInPtr.Position;

                    while (ch.IsIdAtomContinueChar(extraUnquotedAtomChar))
                    {
                        NextCh();
                    }

                    if (pos != streamInPtr.Position)
                    {
                        symbol.TerminalId = Undefined;
                    }
                }

                // Dot-patch: a '.' is a Dot only if it is followed by layout,
                // otherwise it is an atom (or an operator if defined as such)
                if (isDot && aLen == 1 && (ch == '\0' || ch == '%' || Char.IsWhiteSpace(ch)))
                {
                    symbol.TerminalId = Dot;
                }
            }

            public void SetDollarAsPossibleUnquotedAtomChar(bool set)
            {
                extraUnquotedAtomChar = set ? '$' : '_';
            }

            private void ScanQuotedAtom(out bool canUnquote)
            {
                canUnquote = true;
                bool specialsOnly = true;
                bool first = true;

                do
                {
                    NextCh();

                    if (!streamInPtr.EndLine)
                    {
                        if (ch == '\'') // possibly an escape
                        {
                            NextCh();

                            if (streamInPtr.EndLine || ch != '\'') // done
                            {
                                specialsOnly &= !first;
                                canUnquote &= !first; // '' is an atom !!!
                                symbol.TerminalId = Atom;
                            }
                            else // atom contains quote
                            {
                                canUnquote = false;
                            }
                        }
                        else
                        {
                            if (first)
                            {
                                canUnquote = Char.IsLower(ch);
                            }
                            else
                            {
                                canUnquote = canUnquote && (ch == '_' || Char.IsLetterOrDigit(ch));
                            }

                            specialsOnly = specialsOnly && ch.IsSpecialAtomChar();
                        }

                        first = false;
                    }
                } while (!streamInPtr.EndLine && symbol.TerminalId != Atom);

                if (streamInPtr.EndLine && symbol.TerminalId != StringLiteral)
                {
                    SyntaxError = "Unterminated atom: " + symbol;
                }

                canUnquote |= specialsOnly;

                // check whether the atom is an operator:
                int start = symbol.Start + (canUnquote ? 1 : 0);
                int stop = streamInPtr.Position - (canUnquote ? 1 : 0);
                TerminalDescr tRec;

                if (TerminalTable.Find(StreamInClip(start, stop), out tRec))
                {
                    if (tRec.Payload != null)
                    {
                        symbol.TerminalId = Operator;
                        symbol.Payload = tRec.Payload;
                    }
                }
            }

            protected override void NextSymbol(string _Proc)
            {
                if (symbol.AbsSeqNo != 0 && streamInPtr.FOnLine)
                {
                    streamInPtr.FOnLine = false;
                }

                symbol.PrevFinal = symbol.Final;

                if (symbol.TerminalId == EndOfInput)
                {
                    SyntaxError = "*** Trying to read beyond end of input";
                }

                prevTerminal = symbol.TerminalId;
                symbol.Class = SymbolClass.None;
                symbol.Payload = null;
                bool Break = false;
                bool canUnquote;

                do
                {
                    do
                    {
                        while (Char.IsWhiteSpace(ch))
                        {
                            NextCh();
                        }

                        symbol.Start = streamInPtr.Position;
                        symbol.LineNo = streamInPtr.LineNo;
                        symbol.LineStart = streamInPtr.LineStart;
                        symbol.TerminalId = Undefined;
                        symbol.Class = SymbolClass.None;
                        canUnquote = false; // used for quoted atoms only

                        if (endText)
                        {
                            symbol.TerminalId = EndOfInput;
                            Cdh.HandleSymbol(symbol); // check for missing #endif missing
                        }
                        else if (streamInPtr.EndLine)
                        {
                            symbol.TerminalId = EndOfLine;
                        }
                        else if (Char.IsDigit(ch))
                        {
                            ScanNumber();
                        }
                        else if (ch == SQUOTE)
                        {
                            ScanQuotedAtom(out canUnquote);
                        }
                        else if (ch == DQUOTE)
                        {
                            ScanString();
                        }
                        else
                        {
                            ScanIdOrTerminalOrCommentStart();
                        }

                        symbol.Final = streamInPtr.Position;
                        symbol.IsFollowedByLayoutChar = ch == '%' || Char.IsWhiteSpace(ch); // '/*' not covered

                        if (symbol.Class == SymbolClass.Comment)
                        {
                            break;
                        }

                        if (Cdh.IsExpectingId || symbol.Class == SymbolClass.Meta)
                        {
                            Cdh.HandleSymbol(symbol); // if expecting: symbol must be an identifier
                        }
                    } while (Cdh.CodeIsInactive || symbol.Class == SymbolClass.Meta);

                    if (canUnquote)
                    {
                        symbol.Start++;
                        symbol.Final--;
                    }

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
                            case Atom:
                                Break = true;
                                break;

                            case EndOfInput:
                                Break = true;
                                break;

                            case VerbatimStringStart:
                                ScanVerbatimString();
                                Break = true;
                                break;

                            case CommentStart:
                                if (stringMode)
                                {
                                    Break = true;
                                }

                                if (!DoComment("*/", true, streamInPtr.FOnLine))
                                {
                                    ErrorMessage = "Unterminated comment starting at line " + symbol.LineNo;
                                }

                                break;

                            case CommentSingle:
                                if (stringMode)
                                {
                                    Break = true;
                                }
                                else
                                {
                                    Break = false;
                                }

                                if (symbol.InputLine.TrimStart().StartsWith("%"))
                                {
                                    _lastCommentBlock.AppendLine(symbol.InputLine.Replace("%", string.Empty).Trim());
                                }

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
                                {
                                    streamInPtr.FOnLine = false;
                                }

                                Break = true;
                                break;
                        }
                    }
                } while (!Break);

                symbol.AbsSeqNo++;
                symbol.RelSeqNo++;
            }

            public BaseTerm ParseTerm()
            {
                if (symbol.TerminalId == EndOfInput)
                {
                    return null;
                }

                BaseTerm result;

                try
                {
                    OptionalPrologTerm(new TerminalSet(terminalCount), out result);
                    LineCount = LineNo;

                    return result;
                }
                catch (ConsultException e)
                {
                    throw e;
                }
                catch (Exception e) // other errors
                {
                    errorMessage =
                        $"*** Line {LineNo}: {e.Message}{(ShowErrTrace ? Environment.NewLine + e.StackTrace : null)}";

                    throw new ConsultException(errorMessage, symbol: symbol);
                }
            }
        }
    }

    internal static class PrologParserExtensions
    {
        public static bool IsSpecialAtomChar(this char c)
        {
            return @"+-*/\^<=>`~:.?@#$&".IndexOf(c) != -1;
        }

        public static bool IsIdAtomContinueChar(this char c, char extraUnquotedAtomChar)
        {
            return Char.IsLetterOrDigit(c) || c == '_' || c == extraUnquotedAtomChar;
        }
    }
}