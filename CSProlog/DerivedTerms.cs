/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2015 John Pool -- j.pool@ision.nl

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU Lesser General Public License as published by the Free Software Foundation; either
  version 3.0 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License (http://www.gnu.org/licenses/lgpl-3.0.html), or
  enter 'license' at the command prompt.

-------------------------------------------------------------------------------------------*/

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using static Prolog.PrologEngine.BaseParser;

namespace Prolog
{
    public partial class PrologEngine
    {
        private static readonly TryCatchTerm TC_CLOSE = new TryCatchTerm(null, ")");

        public BaseTerm NewIsoOrCsStringTerm(Symbol symbol, string s)
        {
            if (CsharpStrings)
            {
                return new StringTerm(symbol, s);
            }

            return new ListTerm(symbol, s);
        }

        public class TryCatchTerm : AtomTerm
        {
            public TryCatchTerm(Symbol symbol, string a) : base(symbol, a)
            {
            }
        }

        public class TryOpenTerm : TryCatchTerm
        {
            public TryOpenTerm(Symbol symbol) : base(symbol, "TRY")
            {
            }

            public int Id { get; set; }
        }

        public class CatchOpenTerm : TryCatchTerm
        {
            public CatchOpenTerm(Symbol symbol, string exceptionClass, BaseTerm msgVar, int seqNo)
                : base(symbol, "CATCH")
            {
                ExceptionClass = exceptionClass;
                SeqNo = seqNo;
                MsgVar = (Variable)msgVar;
            }

            public CatchOpenTerm(Symbol symbol, int id, string exceptionClass, BaseTerm msgVar, int seqNo,
                int saveStackSize)
                : base(symbol, "CATCH")
            {
                Id = id;
                ExceptionClass = exceptionClass;
                SeqNo = seqNo;
                SaveStackSize = saveStackSize;
                MsgVar = (Variable)msgVar;
            }

            public string ExceptionClass { get; }
            public int Id { get; set; }

            public int SeqNo { get; } // CATCH-clauses for one TRY are number from 0 onwards
            public int SaveStackSize { get; set; } // not used
            public Variable MsgVar { get; }
        }

        public class Variable : BaseTerm
        {
            public Variable(Symbol symbol, VarStack varStack)
                : base(symbol)
            {
                varNo = varStack.varNoMax++;
                VerNo = 0;
                UnifyCount = 0;
                CompoundTermType = TermType.UnboundVar;
            }

            public BaseTerm NewVar { get; set; }
            protected BaseTerm ULink { get; set; }
            protected int UnifyCount { get; set; }
            public int VerNo { get; set; }

            protected int varNo
            {
                get => TermId;
                set => TermId = value;
            }

            public override string Name => "_" + varNo;
            public override int Arity => -1;

            public override bool IsUnified => ULink != null;

            public bool IsUnifiedWith(Variable v)
            {
                return ChainEnd() == v.ChainEnd();
            }

            public override BaseTerm ChainEnd()
            {
                return IsUnified ? ULink.ChainEnd() : this;
            }

            // - this is a non-unified var, or is is a var with a refUnifyCount > arg  -> return this
            // - return ChainEnd (refUnifyCount)
            public override BaseTerm ChainEnd(int refUnifyCount)
            {
                return ULink == null || UnifyCount > refUnifyCount
                    ? this
                    : ULink.ChainEnd(refUnifyCount); // resolves to uLink for a nonvar
            }

            protected override int CompareValue(BaseTerm t)
            {
                return varNo.CompareTo(((Variable)t).varNo);
            }

            public override bool Unify(BaseTerm t, VarStack varStack)
            {
                if (IsUnified)
                {
                    return ChainEnd().Unify(t, varStack);
                }

                if (t.IsUnified)
                {
                    return Unify(t.ChainEnd(), varStack);
                }

                varStack.NextUnifyCount();
                Bind(t, varStack);
                varStack.Push(this);

                return true;
            }

            public void Bind(BaseTerm t, VarStack varStack)
            {
                if (this == t)
                {
                    return; // cannot bind to self
                }

                ULink = t;
                UnifyCount = varStack.CurrUnifyCount;
            }

            public void Unbind()
            {
                ULink = null;
                UnifyCount = 0;
            }

            public override string ToWriteString(int level)
            {
                if (ULink == null)
                {
                    return Name;
                }

                return ULink.ToWriteString(level);
            }
        }

        // carries a variable's symbolic name as found in the source
        public class NamedVariable : Variable
        {
            public NamedVariable(Symbol symbol, string name, VarStack varStack)
                : base(symbol, varStack)
            {
                VarName = name;
                CompoundTermType = TermType.NamedVar;
            }

            protected string VarName { get; }

            public override string Name => VarName;

            protected override int CompareValue(BaseTerm t)
            {
                return VarName.CompareTo(((NamedVariable)t).VarName);
            }
        }

        // not really necessary, but it can be convenient to recognize one
        public class AnonymousVariable : Variable
        {
            public AnonymousVariable(Symbol symbol, VarStack varStack)
                : base(symbol, varStack)
            {
            }
        }

        public class AtomTerm : BaseTerm
        {
            // engine, pi, i, today, ...

            public AtomTerm(Symbol symbol, object functor)
                : base(symbol)
            {
                CompoundFunctor = functor;
                CompoundTermType = TermType.Atom;
            }

            public AtomTerm(Symbol symbol, string value)
                : base(symbol)
            {
                CompoundFunctor = value.Unescaped();
                CompoundTermType = TermType.Atom;
            }

            public AtomTerm(Symbol symbol, string value, string commentHeader, string commentBody, string testGroup)
                : base(symbol)
            {
                CompoundFunctor = value.Unescaped();
                CommentHeader = commentHeader;
                CommentBody = commentBody;
                TestGroup = testGroup;
                CompoundTermType = TermType.Atom;
            }

            public override bool IsCallable => true;
            public override bool IsEvaluatable => true;

            protected override int CompareValue(BaseTerm t)
            {
                return FunctorToString.CompareTo(t.FunctorToString);
            }

            public override string ToWriteString(int level)
            {
                return FunctorToString == PrologParser.DOT ? "'.'" : FunctorToString;
            }
        }

        public class ClauseTerm : BaseTerm
        {
            public ClauseTerm(Symbol symbol, ClauseNode c,
                VarStack varStack) // Create a BaseTerm from a NextClause (= Head + Body)
                : base(symbol)
            {
                ClauseNode = c;

                if (c.NextNode == null) // fact
                {
                    CopyValuesFrom(c.Head);
                }
                else
                {
                    CompoundFunctor = PrologParser.IMPLIES;
                    CompoundArgs = new BaseTerm[2];
                    CompoundArgs[0] = c.Head;
                    CompoundTermType = TermType.Atom;
                    AssocType = AssocType.xfx;
                    CompoundArgs[1] = c.NextNode.TermSeq(varStack);
                    CompoundPrecedence = 1200;
                }
            }

            public override bool IsCallable => true;

            public ClauseNode ClauseNode { get; }
        }

        public class WrapperTerm : CompoundTerm
        {
            private readonly string wrapClose;
            private readonly string wrapOpen;

            public WrapperTerm(Symbol symbol, string wrapOpen, string wrapClose, BaseTerm[] a)
                : base(symbol, (wrapOpen + ".." + wrapClose).ToAtom(), a)
            {
                this.wrapOpen = wrapOpen;
                this.wrapClose = wrapClose;
                CompoundTermType = TermType.Compound;
            }

            public WrapperTerm(WrapperTerm that, BaseTerm[] a) // for Copy only
                : base(that.Symbol, (that.wrapOpen + ".." + that.wrapClose).ToAtom(), a)
            {
                wrapOpen = that.wrapOpen;
                wrapClose = that.wrapClose;
                CompoundTermType = TermType.Compound;
            }

            private string wrapFunctor => (wrapOpen + ".." + wrapClose).ToAtom();

            public override string ToWriteString(int level)
            {
                if (MaxWriteDepthExceeded(level))
                {
                    return "...";
                }

                StringBuilder sb = new StringBuilder(wrapOpen + SpaceAtLevel(level));
                bool first = true;

                for (int i = 0; i < arity; i++)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        sb.Append(CommaAtLevel(level));
                    }

                    sb.AppendPacked(Arg(i).ToWriteString(level + 1), Arg(i).FunctorIsBinaryComma);
                }

                sb.Append(SpaceAtLevel(level) + wrapClose);

                return sb.ToString();
            }

            public override string ToDisplayString(int level)
            {
                if (MaxWriteDepthExceeded(level))
                {
                    return "...";
                }

                StringBuilder sb = new StringBuilder(wrapFunctor);

                bool first = true;

                sb.Append("(");

                foreach (BaseTerm a in Args)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        sb.Append(CommaAtLevel(level));
                    }

                    sb.Append(a.ToDisplayString(level + 1));
                }

                sb.Append(")");

                return sb.ToString();
            }

            public override void TreePrint(int level, PrologEngine e)
            {
                string margin = Spaces(2 * level);

                if (arity == 0)
                {
                    e.WriteLine("{0}{1}", margin, wrapFunctor);

                    return;
                }

                e.WriteLine("{0}{1}", margin, wrapOpen);

                foreach (BaseTerm a in CompoundArgs)
                {
                    a.TreePrint(level + 1, e);
                }

                e.WriteLine("{0}{1}", margin, wrapClose);
            }
        }

        public class CompoundTerm : BaseTerm
        {
            public CompoundTerm(Symbol symbol, string functor, BaseTerm[] args)
                : base(symbol)
            {
                CompoundFunctor = functor;
                CompoundArgs = args;
                CompoundTermType = TermType.Compound;
            }

            public CompoundTerm(Symbol symbol, string functor, string commentHeader, string commentBody,
                string testGroup, BaseTerm[] args)
                : base(symbol)
            {
                CompoundFunctor = functor;
                CommentHeader = commentHeader;
                CommentBody = commentBody;
                TestGroup = testGroup;
                CompoundArgs = args;
                CompoundTermType = TermType.Compound;
            }

            public CompoundTerm(Symbol symbol, string functor, BaseTerm a)
                : base(symbol)
            {
                CompoundFunctor = functor;
                CompoundArgs = new BaseTerm[1];
                CompoundArgs[0] = a;
                CompoundTermType = TermType.Compound;
            }

            public CompoundTerm(Symbol symbol, string functor, BaseTerm a0, BaseTerm a1)
                : base(symbol)
            {
                CompoundFunctor = functor;
                CompoundArgs = new BaseTerm[2];
                CompoundArgs[0] = a0;
                CompoundArgs[1] = a1;
                CompoundTermType = TermType.Compound;
            }

            public CompoundTerm(Symbol symbol, object functor, BaseTerm[] args)
                : base(symbol)
            {
                CompoundFunctor = functor;
                CompoundArgs = args;
                CompoundTermType = TermType.Compound;
            }

            public CompoundTerm(Symbol symbol, string functor) // degenerated case (for EMPTYLIST and operator)
                : base(symbol)
            {
                CompoundFunctor = functor;
                CompoundTermType = TermType.Atom;
            }

            public CompoundTerm(Symbol symbol)
                : base(symbol)
            {
            }

            public override bool IsCallable => true;
            public override bool IsEvaluatable => true;

            protected override int CompareValue(BaseTerm t)
            {
                int result = Arity.CompareTo(t.Arity); // same FunctorString: lowest arity first

                if (result != 0)
                {
                    return result; // different arities
                }

                if (Arity == 0)
                {
                    return FunctorToString.CompareTo(t.FunctorToString);
                }

                for (int i = 0; i < Arity; i++)
                {
                    if ((result = Arg(i).CompareTo(t.Arg(i))) != 0)
                    {
                        return result;
                    }
                }

                return 0;
            }

            public override string ToWriteString(int level)
            {
                if (MaxWriteDepthExceeded(level))
                {
                    return "...";
                }

                StringBuilder sb = new StringBuilder();

                if (FunctorToString == PrologParser.COMMA && arity == 2)
                {
                    sb = new StringBuilder("(" + Arg(0).ToWriteString(level) + CommaAtLevel(level)
                                           + Arg(1).ToWriteString(level) + ")");
                }
                else if (this == NULLCURL)
                {
                    return PrologParser.CURL;
                }
                else if (FunctorToString == PrologParser.CURL)
                {
                    sb.Append("{");
                    bool first = true;

                    foreach (BaseTerm arg in Args)
                    {
                        if (first)
                        {
                            first = false;
                        }
                        else
                        {
                            sb.Append(CommaAtLevel(level));
                        }

                        sb.Append(arg.ToWriteString(level + 1).Packed(arg.FunctorIsBinaryComma));
                    }

                    sb.Append("}");
                }
                else
                {
                    sb.AppendPossiblySpaced(FunctorIsBinaryComma ? "','" : FunctorToString);
                    sb.Append("(");
                    bool first = true;

                    for (int i = 0; i < arity; i++)
                    {
                        if (first)
                        {
                            first = false;
                        }
                        else
                        {
                            sb.Append(CommaAtLevel(level));
                        }

                        sb.AppendPacked(Arg(i).ToWriteString(level + 1), Arg(i).FunctorIsBinaryComma);
                    }

                    sb.Append(")");
                }

                return sb.ToString();
            }

            public override string ToDisplayString(int level)
            {
                if (MaxWriteDepthExceeded(level))
                {
                    return "...";
                }

                string functor = FunctorToString == PrologParser.CURL ? "'{{}}'" : FunctorToString;

                StringBuilder sb = new StringBuilder(FunctorIsBinaryComma ? "','" : functor);
                bool first = true;

                sb.Append("(");

                foreach (BaseTerm a in Args)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        sb.Append(CommaAtLevel(level));
                    }

                    sb.Append(a.ToDisplayString(level + 1));
                }

                sb.Append(")");

                return sb.ToString();
            }
        }

        public class OperatorTerm : CompoundTerm
        {
            private readonly AssocType assoc;

            public OperatorTerm(Symbol symbol, OperatorDescr opDescr, BaseTerm a0, BaseTerm a1)
                : base(symbol, opDescr.Name, a0, a1)
            {
                OpDescr = opDescr;
                assoc = opDescr.Assoc;
                CompoundPrecedence = (short)opDescr.Prec;
            }

            public OperatorTerm(Symbol symbol, OperatorDescr opDescr, BaseTerm a)
                : base(symbol, opDescr.Name, a)
            {
                OpDescr = opDescr;
                assoc = opDescr.Assoc;
                CompoundPrecedence = (short)opDescr.Prec;
            }

            public OperatorTerm(Symbol symbol, OperatorDescr opDescr, BaseTerm[] a)
                : base(symbol, opDescr.Name, a)
            {
                OpDescr = opDescr;
                assoc = opDescr.Assoc;
                CompoundPrecedence = (short)opDescr.Prec;
            }

            public OperatorTerm(Symbol symbol, string name) // stand-alone operator used as term
                : base(symbol, name)
            {
                OpDescr = null;
                assoc = AssocType.None;
                CompoundPrecedence = 1001;
            }

            // ConfigSettings.VerbatimStringsAllowed
            public OperatorDescr OpDescr { get; }

            public override bool HasUnaryOperator()
            {
                return OpDescr.IsPostfix || OpDescr.IsPrefix;
            }

            public override bool HasBinaryOperator()
            {
                return OpDescr.IsInfix;
            }

            public override bool HasUnaryOperator(params string[] names)
            {
                if (!(OpDescr.IsPrefix || OpDescr.IsPostfix))
                {
                    return false;
                }

                foreach (string name in names)
                {
                    if (OpDescr.Name == name)
                    {
                        return true;
                    }
                }

                return false;
            }

            public override bool HasBinaryOperator(params string[] names)
            {
                if (!OpDescr.IsInfix)
                {
                    return false;
                }

                foreach (string name in names)
                {
                    if (OpDescr.Name == name)
                    {
                        return true;
                    }
                }

                return false;
            }

            public override string ToWriteString(int level)
            {
                if (MaxWriteDepthExceeded(level))
                {
                    return "...";
                }

                StringBuilder sb = new StringBuilder();
                bool mustPack;

                if (arity == 2)
                {
                    mustPack = CompoundPrecedence < Arg(0).Precedence ||
                               (CompoundPrecedence == Arg(0).Precedence &&
                                (assoc == AssocType.xfx || assoc == AssocType.xfy));
                    sb.AppendPacked(Arg(0).ToWriteString(level + 1), mustPack);

                    sb.AppendPossiblySpaced(FunctorToString);

                    mustPack =
                        CompoundPrecedence < Arg(1).Precedence ||
                        (CompoundPrecedence == Arg(1).Precedence && (assoc == AssocType.xfx || assoc == AssocType.yfx));
                    sb.AppendPacked(Arg(1).ToWriteString(level + 1), mustPack);

                    return sb.ToString();
                }

                if (arity == 1)
                {
                    switch (assoc)
                    {
                        case AssocType.fx:
                            sb.Append(FunctorToString);
                            sb.AppendPacked(Arg(0).ToWriteString(level + 1), CompoundPrecedence <= Arg(0).Precedence);
                            break;

                        case AssocType.fy:
                            sb.Append(FunctorToString);
                            sb.AppendPacked(Arg(0).ToWriteString(level + 1), CompoundPrecedence < Arg(0).Precedence);
                            break;

                        case AssocType.xf:
                            sb.AppendPacked(Arg(0).ToWriteString(level + 1), CompoundPrecedence <= Arg(0).Precedence);
                            sb.AppendPossiblySpaced(FunctorToString);
                            break;

                        case AssocType.yf:
                            sb.AppendPacked(Arg(0).ToWriteString(level + 1), CompoundPrecedence < Arg(0).Precedence);
                            sb.AppendPossiblySpaced(FunctorToString);
                            break;
                    }

                    return sb.ToString();
                }

                return FunctorToString;
            }

            public override string ToDisplayString(int level)
            {
                if (MaxWriteDepthExceeded(level))
                {
                    return "...";
                }

                StringBuilder sb = new StringBuilder(FunctorIsBinaryComma ? "','" : FunctorToString);

                if (Arity > 0)
                {
                    bool first = true;

                    sb.Append("(");

                    foreach (BaseTerm a in Args)
                    {
                        if (first)
                        {
                            first = false;
                        }
                        else
                        {
                            sb.Append(CommaAtLevel(level));
                        }

                        sb.Append(a.ToDisplayString(level + 1));
                    }

                    sb.Append(")");
                }

                return sb.ToString();
            }
        }

        public class ValueTerm : BaseTerm // a BaseTerm that can be expression-evaluated by is/2.
        {
            public ValueTerm(Symbol symbol)
                : base(symbol)
            {
            }

            public override bool IsEvaluatable => true;
        }

        public class StringTerm : ValueTerm
        {
            public StringTerm(Symbol symbol, string value)
                : base(symbol)
            {
                CompoundFunctor = value;
                Value = value;
                CompoundTermType = TermType.String;
            }

            public StringTerm(Symbol symbol, char value)
                : base(symbol)
            {
                CompoundFunctor = value.ToString();
                CompoundTermType = TermType.String;
            }

            public string Value { get; set; }

            public override string ToWriteString(int level)
            {
                return '"' + FunctorToString.Replace(@"\", @"\\").Replace(@"""", @"\""") + '"';
            }
        }

        public class DecimalTerm : ValueTerm
        {
            private const double EPS = 1.0e-6; // arbitrary, cosmetic
            public static readonly DecimalTerm ZERO;
            public static readonly DecimalTerm ONE;
            public static readonly DecimalTerm MINUS_ONE;

            static DecimalTerm()
            {
                ZERO = new DecimalTerm(null, 0);
                ONE = new DecimalTerm(null, 1);
                MINUS_ONE = new DecimalTerm(null, -1);
            }

            public DecimalTerm(Symbol symbol, decimal value)
                : base(symbol)
            {
                Value = value;
                CompoundFunctor = value;
                CompoundTermType = TermType.Number;
            }

            public DecimalTerm(Symbol symbol, int value)
                : base(symbol)
            {
                CompoundFunctor = Value = value;
                CompoundTermType = TermType.Number;
            }

            public DecimalTerm(Symbol symbol, double value)
                : base(symbol)
            {
                CompoundFunctor = Value = (decimal)value;
                CompoundTermType = TermType.Number;
            }

            public DecimalTerm(Symbol symbol, long value)
                : base(symbol)
            {
                CompoundFunctor = Value = value;
                CompoundTermType = TermType.Number;
            }

            public decimal Value { get; }
            public override string FunctorToString => Value.ToString(CIC);

            public override bool Unify(BaseTerm t, VarStack varStack)
            {
                if (t is Variable)
                {
                    return t.Unify(this, varStack);
                }

                varStack.NextUnifyCount();

                if (t is DecimalTerm)
                {
                    return Value == ((DecimalTerm)t).Value;
                }

                return false;
            }

            protected override int CompareValue(BaseTerm t)
            {
                return To<decimal>().CompareTo(t.To<decimal>());
            }

            public override string ToWriteString(int level)
            {
                if (Value == Math.Truncate(Value))
                {
                    return Value.ToString();
                }

                return Value.ToString(Math.Abs(Value) < (decimal)EPS ? "e" : "0.######", CIC);
            }
        }

        public class DateTimeTerm : ValueTerm
        {
            public DateTimeTerm(Symbol symbol, DateTime value)
                : base(symbol)
            {
                CompoundFunctor = value;
                CompoundTermType = TermType.DateTime;
            }

            protected override int CompareValue(BaseTerm t)
            {
                return To<DateTime>().CompareTo(t.To<DateTime>());
            }

            public override string ToWriteString(int level)
            {
                return "'" + ((DateTime)CompoundFunctor).ToString("yyyy-MM-dd HH-mm-ss") + "'";
            }
        }

        public class TimeSpanTerm : ValueTerm
        {
            public TimeSpanTerm(Symbol symbol, TimeSpan value)
                : base(symbol)
            {
                CompoundFunctor = value;
                CompoundTermType = TermType.TimeSpan;
            }

            protected override int CompareValue(BaseTerm t)
            {
                return To<TimeSpan>().CompareTo(t.To<TimeSpan>());
            }

            public override string ToWriteString(int level)
            {
                return "'" + (TimeSpan)CompoundFunctor + "'";
            }
        }

        public class BoolTerm : ValueTerm
        {
            public BoolTerm(Symbol symbol, bool value)
                : base(symbol)
            {
                CompoundFunctor = value;
                CompoundTermType = TermType.Bool;
            }

            protected override int CompareValue(BaseTerm t)
            {
                return To<bool>().CompareTo(t.To<bool>());
            }

            public override string ToWriteString(int level)
            {
                return (bool)CompoundFunctor ? "true" : "false";
            }
        }

        public class FileTerm : BaseTerm
        {
            // in order to be able to close all open streams after command termination:
            public static readonly AtomTerm END_OF_FILE;

            static FileTerm()
            {
                END_OF_FILE = new AtomTerm(null, "end_of_file");
            }

            public FileTerm(Symbol symbol)
                : base(symbol)
            {
            }

            protected PrologEngine Engine { get; set; }
            protected string FileName { get; set; }

            public virtual bool IsOpen => false;

            public virtual void Close()
            {
            }
        }

        public class FileReaderTerm : FileTerm
        {
            private FileStream fs;

            private PrologParser p;
            private TextReader tr;

            public FileReaderTerm(Symbol symbol, PrologEngine engine, string fileName)
                : base(symbol)
            {
                Engine = engine;
                CompoundFunctor = FileName = fileName;
                CompoundTermType = TermType.FileReader;
            }

            public override bool IsOpen => tr != null;

            public void Open()
            {
                try
                {
                    if (tr == null)
                    {
                        fs = new FileStream(FileName, FileMode.Open, FileAccess.Read, FileShare.Read);
                        tr = new StreamReader(fs);
                    }
                }
                catch (Exception e)
                {
                    IO.ThrowRuntimeException($"Error while opening file '{FileName}' for input.\r\nMessage was:\r\n{e.Message}", null, null);
                }


                p = new PrologParser(Engine);
                p.SetInputStream(tr);
                p.InitParse();
            }

            public int ReadChar() // returns -1 at end of file
            {
                return p.ReadChar();
            }

            public string ReadLine() // returns null at end of file
            {
                return p.ReadLine();
            }

            public BaseTerm ReadTerm()
            {
                BaseTerm result = p.ParseTerm();

                return result == null ? END_OF_FILE : result;
            }

            public override void Close()
            {
                p?.ExitParse();

                tr?.Dispose();
            }
        }

        public class FileWriterTerm : FileTerm
        {
            private FileStream fs;
            private TextWriter tw;

            public FileWriterTerm(Symbol symbol, PrologEngine engine, string fileName)
                : base(symbol)
            {
                Engine = engine;
                CompoundFunctor = FileName = fileName;
                CompoundTermType = TermType.FileWriter;
            }

            public override bool IsOpen => tw != null;

            public void Open()
            {
                try
                {
                    if (tw == null)
                    {
                        fs = new FileStream(FileName, FileMode.Create, FileAccess.Write);
                        tw = new StreamWriter(fs);
                    }
                }
                catch (Exception e)
                {
                    IO.ThrowRuntimeException($"Error while opening file '{FileName}' for input.\r\nMessage was:\r\n{e.Message}", null, null);
                }
            }

            public void Write(string s)
            {
                tw.Write(s);
            }

            public override void Close()
            {
                if (tw != null)
                {
                    tw.Dispose();
                }
            }
        }

        public class CollectionTerm : AtomTerm // for creating collections of terms (e.g. setof)
        {
            private readonly BaseTermSet set;

            public CollectionTerm(Symbol symbol, DupMode dupMode)
                : base(symbol, "<term collection>")
            {
                set = new BaseTermSet(dupMode);
            }

            public int Count => set.Count;
            public override bool IsCallable => false;

            public void Add(BaseTerm t)
            {
                set.Add(t);
            }

            public void Insert(BaseTerm t)
            {
                set.Insert(t);
            }

            public ListTerm ToList()
            {
                return set.ToList();
            }
        }

        public class Cut : BaseTerm
        {
            public Cut(Symbol symbol, int stackSize)
                : base(symbol)
            {
                CompoundFunctor = PrologParser.CUT;
                TermId = stackSize;
            }

            public override bool IsCallable => false;
            public override bool IsEvaluatable => false;

            public override string ToWriteString(int level)
            {
                return PrologParser.CUT;
            }
        }

        public class ListTerm : CompoundTerm
        {
            public ListTerm(Symbol symbol)
                : base(symbol, "[]")
            {
            }

            public ListTerm(Symbol symbol, BaseTerm t)
                : base(symbol, PrologParser.DOT, t.ChainEnd(), EMPTYLIST)
            {
            }

            public ListTerm(Symbol symbol, BaseTerm t0, BaseTerm t1)
                : base(symbol, PrologParser.DOT, t0.ChainEnd(), t1.ChainEnd())
            {
            }

            // for ListPattern; *not* intended for creating a list from an array, use ListFromArray
            public ListTerm(Symbol symbol, BaseTerm[] a)
                : base(symbol, PrologParser.DOT, a)
            {
            }

            public ListTerm(Symbol symbol, string charCodeString)
                : base(symbol, PrologParser.DOT)
            {
                if (charCodeString.Length == 0)
                {
                    CompoundFunctor = "[]";

                    return;
                }

                CharCodeString = charCodeString;
                CompoundArgs = new BaseTerm[2];

                CompoundArgs[0] = new DecimalTerm(Symbol, (decimal)charCodeString[0]);
                CompoundArgs[1] = new ListTerm(Symbol, charCodeString.Substring(1));
            }

            protected ListTerm EmptyList { get; } = EMPTYLIST;
            protected bool IsAltList { get; set; } = false;

            public override bool IsEvaluatable => true;
            // evaluate all members

            public string LeftBracket { get; set; } = "[";

            public string RightBracket { get; set; } = "]";

            public string CharCodeString { get; }
            public bool IsEvaluated { get; set; } = false;

            //public override bool IsCallable { get { return false; } }
            private int properLength // only defined for proper lists
            {
                get
                {
                    BaseTerm t = ChainEnd();
                    int len = 0;

                    while (t.Arity == 2 && t is ListTerm)
                    {
                        t = t.Arg(1);
                        len++;
                    }

                    return t.IsEmptyList ? len : -1;
                }
            }

            public int ProperLength => properLength;

            public override bool IsListNode
            {
                get
                {
                    BaseTerm t = ChainEnd();
                    return t is ListTerm && t.Arity == 2;
                }
            }

            public override bool IsProperOrPartialList
            {
                get
                {
                    BaseTerm t = ChainEnd();

                    while (t.Arity == 2 && t is ListTerm)
                    {
                        t = t.Arg(1);
                    }

                    return t.IsEmptyList || t is Variable;
                }
            }

            public override bool IsProperList // e.g. [foo] (= [foo|[]])
            {
                get
                {
                    BaseTerm t = ChainEnd();

                    while (t.Arity == 2 && t is ListTerm)
                    {
                        t = t.Arg(1);
                    }

                    return t.IsEmptyList;
                }
            }

            public override bool IsPartialList // e.g. [foo|Atom]
            {
                get
                {
                    BaseTerm t = ChainEnd();

                    while (t.Arity == 2 && t is ListTerm)
                    {
                        t = t.Arg(1);
                    }

                    return t is Variable;
                }
            }

            public override bool IsPseudoList // e.g.: [foo|baz]
            {
                get
                {
                    BaseTerm t = ChainEnd();

                    while (t.Arity == 2 && t is ListTerm)
                    {
                        t = t.Arg(1);
                    }

                    return !(t.IsEmptyList || t is Variable);
                }
            }

            public static ListTerm ListFromArray(BaseTerm[] ta, BaseTerm afterBar)
            {
                ListTerm result = null;

                for (int i = ta.Length - 1; i >= 0; i--)
                {
                    result = new ListTerm(ta[i].Symbol, ta[i], result == null ? afterBar : result);
                }

                return result;
            }

            public List<BaseTerm> ToList()
            {
                List<BaseTerm> result = new List<BaseTerm>();

                foreach (BaseTerm t in this)
                {
                    result.Add(t);
                }

                return result;
            }

            public static ListTerm ListFromArray(BaseTerm[] ta)
            {
                return ListFromArray(ta, EMPTYLIST);
            }

            public IEnumerator GetEnumerator()
            {
                BaseTerm t = ChainEnd();

                while (t.Arity == 2)
                {
                    yield return t.Arg(0);

                    t = t.Arg(1);
                }
            }

            public virtual ListTerm Reverse()
            {
                ListTerm result = EmptyList;

                foreach (BaseTerm t in this)
                {
                    result = new ListTerm(t.Symbol, t, result);
                }

                return result;
            }

            public BaseTerm Append(BaseTerm list) // append t to 'this'
            {
                if (IsEmptyList)
                {
                    return list; // not necessarily a ListTerm
                }

                if (list.IsEmptyList)
                {
                    return this;
                }

                BaseTerm t0, t1;
                t1 = t0 = this;

                // find rightmost '.'-term and replace its right arg by t
                while (t1.Arity == 2)
                {
                    t0 = t1;
                    t1 = t1.Arg(1);
                }

                ((ListTerm)t0.ChainEnd()).SetArg(1, list);

                return this;
            }

            public BaseTerm AppendElement(BaseTerm last) // append last to 'this'
            {
                return Append(new ListTerm(last.Symbol, last));
            }

            public virtual ListTerm FlattenList()
            {
                List<BaseTerm> a = FlattenListEx(CompoundFunctor); // only sublists with the same functor

                ListTerm result = EmptyList;

                for (int i = a.Count - 1; i >= 0; i--)
                {
                    result = new ListTerm(a[i].Symbol, a[i], result); // [a0, a0, ...]
                }

                return result;
            }

            protected List<BaseTerm> FlattenListEx(object functor)
            {
                BaseTerm t = this;
                BaseTerm t0;
                List<BaseTerm> result = new List<BaseTerm>();

                while (t.IsListNode)
                {
                    if ((t0 = t.Arg(0)).IsProperOrPartialList && ((ListTerm)t0).CompoundFunctor.Equals(functor))
                    {
                        result.AddRange(((ListTerm)t0).FlattenListEx(functor));
                    }
                    else
                    {
                        result.Add(t0);
                    }

                    t = t.Arg(1);
                }

                if (t.IsVar)
                {
                    result.Add(t); // open tail, i.e. [1|M]
                }

                return result;
            }

            public override string ToWriteString(int level)
            {
                // insert an extra space in case of non-standard list brackets
                string altListSpace = IsAltList ? " " : null;

                if (IsEmptyList)
                {
                    return LeftBracket + altListSpace + RightBracket;
                }

                if (MaxWriteDepthExceeded(level))
                {
                    return "[...]";
                }

                StringBuilder sb = new StringBuilder(LeftBracket + altListSpace);
                BaseTerm t = ChainEnd();

                bool first = true;

                while (t.IsListNode)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        sb.Append(CommaAtLevel(level));
                    }

                    sb.AppendPacked(t.Arg(0).ToWriteString(level + 1), t.Arg(0).FunctorIsBinaryComma);
                    t = t.Arg(1);
                }

                if (!t.IsEmptyList)
                {
                    sb.AppendFormat("|{0}", t.ToWriteString(level + 1).Packed(t.FunctorIsBinaryComma));
                }

                sb.Append(altListSpace + RightBracket);

                if (CharCodeString != null) // show string value in comment
                {
                    sb.AppendFormat("  /*{0}*/", CharCodeString.Replace("*/", "\\x2A/"));
                }

                return sb.ToString();
            }

            public override string ToDisplayString(int level)
            {
                if (IsEmptyList)
                {
                    return "leftBracket + rightBracket";
                }

                StringBuilder sb = new StringBuilder(".(");
                sb.Append(Arg(0).ToDisplayString(level));
                sb.Append(CommaAtLevel(level));
                sb.Append(Arg(1).ToDisplayString(level));
                sb.Append(")");

                return sb.ToString();
            }

            public override void TreePrint(int level, PrologEngine e)
            {
                string margin = Spaces(2 * level);

                if (IsEmptyList)
                {
                    e.WriteLine("{0}{1}", margin, EMPTYLIST);

                    return;
                }

                e.WriteLine("{0}{1}", margin, LeftBracket);

                BaseTerm t = ChainEnd();

                while (t.IsListNode)
                {
                    t.Arg(0).TreePrint(level + 1, e);
                    t = t.Arg(1);
                }

                e.WriteLine("{0}{1}", margin, RightBracket);
            }

            public string[] ToStringArray()
            {
                if (!IsProperList)
                {
                    return null;
                }

                string[] result = new string[properLength];

                BaseTerm t = ChainEnd();
                int i = 0;

                while (t.Arity == 2 && t is ListTerm)
                {
                    string s = t.Arg(0).ToString();
                    result[i++] = s.Dequoted("'").Dequoted("\"").Unescaped();
                    t = t.Arg(1);
                }

                return result;
            }

            /*
                From: http://www.sics.se/isl/quintus/html/quintus/lib-lis-prl.html

                What is a "Proper" List?

                Several of the predicate descriptions below indicate that a particular predicate
                only works when a particular argument "is a proper list".

                A proper list is either the Atom [] or else it is of the form [_|L] where L is a
                proper list.

                X is a partial list if and only if var(X) or X is [_|L] where L is a partial list.
                A term is a list if it is either a proper list or a partial list; that is, [_|foo]
                is not normally considered to be a list because its tail is neither a variable nor [].

                Note that the predicate list(X) defined in library(lists) really tests whether
                X is a proper list. The name is retained for compatibility with earlier releases
                of the library. Similarly, is_set(X) and is_ordset(X) test whether X is a proper
                list that possesses the additional properties defining sets and ordered sets.

                The point of the definition of a proper list is that a recursive procedure working
                its way down a proper list can be certain of terminating. Let us take the case of
                list/2 as an example. list(X, L) ought to be true when append(_, [X], L) is true.
                The obvious way of doing this is

                     list(List, [List]).
                     list(List, [_|End]) :-
                             list(List, End).

                If called with the second argument a proper list, this definition can be sure of
                terminating (though it will leave an extra choice point behind). However, if you Call

                     | ?- list(X, L), properLength(L, 0).

                where L is a variable, it will backtrack forever, trying ever longer lists.
                Therefore, users should be sure that only proper lists are used in those argument
                positions that require them.

               */
        }

        public class DcgTerm : CompoundTerm
        {
            public DcgTerm(Symbol symbol, BaseTerm t, ref BaseTerm z, VarStack varStack)
                : base(symbol, t.FunctorToString, new BaseTerm[t.Arity + 2])
            {
                for (int i = 0; i < t.Arity; i++)
                {
                    CompoundArgs[i] = t.Arg(i);
                }

                CompoundArgs[arity - 2] = z;
                CompoundArgs[arity - 1] = z = new Variable(symbol, varStack);
            }

            public DcgTerm(Symbol symbol, BaseTerm t0, BaseTerm t1) : base(symbol, PrologParser.CURL, t0, t1)
            {
            }

            public DcgTerm(Symbol symbol) : base(symbol, PrologParser.CURL)
            {
            }

            public DcgTerm(Symbol symbol, object functor, BaseTerm[] args) : base(symbol, functor, args)
            {
            }

            public override bool IsDcgList => ChainEnd() == NULLCURL || ChainEnd() is DcgTerm;

            public DcgTerm FlattenDcgList()
            {
                List<BaseTerm> a = FlattenDcgListEx();

                DcgTerm result = NULLCURL; // {}

                for (int i = a.Count - 1; i >= 0; i--)
                {
                    result = new DcgTerm(a[i].Symbol, a[i], result); // {a0, a0, ...}
                }

                return result;
            }

            private List<BaseTerm> FlattenDcgListEx()
            {
                BaseTerm t = this;
                BaseTerm t0;
                List<BaseTerm> result = new List<BaseTerm>();

                while (t.FunctorToString == PrologParser.CURL && t.Arity == 2)
                {
                    if ((t0 = t.Arg(0)).IsDcgList)
                    {
                        result.AddRange(((DcgTerm)t0).FlattenDcgListEx());
                    }
                    else
                    {
                        result.Add(t0);
                    }

                    t = t.Arg(1);
                }

                if (t.IsVar)
                {
                    result.Add(t);
                }

                return result;
            }

            public override string ToDisplayString(int level)
            {
                if (this == NULLCURL)
                {
                    return PrologParser.CURL;
                }

                StringBuilder sb = new StringBuilder("'{}'(");
                sb.Append(Arg(0).ToDisplayString(level));
                sb.Append(CommaAtLevel(level));
                sb.Append(Arg(1).ToDisplayString(level));
                sb.Append(")");

                return sb.ToString();
            }

            public override void TreePrint(int level, PrologEngine e)
            {
                string margin = Spaces(2 * level);

                e.WriteLine("{0}{1}", margin, '{');

                BaseTerm t = ChainEnd();

                while (t.IsListNode)
                {
                    t.Arg(0).TreePrint(level + 1, e);
                    t = t.Arg(1);
                }

                e.WriteLine("{0}{1}", margin, '}');
            }
        }

        public class UserClassTerm<T> : BaseTerm
        {
            public UserClassTerm(Symbol symbol, T obj)
                : base(symbol)
            {
                UserObject = obj;
            }

            public T UserObject { get; set; }
        }
    }
}