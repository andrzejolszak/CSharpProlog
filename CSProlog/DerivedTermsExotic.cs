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
using System.Text;
using System.Collections.Generic;
using System.Collections;
using static Prolog.PrologEngine.BaseParser;

namespace Prolog
{
    public partial class PrologEngine
    {
                public class AltListTerm : ListTerm
        {
            public override string FunctorToString => functor.ToString().ToAtom();

            public AltListTerm(Symbol symbol, string leftBracket, string rightBracket)
                : base(symbol)
            {
                isAltList = true;
                functor = leftBracket + ".." + rightBracket;
                this.leftBracket = leftBracket;
                this.rightBracket = rightBracket;
            }

            public AltListTerm(Symbol symbol, string leftBracket, string rightBracket, BaseTerm t0, BaseTerm t1)
              : base(symbol, t0.ChainEnd(), t1.ChainEnd())
            {
                isAltList = true;
                functor = leftBracket + ".." + rightBracket;
                this.leftBracket = leftBracket;
                this.rightBracket = rightBracket;
            }

            public AltListTerm(Symbol symbol, string leftBracket, string rightBracket, BaseTerm[] a)
              : base(symbol, a)
            {
                isAltList = true;
                functor = leftBracket + ".." + rightBracket;
                this.leftBracket = leftBracket;
                this.rightBracket = rightBracket;
            }

            public static AltListTerm ListFromArray(Symbol symbol,
              string leftBracket, string rightBracket, BaseTerm[] ta, BaseTerm afterBar)
            {
                AltListTerm result = null;

                for (int i = ta.Length - 1; i >= 0; i--)
                    result = new AltListTerm(symbol, leftBracket, rightBracket, ta[i], result == null ? afterBar : result);

                return result;
            }


            public override ListTerm Reverse()
            {
                AltListTerm result = new AltListTerm(this.Symbol, leftBracket, rightBracket);

                foreach (BaseTerm t in this) result =
                  new AltListTerm(this.Symbol, leftBracket, rightBracket, t, result);

                return result;
            }


            public override ListTerm FlattenList()
            {
                List<BaseTerm> a = FlattenListEx(functor);

                AltListTerm result = new AltListTerm(this.Symbol, leftBracket, rightBracket);

                for (int i = a.Count - 1; i >= 0; i--)
                    result = new AltListTerm(this.Symbol, leftBracket, rightBracket, a[i], result); // [a0, a0, ...]

                return result;
            }
        }
        
                public class JsonTerm : ListTerm
        {
            public int MaxIndentLevel { get; set; } = 1;
            private bool noQuotes = false;
            private JsonTextBuffer jtb;

            public JsonTerm(Symbol symbol) : base(symbol){ }

            public JsonTerm(Symbol symbol, BaseTerm t0, BaseTerm t1) : base(symbol, t0, t1) { }

            public JsonTerm(ListTerm lt)
                : base(lt.Symbol)
            {
                functor = lt.Functor;
                args = lt.Args;
            }

            public static JsonTerm FromArray(BaseTerm[] ta)
            {
                JsonTerm result = new JsonTerm((Symbol)null);

                for (int i = ta.Length - 1; i >= 0; i--)
                    result = new JsonTerm(ta[i].Symbol, ta[i], result);

                return result;
            }


            // JSON formatting
            
            private class JsonTextBuffer
            {
                private StringBuilder sb; // sealed, cannot inheritate from
                private int level;
                private int indentDelta; // increment per indentation level
                private int maxIndentLevel; // no indentation beyond this level (flat structures only)
                private bool noCommas; // no commas between list items
                private string Indentation => new string(' ', level * indentDelta);

                public JsonTextBuffer(int indentDelta, int maxIndentLevel, bool noCommas)
                {
                    sb = new StringBuilder();
                    level = 0;
                    this.indentDelta = indentDelta;
                    this.maxIndentLevel = maxIndentLevel;
                    this.noCommas = noCommas;
                }


                public void EmitOpenBracket(char c)
                {
                    sb.Append(c);

                    if (level <= maxIndentLevel)
                    {
                        level++;
                        sb.Append(Indentation);
                    }
                }


                public void EmitCloseBracket(char c)
                {
                    if (level <= maxIndentLevel)
                    {
                        level--;
                        sb.AppendLine();
                        sb.Append(Indentation);
                    }
                    else
                        sb.Append(' ');

                    sb.Append(c);
                }


                public void Newline()
                {
                    if (level <= maxIndentLevel)
                    {
                        sb.AppendLine();
                        sb.Append(Indentation);
                    }
                    else
                        sb.Append(' ');
                }


                public void AppendPossibleCommaAndNewLine(ref bool first, int MaxIndentLevel)
                {
                    if (first)
                        first = false;
                    else if (!noCommas)
                        sb.Append(',');

                    Newline();
                }


                public void EmitString(string value)
                {
                    sb.Append(value);
                }


                public override string ToString()
                {
                    return sb.ToString();
                }
            }
            
            public string ToJsonString(int indentDelta, int maxIndentLevel, bool noCommas, bool noQuotes)
            {
                jtb = new JsonTextBuffer(indentDelta, maxIndentLevel, noCommas);
                this.noQuotes = noQuotes;
                DoJsonStruct(this);

                return jtb.ToString().Dequoted();
            }


            public override string ToWriteString(int maxIndentLevel)
            {
                return ToJsonString(0, 0, false, false);
            }


            private void DoJsonStruct(BaseTerm t) // object or array
            {
                if (t.FunctorToString == "array")
                    DoJsonArray(t.Arg(0));
                else if (t.IsProperList)
                    DoJsonObject(t);
                else
                    IO.ErrorRuntime($"Not a well-formed JSON-term: {t}", null, t);
            }


            private void DoJsonObject(BaseTerm t) // object, array or literal
            {
                jtb.EmitOpenBracket('{');
                bool first = true;

                // traverse list
                foreach (BaseTerm e in (ListTerm)t)
                {
                    jtb.AppendPossibleCommaAndNewLine(ref first, MaxIndentLevel); // '{' <pair>+ '}'
                    DoJsonPair(e);
                }

                jtb.EmitCloseBracket('}');
            }


            private void DoJsonArray(BaseTerm t) // '[' <value>+ ']'
            {
                jtb.EmitOpenBracket('[');
                bool first = true;

                // traverse list
                foreach (BaseTerm e in (ListTerm)t)
                {
                    jtb.AppendPossibleCommaAndNewLine(ref first, MaxIndentLevel); // newline & indentation
                    DoJsonValue(e);
                }

                jtb.EmitCloseBracket(']');
            }


            private void DoJsonPair(BaseTerm t) // <string> ':' <value>
            {
                if (t.Arity < 2)
                    IO.ErrorRuntime($"Not a well-formed JSON-term: {t}", null, t);

                DoJsonLiteral(t.Arg(0));
                jtb.EmitString(": ");
                BaseTerm arg1 = t.Arg(1);

                if (arg1.Arity == 0)
                    DoJsonLiteral(arg1);
                else
                {
                    jtb.Newline();
                    DoJsonValue(arg1);
                }
            }


            private void DoJsonValue(BaseTerm t) // <object> | <array> | <literal>
            {
                if (t.Arity == 0)
                    DoJsonLiteral(t);
                else
                    DoJsonStruct(t);
            }


            private void DoJsonLiteral(BaseTerm t)
            {
                string s = t.FunctorToString; // not quoted

                if (noQuotes)
                    jtb.EmitString(s);
                else
                {
                    if (s.HasSignedRealNumberFormat() || s == "false" || s == "true" || s == "null")
                        jtb.EmitString(s);
                    else
                        jtb.EmitString(t.ToString());
                }
            }
        }
        
                public class IntRangeTerm : CompoundTerm
        {
            private BaseTerm lowBound;
            private BaseTerm hiBound;
            private IEnumerator iEnum;
            public override bool IsCallable => false;

            public IntRangeTerm(Symbol symbol, BaseTerm lowBound, BaseTerm hiBound)
              : base(symbol, "..", lowBound, hiBound)
            {
                this.lowBound = lowBound;
                this.hiBound = hiBound;
                iEnum = GetEnumerator();
            }

            public IntRangeTerm(IntRangeTerm that) // for copying only
              : base(that.Symbol, "..", that.lowBound, that.hiBound)
            {
                this.lowBound = that.lowBound;
                this.hiBound = that.hiBound;
                iEnum = GetEnumerator();
            }

            public ListTerm ToList()
            {
                ListTerm result = ListTerm.EMPTYLIST;

                int lo = lowBound.To<int>();
                int hi = hiBound.To<int>();

                for (int i = hi; i >= lo; i--)
                    result = new ListTerm(this.Symbol, new DecimalTerm(this.Symbol, i), result);

                return result;
            }

            private IEnumerator GetEnumerator()
            {
                int lo = lowBound.To<int>();
                int hi = hiBound.To<int>();

                for (int i = lo; i <= hi; i++)
                    yield return new DecimalTerm(this.Symbol, i);
            }

            public bool GetNextValue(out DecimalTerm dt)
            {
                dt = null;

                if (!iEnum.MoveNext()) return false;

                dt = (DecimalTerm)iEnum.Current;

                return true;
            }


            public override string ToWriteString(int level)
            {
                return $"{lowBound.To<int>()}..{hiBound.To<int>()}";
            }


            public override void TreePrint(int level, PrologEngine e)
            {
                e.WriteLine("{0}{1}..{2}", Spaces(2 * level), lowBound.To<int>(), hiBound.To<int>());
            }
        }
    }
}
