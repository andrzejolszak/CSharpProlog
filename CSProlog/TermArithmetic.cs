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

// Code for calculating expression values (is/2)

// Future: use Visual Studio's 2010 'big number' features ?

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;

namespace Prolog
{
    public partial class PrologEngine
    {
        public partial class BaseTerm
        {
            public T To<T>() where T : struct
            {
                BaseTerm e = ChainEnd();

                try
                {
                    return (e is ValueTerm)
                      ? (T)Convert.ChangeType(e.functor, typeof(T))
                      : e.Eval().To<T>();
                }
                catch
                {
                    if (e is NamedVariable)
                        IO.ErrorRuntime(
                            $"Unable to convert unbound variable {((NamedVariable) e).Name} to type {typeof(T).Name}", null, e);
                    else if (e is Variable)
                        IO.ErrorRuntime(
                            $"Unable to convert an unbound variable to type {typeof(T).Name}", null, e);
                    else if (e is ListTerm)
                        IO.ErrorRuntime($"Unable to convert list {e} to type {typeof(T).Name}", null, e);
                    else
                        IO.ErrorRuntime(
                            $"Unable to convert '{e.FunctorToString}' to type {typeof(T).Name}", null, e);

                    return default(T); // IO.Error throws error, but compiler insists on a return value
                }
            }

            public int CompareValue(ValueTerm t)
            {
                return 0;
            }

            /// <summary>Retrieves the argument value of a term </summary>
            public T Arg<T>(int pos) where T : struct
            {
                return args[pos].To<T>();
            }


            private decimal Trunc(decimal d) // chop decimal part
            {
                return (d > 0) ? Math.Floor(d) : Math.Ceiling(d);
            }


            public BaseTerm Eval() // evaluate the term
            {
                BaseTerm t = ChainEnd();

                if (!t.IsEvaluatable)
                    IO.ErrorRuntime($"{t} cannot be evaluated by is/2", null, t);

                if (t is ValueTerm) return t; // a ValueTerm stands for itself

                if (t.IsProperList && !((ListTerm)t).IsEvaluated) // evaluate all members recursively
                {
                    ListTerm result = ListTerm.EMPTYLIST;
                    List<BaseTerm> tl = ((ListTerm)t).ToList();

                    for (int i = tl.Count - 1; i >= 0; i--)
                        result = new ListTerm(this.Symbol, tl[i].Eval(), result);

                    result.IsEvaluated = true;

                    return result;
                }

                return t.Apply();
            }


            private BaseTerm Apply() // apply the functor to the arguments
            {
                BaseTerm a0, a1, a2, a3;

                if (this.IsVar) IO.ErrorRuntime($"Unable to evaluate '{((Variable) this).Name}'", null, this);

                if (arity == 0)
                {
                    switch (FunctorToString)
                    {
                        case "pi":
                            return new DecimalTerm(this.Symbol, Math.PI);
                        case "e":
                            return new DecimalTerm(this.Symbol, Math.E);
                        case "now":
                            return new DateTimeTerm(this.Symbol, DateTime.Now);
                        case "today":
                            return new DateTimeTerm(this.Symbol, DateTime.Now.Date);
                        case "yesterday":
                            return new DateTimeTerm(this.Symbol, DateTime.Now.AddDays(-1).Date);
                        case "tomorrow":
                            return new DateTimeTerm(this.Symbol, DateTime.Now.AddDays(1).Date);
                        case "false":
                            return new BoolTerm(this.Symbol, false);
                        case "true":
                            return new BoolTerm(this.Symbol, true);
                        default:
                            return new StringTerm(this.Symbol, FunctorToString);
                            //IO.Error ("Unable to evaluate '{0}'", FunctorToString);
                            //break;
                    }
                }
                else if (arity == 1)
                {
                    // do not evaluate the first arg of string/1.
                    // E.engine 'string( 1+1)' will evaluate to "1+1", not "2"
                    a0 = (FunctorToString == "string") ? Arg(0) : Arg(0).Eval();

                    switch (FunctorToString)
                    {
                        case "+":
                            return new DecimalTerm(this.Symbol, a0.To<decimal>());
                        case "-":
                            return new DecimalTerm(this.Symbol, -a0.To<decimal>());
                        case "~":
                            break;
                        case @"\":
                            if (a0 is BoolTerm)
                                return new BoolTerm(this.Symbol, !a0.To<bool>());
                            else
                                return new DecimalTerm(this.Symbol, ~(long)a0.To<double>());
                        case "abs":
                                return new DecimalTerm(this.Symbol, Math.Abs(a0.To<decimal>()));
                        case "exp":
                                return new DecimalTerm(this.Symbol, Math.Exp(a0.To<double>()));
                        case "sin":
                            return new DecimalTerm(this.Symbol, Math.Sin(a0.To<double>()));
                        case "cos":
                            return new DecimalTerm(this.Symbol, Math.Cos(a0.To<double>()));
                        case "tan":
                            return new DecimalTerm(this.Symbol, Math.Tan(a0.To<double>()));
                        case "sinh":
                            return new DecimalTerm(this.Symbol, Math.Sinh(a0.To<double>()));
                        case "cosh":
                            return new DecimalTerm(this.Symbol, Math.Cosh(a0.To<double>()));
                        case "tanh":
                            return new DecimalTerm(this.Symbol, Math.Tanh(a0.To<double>()));
                        case "asin":
                            return new DecimalTerm(this.Symbol, Math.Asin(a0.To<double>()));
                        case "acos":
                            return new DecimalTerm(this.Symbol, Math.Acos(a0.To<double>()));
                        case "atan":
                            return new DecimalTerm(this.Symbol, Math.Atan(a0.To<double>()));
                        case "log":
                                return new DecimalTerm(this.Symbol, Math.Log(a0.To<double>()));
                        case "log10":
                            return new DecimalTerm(this.Symbol, Math.Log10(a0.To<double>()));
                        case "round":
                            // WARNING: The Round method follows the IEEE Standard 754, section 4 standard.
                            // If the number being rounded is halfway between two numbers, the C# Round operation
                            // will always round to the even number. E.i. Round(1.5) = Round(2.5) = 2.
                            return new DecimalTerm(this.Symbol, Math.Round(a0.To<decimal>()));
                        case "floor":
                            return new DecimalTerm(this.Symbol, Math.Floor(a0.To<decimal>()));
                        case "trunc":
                            return new DecimalTerm(this.Symbol, Trunc(a0.To<decimal>()));
                        case "ceil":
                            return new DecimalTerm(this.Symbol, Math.Ceiling(a0.To<decimal>()));
                        case "sign":
                            return new DecimalTerm(this.Symbol, Math.Sign(a0.To<decimal>()));
                        case "sqrt":
                                return new DecimalTerm(this.Symbol, Math.Sqrt(a0.To<double>()));
                        case "sqr":
                            {
                                decimal d = a0.To<decimal>();
                                return new DecimalTerm(this.Symbol, d * d);
                            }
                        case "re":
                                return new DecimalTerm(this.Symbol, a0.To<decimal>()); // catchall
                        case "im":
                            if (a0 is DecimalTerm)
                                return new DecimalTerm(this.Symbol, 0);
                            else
                            {
                                IO.ErrorRuntime($"Cannot take the imaginary part of '{a0}'", null, a0);
                                break;
                            }
                        case "conj":
                            if (a0 is DecimalTerm)
                                return a0;
                            else
                            {
                                IO.ErrorRuntime($"Cannot take the complex conjugate of '{a0}'", null, a0);
                                break;
                            }
                        case "arg":
                        case "phase":
                        case "phi":
                            if (a0 is DecimalTerm)
                                return new DecimalTerm(this.Symbol, 0);
                            else
                            {
                                IO.ErrorRuntime($"Cannot take the arg/phase/phi of '{a0}'", null, a0);
                                break;
                            }
                        case "magnitude":
                            if (a0 is DecimalTerm)
                                return a0;
                            else
                            {
                                IO.ErrorRuntime($"Cannot take the complex magnitude of '{a0}'", null, a0);
                                break;
                            }
                        // string handling
                        case "string":
                        case "string2":
                            return new StringTerm(this.Symbol, $"{a0}");
                        case "length":
                            return new DecimalTerm(this.Symbol, (a0.FunctorToString).Length);
                        case "upcase":
                            return new StringTerm(this.Symbol, (a0.FunctorToString).ToUpper());
                        case "upcase1": // upcase first char; rest unchanged
                            {
                                string s = a0.FunctorToString;
                                return new StringTerm(this.Symbol, (s.Length == 0) ? "" : Char.ToUpper(s[0]) + s.Substring(1));
                            }
                        case "lowcase":
                            return new StringTerm(this.Symbol, a0.FunctorToString.ToLower());
                        case "trim":
                            return new StringTerm(this.Symbol, a0.FunctorToString.Trim());
                        case "trimstart":
                            return new StringTerm(this.Symbol, a0.FunctorToString.TrimStart());
                        case "reverse":
                            return new StringTerm(this.Symbol, a0.FunctorToString.Reverse());
                        case "trimend":
                            return new StringTerm(this.Symbol, a0.FunctorToString.TrimEnd());
                        case "singleline": // replace newlines by a single space (or null if already followed by a space)
                            return new StringTerm(this.Symbol, Regex.Replace(a0.FunctorToString, "(\r(\n| )?|\n ?)", " "));
                        // DateTime stuff
                        case "year":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Year);
                        case "month":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Month);
                        case "day":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Day);
                        case "hour":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Hour);
                        case "minute":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Minute);
                        case "second":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Second);
                        case "millisecond":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Millisecond);
                        case "dayofweek":
                            return new DecimalTerm(this.Symbol, (int)((a0.To<DateTime>()).DayOfWeek));
                        case "dayofyear":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).DayOfYear);
                        case "ticks":
                            return new DecimalTerm(this.Symbol, (a0.To<DateTime>()).Ticks);
                        case "today":
                            return new DateTimeTerm(this.Symbol, DateTime.Today);
                        case "timeofday":
                            return new TimeSpanTerm(this.Symbol, DateTime.Now.TimeOfDay);
                        case "weekno":
                            return new DecimalTerm(this.Symbol, Utils.WeekNo(a0.To<DateTime>()));
                        case "dayname":
                            return new StringTerm(this.Symbol, ((a0.To<DateTime>()).DayOfWeek).ToString("G"));
                        default:
                            IO.ErrorRuntime($"Not a built-in function: {FunctorToString}/1", null, this);
                            break;
                    }
                }
                else if (arity == 2)
                {
                    a0 = Arg(0).Eval();

                    // do not evaluate the second arg of format/2.
                    // E.engine 'format( "{0}", 1+1)' will evaluate to "1+1", not "2"
                    a1 = (FunctorToString == "format") ? Arg(1) : Arg(1).Eval();

                    switch (FunctorToString)
                    {
                        case "..": // range -> list
                            int lo = a0.To<int>();
                            int hi = a1.To<int>();
                            // create a list with elements [lo, lo+1, ... hi]
                            ListTerm result = ListTerm.EMPTYLIST;

                            for (int i = hi; i >= lo; i--)
                                result = new ListTerm(this.Symbol, new DecimalTerm(this.Symbol, i), result);

                            return result;
                        case "+":
                            if ((a0 is StringTerm || a0 is AtomTerm) &&
                              (a1 is StringTerm || a1 is AtomTerm))
                                return new StringTerm(this.Symbol, a0.FunctorToString.Unescaped() + a1.FunctorToString.Unescaped());
                            else if (a0 is DateTimeTerm && a1 is TimeSpanTerm)
                                return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).Add(a1.To<TimeSpan>()));

                            return new DecimalTerm(this.Symbol, a0.To<decimal>() + a1.To<decimal>());
                        case "-":
                            if (a0 is DateTimeTerm)
                            {
                                if (a1 is TimeSpanTerm)
                                    return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).Subtract(a1.To<TimeSpan>()));
                                else if (a1 is DateTimeTerm)
                                    return new TimeSpanTerm(this.Symbol, (a0.To<DateTime>()).Subtract(a1.To<DateTime>()));
                                else
                                    break;
                            }

                            return new DecimalTerm(this.Symbol, a0.To<decimal>() - a1.To<decimal>());
                        case "*":

                            return new DecimalTerm(this.Symbol, a0.To<decimal>() * a1.To<decimal>());
                        case "/":

                            return new DecimalTerm(this.Symbol, a0.To<decimal>() / a1.To<decimal>());
                        case "<<":
                            return new DecimalTerm(this.Symbol, a0.To<long>() << a1.To<int>());
                        case ">>":
                            return new DecimalTerm(this.Symbol, a0.To<long>() >> a1.To<int>());
                        case "=":
                            return new BoolTerm(this.Symbol, a0.CompareTo(a1) == 0);
                        case "\\=":
                            return new BoolTerm(this.Symbol, a0.CompareTo(a1) != 0);
                        case "<>":
                            return new BoolTerm(this.Symbol, a0.CompareTo(a1) != 0);
                        case "<":
                            return new BoolTerm(this.Symbol, a0.CompareTo(a1) < 0);
                        case "=<":
                            return new BoolTerm(this.Symbol, a0.CompareTo(a1) <= 0);
                        case ">":
                            return new BoolTerm(this.Symbol, a0.CompareTo(a1) > 0);
                        case ">=":
                            return new BoolTerm(this.Symbol, a0.CompareTo(a1) >= 0);
                        case "//":
                            return new DecimalTerm(this.Symbol, Trunc(a0.To<decimal>() / a1.To<decimal>()));
                        case "#":
                            return new DecimalTerm(this.Symbol, a0.To<long>() ^ a1.To<long>());
                        case @"/\":
                            if (a0 is BoolTerm && a1 is BoolTerm)
                                return new BoolTerm(this.Symbol, a0.To<bool>() && a1.To<bool>());
                            else
                                return new DecimalTerm(this.Symbol, a0.To<long>() & a1.To<long>());
                        case @"\/":
                            if (a0 is BoolTerm && a1 is BoolTerm)
                                return new BoolTerm(this.Symbol, a0.To<bool>() || a1.To<bool>());
                            else
                                return new DecimalTerm(this.Symbol, a0.To<long>() | a1.To<long>());
                        case "^":
                            if (a0 is BoolTerm && a1 is BoolTerm)
                                return new BoolTerm(this.Symbol, a0.To<bool>() ^ a1.To<bool>());

                            return new DecimalTerm(this.Symbol, Math.Pow(a0.To<double>(), a1.To<double>()));
                        case "mod":
                            return new DecimalTerm(this.Symbol, a0.To<decimal>() % a1.To<decimal>());
                        case "round":
                            return new DecimalTerm(this.Symbol, Math.Round(a0.To<decimal>(), a1.To<int>()));
                        case "atan2":
                            return new DecimalTerm(this.Symbol,
                              Math.Atan2(a0.To<double>(), a1.To<double>()));
                        case "max":
                            return new DecimalTerm(this.Symbol, Math.Max(a0.To<decimal>(), a1.To<decimal>()));
                        case "min":
                            return new DecimalTerm(this.Symbol, Math.Min(a0.To<decimal>(), a1.To<decimal>()));
                        // string handling
                        case "format":  // format without argument evaluation before substitution
                        case "format2": // format with ...
                            if (a0 is StringTerm && a1 is ListTerm)
                                return new StringTerm(this.Symbol, string.Format(a0.FunctorToString, ((ListTerm)a1).ToStringArray()));
                            else if (a0 is DateTimeTerm)
                                return new StringTerm(this.Symbol, (a0.To<DateTime>()).ToString(a1.FunctorToString));
                            else if (a1 is DecimalTerm)
                                return new StringTerm(this.Symbol, string.Format(a0.FunctorToString, a1.To<decimal>()));
                            else
                                return new StringTerm(this.Symbol, string.Format(a0.FunctorToString, a1));
                        case "indexof":
                            return new DecimalTerm(this.Symbol, (a0.FunctorToString).IndexOf(a1.FunctorToString));
                        case "padleft":
                            return new StringTerm(this.Symbol, (a0.FunctorToString).PadLeft(a1.To<int>()));
                        case "padright":
                            return new StringTerm(this.Symbol, (a0.FunctorToString).PadRight(a1.To<int>()));
                        case "remove":
                            int len = a1.To<int>();
                            return new StringTerm(this.Symbol, (a0.FunctorToString).Remove(len, (a1.FunctorToString).Length - len));
                        case "substring":
                            len = a1.To<int>();
                            return new StringTerm(this.Symbol, (a0.FunctorToString).Substring(len, (a0.FunctorToString).Length - len));
                        case "wrap":
                            return new StringTerm(this.Symbol, Utils.ForceSpaces(a0.FunctorToString, (a1.To<int>())));
                        case "split":
                            string splitChars = a1.FunctorToString;
                            ListTerm splitList = ListTerm.EMPTYLIST;
                            if (splitChars.Length == 0)
                            {
                                splitChars = a0.FunctorToString;
                                for (int i = splitChars.Length - 1; i >= 0; i--)
                                    splitList = new ListTerm(this.Symbol, new StringTerm(this.Symbol, splitChars[i]), splitList);
                            }
                            else
                            {
                                string[] part = a0.FunctorToString.Split(splitChars.ToCharArray());
                                for (int i = part.Length - 1; i >= 0; i--)
                                    splitList = new ListTerm(this.Symbol, new StringTerm(this.Symbol, part[i]), splitList);
                            }
                            return splitList;
                        case "chain":
                            if (!a0.IsProperList)
                                IO.ErrorRuntime($"chain/2:first argument '{a0}' is not a proper list", null, a0);
                            StringBuilder chain = new StringBuilder();
                            string separator = a1.FunctorToString;
                            foreach (BaseTerm t in (ListTerm)a0)
                            {
                                if (chain.Length != 0) chain.Append(separator);
                                chain.Append(t.FunctorToString);
                            }
                            return new StringTerm(this.Symbol, chain.ToString());
                        case "repeat":
                            return new StringTerm(this.Symbol, a0.FunctorToString.Repeat(a1.To<int>()));
                        case "levdist": // Levenshtein distance
                            return new DecimalTerm(this.Symbol, a0.FunctorToString.Levenshtein(a1.FunctorToString));
                        // date/time
                        case "addyears":
                            return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).AddYears(a1.To<int>()));
                        case "addmonths":
                            return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).AddMonths(a1.To<int>()));
                        case "adddays":
                            return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).AddDays(a1.To<int>()));
                        case "addhours":
                            return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).AddHours(a1.To<int>()));
                        case "addminutes":
                            return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).AddMinutes(a1.To<int>()));
                        case "addseconds":
                            return new DateTimeTerm(this.Symbol, (a0.To<DateTime>()).AddSeconds(a1.To<int>()));
                        default:
                            IO.ErrorRuntime($"Not a built-in function: {FunctorToString}/2", null, this);
                            break;
                    }
                }
                else if (arity == 3)
                {
                    a0 = Arg(0).Eval();
                    a1 = Arg(1).Eval();
                    a2 = Arg(2).Eval();

                    switch (FunctorToString)
                    {
                        case "indexof":
                            return new DecimalTerm(this.Symbol, (a0.FunctorToString).IndexOf(a1.FunctorToString, a2.To<int>()));
                        case "remove":
                            return new StringTerm(this.Symbol, (a0.FunctorToString).Remove(a1.To<int>(), a2.To<int>()));
                        case "substring":
                            return new StringTerm(this.Symbol, (a0.FunctorToString).Substring(a1.To<int>(), a2.To<int>()));
                        case "replace":
                            return new StringTerm(this.Symbol, (a0.FunctorToString).Replace(a1.FunctorToString, a2.FunctorToString));
                        case "regexreplace":
                            return new StringTerm(this.Symbol, Regex.Replace(a0.FunctorToString, a1.FunctorToString, a2.FunctorToString));
                        case "time":
                        case "timespan":
                            return new TimeSpanTerm(this.Symbol,
                              new TimeSpan(a0.To<int>(), a1.To<int>(), a2.To<int>()));
                        case "date":
                        case "datetime":
                            return new DateTimeTerm(this.Symbol,
                              new DateTime(a0.To<int>(), a1.To<int>(), a2.To<int>()));
                        case "if":
                            return new StringTerm(this.Symbol, a0.To<bool>() ? a1.FunctorToString : a2.FunctorToString);
                        default:
                            IO.ErrorRuntime($"Not a built-in function: {FunctorToString}/3", null, this);
                            break;
                    }
                }
                else if (arity == 4)
                {
                    a0 = Arg(0).Eval();
                    a1 = Arg(1).Eval();
                    a2 = Arg(2).Eval();
                    a3 = Arg(3).Eval();

                    if (HasFunctor("timespan"))
                        return new TimeSpanTerm(this.Symbol,
                          new TimeSpan(a0.To<int>(), a1.To<int>(), a2.To<int>(), a3.To<int>()));
                    else
                        IO.ErrorRuntime($"Not a built-in function: {FunctorToString}/4", null, this);
                }
                else
                    IO.ErrorRuntime($"Not a built-in function: {FunctorToString}/{Arity}", null, this);

                return null;
            }
        }
    }
}
