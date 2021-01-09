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

namespace Prolog
{
    public enum AssocType { None = -1, xfx, xfy, yfx, fx, fy, xf, yf, f } // f: operator as term

    internal enum AssocGroup { None = -1, Infix, Prefix, Postfix, Zerofix }
    public enum RelOp { None, LT, LE }

    public partial class PrologEngine
    {
                public class OperatorDescr
        {
            public string Name { get; set; }

            public bool IsComma => (Name == ",");
            public int Prec { get; set; }

            public AssocType Assoc { get; set; }

            public bool User { get; set; }

            public bool IsInfix { get; set; }

            public bool IsPrefix { get; set; }

            public bool IsPostfix { get; set; }

            public RelOp LeftRelOp { get; set; }

            public RelOp RightRelOp { get; set; }

            public OperatorDescr()
            {
                Undefine();
            }


            public void Undefine()
            {
                Name = null;
                Prec = -1;
                Assoc = AssocType.None;
                User = false;
                LeftRelOp = RightRelOp = RelOp.None;
                IsInfix = false;
                IsPrefix = false;
                IsPostfix = false;
            }

            public bool HasValidArg(OperatorDescr that, out string msg)
            {
                msg = null;

                if (that == null) return true;

                switch (Assoc)
                {
                    case AssocType.fx:
                    case AssocType.xf:
                        if (this.Prec <= that.Prec) msg = GT_error(this, that);
                        return (msg == null);
                    case AssocType.fy:
                    case AssocType.yf:
                        if (this.Prec < that.Prec) msg = GE_error(this, that);
                        return (msg == null);
                }

                throw new RuntimeException(
                    $"HasValidArg (...) not legal for operator {this}");
            }

            public bool HasValidLeftArg(OperatorDescr that, out string msg)
            {
                msg = null;

                if (that == null) return true;

                switch (Assoc)
                {
                    case AssocType.xfx:
                    case AssocType.xfy:
                        if (this.Prec <= that.Prec) msg = GT_error(this, that);
                        return (msg == null);
                    case AssocType.yfx:
                        if (this.Prec < that.Prec) msg = GE_error(this, that);
                        return (msg == null);
                }

                throw new RuntimeException(
                    $"HasValidLeftArg (...) not legal for operator {this}");
            }

            public bool HasValidRightArg(OperatorDescr that, out string msg)
            {
                msg = null;

                if (that == null) return true;

                switch (Assoc)
                {
                    case AssocType.xfx:
                    case AssocType.yfx:
                        if (this.Prec <= that.Prec) msg = GT_error(this, that);
                        return (msg == null);
                    case AssocType.xfy:
                        if (this.Prec < that.Prec) msg = GE_error(this, that);
                        return (msg == null);
                }

                throw new RuntimeException(
                    $"HasValidRightArg (...) not legal for operator {this}");
            }


            private string GT_error(OperatorDescr od0, OperatorDescr od1)
            {
                if (od0 == od1)
                    return $"Parentheses required for this combination of {od0.Assoc}-operators '{od0.Name}'.";
                else
                    return $"Precedence of {od0} must be greater than the precedence of {od1}";
            }


            private string GE_error(OperatorDescr od0, OperatorDescr od1)
            {
                if (od0 == od1)
                    return $"Parentheses required for this combination of {od0.Assoc}-operators '{od0.Name}'.";
                else
                    return $"Precedence of {od0} must be greater than or equal to the precedence of {od1}";
            }


            public bool IsDefined => (Prec != -1);

            public override string ToString()
            {
                return $"operator ({Prec},{Assoc},{Name})";
            }
        }
        
                public class OpDescrTriplet // contains three OperatorDescr's with inf/pre/post-role, resp.
        {
            private OperatorDescr[] triplet;
            private OperatorDescr od; // general use
            public string Name { get; }

            public OpDescrTriplet(string name, int prec, AssocType assoc, bool user)
            {
                this.Name = name;
                triplet = new OperatorDescr[3];

                for (int i = 0; i < 3; i++) triplet[i] = new OperatorDescr();

                Assign(name, prec, assoc, user);
            }

            public OperatorDescr this[AssocType assoc] => triplet[(int)GetFixType(assoc)];
            public OperatorDescr this[TT role] => triplet[(int)role];

            public bool HasInfixDef => (triplet[0].IsDefined);
            public bool HasPrefixDef => (triplet[1].IsDefined);
            public bool HasPostfixDef => (triplet[2].IsDefined);
            public bool IsOverloaded => HasInfixDef && (HasPrefixDef || HasPostfixDef);

            public bool HasBinOpDef(out OperatorDescr od)
            {
                return ((od = triplet[0]).IsDefined);
            }

            public bool HasUnOpDef(out OperatorDescr od)
            {
                return ((od = triplet[1]).IsDefined || (od = triplet[2]).IsDefined);
            }

            public void Assign(string name, int prec, AssocType assoc, bool user)
            {
                AssocGroup fixType = GetFixType(assoc);
                od = triplet[(int)fixType];
                od.Name = (name == "','") ? "," : name;
                od.Prec = prec;
                od.Assoc = assoc;
                od.User = user;
                od.IsInfix = (fixType == AssocGroup.Infix);
                od.IsPrefix = (fixType == AssocGroup.Prefix);
                od.IsPostfix = (fixType == AssocGroup.Postfix);
                od.LeftRelOp = (assoc == AssocType.yfx || assoc == AssocType.fy || assoc == AssocType.yf)
                             ? RelOp.LE
                             : RelOp.LT;
                od.RightRelOp = (assoc == AssocType.xfy || assoc == AssocType.fy || assoc == AssocType.yf)
                             ? RelOp.LE
                             : RelOp.LT;

                // An operator can be either prefix or postfix, but not both. In addition, it can be infix.
                if (fixType == AssocGroup.Prefix)
                    triplet[(int)AssocGroup.Postfix].Undefine();
                else if (fixType == AssocGroup.Postfix)
                    triplet[(int)AssocGroup.Prefix].Undefine();
            }


            public void Unassign(string name, AssocType assoc)
            {
                AssocGroup fixType = GetFixType(assoc);
                od = triplet[(int)fixType];

                if (od == null || od.Assoc == AssocType.None)
                    IO.ErrorConsult($"Operator '{name}' does not have an association type '{assoc}'", (BaseParser.Symbol)null);

                triplet[(int)fixType].Undefine();
            }


            public IEnumerator<OperatorDescr> GetEnumerator()
            {
                for (int i = 0; i < 3; i++)
                {
                    if (triplet[i].IsDefined)
                        yield return triplet[i];
                }
            }


            private static AssocGroup GetFixType(AssocType assoc)
            {
                switch (assoc)
                {
                    case AssocType.xfx:
                    case AssocType.xfy:
                    case AssocType.yfx:
                        return AssocGroup.Infix;
                    case AssocType.fx:
                    case AssocType.fy:
                        return AssocGroup.Prefix;
                    case AssocType.xf:
                    case AssocType.yf:
                        return AssocGroup.Postfix;
                }

                throw new RuntimeException("Illegal call to GetFixType");
            }


            public override string ToString()
            {
                StringBuilder sb = new StringBuilder("<" + Name.ToAtom() + " ");
                bool first = true;

                foreach (OperatorDescr od in this)
                {
                    if (first) first = false; else sb.Append(" ");

                    sb.Append($"({od.Prec},{od.Assoc})");
                }

                return sb + ">";
            }
        }
        
                public class OperatorTable : Dictionary<string, OpDescrTriplet>
        {
            public bool Find(string name, out OpDescrTriplet triplet)
            {
                return TryGetValue(name, out triplet);
            }


            public bool HasOpDef(string name)
            {
                return ContainsKey(name);
            }


            public bool IsUnaryOperator(string name, out OperatorDescr od)
            {
                OpDescrTriplet triplet;

                if (TryGetValue(name, out triplet))
                    return triplet.HasUnOpDef(out od);

                od = null;

                return false;
            }


            public bool IsBinaryOperator(string name, out OperatorDescr od)
            {
                OpDescrTriplet triplet;

                if (TryGetValue(name, out triplet))
                    return triplet.HasBinOpDef(out od);

                od = null;

                return false;
            }


            public OpDescrTriplet Add(int prec, string sassoc, string name, bool user)
            {
                AssocType assoc = AssocType.None;
                OpDescrTriplet triplet;

                try
                {
                    assoc = (AssocType)Enum.Parse(typeof(AssocType), sassoc);
                }
                catch
                {
                    IO.ErrorConsult($"Illegal operator associativity '{sassoc}'", (BaseParser.Symbol)null);
                }

                if (prec < 0 || prec > 1200)
                    IO.ErrorConsult($"Illegal precedence value {prec} for operator '{name}'", (BaseParser.Symbol) null);

                if (TryGetValue(name, out triplet)) // operator exists -- modify its properties
                    triplet.Assign(name, prec, assoc, user);
                else
                    this[name] = triplet = new OpDescrTriplet(name, prec, assoc, user);

                return triplet;
            }


            public new IEnumerator<OperatorDescr> GetEnumerator()
            {
                foreach (OpDescrTriplet triplet in Values)
                    foreach (OperatorDescr od in triplet)
                        yield return od;
            }


            public override string ToString()
            {
                StringBuilder sb = new StringBuilder();

                foreach (OpDescrTriplet od in this.Values)
                    sb.AppendLine(od.ToString());

                return sb.ToString();
            }
        }
        
                public class BracketTable : Dictionary<string, string>
        {
            public void Add(ref string openBracket, ref string closeBracket)
            {
                openBracket = openBracket.Dequoted();
                closeBracket = closeBracket.Dequoted();
                this[openBracket] = closeBracket;
            }


            public string FindCloseBracket(string openBracket)
            {
                string closeBracket;
                TryGetValue(openBracket, out closeBracket);

                return closeBracket;
            }
        }
            }

}
