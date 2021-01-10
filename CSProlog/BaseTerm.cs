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
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using static Prolog.PrologEngine.BaseParser;

namespace Prolog
{
    /*
       All Prolog data structures are called terms. A term is either:
       - A variable (unbound, named).
       - A constant, which can be either an Atom, Number, String, DateTime, TimeSpan or Bool
       - A compound term (a functor (an atom) and one or more arguments)
    */
    public enum TermType
    { // order is important for term comparison. See CompareTo below.
        None,
        Var,
        UnboundVar,
        NamedVar,
        Number,
        ImagNumber,
        Atom,
        String,
        DateTime,
        TimeSpan,
        Bool,
        FileReader,
        FileWriter,
        SqlCommand,
        Binary,
        Compound
    }

    public interface ITermNode
    {
        string Functor { get; }
        ITermNode[] Args { get; }
        T To<T>() where T : struct;
        string ToString();
    }

    public partial class PrologEngine
    {
        public class NodeIterator : IEnumerable<BaseTerm>
        {
            private BaseTerm root;
            private BaseTerm pattern;
            private BaseTerm minLenTerm;
            private BaseTerm maxLenTerm;
            private BaseTerm path;
            private Stack<int> pos;
            private Stack<BaseTerm> terms;
            private VarStack varStack;
            private IEnumerator<BaseTerm> iterator;
            private bool skipVars; // iff true variables in the Term tree will never match the pattern

            public NodeIterator(BaseTerm root, BaseTerm pattern, BaseTerm minLenTerm,
              BaseTerm maxLenTerm, bool skipVars, BaseTerm path, VarStack varStack)
            {
                this.root = root;
                this.pattern = pattern;
                this.varStack = varStack;
                this.minLenTerm = minLenTerm;
                this.maxLenTerm = maxLenTerm;
                this.skipVars = skipVars;
                this.path = path;
                iterator = GetEnumerator();
            }

            /*
              GetEnumerator () traverses the Term tree and returns each node in a
              depth-first fashion. I have chosen to implemement it non-recursively,
              using a stack mechanism, as I also wanted the 'path' in the tree to
              be determined. Pseudo-code:

              TraverseAllNodes (root)
              {
                push (<root, 0>) // index of next root-child to be traversed
             L: pop (<term, childNo>)
                if (childNo == 0) Process (term) // contains test & yield
                if ({no such child}) goto L
                push (<term, childNo+1>)
                push (<term.Arg(childNo), 0>)
                if ({stack not empty}) goto L
              }

              // recursive version (not tested)
              IEnumerable<BaseTerm> TraverseAllNodes (BaseTerm root, BaseTerm pattern)
              {
                if (root.IsUnifiableWith (pattern))
                  yield return root.LinkEnd;

                foreach (BaseTerm arg in root.args)
                  foreach (BaseTerm node in TraverseTree (arg, pattern))
                     yield return node;
              }
            */

            public IEnumerator<BaseTerm> GetEnumerator()
            {
                pos = new Stack<int>(0);
                terms = new Stack<BaseTerm>();
                BaseTerm term = root;
                int childNo = 0;
                int minLevel = minLenTerm.IsVar ? 0 : minLenTerm.To<int>();
                int maxLevel = maxLenTerm.IsVar ? int.MaxValue : maxLenTerm.To<int>();

                terms.Push(term);
                pos.Push(childNo);

                do
                {
                    term = terms.Peek();
                    childNo = pos.Pop();

                    // do not match the pattern with single variables, except if the pattern is an atom
                    if ((!(skipVars && term is Variable) || pattern.Arity == 0) &&
                        childNo == 0 &&
                        pos.Count >= minLevel &&
                        pos.Count <= maxLevel &&
                        pattern.Unify(term, varStack))
                    {
                        if (minLenTerm.IsVar)
                            minLenTerm.Unify(new DecimalTerm(NodePath.Symbol, NodePath.ProperLength), varStack);

                        if (maxLenTerm.IsVar)
                            maxLenTerm.Unify(new DecimalTerm(NodePath.Symbol, NodePath.ProperLength), varStack);

                        if (!path.IsCut && !path.Unify(NodePath, varStack))
                            continue;

                        yield return pattern;
                    }

                    if (childNo < term.Arity)
                    {
                        pos.Push(childNo + 1); // next child to be processed ...

                        terms.Push(term.Arg(childNo)); // .. but process its children first
                        pos.Push(0);
                    }
                    else
                        terms.Pop();
                }
                while (terms.Count > 0);
            }


            // compiler complains if this is missing -- cf. Jon Skeet C# in Depth p.91
            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }


            public bool MoveNext()
            {
                return iterator.MoveNext();
            }

            // NodePath returns the position in the tree (i.e. [1,2] means: 2nd arg of 1st arg of root)
            public ListTerm NodePath
            {
                get
                {
                    ListTerm result = ListTerm.EMPTYLIST;

                    foreach (int i in pos.ToArray()) result = new ListTerm(null, new DecimalTerm(null, i - 1), result);

                    return result;
                }
            }
        }


        public partial class BaseTerm : ITermNode, IComparable<BaseTerm>
        {
            public string CommentHeader { get; set; }
            public string CommentBody { get; set; }
            public string TestGroup { get; set; }

            protected int termId; // for variables: the varNo, for (some) other term types: unique int for functor+arity combination
            protected short precedence;
            protected AssocType assocType; // i.e. fx, fy, xfx, xfy, yfx, xf, yf.
            protected BaseTerm[] args; // compound term arguments
            protected TermType termType;
            protected object functor;

            protected int arity => (args == null) ? 0 : args.Length;
            public int TermId { get { return termId; } set { termId = value; } }

            // ChainEnd () is the end term of a unification chain, i.e. the term with which all the
            // other terms in the chain are unified. The ChainEnd () does not necessarily have a value! (i.e. it can be nonvar)
            public virtual BaseTerm ChainEnd() { return this; }
            public virtual BaseTerm ChainEnd(int unifyCount) { return this; }

            // could move all prop's referring to ChainEnd () to Variable, and leave the base types here
            public virtual object Functor { get { return ChainEnd().functor; } set { ChainEnd().functor = value; } }
            public virtual int Arity => ChainEnd().arity;
            public TermType TermType => ChainEnd().termType;
            public virtual string FunctorToString => functor == null ? null : ChainEnd().functor.ToString();
            public bool HasFunctor(string s) { return (FunctorToString == s); }
            public AssocType AssocType => ChainEnd().assocType;
            public int Precedence => ChainEnd().precedence;
            public BaseTerm[] Args => ChainEnd().args;
// Args [i] != Arg (i) !!!!
            public string Index => ChainEnd().FunctorToString + "/" + ChainEnd().arity;
// for readability only
            public override string ToString() { return ChainEnd().ToWriteString(0); }
            public string ToDisplayString() { return ChainEnd().ToDisplayString(0); }
            public virtual string ToDisplayString(int level) { return ChainEnd().ToWriteString(level); }

            public bool FunctorIsDot => (FunctorToString == ".");
            public string Key => MakeKey(FunctorToString, arity);
            public virtual string Name => FunctorToString + '/' + arity;
            public bool FunctorIsBinaryComma => (FunctorToString == ",");
            public virtual bool IsCallable => false;
// i.e. if it can be a predicate head
            public virtual bool IsEvaluatable => false;
// i.e. if it can be evaluated by is/2
            public virtual bool IsUnified => false;
            public virtual string ToWriteString(int level) { return FunctorToString; }
            
            // TODO: statics
            private static int verNoMax;
            protected static int varNoMax;

            protected static readonly string NUMVAR;
            public static readonly ListTerm EMPTYLIST;
            public static readonly DcgTerm NULLCURL;
            public static readonly Variable VAR;
            public static readonly BaseTerm TREEROOT;
            public static readonly BaseTerm CUT;
            public static readonly BaseTerm FAIL;
            public static readonly BaseTerm DBNULL;
            public static string MakeKey(string f, int a) { return a + f; }

            public bool IsVar => (ChainEnd() is Variable);
            public bool IsAtomic => (ChainEnd().IsAtom || ChainEnd() is ValueTerm);
            public bool IsString => (ChainEnd() is StringTerm);
            public bool IsBool => (ChainEnd() is BoolTerm);
            public bool IsDateTime => (ChainEnd() is DateTimeTerm);
            public bool IsTimeSpan => (ChainEnd() is TimeSpanTerm);
            public bool IsCompound => (ChainEnd() is CompoundTerm);
            public bool IsNamedVar => (ChainEnd() is NamedVariable);
            public bool IsUnboundTerm => (ChainEnd() is Variable);
            public bool IsEmptyList => (ChainEnd() is ListTerm && Arity == 0);
            public bool IsCut => (ChainEnd() is Cut);
            public bool IsAtom => (Arity == 0 && !(ChainEnd() is ValueTerm));
// Var has -1
            public bool IsAtomOrString => (IsAtom || IsString);
            public bool IsInteger => (IsNumber && Decimal.Remainder(To<decimal>(), 1) == 0);
            public bool IsNatural => (IsNumber && To<decimal>() >= 0);
            public bool IsFloat => (IsNumber && Decimal.Remainder(To<decimal>(), 1) != 0);

            public bool IsNumber
            {
                get
                {
                    BaseTerm t = ChainEnd();

                    return (t is DecimalTerm ||
                            t is OperatorTerm && t.arity == 1 && t.Arg(0).IsNumber && t.HasUnaryOperator("+", "-"));
                }
            }

            protected TermType Rank => TermType;
            protected virtual int CompareValue(BaseTerm t) { return FunctorToString.CompareTo(t.FunctorToString); }
            public virtual bool IsProperList => false;
            public virtual bool IsPartialList => false;
            public virtual bool IsPseudoList => false;
            public virtual bool IsProperOrPartialList => false;
            public virtual bool IsListNode => false;
            public virtual bool IsDcgList => false;
            public virtual bool HasUnaryOperator() { return false; }
            public virtual bool HasBinaryOperator() { return false; }
            public virtual bool HasUnaryOperator(params string[] names) { return false; }
            public virtual bool HasBinaryOperator(params string[] names) { return false; }

                        public BaseTerm Arg(int pos) { return args[pos].ChainEnd(); }
            public void SetArg(int pos, BaseTerm t) { ChainEnd().args[pos] = t; }
            


            public bool IsGround
            {
                get
                {
                    if (IsVar) return false;

                    for (int i = 0; i < arity; i++)
                        if (Arg(i).IsVar) return false;

                    return true;
                }
            }

            // interface properties
            string ITermNode.Functor => FunctorToString;
            ITermNode[] ITermNode.Args => Args;

            public Symbol Symbol { get; }

            protected string CommaAtLevel(int level) { return (level == 0 ? ", " : ","); }
            protected string SpaceAtLevel(int level) { return (level == 0 ? " " : ""); }
            
            // BaseTerm comparison according to the ISO standard (apart from the extra data types)
            // 1. Variables < Numbers < Atoms < Strings < Compound Terms
            // 2. Variables are sorted by address.
            // 3. Atoms are compared alphabetically.
            // 4. Strings are compared alphabetically.
            // 5. Numbers are compared by value. Integers and floats are treated identically.
            // 6. Compound terms are first checked on their FunctorString-name (alphabetically), then on their arity
            //    and finally recursively on their arguments, leftmost argument first.
            public virtual int CompareTo(BaseTerm t) // for terms of identical subtype
            {
                BaseTerm t0 = this.ChainEnd();
                BaseTerm t1 = t.ChainEnd();
                int result = t0.Rank.CompareTo(t1.Rank); // BaseTerm types are ranked according to TermType enum order

                return (result == 0) ? t0.CompareValue(t1) : result;
            }

            public void CopyValuesFrom(BaseTerm t)
            {
                functor = t.functor;
                args = t.args;
                termType = t.termType;
                assocType = t.assocType;
                precedence = t.precedence;
                // TODO: overwrite the symbol?

                CopyDecoratedTermState(this, t);
            }

            private void CopyDecoratedTermState(BaseTerm to, BaseTerm from)
            {
                to.CommentBody = from.CommentBody;
                to.CommentHeader = from.CommentHeader;
                to.TestGroup = from.TestGroup;
            }

            static BaseTerm()
            {
                EMPTYLIST = new ListTerm(null);
                NULLCURL = new DcgTerm((Symbol)null);
                DBNULL = new AtomTerm(null, "db_null");
                VAR = new Variable(null);
                verNoMax = 0;
                varNoMax = 0;
                NUMVAR = "'$VAR'";
                CUT = new Cut(null, 0);
                FAIL = new AtomTerm(null, "fail");
            }


            protected BaseTerm(Symbol symbol)
            {
                this.Symbol = symbol?.Clone();
                assocType = AssocType.None;
                precedence = 0;
            }


            public bool OneOfArgsIsVar(params int[] args)
            {
                foreach (int i in args)
                    if (Arg(i).IsVar)
                    {
                        IO.ErrorRuntime($"Argument {i} of {FunctorToString}/{arity} is not sufficiently instantiated", null, null);

                        return true;
                    }

                return false;
            }


            public TermNode ToGoalList() // called during consult
            {
                return ToGoalList(0, 0);
            }


            public TermNode ToGoalList(int stackSize, int level) // called during execution (when there is a stack)
            {
                TermNode result = null;
                BaseTerm t0, t1;

                if (this is Cut)
                {
                    if (stackSize == 0)
                        return new TermNode(this, null, level);
                    else
                        return new TermNode(new Cut(this.Symbol, stackSize), null, level);
                }

                switch (this.Functor as string)
                {
                    case PrologParser.IMPLIES:
                        t0 = Arg(0);
                        if (!t0.IsCallable)
                            IO.ErrorConsult( "Illegal predicate head: {0}", t0);
                        t1 = Arg(1);
                        result = new TermNode(t0, t1.ToGoalList(stackSize, level));
                        break;
                    case PrologParser.DCGIMPL:
                        t0 = Arg(0);
                        if (!t0.IsCallable) IO.ErrorConsult( "Illegal DCG head: {0}", t0);
                        t1 = Arg(1);
                        result = new TermNode(t0, t1.ToGoalList(stackSize, level));
                        break;
                    case PrologParser.COMMA:
                        t0 = Arg(0);
                        t1 = Arg(1);
                        result = t0.ToGoalList(stackSize, level);
                        result.Append(t1.ToGoalList(stackSize, level));
                        break;
                    case PrologParser.DOT:
                        t0 = Arg(0);
                        t1 = Arg(1);
                        result = (new CompoundTerm(this.Symbol, "consult", new ListTerm(this.Symbol, t0, t1))).ToGoalList(stackSize, level);
                        break;
                    case PrologParser.CURL:
                        t0 = Arg(0);
                        result = t0.ToGoalList(stackSize, level);
                        break;
                    default:
                        if (this.IsVar)
                            result = new TermNode(new CompoundTerm(this.Symbol, "meta$call", this), null, level);
                        else if (this.IsCallable)
                            result = new TermNode(this, null, level);
                        else
                            IO.ErrorConsult( "Illegal term {0} in goal list", this);
                        break;
                }

                return result;
            }


            // DCG stuff

            public TermNode ToDCG(ref BaseTerm lhs) // called from parser
            {
                TermNode body = new TermNode();
                BaseTerm result = null;

                BaseTerm inVar = new Variable(null);
                BaseTerm inVarSave = inVar;
                BaseTerm outVar = inVar;
                lhs = new DcgTerm(this.Symbol, lhs, ref outVar); // outVar becomes new term
                BaseTerm remainder;

                List<BaseTerm> alternatives = AlternativesToArrayList();

                for (int i = 0; i < alternatives.Count; i++)
                {
                    BaseTerm alt = alternatives[i];
                    bool embedded = (alternatives.Count > 1);
                    List<BaseTerm> terms = alt.ToTermList();

                    body.Clear();
                    remainder = inVarSave;

                    for (int ii = 0; ii < terms.Count; ii++)
                        DCGGoal(terms[ii], ref body, ref remainder, ref embedded);

                    // create a term-tree from the array
                    if (i == 0)
                        result = body.TermSeq();
                    else
                        result = new OperatorTerm(this.Symbol, SemiOpDescr, result, body.TermSeq());

                    ((Variable)remainder).Bind(outVar);
                }

                return (result == null) ? null : result.ToGoalList(); // empty body treated similar to null
            }


            public List<BaseTerm> AlternativesToArrayList()
            {
                BaseTerm t = this;
                List<BaseTerm> a = new List<BaseTerm>();

                while (t.HasFunctor(PrologParser.SEMI) && t.Arity == 2)
                {
                    a.Add(t.Arg(0));
                    t = t.Arg(1); // xfy
                }

                a.Add(t);

                return a;
            }


            public List<BaseTerm> ToTermList()
            {
                BaseTerm t = this;
                List<BaseTerm> a = new List<BaseTerm>();

                while (t.HasFunctor(PrologParser.COMMA) && t.Arity == 2)
                {
                    a.AddRange(t.Arg(0).ToTermList());
                    t = t.Arg(1); // xfy
                }

                a.Add(t);

                return a;
            }

            public List<BaseTerm> GetArgumentsRecursive(BaseTerm term = null)
            {
                BaseTerm t = term ?? this;
                List<BaseTerm> a = new List<BaseTerm>();

                a.Add(t);

                if (t.Args == null)
                {
                    return a;
                }

                foreach (BaseTerm arg in t.Args)
                {
                    a.AddRange(arg.GetArgumentsRecursive(arg));
                }

                return a;
            }

            private static void DCGGoal(BaseTerm t, ref TermNode body, ref BaseTerm remainder, ref bool embedded)
            {
                BaseTerm temp;

                if (t.IsString || t is Cut)
                    body.Append(t);
                else if (t.HasFunctor(PrologParser.CURL))
                {
                    while (t.Arity == 2)
                    {
                        body.Append(t.Arg(0));
                        t = t.Arg(1);
                        embedded = true;
                    }
                }
                else if (t.IsProperList)
                {
                    temp = new Variable(null);

                    t = (t.IsEmptyList) ? temp : ((ListTerm)t).Append(temp);

                    if (embedded)
                    {
                        body.Append(new CompoundTerm(t.Symbol, PrologParser.EQ, remainder, t));
                        embedded = false;
                    }
                    else
                        ((Variable)remainder).Bind(t);
                    // in this case, nothing is appended to body, which may be left empty (e.g. t-->[x])

                    remainder = temp;
                }
                else if (t.IsAtom || t.IsCompound)
                {
                    t = new DcgTerm(t.Symbol, t, ref remainder);
                    body.Append(t);
                }
                else if (t.IsNamedVar)
                    IO.ErrorRuntime($"Variable not allowed in DCG-clause: {((NamedVariable) t).Name}", null, t);
                else if (t.IsUnboundTerm)
                    IO.ErrorRuntime( "Unbound variable not allowed in DCG-clause", null, t);
                else
                    IO.ErrorRuntime( "Illegal term in DCG-clause: {0}", null, t);
            }


            // UNIFICATION
            // The stack is used to store the variables and choice points that are bound
            // by the unification. This is required for backtracking. Unify does not do
            // any unbinding in case of failure. This will be done during backtracking.
            // refUnifyCount: can be used for calculating the 'cost' of a predicate Call
            public virtual bool Unify(BaseTerm t, VarStack varStack)
            {
                NextUnifyCount();

                if (t.IsUnified) return this.Unify(t.ChainEnd(), varStack);

                if (t is Variable varT) // t not unified
                {
                    varT.Bind(this);
                    varStack.Push(varT);

                    return true;
                }

                if (t is ListPatternTerm)
                    return t.Unify(this, varStack);

                if (termType != t.termType) return false; // gives a slight improvement

                if (functor.Equals(t.functor) && arity == t.arity)
                {
                    for (int i = 0; i < arity; i++)
                        if (!args[i].Unify(t.args[i], varStack)) return false;

                    return true;
                }

                return false;
            }


            public bool IsUnifiableWith(BaseTerm t, VarStack varStack) // as Unify, but does not actually bind
            {
                int marker = varStack.Count;
                bool result = Unify(t, varStack);
                UnbindToMarker(varStack, marker);

                return result;
            }


            public static void UnbindToMarker(VarStack varStack, int marker)
            {
                for (int i = varStack.Count - marker; i > 0; i--) // unbind all vars that got bound by Unify
                {
                    Variable v = varStack.Pop() as Variable;

                    if (v != null) v.Unbind();
                }
            }


            public static string VarList(VarStack varStack) // debugging only
            {
                StringBuilder result = new StringBuilder();

                foreach (object v in varStack.ToArray())
                    if (v != null && v is Variable)
                        result.AppendLine($">> {((Variable) v).Name} = {(Variable) v}");

                return result.ToString();
            }

            /* TERM COPYING

               A term is copied recursively, by first copying its arguments and then creating
               a new term consisting of the term functor and its copied arguments. The term’s
               type is taken into account. A variable is copied by creating a new instance.

               In this latter process, there is a complication. If a term contains more
               instances of the same variable, then one has to make sure that all copies refer
               to one and the same new instance. Therefore, in copying a var, it is necessary
               to check whether that same var has been copied before and to let the copy point
               to that instance rather than creating a new one.

               A solution to this problem is to set a boolean indicator in a term’s variable
               to denote whether it has already been copied. This, however, is not sufficient,
               as one also needs to know what the copied instance is. Therefore, the solution
               that has been adopted, is to equip the variable with an extra field newVar,
               that contains the newly created copy.

               In order to make this also work for copying a nextClause (which is implemented as
               a list of terms which are handled consecutively) one must make sure that this
               principle of one copy for all identical variables holds for all terms making up
               the nextClause. Therefor, another int attribute verNo (version number) has been
               introduced. When (bool) newVersion is switched off, the already created
               newVar is used. Copy(false) is only used when the nextClause head is copied.
               The variables in the nextClause body will then implicitly point to the same
               newVars that where created by copying the nextClause head. These vars will then
               subsequently be unified with the current goal (in ExecuteGoalList () at the
               call to cleanClauseHead.Unify ().
            */

            // Create an identical new term, with verNo+varNo value that does not occur
            // in any other term created sofar. The uLinks are resolved.
            // A new term is constructed by creating a new instance for each unbound term.
            // This new instance gets a version number that is one higher than its original.

            public BaseTerm Copy()
            {
                return Copy(true);
            }

            public BaseTerm Copy(bool newVersion)
            {
                if (newVersion) verNoMax++;

                return CopyEx(verNoMax, true);
            }

            public BaseTerm Copy(bool newVersion, bool mustBeNamed) // called by copy_term
            {
                if (newVersion) verNoMax++;

                return CopyEx(verNoMax, mustBeNamed);
            }

            private BaseTerm CopyEx(int newVerNo, bool mustBeNamed)
            {
                if (IsUnified) return ChainEnd().CopyEx(newVerNo, mustBeNamed);

                // A neater solution would be to use overrides for each term subtype.
                if (this is Variable)
                {
                    Variable v = (Variable)this;

                    if (newVerNo == v.verNo) return v.newVar;

                    v.verNo = newVerNo;

                    v.newVar = (mustBeNamed && this is NamedVariable)
                      ? new NamedVariable(v.Symbol, ((NamedVariable)v).Name)
                      : new Variable(v.Symbol);

                    CopyDecoratedTermState(v.newVar, this);

                    return v.newVar;
                }
                else if (this is CatchOpenTerm)
                {
                    CatchOpenTerm c = (CatchOpenTerm)this;

                    var res = new CatchOpenTerm(c.Symbol, c.Id, c.ExceptionClass, c.MsgVar.CopyEx(newVerNo, mustBeNamed),
                      c.SeqNo, c.SaveStackSize);

                    CopyDecoratedTermState(res, this);

                    return res;
                }
                else
                {
                    if (arity == 0) return this;

                    BaseTerm t = null;
                    BaseTerm[] a = new BaseTerm[arity];

                    for (int i = 0; i < arity; i++)
                        if (args[i] != null) // may be null for a GapTerm
                            a[i] = args[i].CopyEx(newVerNo, mustBeNamed); // recursively refresh arguments

                    if (this is ListPatternTerm)
                        t = new ListPatternTerm(this.Symbol, a);
                    else if (this is AltListTerm)
                    {
                        AltListTerm alt = (AltListTerm)this;
                        t = new AltListTerm(alt.Symbol, alt.LeftBracket, alt.RightBracket, a[0], a[1]);
                    }
                    else if (this is ListTerm)
                    {
                        t = ((ListTerm)this).CharCodeString == null ? new ListTerm(this.Symbol, a[0], a[1]) : new ListTerm(this.Symbol, ((ListTerm)this).CharCodeString);
                    }
                    else if (this is OperatorTerm)
                        t = new OperatorTerm(this.Symbol, ((OperatorTerm)this).od, a);
                    else if (this is DcgTerm)
                        t = new DcgTerm(this.Symbol, functor, a);
                    else if (this is WrapperTerm)
                        t = new WrapperTerm((WrapperTerm)this, a);
                    else if (this is IntRangeTerm)
                        t = new IntRangeTerm((IntRangeTerm)this);
                    else if (this is ListPatternElem)
                        t = new ListPatternElem(this.Symbol, a, ((ListPatternElem)this).downRepFactor, ((ListPatternElem)this).IsNegSearch);
                    else if (this is CompoundTerm)
                        t = new CompoundTerm(this.Symbol, functor, a);
                    else
                        IO.ErrorRuntime($"CopyEx(): type '{this.GetType()}' not handled explicitly", null, this);

                    CopyDecoratedTermState(t, this);

                    return t;
                }
            }


            public void NumberVars(ref int k, VarStack s)
            {
                if (IsVar)
                    this.Unify(new CompoundTerm(this.Symbol, NUMVAR, new DecimalTerm(this.Symbol, k++)), s);
                else
                {
                    if (IsUnified)
                        ChainEnd().NumberVars(ref k, s);
                    else if (arity != 0) // nonvar & not isUnified
                        for (int i = 0; i < arity; i++) args[i].NumberVars(ref k, s);
                }
            }


            public static BaseTerm MakeMatchTerm(Match m, bool asAtom)
            {
                BaseTerm[] args = new BaseTerm[4];

                if (asAtom)
                    args[0] = new AtomTerm(null, m.Value.ToAtom());
                else
                    args[0] = new StringTerm(null, m.Value);

                args[1] = new DecimalTerm(null, m.Index);
                args[2] = new DecimalTerm(null, m.Length);
                args[3] = new AtomTerm(null, "m.Groups");

                return new CompoundTerm(null, "match", args);
            }


            public virtual void TreePrint(int level, PrologEngine e)
            {
                e.Write(Spaces(2 * level));

                if (this is Variable)
                    e.WriteLine(this.ToString());
                else
                {
                    e.WriteLine(FunctorToString);

                    if (arity > 0)
                        foreach (BaseTerm a in args)
                            a.ChainEnd().TreePrint(level + 1, e);
                }
            }

        }
    }
}
