//#define arg1index // if (un)defined, do the same in PredStorage.cs !!!
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

namespace Prolog
{
    /*
      --------
      TermNode
      --------

      A TermNode serves two purposes:
      - to store the goals of a command
      - to store predicates (clauses)

      A goal is constructed as a simple chained term of TermNodes. This makes it easy
      (and also more efficient in terms of GC) to revert to a previous state upon backtracking
      (in contrast to i.e. an ArrayList).

      A predicate consists of one or more clauses. A clause consist of a head and optionally a
      body. A head is a term, the body is a sequence of terms. A predicate is stored as a chain
      of TermNodes, where each TermNode represents a clause. These TermNodes are linked via the
      nextClause field. In each nextClause/TermNode the clause head is stored in term, and the
      clause body (which may be null) in NextNode.

    */

    public partial class PrologEngine
    {
        public class TermNode
        {
            public TermNode()
            {
            }

            public TermNode(BaseTerm term, PredicateDescr predDescr)
            {
                PredDescr = predDescr;
                Head = term;
            }

            public TermNode(BaseTerm term, PredicateDescr predDescr, int level)
            {
                Head = term;
                PredDescr = predDescr;
                Level = level;
            }

            public TermNode(string tag) // builtin predicates
            {
                try
                {
                    BuiltinId = (BI)Enum.Parse(typeof(BI), tag, false);
                }
                catch
                {
                    IO.ThrowConsultException(string.Format("Bootstrap.cs: unknown BI enum value '{0}'", tag), Term?.Symbol);
                }
            }

            public TermNode(BaseTerm term, TermNode nextNode)
            {
                Head = term;
                NextNode = nextNode;
            }

            public BaseTerm Head { get; set; }

            public ClauseNode NextClause { get; set; }

            public int Level { get; set; }

            public BaseTerm Term => Head;

            public TermNode NextNode { get; set; }

            public TermNode NextGoal
            {
                get => NextNode;
                set => NextNode = value;
            }

            public BI BuiltinId { get; } = BI.none;

            public PredicateDescr PredDescr { get; set; }

            // put the predicate definition (if found) into the TermNode if it is not already there
            public bool FindPredicateDefinition(PredicateTable predicateTable)
            {
                if (PredDescr == null)
                {
                    if ((PredDescr = predicateTable[Head.Key]) == null)
                    {
                        return false;
                    }
                }

#if arg1index // first-argument indexing enabled
        BaseTerm arg;

        // caching would disturb the search process (since caching does not
        // cause the arg0Index to be rebuild, since this might be to costly)
        if (predDescr.IsFirstArgIndexed && !predDescr.HasCachedValues)
        {
          if ((arg = term.Arg (0)).IsVar)
            nextClause = predDescr.FirstArgVarClause ();
          else // not a variable
          {
            nextClause = predDescr.FirstArgNonvarClause (arg.FunctorToString);

            // check whether there is an indexed var clause
            if (nextClause == null)
              nextClause = predDescr.FirstArgVarClause ();

            // if the above failed, the entire predicate fails (no unification possible)
            if (nextClause == null)
              nextClause = ClauseNode.FAIL;
          }

          if (nextClause == null)
            nextClause = predDescr.ClauseList;
        }
        else // not indexed
#endif
                NextClause = PredDescr.ClauseList;

                return true;
            }

            public void Append(BaseTerm t)
            {
                if (Head == null) // empty term
                {
                    Head = t;

                    return;
                }

                TermNode tail = this;
                TermNode next = NextNode;

                while (next != null)
                {
                    tail = next;
                    next = next.NextNode;
                }

                tail.NextNode = new TermNode(t, (PredicateDescr)null);
            }

            public TermNode Append(TermNode t)
            {
                TermNode tail = this;
                TermNode next = NextNode;

                while (next != null) // get the last TermNode
                {
                    tail = next;
                    next = next.NextNode;
                }

                tail.NextNode = t;

                return this;
            }

            public void Clear()
            {
                Head = null;
                NextNode = null;
                NextClause = null;
                Level = 0;
            }

            public BaseTerm TermSeq(VarStack varStack)
            {
                return NextNode == null
                    ? Term // last term of TermNode
                    : new OperatorTerm(Term?.Symbol, varStack.CommaOpDescr, Term, NextNode.TermSeq(varStack));
            }

            public override string ToString()
            {
                StringBuilder sb = new StringBuilder();
                bool first = true;
                TermNode tn = this;
                BaseTerm t;

                while (tn != null)
                {
                    if ((t = tn.Head) is TryOpenTerm)
                    {
                        if (!first)
                        {
                            sb.Append(", ");
                        }

                        sb.AppendFormat(" TRY( ");
                        first = true;
                    }
                    else if (t is CatchOpenTerm)
                    {
                        CatchOpenTerm co = (CatchOpenTerm)t;
                        string msgVar = co.MsgVar is AnonymousVariable ? null : co.MsgVar.Name;
                        string comma = co.ExceptionClass == null || msgVar == null ? null : ", ";

                        sb.AppendFormat(" )CATCH {0}{1}{2}(", co.ExceptionClass, comma, msgVar);
                        first = true;
                    }
                    else if (t == TC_CLOSE)
                    {
                        sb.AppendFormat(")");
                        first = false;
                    }
                    else
                    {
                        if (first)
                        {
                            first = false;
                        }
                        else
                        {
                            sb.Append(", ");
                        }

                        sb.AppendFormat("{0}", t);
                    }

                    tn = tn.NextNode;
                }

                return sb.ToString();
            }

            public TermNode GetLastNode()
            {
                TermNode tl = this;

                while (tl.NextNode != null)
                {
                    tl = tl.NextNode;
                }

                return tl;
            }
        }

        // the terms (connected by NextNode) of a single clause
        public class ClauseNode : TermNode
        {
            public ClauseNode(BaseTerm t, TermNode body)
                : base(t, body)
            {
            }

            public override string ToString()
            {
                string NL = string.Empty;

                StringBuilder sb = new StringBuilder(NL + Head);

                bool first = true;
                TermNode tl = NextNode;

                if (tl == null)
                {
                    return sb.ToString() + '.' + NL;
                }

                while (true)
                {
                    if (first)
                    {
                        sb.Append(" :-");
                    }

                    sb.Append(NL + "  " + tl.Term);

                    if ((tl = tl.NextNode) == null)
                    {
                        return sb.ToString() + '.' + NL;
                    }

                    if (!first)
                    {
                        sb.AppendFormat(",");
                    }

                    first = false;
                }
            }
        }
    }
}