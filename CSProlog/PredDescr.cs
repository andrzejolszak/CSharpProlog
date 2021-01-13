#define enableSpying
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

using System.Collections.Generic;

namespace Prolog
{
    public partial class PrologEngine
    {
        public partial class PredicateDescr
        {
            private const short ARG0COUNT_MIN = 8;

            /* First-argument indexing

               A dictionary is maintained of clauses that have a nonvar first argument.
               Each predicate has its own dictionary. The functors of these first arguments are
               distinct (that is, in the dictionary). For predicate clauses that have identical
               first argument functors, only the first of these clauses is included in the
               dictionary. The clauses are traversed in the order in which they were read in.

               If a clause with a *variable* first argument is encountered, it is added
               to the dictionary as well, but no more entries wil be added after that.
               It will serve as a catch-all for instantiated arguments that do not match with
               any of the other dictionary entries.

               First-argument indexing is applied when a goal is about to be resolved. If the
               first argument of the goal is a bound term, it will be looked up in the
               dictionary. If it is found, the first clause of the defining predicate (cf.
               ExecuteGoalList()) is set to the dictionary entry. If it is not found, and the
               dictionary has a nonvar-entry, then the first clause is set to that
               entry. The (nonvar) goal fails if it is not found in the dictionary.

               First-argument indexing is not used upon backtracking. Choicepoints are simply
               the next clauses after the current one.

               Notice that this implementation of first-argument indexing is of limited use if
               there are many similar first arguments that are scattered throughout the
               predicate clauses, especially when backtracking takes place.
               A more sopisticated scheme could be envisaged where all similar fa-clauses are
               chained together. Now, for optimal use the user has to take care that all similar
               fa-clauses are grouped together (which seems advisable anyway).
            */

            private const bool VARARG = true;
            private Dictionary<object, ClauseNode> arg0Index; // for first argument indexing

            public PredicateDescr(string module, string definitionFile, string functor, int arity,
                ClauseNode clauseList)
            {
                Module = module;
                DefinitionFile = definitionFile == null ? "predefined or asserted predicate" : definitionFile;
                IsPredefined = definitionFile == null;
                Functor = functor;
                Arity = arity;

                ClauseList = ClauseListEnd = clauseList;
            }

            public ClauseNode ClauseListEnd { get; set; }
            public TermNode TermListEnd { get; set; }

            public string Functor { get; set; }

            public int Arity { get; set; }

            public string Name => Functor + '/' + Arity;
            public ClauseNode ClauseList { get; private set; }
            public bool IsDiscontiguous { get; set; } = false;
            public int ProfileCount { get; set; }
            public bool IsPredefined { get; }

            public string Module { get; set; }

            public string DefinitionFile { get; }

            public void IncProfileCount()
            {
                ProfileCount++;
            }

            public void SetClauseListHead(ClauseNode c)
            {
                ClauseList = ClauseListEnd = c;

                while (ClauseListEnd.NextClause != null)
                {
                    ClauseListEnd = ClauseListEnd.NextClause;
                }

                DestroyFirstArgIndex();
            }

            public void AdjustClauseListEnd() // forward clauseListEnd to the last clause
            {
                if ((ClauseListEnd = ClauseList) != null)
                {
                    while (ClauseListEnd.NextClause != null)
                    {
                        ClauseListEnd = ClauseListEnd.NextClause;
                    }
                }
            }

            public void AdjustNodeListEnd() // forward clauseListEnd to the last clause
            {
                if ((TermListEnd = ClauseList) != null)
                {
                    while (TermListEnd.NextNode != null)
                    {
                        TermListEnd = TermListEnd.NextNode;
                    }
                }
            }

            public void AppendToClauseList(ClauseNode c) // NextClause and ClauseListEnd are != null
            {
                ClauseListEnd.NextClause = c;

                do
                {
                    ClauseListEnd = ClauseListEnd.NextClause;
                } while
                    (ClauseListEnd.NextClause != null);

                DestroyFirstArgIndex();
            }

            public bool CreateFirstArgIndex()
            {
                return CreateFirstArgIndex(false); // false: do not create if it already exists
            }

            public bool CreateFirstArgIndex(bool force) // Create the index if the predicate qualifies
            {
                if (arg0Index != null && !force)
                {
                    return false; // index already exists
                }

                // Check each nextClause whether with the addition of this nextClause the predicate
                // still qualifies for first argument indexing.
                // Indexing y/n must be (re)determined after a file consult or an assert.

                ClauseNode c = ClauseList;
                short arg0Count = 0;

                while (c != null)
                {
                    if (c.Head.Arity != 0) // no first arg
                    {
                        arg0Count++; // Indexing not worthwile if only a few number of clauses

                        if (c.Head.Arg(0).IsVar)
                        {
                            break;
                        }
                    }

                    c = c.NextClause;
                }

                if (arg0Count < ARG0COUNT_MIN)
                {
                    return false;
                }

                // second pass: build the index

                arg0Index = new Dictionary<object, ClauseNode>();
                c = ClauseList;

                while (c != null)
                {
                    string s;

                    BaseTerm t = c.Head.Arg(0);

                    if (t.IsVar)
                    {
                        arg0Index[VARARG] = c; // stop immediately after having included the first variable

                        break;
                    }

                    if (!arg0Index.ContainsKey(s = t.FunctorToString))
                    {
                        arg0Index[s] = c;
                    }

                    c = c.NextClause;
                }

                if (arg0Index.Count == 1) // e.g. c(a(1)), c(a(2)), c(a(3)), ...
                {
                    arg0Index = null;

                    return false;
                }

                return true;
            }

            public void DestroyFirstArgIndex()
            {
                arg0Index = null;
            }

            public override string ToString()
            {
                return $"pd[{Functor}/{Arity} clauselist {ClauseList}]";
            }
        }
    }
}