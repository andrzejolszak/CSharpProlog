//#define arg1index // if (un)defined, do the same in TermNodeList.cs !!!
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
#define arg1index

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using static Prolog.PrologEngine.BaseParser;

namespace Prolog
{
    public partial class PrologEngine
    {
        public enum UndefAction { None, Fail, Succeed, Warning, Error }

        public class PredicateTable
        {
            private PrologEngine engine;
            public Dictionary<string, PredicateDescr> Predicates;
            public List<BaseTerm> AllConsultedTerms { get; set; }
            private Dictionary<string, string> moduleName;
            private Dictionary<string, string> definedInCurrFile;
            private Dictionary<string, string> isDiscontiguous;
            private Dictionary<string, UndefAction> actionWhenUndefined; // currently not used
            public CrossRefTable CrossRefTable;
            private bool crossRefInvalid;
            private string prevIndex = null;
            private bool allDiscontiguous = false;
            // True if a predicate's clauses are not grouped together but are scattered
            // over a source file. Should normally not be used (used for code from others).
            private const string SLASH = "/";
            public HashSet<string> Predefined { get; }
            public Stack<string> consultFileStack;
            private Stack<PrologParser> consultParserStack;
            public string ConsultFileName => (consultFileStack.Count == 0) ? null : consultFileStack.Peek();

            public PredicateTable(PrologEngine engine)
            {
                this.engine = engine;
                Predicates = new Dictionary<string, PredicateDescr>();
                CrossRefTable = new CrossRefTable();
                crossRefInvalid = true;
                Predefined = new HashSet<string>();
                moduleName = new Dictionary<string, string>();
                definedInCurrFile = new Dictionary<string, string>();
                isDiscontiguous = new Dictionary<string, string>();
                actionWhenUndefined = new Dictionary<string, UndefAction>();
                consultFileStack = new Stack<string>();
                consultParserStack = new Stack<PrologParser>();
                AllConsultedTerms = new List<BaseTerm>();
            }


            public void Reset()
            {
                Predicates.Clear();
                Predefined.Clear();
                moduleName.Clear();
                definedInCurrFile.Clear();
                isDiscontiguous.Clear();
                actionWhenUndefined.Clear();
                prevIndex = null;
                consultFileStack.Clear();
                consultParserStack.Clear();
            }


            public PredicateDescr this[string key]
            {
                get
                {
                    PredicateDescr result;
                    Predicates.TryGetValue(key, out result);

                    return result;
                }
                set
                {
                    Predicates[key] = value;
                }
            }


            public bool IsPredefined(string key)
            {
                return Predefined.Contains(key);
            }


            public void InvalidateCrossRef()
            {
                crossRefInvalid = true;
            }


            public void SetActionWhenUndefined(string f, int a, UndefAction u)
            {
                actionWhenUndefined[BaseTerm.MakeKey(f, a)] = u;
            }


            public UndefAction ActionWhenUndefined(string f, int a)
            {
                UndefAction u;
                actionWhenUndefined.TryGetValue(BaseTerm.MakeKey(f, a), out u);

                return u;
            }


            public bool IsPredicate(string functor, int arity)
            {
                return this.Contains(BaseTerm.MakeKey(functor, arity));
            }

            private PredicateDescr SetClauseList(string f, int a, ClauseNode c)
            {
                string key = BaseTerm.MakeKey(f, a);
                PredicateDescr pd = this[key];

                if (pd == null)
                {
                    this[key] = pd =
                   new PredicateDescr(null, ConsultFileName, f, a, c);
                }
                else
                    pd.SetClauseListHead(c);

                pd.AdjustClauseListEnd();

                return pd;
            }


            public bool Contains(string key)
            {
                return (this[key] != null);
            }


            public override string ToString()
            {
                return Predicates.ToString();
            }

            public int Consult(string fileName)
            {
                return Consult(null, fileName);
            }

            public int Consult(Stream stream, string streamName = null)
            {
                // string as ISO-style charcode lists or as C# strings
                var fileName = streamName ?? Guid.NewGuid().ToString("N");
                consultFileStack.Push(fileName);
                consultParserStack.Push(Globals.CurrentParser);
                PrologParser parser = Globals.CurrentParser = new PrologParser(engine);
                allDiscontiguous = false;

                try
                {
                    prevIndex = null;
                    definedInCurrFile.Clear();
                    isDiscontiguous.Clear();
                    //Globals.ConsultModuleName = null;
                    parser.Prefix = "&program\r\n";
                    IO.Write("--- Consulting {0} ... ", fileName);
                    parser.LoadFromStream(stream, fileName);
                    IO.WriteLine("{0} lines read", parser.LineCount);
                    InvalidateCrossRef();

                    // Adjust node list ends and populate AllConsultedTerms
                    foreach (var pred in this.Predicates.Values)
                    {
                        pred.AdjustNodeListEnd();

                        if (pred.IsPredefined)
                        {
                            continue;
                        }

                        TermNode node = pred.ClauseList;
                        while (node != null)
                        {
                            if (node.Term != null)
                            {
                                AllConsultedTerms.AddRange(node.Term.GetArgumentsRecursive());
                            }

                            node = node.NextNode;
                        }
                    }

                    SetupCrossRefTable();
                }
                finally
                {
                    Globals.CurrentParser = consultParserStack.Pop(); ;
                    //Globals.ConsultModuleName = null; // Currently not used
                }

                return parser.LineCount;
            }

            public void AddPredefined(ClauseNode clause)
            {
                BaseTerm head = clause.Head;
                string key = head.Key;
                PredicateDescr pd = this[key];

                if (pd == null)
                {
                    Predefined.Add(key);
                    SetClauseList(head.FunctorToString, head.Arity, clause); // create a PredicateDescr
                }
                else if (prevIndex != null && key != prevIndex)
                    IO.ErrorRuntime($"Definition for predefined predicate '{head.Index}' must be contiguous", null, clause.Term);
                else
                    pd.AppendToClauseList(clause);

                prevIndex = key;
            }


            public void SetDiscontiguous(BaseTerm t)
            {
                if (t == null || t.FunctorToString != SLASH || !t.Arg(0).IsAtom || !t.Arg(1).IsInteger)
                    IO.ErrorConsult( "Illegal or missing argument '{0}' for discontiguous/1", t);

                // The predicate descriptor does not yet exist (and may even not come at all!)
                string key = BaseTerm.MakeKey(t.Arg(0).FunctorToString, t.Arg(1).To<short>());

                //IO.WriteLine ("--- Setting discontiguous for {0} in definitionFile {1}", key, Globals.ConsultFileName);
                isDiscontiguous[key] = "true";
            }


            public void SetDiscontiguous(bool mode)
            {
                allDiscontiguous = mode;
            }


            public void HandleSimpleDirective(PrologParser p, Symbol symbol, string directive, string argument, int arity)
            {
                //IO.WriteLine ("HandleSimpleDirective ({0}, {1}, {2})", directive, argument, arity);

                switch (directive)
                {
                    case "fail_if_undefined":
                        SetActionWhenUndefined(argument, arity, UndefAction.Fail);
                        break;
                    case "stacktrace":
                        if (argument == "on")
                            engine.userSetShowStackTrace = true;
                        else if (argument == "off")
                            engine.userSetShowStackTrace = false;
                        else
                            IO.ErrorConsult($":- stacktrace: illegal argument '{argument}'; use 'on' or 'off' instead", symbol);
                        break;
                    case "initialization":
                        IO.Warning("':- initialization' directive not implemented -- ignored");
                        break;
                    default:
                        IO.ErrorConsult($"\r\nUnknown directive ':- {directive}'", symbol);
                        break;
                }
            }


            public void SetModuleName(string n, Symbol symbol)
            {
                moduleName.TryGetValue(n, out string o);
                string currFile = ConsultFileName;

                if (o == null)
                {
                    moduleName[n] = currFile;
                    // ConsultModuleName = null;
                }
                else if ((string)o != currFile)
                    IO.ErrorConsult(string.Format($"Module name {n} already declared in file {o}", n), symbol);

                // ACTUAL FUNCTIONALITY TO BE IMPLEMENTED, using a 
                // ConsultModuleName stack, analoguous to ConsultFileName
            }

            public void AddClause(ClauseNode clause)
            {
                BaseTerm head = clause.Head;

                string key = head.Key;
                string index = head.Index;

                if (Predefined.Contains(key))
                    IO.ErrorConsult($"Modification of predefined predicate {index} not allowed", clause?.Term?.Symbol);

                if (prevIndex == key) // previous clause was for the same predicate
                {
                    PredicateDescr pd = this[key];
                    pd.AppendToClauseList(clause);
                }
                else // first predicate or different predicate
                {
                    PredicateDescr pd = this[key];

                    if (!definedInCurrFile.ContainsKey(key)) //  very first clause of this predicate in this file -- reset at start of consult
                    {
                        if (pd != null && pd.DefinitionFile != ConsultFileName)
                            IO.ErrorConsult($"Predicate '{index}' is already defined in {pd.DefinitionFile}", clause.Term);

                        definedInCurrFile[key] = "true";
                        pd = SetClauseList(head.FunctorToString, head.Arity, clause); // implicitly erases all previous definitions
                        pd.IsDiscontiguous = isDiscontiguous.ContainsKey(key) || allDiscontiguous;
                        prevIndex = key;
                    }
                    else // not the first clause. First may be from another definitionFile (which is an error).
                    {    // If from same, IsDiscontiguous must hold, unless DiscontiguousAllowed = "1" in .config
                        bool b = false;

                        if (pd.IsDiscontiguous || (b = true))
                        {
                            if (b)
                                IO.Warning($"Predicate '{index}' is defined discontiguously but is not declared as such", clause.Term);

                            if (pd.DefinitionFile == ConsultFileName)
                                pd.AppendToClauseList(clause);
                            else // OK
                                IO.ErrorConsult($"Discontiguous predicate {index} must be in one file (also found in {pd.DefinitionFile})", clause.Term);
                        }
                        else if (pd.DefinitionFile == ConsultFileName) // Warning or Error?
                            IO.Warning($"Predicate '{index}' occurs discontiguously but is not declared as such", clause.Term);
                        else
                            IO.ErrorConsult($"Predicate '{index}' is already defined in {pd.DefinitionFile}", clause.Term);
                    }
                }
            }


            public void Assert(BaseTerm assertion, bool asserta)
            {
                BaseTerm head;
                TermNode body = null;
                PredicateDescr pd;
                BaseTerm assertionCopy = assertion.Copy(true);

                if (assertionCopy.HasFunctor(PrologParser.IMPLIES))
                {
                    head = assertionCopy.Arg(0);
                    body = assertionCopy.Arg(1).ToGoalList();
                }
                else
                    head = assertionCopy;

                if (!head.IsCallable) IO.ErrorRuntime($"Illegal predicate head '{head}'", null, assertion);

                string key = head.Key;

                if ((Predefined.Contains(key)) || (head.Precedence >= 1000))
                    IO.ErrorRuntime(
                        $"assert/1 cannot be applied to predefined predicate or operator '{assertionCopy.Index}'", null, assertion);

                Predicates.TryGetValue(key, out pd);
                ClauseNode newC = new ClauseNode(head, body);

                if (pd == null) // first head
                {
                    SetClauseList(head.FunctorToString, head.Arity, newC);
                    ResolveIndices();
                }
                else if (asserta) // at beginning
                {
                    newC.NextClause = pd.ClauseList; // pd.ClauseList may be null
                    SetClauseList(head.FunctorToString, head.Arity, newC);
#if arg1index
          pd.CreateFirstArgIndex (); // re-create
#endif
                }
                else // at end
                {
                    pd.AppendToClauseList(newC);
#if arg1index
          pd.CreateFirstArgIndex (); // re-create
#endif
                }

                InvalidateCrossRef();
            }

            public bool Retract(BaseTerm t, VarStack varStack, BaseTerm where)
            {
                string key = t.Key;

                if (Predefined.Contains(key))
                    IO.ErrorRuntime($"retract of predefined predicate {key} not allowed", varStack, t);

                PredicateDescr pd = this[key];

                if (pd == null) return false;

                InvalidateCrossRef();
                ClauseNode c = pd.ClauseList;
                ClauseNode prevc = null;
                BaseTerm cleanTerm;
                int top;

                while (c != null)
                {
                    cleanTerm = c.Head.Copy();

                    top = varStack.Count;

                    if (cleanTerm.Unify(t, varStack)) // match found -- remove this term from the chain
                    {
                        if (prevc == null) // remove first clause
                        {
                            if (c.NextClause == null) // we are about to remove the last remaining clause for this predicate
                            {
                                Predicates.Remove(key);        // ... so remove its PredicateDescr as well
#if arg1index
                pd.CreateFirstArgIndex (); // re-create
#endif
                                ResolveIndices();
                            }
                            else
                                pd.SetClauseListHead(c.NextClause);
                        }
                        else // not the first
                        {
                            prevc.NextClause = c.NextClause;
                            prevc = c;
                            pd.AdjustClauseListEnd();
#if arg1index
              pd.CreateFirstArgIndex (); // re-create
#endif
                        }

                        return true; // possible bindings must stay intact (e.g. if p(a) then retract(p(X)) yields X=a)
                    }

                    Variable s;
                    for (int i = varStack.Count - top; i > 0; i--) // unbind all vars that got bound by the above Unification
                    {
                        s = (Variable)varStack.Pop();
                        s.Unbind();
                    }

                    prevc = c;
                    c = c.NextClause;
                }

                ResolveIndices();

                return false;
            }


            public bool RetractAll(BaseTerm t, VarStack varStack)
            {
                // remark: first-argument indexing is not affected by deleting clauses

                string key = t.Key;

                if (Predefined.Contains(key))
                    IO.ErrorRuntime($"retract of predefined predicate {key} not allowed", varStack, t);

                PredicateDescr pd = this[key];

                if (pd == null) return true;

                ClauseNode c = pd.ClauseList;
                ClauseNode prevc = null;
                bool match = false;

                while (c != null)
                {
                    BaseTerm cleanTerm = c.Term.Copy();

                    if (cleanTerm.IsUnifiableWith(t, varStack)) // match found -- remove this head from the chain
                    {
                        match = true; // to indicate that at least one head was found

                        if (prevc == null) // remove first clause
                        {
                            if (c.NextClause == null) // we are about to remove the last remaining clause for this predicate
                            {
                                Predicates.Remove(key); // ... so remove its PredicateDescr as well

                                break;
                            }
                            else
                                pd.SetClauseListHead(c.NextClause);
                        }
                        else // not the first
                        {
                            prevc.NextClause = c.NextClause;
                            prevc = c;
                        }
                    }
                    else
                        prevc = c;

                    c = c.NextClause;
                }

                if (match)
                {
#if arg1index
          pd.DestroyFirstArgIndex (); // rebuilt by ResolveIndices()
#endif
                    pd.AdjustClauseListEnd();
                    ResolveIndices();
                }

                return true;
            }


            public bool Abolish(string functor, int arity)
            {
                string key = BaseTerm.MakeKey(functor, arity);

                if (Predefined.Contains(key))
                    IO.ErrorRuntime($"abolish of predefined predicate '{functor}/{arity}' not allowed", null, null);

                PredicateDescr pd = this[key];

                if (pd == null) return false;

                Predicates.Remove(key);

#if arg1index
        pd.DestroyFirstArgIndex (); // rebuilt by ResolveIndices()
#endif
                ResolveIndices();

                return true;
            }
            
            private bool ListClause(PredicateDescr pd, string functor, int arity, int seqno)
            {
                ClauseNode clause = null;
                string details;

                if ((clause = pd.ClauseList) == null) return false;

                details = "source: " + pd.DefinitionFile;

                //        if (pd.IsFirstArgIndexed) details += "; arg1-indexed (jump points marked with '.')";

                IO.WriteLine("{0}/{1}: ({2}) {3}", functor, arity, details,
                  ((seqno == 1) ? "" : (seqno.ToString().Packed())));

                while (clause != null)
                {
                    TermNode next;

                    //          // prefix a clause that is pointed to by first-argument indexing with '.'
                    //          IO.Write (" {0}{1}", (pd.IsFirstArgMarked (clause))?".":" ", nextClause.Term);
                    IO.Write("  {0}", clause.Term);

                    if ((next = clause.NextNode) != null)
                    {
                        BI builtinId = next.BuiltinId;
                        IO.Write(" :-{0}", (builtinId == BI.none)
                          ? next.ToString()
                          : Environment.NewLine + builtinId);
                    }

                    IO.WriteLine(".");
                    clause = clause.NextClause;
                }

                return true;
            }


            public bool ListAll(string functor, int arity, bool showPredefined, bool showUserDefined)
            {
                bool result = false; // no predicate <functor>/<arity> assumed
                PredicateDescr pd;

                // for sorting the predicates alphabetically:
                SortedDictionary<string, PredicateDescr> sl = new SortedDictionary<string, PredicateDescr>();

                foreach (KeyValuePair<string, PredicateDescr> kv in Predicates)
                {
                    pd = kv.Value;

                    if (functor == null || functor == pd.Functor)
                    {
                        bool isPredefined = IsPredefined(kv.Key);

                        if ((showPredefined && showUserDefined ||
                           showPredefined && isPredefined ||
                           showUserDefined && !isPredefined) &&
                           (arity == -1 || arity == pd.Arity))
                            sl.Add(pd.Functor + pd.Arity, pd);
                    }
                }

                int seqNo = 0;

                foreach (KeyValuePair<string, PredicateDescr> kv in sl)
                    result = ListClause(pd = kv.Value, pd.Functor, pd.Arity, ++seqNo) || result;

                return result;
            }

            private void SetupCrossRefTable() //TODO (later...): deal with arguments of not/1 and call/1
            {
                if (!crossRefInvalid) return;

                CrossRefTable.Reset();
                PredicateDescr pd;

                foreach (KeyValuePair<string, PredicateDescr> kv in Predicates)
                {
                    pd = kv.Value;
                    bool isPredefined = IsPredefined(kv.Key);
                    ClauseNode clause = pd.ClauseList;

                    if (!isPredefined)
                    {
                        CrossRefTable.AddPredicate(pd);
                    }

                    // iterate over NextClause and NextClause.NextNode
                    while (clause != null)
                    {
                        TermNode node = clause.NextNode;

                        while (node != null)
                        {
                            if (!isPredefined)
                            {
                                node.FindPredicateDefinition(this);

                                if (node.PredDescr != null)
                                {
                                    // Direct call
                                    PredicateDescr npd;
                                    CrossRefTable[pd, npd = node.PredDescr] = false;

                                    if (npd.Name == "not/1" || npd.Name == "call/1") // add args to cref
                                    {
                                        TermNode arg = node.NextNode;
                                        IO.WriteLine("{0} arg is {1}", npd.Name, arg);
                                    }
                                }
                            }

                            node = node.NextNode;
                        }

                        clause = clause.NextClause;
                    }
                }

                // Indirect calls
                CrossRefTable.CalculateClosure();
                crossRefInvalid = false;
            }

            public void ResolveIndices() // functor/arity-key resolution
            {
                PredicateDescr pd;

                foreach (KeyValuePair<string, PredicateDescr> kv in Predicates) // traverse all program predicates
                {
                    ResolveIndex(pd = kv.Value);
                    pd.CreateFirstArgIndex(); // check whether first-argument indexing is applicable, and build the index if so
                }
            }


            private void ResolveIndex(PredicateDescr pd)
            {
                ClauseNode clause = pd.ClauseList;

                while (clause != null) // iterate over all clauses of this predicate. NextClause.BaseTerm contains predicate clauseHead
                {
                    TermNode clauseTerm = clause.NextNode;

                    while (clauseTerm != null) // non-facts only. Iterate over all clauseTerm-terms at of this clause
                    {
                        if (clauseTerm.BuiltinId == BI.none) clauseTerm.PredDescr = this[clauseTerm.Term.Key];
                        // builtins (>=0) are handled differently (in Execute ())

                        clauseTerm = clauseTerm.NextNode;
                    }
                    clause = clause.NextClause;
                }
                return;
            }


            
            private class ProfileCountList : List<KeyValuePair<int, string>>
            {
                private class ProfileCountComparer : IComparer<KeyValuePair<int, string>>
                {
                    public int Compare(KeyValuePair<int, string> kv0, KeyValuePair<int, string> kv1)
                    {
                        int result = -kv0.Key.CompareTo(kv1.Key); // descending count order

                        if (result == 0) return kv0.Value.CompareTo(kv1.Value);

                        return result;
                    }
                }

                private static IComparer<KeyValuePair<int, string>> SortProfileCounts => (IComparer<KeyValuePair<int, string>>)new ProfileCountComparer();

                public void Add(int count, string name)
                {
                    Add(new KeyValuePair<int, string>(count, name));
                }

                public new void Sort()
                {
                    base.Sort(SortProfileCounts);
                }

                public int MaxNameLen(int maxEntry)
                {
                    int maxNameLen = 0;
                    int i = 0;

                    foreach (KeyValuePair<int, string> kv in this)
                    {
                        if (i++ == maxEntry) break;

                        maxNameLen = Math.Max(maxNameLen, kv.Value.Length);
                    }

                    return maxNameLen;
                }
            }


            public void ShowProfileCounts(int maxEntry) // maximum number of entries to be shown
            {
                ProfileCountList profile = new ProfileCountList();
                int maxLen = 0;
                int maxVal = 0;

                foreach (KeyValuePair<string, PredicateDescr> kv in Predicates)
                    if (!IsPredefined(kv.Key) && kv.Value.ProfileCount > 0)
                    {
                        profile.Add(kv.Value.ProfileCount, kv.Value.Name);
                        maxVal = Math.Max(maxVal, kv.Value.ProfileCount);
                        maxLen = 1 + (int)Math.Log10((double)maxVal);
                    }

                profile.Sort();

                IO.WriteLine();
                string format =
                  "  {0,-" + profile.MaxNameLen(maxEntry) +
                  "} : {1," + maxLen + ":G}";

                int entryCount = 0;

                foreach (KeyValuePair<int, string> kv in profile)
                {
                    if (entryCount++ > maxEntry) break;

                    IO.WriteLine(format, kv.Value, kv.Key);
                }
            }


            public void ClearProfileCounts()
            {
                foreach (KeyValuePair<string, PredicateDescr> kv in Predicates)
                    kv.Value.ProfileCount = 0;
            }

            // try to match the name of an unrecognised command
            public PredicateDescr FindClosestMatch(string predName)
            {
                const float THRESHOLD = 0.5F; // maximum value for likely match
                PredicateDescr closestMatch = null;
                float closestMatchValue = 1.0F;

                foreach (KeyValuePair<string, PredicateDescr> kv in Predicates)
                {
                    PredicateDescr pd = kv.Value;
                    float matchValue;

                    if ((matchValue = pd.Name.Levenshtein(predName)) < closestMatchValue)
                    {
                        closestMatchValue = matchValue;
                        closestMatch = pd;
                    }
                }

                return (closestMatchValue < THRESHOLD) ? closestMatch : null;
            }
        }

        // classes for iterating over a predicate's clauses (used by clause/2)
        
        private class ClauseIterator : IEnumerable<BaseTerm>
        {
            private PredicateDescr pd;
            private BaseTerm clauseHead;
            private IEnumerator<BaseTerm> iterator;
            public BaseTerm ClauseBody { get; private set; }
            private VarStack varStack;

            public ClauseIterator(PredicateTable predTable, BaseTerm clauseHead, VarStack varStack)
            {
                this.pd = predTable[clauseHead.Key]; // null if not found
                this.clauseHead = clauseHead;
                this.varStack = varStack;
                iterator = GetEnumerator();
            }

            // A predicate consists of one or more clauses. A clause consist of a head and optionally a
            // body. A head is a term, the body is a sequence of terms. A predicate is stored as a chain
            // of TermNodes, where each TermNode represents a clause. These TermNodes are linked via the
            // nextClause field. In each nextClause/TermNode the clause head is stored in term, and the
            // clause body (which may be null) in NextNode.

            public IEnumerator<BaseTerm> GetEnumerator()
            {
                if (pd == null) yield break;

                ClauseNode clause = pd.ClauseList;

                while (clause != null) // iterate over all clauses of this predicate
                {
                    TermNode bodyNode = clause.NextNode;

                    int marker = varStack.Count; // register the point to which we must undo unification

                    if (clause.Head.Unify(clauseHead, varStack))
                    {
                        if (bodyNode == null) // a fact
                            ClauseBody = new BoolTerm(clause.Term.Symbol, true);
                        else if (bodyNode.BuiltinId == BI.none)
                            ClauseBody = bodyNode.TermSeq();
                        else
                            ClauseBody = new StringTerm(clause.Term.Symbol, "<builtin>");

                        yield return ClauseBody;
                    }

                    // undo unification with clauseHead before attempting the next clause head
                    BaseTerm.UnbindToMarker(varStack, marker);
                    clause = clause.NextClause;
                }
            }


            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }


            public bool MoveNext()
            {
                return iterator.MoveNext();
            }
        }
            }
}
