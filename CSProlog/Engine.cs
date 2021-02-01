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
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Serilog;
using static Prolog.PrologEngine;

namespace Prolog
{
    public class AbortQueryException : PrologException
    {
        public AbortQueryException(BaseTerm term = null, BaseParser.Symbol symbol = null) : base(" Execution terminated by user", term, symbol)
        { }
    }

    public partial class PrologEngine
    {
        public delegate void CurrentTerm(TermNode termNode);

        private const int INF = Int32.MaxValue;

        private static readonly string IOException = "ioException";

        public static readonly int maxWriteDepth = -1; // Set by maxwritedepth/1. Subterms beyond this depth are written as "..."

        private static readonly string YES = "\r\n" + "Yes";
        private static readonly string NO = "\r\n" + "No";
        private ChoicePoint currentCp;
        private bool debug;

        private bool findFirstClause; // find the first clause of predicate that matches the current goal goal (-last head)

        private int gensymInt;
        private GlobalTermsTable globalTermsTable;

        private TermNode goalListHead;
        private bool goalListProcessed;
        private bool goalListResult;

        private PrologParser parser;
        private string query;
        private int queryTimeout; // maximum Number of milliseconds that a command may run -- 0 means unlimited
        private ClauseNode retractClause;
        private ManualResetEvent sema;

        /* The parser's terminalTable is associated with the engine, not with the parser.
         * For each consult (or read), a new instance of the parser is created. However,
         * when each parser would have a new terminalTable, any operator definitions from
         * previously consulted files would get lost (operators are symbols that must be
         * recognized by the parser and hence they are stored in the terminalTable)
         */
        private BaseParser.BaseTrie terminalTable;

        public PrologEngine(ExecutionDetails executionDetails = null)
        {
            this.ExecutionDetails = executionDetails;
            Reset();
            PostBootstrap();
        }

        public ExecutionDetails? ExecutionDetails { get; private set; }

        public Stack<int> CatchIdStack { get; set; }

        public bool CsharpStrings { get; set; }
        public bool EventDebug { get; set; }

        public DateTime? LastConsulted { get; set; }
        public int LevelMax { get; set; }
        public int LevelMin { get; set; }
        public PredicateTable PredTable { get; set; }
        public long ProcTime { get; set; }
        public bool Profiling { get; set; }
        public bool Reporting { get; set; }
        public Solution Solution1 { get; set; }

        public IEnumerable<ISolution> SolutionIterator { get; set; }
        public long StartTime { get; set; }
        public bool Trace1 { get; set; }
        public int TryCatchId { get; set; }
        public List<AtomTerm> UserAtoms { get; set; }
        public bool UserInterrupted { get; set; }
        public bool UserSetShowStackTrace { get; set; } = true;
        public VarStack CurrVarStack { get; set; }

        public Stack<CallReturn> CallStack { get; private set; }

        public bool Error { get; set; }

        public OperatorTable OpTable { get; private set; }
        public BracketTable WrapTable { get; private set; }
        public BracketTable AltListTable { get; private set; }

        public string Query
        {
            get => query;
            set => query = value.Trim();
        }

        public bool Debugging
        {
            get => debug;
            set => debug = value;
        }

        public bool Halted { get; set; }


        public event Action FoundAllSolutions;

        public event Func<TermNode, TermNode, bool, VarStack, Stack<CallReturn>, bool> DebugEventBlocking;

        public static bool MaxWriteDepthExceeded(int level)
        {
            return maxWriteDepth != -1 && level > maxWriteDepth;
        }

        public void Reset()
        {
            Initialize();
            ReadBuiltinPredicates();
        }

        private void Initialize() // also called by ClearAll command
        {
            CurrVarStack = new VarStack();
            // Needed by BaseTerm.VAR which will get varNo = 0
            CurrVarStack.varNoMax++;

            CallStack = new Stack<CallReturn>();
            Solution1 = new Solution(this);
            SolutionIterator = GetEnumerator();
            OpTable = new OperatorTable();
            WrapTable = new BracketTable();
            AltListTable = new BracketTable();
            globalTermsTable = new GlobalTermsTable();
            PredTable = new PredicateTable(this);
            UserAtoms = new List<AtomTerm>();
            CatchIdStack = new Stack<int>();
            TryCatchId = 0;

            Error = false;
            Halted = false;
            Trace1 = false;
            EventDebug = false;
            StartTime = -1;
            ProcTime = 0;
            terminalTable = new BaseParser.BaseTrie(PrologParser.terminalCount, true);
            PrologParser.FillTerminalTable(terminalTable);
            parser = new PrologParser(this); // now this.terminalTable is passed on as well

            parser.AddPrologOperator(1200, "xfx", PrologParser.IMPLIES, false);
            parser.AddPrologOperator(1200, "fx", PrologParser.IMPLIES, false);
            parser.AddPrologOperator(1200, "xfx", PrologParser.DCGIMPL, false);
            parser.AddPrologOperator(1150, "xfy", PrologParser.ARROW, false);

            CurrVarStack.CommaOpDescr = parser.AddPrologOperator(1050, "xfy", PrologParser.COMMA, false);
            OpTable.Find(PrologParser.COMMA, out CurrVarStack.CommaOpTriplet);
            CurrVarStack.SemiOpDescr = parser.AddPrologOperator(1100, "xfy", PrologParser.SEMI, false);

            this.ExecutionDetails?.Reset();
        }

        private void PostBootstrap()
        {
            if (!OpTable.IsBinaryOperator(PrologParser.EQ, out _))
            {
                IO.ThrowRuntimeException($"No definition found for binary operator '{PrologParser.EQ}'", CurrVarStack, null);
            }
            else if (!OpTable.IsBinaryOperator(PrologParser.COLON, out _))
            {
                IO.ThrowRuntimeException($"No definition found for binary operator '{PrologParser.COLON}'", CurrVarStack, null);
            }
        }

        private void ReadBuiltinPredicates()
        {
            PredTable.Reset();
            parser.SetDollarAsPossibleUnquotedAtomChar(true);
            parser.StreamIn = PredefinedPredicates;
            parser.SetDollarAsPossibleUnquotedAtomChar(false);
            PredTable.Predefined.Add("0!");
            PredTable.Predefined.Add("0true");
            PredTable.Predefined.Add("0fail");
            PredTable.Predefined.Add("2;");
            PredTable.Predefined.Add("2,");
            retractClause = PredTable[BaseTerm.MakeKey("retract", 1)].ClauseList;
            PredTable.ResolveIndices();
        }

        public IEnumerable<Solution> GetEnumerator()
        {
            if (query != null)
            {
                try
                {
                    if (PrepareSolutions(query))
                    {
                        do
                        {
                            PrologException ex = Execute(); // run the query

                            Solution1.Error = ex;
                            Solution1.IsLast = Halted || !FindChoicePoint();

                            yield return Solution1;
                        } while (!Halted && CanBacktrack(null, true));
                    }
                    else // history command
                    {
                        Solution1.IsLast = true;

                        yield return Solution1;
                    }
                }
                finally
                {
                    if (query != null)
                    {
                        FoundAllSolutions?.Invoke();
                    }
                }
            }
        }

        public ISolution GetFirstSolution(string query)
        {
            Query = query;
            IEnumerator<ISolution> solutions = SolutionIterator.GetEnumerator();
            solutions.MoveNext();

            return solutions.Current;
        }

        public bool PrepareSolutions(string query)
        {
            try
            {
                Solution1.ResetMessage();
                Solution1.Solved = true;
                CurrVarStack.Clear();
                CatchIdStack.Clear();
                TryCatchId = 0;
                findFirstClause = true;
                UserInterrupted = false;
                parser.StreamIn = query;
                LevelMin = 0;
                LevelMax = INF;
                gensymInt = 0;
                IO.Reset(); // clear input character buffer
                goalListHead = parser.QueryNode;

                if (goalListHead == null)
                {
                    return false;
                }
            }
            catch (Exception x)
            {
                Error = true;
                Solution1.SetMessage("{0}{1}\r\n",
                    x.Message, UserSetShowStackTrace ? Environment.NewLine + x.StackTrace : "");

                return false;
            }

            return true;
        }

        private PrologException Execute()
        {
            ElapsedTime();
            ProcessorTime();

            try
            {
                Solution1.Solved = queryTimeout == 0 ? ExecuteGoalList() : StartExecuteGoalListThread();
            }
            catch (AbortQueryException x)
            {
                Solution1.SetMessage(x.Message);
                Solution1.Solved = true;

                return x;
            }
            catch (PrologException x)
            {
                Error = true;
                Solution1.Solved = false;
                string msg = $"{x.Message}{(UserSetShowStackTrace ? Environment.NewLine + x.StackTrace : "")}\r\n";
                Solution1.SetMessage(msg);

                if (x is RuntimeException rex)
                {
                    if (rex.VarStack == null)
                    {
                        rex.VarStack = CurrVarStack;
                    }

                    return rex;
                }
                
                return x;
            }

            return null;
        }

        private bool StartExecuteGoalListThread()
        {
            sema = new ManualResetEvent(false);
            goalListProcessed = false;
            goalListResult = false;
            Task t = Task.Run(RunExecuteGoalList);
            sema.WaitOne(queryTimeout); // wait for timeOutMSecs (while the RunExecuteGoalList thread runs)

            if (!goalListProcessed) // goalListProcessed is set by RunExecuteGoalList()
            {
                Solution1.Solved = false;

                return IO.ThrowRuntimeException($"Query execution timed out after {queryTimeout} milliseconds", CurrVarStack,
                    null);
            }

            return goalListResult;
        }

        private void RunExecuteGoalList()
        {
            try
            {
                goalListResult = ExecuteGoalList();
                goalListProcessed = true;
            }
            catch (Exception e) // any other exception
            {
                Error = true;
                goalListProcessed = true;
                Solution1.Solved = false;

                throw e;
            }
            finally
            {
                sema.Set();
            }
        }

        /*  Although in the code below a number of references is made to 'caching' (storing
            intermediate results of a calculation) this feature is currently not available.
            The reason is that it proved much more complicated than initially thought.
            I left the various fragments in the code, as I want to sort this out later more
            thoroughly.

            So technically spoken, the bool 'caching' will never have a true-value in the code
            below (nor anywhere else).
        */

        // The ExecuteGoalList() algorithm is the standard algorithm as for example
        // described in Ivan Bratko's "Prolog Programming for Artificial Intelligence",
        // 3rd edition p.45+
        private bool ExecuteGoalList()
        {
            while (goalListHead != null) // consume the last of goalNodes until it is exhausted
            {
                if (UserInterrupted)
                {
                    throw new AbortQueryException(goalListHead.Head);
                }

                if (goalListHead.Head is TryCatchTerm)
                {
                    if (goalListHead.Head is TryOpenTerm)
                    {
                        CatchIdStack.Push(((TryOpenTerm)goalListHead.Head)
                            .Id); // CATCH-id of corresponding CATCH-clause(s) now on top
                        goalListHead = goalListHead.NextNode;

                        continue;
                    }

                    if (goalListHead.Head is CatchOpenTerm)
                    {
                        if (((CatchOpenTerm)goalListHead.Head).SeqNo == 0) // only once:
                        {
                            CatchIdStack.Pop(); // CATCH-id of CATCH-clause enclosing this TRY/CATCH now on top
                        }

                        while (goalListHead.Head != TC_CLOSE)
                        {
                            goalListHead = goalListHead.NextNode;
                        }

                        continue;
                    }

                    if (goalListHead.Head == TC_CLOSE)
                    {
                        goalListHead = goalListHead.NextNode;

                        continue;
                    }
                }

                if (goalListHead is CallReturn)
                {
                    TermNode sp = ((CallReturn)goalListHead).SavedGoal;

                    if (Reporting)
                    {
                        Debugger(sp, sp, true);
                    }

                    CallReturn exit = CallStack.Pop();
                    this.ExecutionDetails?.Exit(exit.SavedGoal);

                    goalListHead = sp.NextNode;

                    continue;
                }

                int stackSize = CurrVarStack.Count; // varStack reflects the current program state

                // FindPredicateDefinition tries to find in the program the predicate definition for the
                // functor+arity of goalNode.Term. This definition is stored in goalListHead.NextClause
                if (findFirstClause)
                {
                    // CALL, REDO
                    //if (reporting)
                    //    Debugger(goalListHead, currClause, false);

                    if (!goalListHead.FindPredicateDefinition(PredTable))
                    {
                        // Undefined
                        BaseTerm goal = goalListHead.Head;

                        switch (PredTable.ActionWhenUndefined(goal.FunctorToString, goal.Arity))
                        {
                            case UndefAction.Fail: // pretend the predicate exists, with 'fail' as first and only clause
                                goalListHead.PredDescr = PredTable[BaseTerm.FAIL.Key];
                                goalListHead.NextClause = goalListHead.PredDescr.ClauseList;
                                break;

                            case UndefAction.Error:
                                return IO.ThrowRuntimeException($"Undefined predicate: {goal.Name}", CurrVarStack, goal);

                            default:
                                PredicateDescr pd = PredTable.FindClosestMatch(goal.Name);
                                string suggestion = pd == null
                                    ? null
                                    : $". Maybe '{pd.Name}' is what you mean?";
                                IO.ThrowRuntimeException($"Undefined predicate: {goal.Name}{suggestion}", CurrVarStack, goal);
                                break;
                        }
                    }

                    findFirstClause = false; // i.e. advance to the next clause upon backtracking (redoing)
                }

                if (Profiling && goalListHead.PredDescr != null)
                {
                    goalListHead.PredDescr.IncProfileCount();
                }

                // Save the current goal (Term can get modified by unification)
                TermNode saveGoal = goalListHead;

                // The current clause of the predicate (overwritten by body after unification)
                TermNode currClause = saveGoal.NextClause;

                // instantiations must be retained for clause body -> create newVars
                BaseTerm cleanClauseHead = currClause.Head.Copy(CurrVarStack);

                // The current clause body
                TermNode? currClauseBody = currClause.NextNode;

                // Redo with another clause possible?
                if (currClause.NextClause != null)
                {
                    CurrVarStack.Push(currentCp = new ChoicePoint(goalListHead, currClause.NextClause, goalListHead));
                }

                // CALL, REDO
                if (Reporting)
                {
                    Debugger(goalListHead, currClause, false);
                }

                this.ExecutionDetails?.CurrentGoalBeforeUnify(goalListHead, currClause);
                int varStackCountBeforeUnify = this.CurrVarStack.Count;

                string saveGoalPreUnifyCopy = this.ExecutionDetails != null ? saveGoal.Head.ToString() : null;

                // UNIFICATION of the current goal and the (clause of the) predicate that matches it
                if (!cleanClauseHead.Unify(goalListHead.Head, CurrVarStack))
                {
                    // Unify failed - try backtracking
                    this.ExecutionDetails?.AfterUnify(CurrVarStack, varStackCountBeforeUnify, false, false);

                    bool canRedo = CanBacktrack(currClause, failedUnify: true);
                   
                    if (!canRedo || (goalListHead.NextClause != null && goalListHead.NextClause.Head.Name != currClause.Head.Name))
                    {
                        this.ExecutionDetails?.FailCall(saveGoal);
                        this.ExecutionDetails?.Failed(saveGoal);
                    }

                    PopCallStackFailed(canRedo, saveGoal);

                    if (!canRedo)
                    {
                        return false;
                    }
                }
                else
                {
                    this.ExecutionDetails?.AfterUnify(CurrVarStack, varStackCountBeforeUnify, true, goalListHead.Head.Name == "fail/0");

                    // Matched the clause head, move on the evaluating the body
                    // This is where we lose track of the top-level clause because replace the reference with body
                    // However, we can still get hold of the whole clause through saveGoal.NextClause
                    currClause = currClauseBody;

                    if (Reporting)
                    {
                        Debugger(saveGoal, currClause, false);
                    }

                    // FACT
                    // body is null or :- true., we are matching against a fact
                    if (currClause == null || currClause.IsTrueAtom)
                    {
                        this.ExecutionDetails?.FactCall(saveGoal.Level, saveGoalPreUnifyCopy);
                        this.ExecutionDetails?.Exit(saveGoal);

                        goalListHead = goalListHead.NextNode;
                        if (CallStack.TryPeek(out CallReturn cr) && cr.SavedGoal.NextNode == goalListHead)
                        {
                            CallStack.Pop();
                            this.ExecutionDetails?.Exit(cr.SavedGoal);
                        }

                        findFirstClause = true;
                    }
                    // BUILT-IN
                    else if (currClause.BuiltinId != BI.none)
                    {
                        BI builtinId = currClause.BuiltinId;
                        bool backtrack = false;
                        if (builtinId == BI.call)
                        {
                            BaseTerm t = goalListHead.Head.Arg(0);

                            if (t.IsVar)
                            {
                                return IO.ThrowRuntimeException(
                                    $"Unbound variable '{((Variable)t).Name}' in goal list", CurrVarStack, t);
                            }

                            if (goalListHead.Head.Arity > 1) // implementation of SWI call/1..8
                            {
                                AddCallArgs(goalListHead);
                                t = goalListHead.Head;
                            }

                            TermNode tn0 = t.ToGoalList(stackSize, goalListHead.Level + 1);

                            CallReturn callReturn = new CallReturn(saveGoal);
                            this.ExecutionDetails?.CallCall(callReturn);
                            CallStack.Push(callReturn);

                            if (Reporting)
                            {
                                tn0.Append(callReturn);
                            }

                            goalListHead = goalListHead == null ? tn0 : tn0.Append(goalListHead.NextNode);
                            findFirstClause = true;
                        }
                        else if (builtinId == BI.or)
                        {
                            TermNode tn1 = goalListHead.Head.Arg(1).ToGoalList(stackSize, goalListHead.Level);
                            tn1 = goalListHead == null
                                ? tn1
                                : tn1.Append(goalListHead.NextNode);

                            TermNode tn0 = goalListHead.Head.Arg(0).ToGoalList(stackSize, goalListHead.Level);
                            goalListHead = goalListHead == null ? tn0 : tn0.Append(goalListHead.NextNode);

                            CurrVarStack.Push(new ChoicePoint(tn1, null, goalListHead));

                            findFirstClause = true;
                        }
                        else if (builtinId == BI.cut)
                        {
                            CurrVarStack.DisableChoices(goalListHead.Head.TermId);
                            goalListHead = goalListHead.NextNode;
                            findFirstClause = true;
                        }
                        else if (builtinId == BI.fail)
                        {
                            this.ExecutionDetails?.FailCall(saveGoal);

                            bool canRedo = CanBacktrack(saveGoal);
                            
                            PopCallStackFailed(canRedo, saveGoal);

                            if (!canRedo)
                            {
                                return false;
                            }

                        }
                        else
                        {
                            CallReturn callReturn = new CallReturn(saveGoal);
                            CallStack.Push(callReturn);

                            this.ExecutionDetails?.BuiltInCall(saveGoal);
                            if (DoBuiltin(builtinId, out findFirstClause))
                            {

                            }
                            else
                            {
                                backtrack = true;
                            }
                        }

                        if (backtrack)
                        {
                            bool canRedo = CanBacktrack(saveGoal);
                            
                            PopCallStackFailed(canRedo, saveGoal);

                            if (!canRedo)
                            {
                                return false;
                            }
                        }
                    }
                    else
                    {
                        // PREDICATE RULE: replace goal by body of matching clause of defining predicate
                        // We start with the predicate body in currClause, and build a linked list of goals from the clauses
                        TermNode pHead = null;
                        TermNode pTail = null;
                        TermNode clausePointer = currClause;
                        TermNode parentClause = saveGoal.NextClause;
                        while (clausePointer != null)
                        {
                            BaseTerm currTerm = clausePointer.Head;
                            TermNode p;

                            if (currTerm is TryOpenTerm)
                            {
                                ((TryOpenTerm)currTerm).Id = ++TryCatchId;
                                p = new TermNode(currTerm, parentClause, null, 0);
                            }
                            else if (currTerm is CatchOpenTerm)
                            {
                                ((CatchOpenTerm)currTerm).Id = TryCatchId; // same id as for corresponding TRY
                                p = new TermNode(currTerm.Copy(false, CurrVarStack), parentClause, null, 0);
                            }
                            else if (currTerm is Cut)
                            {
                                p = new TermNode(new Cut(currTerm.Symbol, stackSize), parentClause, null,
                                    goalListHead.Level + 1); // save the pre-unification state
                            }
                            else // Copy (false): keep the varNo constant over all terms of the predicate head+body
                                 // (otherwise each term would get new variables, independent of their previous incarnations)
                            {
                                p = new TermNode(currTerm.Copy(false, CurrVarStack), parentClause, clausePointer.PredDescr,
                                    goalListHead.Level + 1); // gets the newVar version
                            }

                            if (pHead == null)
                            {
                                pHead = p;
                            }
                            else
                            {
                                pTail.NextNode = p;
                            }

                            pTail = p;
                            clausePointer = clausePointer.NextNode;
                        }

                        if (!CallStack.TryPeek(out CallReturn currentContext) || currentContext.SavedGoal != saveGoal)
                        {
                            CallReturn callReturn = new CallReturn(saveGoal);
                            this.ExecutionDetails?.PredicateRuleCall(callReturn);
                            CallStack.Push(callReturn);
                        }

                        if (Reporting)
                        {
                            pTail.NextNode = new CallReturn(saveGoal);
                            pTail = pTail.NextNode;
                        }

                        pTail.NextNode = goalListHead.NextNode;
                        goalListHead = pHead; // will never be a spypoint
                        findFirstClause = true;
                    }
                }
            } // end of while

            // Success, just mark any remaining call stack returns as exit
            while (CallStack.TryPop(out CallReturn remaining))
            {
                this.ExecutionDetails?.Exit(remaining.SavedGoal);
            }

            return true;

            void PopCallStackFailed(bool canRedo, TermNode saveGoal)
            {
                if (ShouldRedo(out CallReturn cr))
                {
                    this.ExecutionDetails?.Redo(cr.SavedGoal);
                    return;
                }

                while (CallStack.TryPeek(out cr) && cr.SavedGoal != null && 
                    (!canRedo || saveGoal.NextNode == null))
                {
                    CallStack.Pop();
                    this.ExecutionDetails?.Failed(cr.SavedGoal);

                    if (ShouldRedo(out cr))
                    {
                        this.ExecutionDetails?.Redo(cr.SavedGoal);
                        return;
                    }
                }

                bool ShouldRedo(out CallReturn crr) => CallStack.TryPeek(out crr) && canRedo && crr.SavedGoal != null && crr.SavedGoal.NextClause != null && crr.SavedGoal.NextClause != saveGoal.Parent;
            }
        }

        private void InsertCutFail()
        {
            ClauseNode fail = new ClauseNode(BaseTerm.FAIL, null);
            fail.NextNode = goalListHead.NextNode;
            ClauseNode cut = new ClauseNode(BaseTerm.CUT, null) { NextNode = fail };
            goalListHead = cut;
        }

        private bool CanBacktrack(TermNode saveGoal, bool nextSolution = false, bool failedUnify = false) // local = false if user wants more (so as not to trigger the debugger)
        {
            if (saveGoal != null)
            {
                if (!failedUnify)
                {
                    this.ExecutionDetails?.Failed(saveGoal);
                }
            }

            findFirstClause = false; // to prevent resetting to the first clause upon re-entering ExecuteGoalList

            while (CurrVarStack.Count != 0)
            {
                object o = CurrVarStack.Pop();

                if (o is Variable)
                {
                    ((Variable)o).Unbind();
                }
                else if (o is ChoicePoint cp)
                {
                    if (nextSolution)
                    {
                        this.ExecutionDetails?.NextSolution(cp.PrevGoal);
                    }
                    else
                    {
                        if (!failedUnify)
                        {
                            this.ExecutionDetails?.Failed(cp.PrevGoal);
                        }
                    }

                    if (cp.IsActive)
                    {
                        goalListHead = cp.GoalListHead; // this was the goal we wanted to prove ...


                        if (cp.NextClause == null) // no next predicate clause ...
                        {
                            if (CallStack.Count > 0)
                            {
                                CallStack.Pop();
                            }

                            findFirstClause = true; // ... so find predicate belonging to the goal last head
                        }
                        else
                        {
                            goalListHead.NextClause = cp.NextClause; // ... and this is next predicate clause to be tried
                        }

                        return true;
                    }
                }
            }

            return false;
        }

        private bool FindChoicePoint()
        {
            foreach (Object o in CurrVarStack)
            {
                if (o is ChoicePoint && ((ChoicePoint)o).IsActive)
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        ///     Prolog throw.
        /// </summary>
        private void Throw(string exceptionClass, string exceptionMessage)
        {
            if (!SearchMatchingCatchClause(exceptionClass, exceptionMessage))
            {
                string comma = exceptionClass == null || exceptionMessage == null ? null : ", ";
                string msg = $"No CATCH found for throw( {exceptionClass}{comma}\"{exceptionMessage}\")";
                IO.ThrowRuntimeException(msg, CurrVarStack, null);
            }
        }
        private void AddCallArgs(TermNode GoalListHead)
        {
            BaseTerm callPred = GoalListHead.Head.Arg(0);
            int arity0 = callPred.Arity;
            int arity = arity0 + GoalListHead.Head.Arity - 1;
            BaseTerm[] callArgs = new BaseTerm[arity];

            for (int i = 0; i < arity0; i++)
            {
                callArgs[i] = callPred.Arg(i);
            }

            for (int i = arity0; i < arity; i++)
            {
                callArgs[i] = GoalListHead.Head.Arg(1 + i - arity0);
            }

            GoalListHead.Head = new CompoundTerm(callPred.Symbol, callPred.Functor, callArgs);
        }

        private bool SearchMatchingCatchClause(string exceptionClass, string exceptionMessage)
        {
            /* A TRY/CATCH predicate has the following format:

               TRY (<terms>) CATCH [<exception class>] (<terms>) CATCH [<exception class>] (<terms>) ...

               So, a TRY/CATCH statement can have more than one CATCH-clauses, each labeled with
               the name of an 'exception class' that can be given freely by the user and that corresponds
               to the exception class name in the throw/2/3 predicate.

               When a clause containing a TRY/CATCH is expanded (i.e. when ExecuteGoalList () places
               the clause body at the beginning of the goal list), not only the TRY-body, but also the
               CATCH-bodies are put on the list. Each TRY/CATCH statement is given a unique number (Id).

               Upon normal execution (no exception occurring), TRY-bodies when appearing on the goal list
               are executed normally, whereas CATCH-bodies are simply skipped. The only action taken
               is that upon hitting upon a TRY, the Id of the TRY/CATCH is pushed on a stack,
               which indicates that in case of an exception, the CATCH-clause(s) with this Id is the
               first candidate for execution.

               When an exception occurs, the goal list is searched for the first CATCH-clause with the
               Id found on top of the stack. If the exception matches the CATCH clause (i.e. the exception
               class name supplied as throw/2/3-parameter matches the exception class name of the CATCH-
               clause OR the CATCH-clause does not have an exception class name), execution is resumed with
               the first CATCH-body predicate as head of the goal list. If none of the CATCH-clauses match,
               the next Id (beloning to the caller TRY/CATCH) is popped off the stack and the search is
               continued.

               I am not sure what to do about unbinding. It seems obvious, when control is given to a
               calling predicate, to undo the variable bindings that occurred within the called predicate.
               But what to do in the case of backtracking, when the called predicate contained a choice
               point? In that case, the variable bindings up to the choice point should be restored again,
               which seems to entail a complicated strategy of temporarily unbinding variables.
               For the time being I decided not to do any unbinding when an exception is thrown, but
               this may prove to be a wrong decision. Anyway, CatchOpenTerm has a property SaveStackSize,
               which in future may be used when the varstack has to be popped and unbindings have to
               be carried out.
            */

            if (CatchIdStack.Count == 0)
            {
                return false;
            }

            int catchId = CatchIdStack.Pop();
            bool catchIdFoundInGoalList = false; // true iff the catchId was found in the list of goals
            Status status = Status.TestGoalNode;
            CatchOpenTerm t = null;

            while (true) // finite state engine implementation turned out to be easiest
            {
                switch (status)
                {
                    case Status.TestGoalNode:
                        if (goalListHead == null)
                        {
                            return false;
                        }

                        if (goalListHead.Head is CatchOpenTerm)
                        {
                            t = (CatchOpenTerm)goalListHead.Head;
                            status = Status.CompareIds;
                        }
                        else
                        {
                            status = Status.NextGoalNode;
                        }

                        break;

                    case Status.NextGoalNode:
                        goalListHead = goalListHead.NextNode;
                        status = Status.TestGoalNode;
                        break;

                    case Status.NextCatchId: // get the Id of a potentially matching CATCH-clause
                        if (CatchIdStack.Count == 0)
                        {
                            return false;
                        }

                        catchId = CatchIdStack.Pop();
                        catchIdFoundInGoalList = false;
                        status = Status.CompareIds;
                        break;

                    case Status.CompareIds:
                        if (t.Id == catchId)
                        {
                            catchIdFoundInGoalList = true;
                            status = Status.TryMatch;
                        }
                        else if (catchIdFoundInGoalList) // CATCH-id does not match anymore, so try ...
                        {
                            status = Status.NextCatchId; // ... the next CATCH (at an enclosing level)
                        }
                        else
                        {
                            status = Status.NextGoalNode; // catchId not yet found, try next goal
                        }

                        break;

                    case Status.TryMatch:
                        if (t.ExceptionClass == null || t.ExceptionClass == exceptionClass)
                        {
                            t.MsgVar.Unify(new StringTerm(t.Symbol, exceptionMessage), CurrVarStack);

                            return true;
                        }

                        status = Status.NextGoalNode;
                        break;
                }
            }
        }

        private bool Debugger(TermNode goalNode, TermNode currClause, bool isReturn)
        {
            if (!Reporting)
            {
                return false;
            }

            // TODO: redo = false;

            if (EventDebug)
            {
                return DebugEventBlocking?.Invoke(goalNode, currClause, isReturn, CurrVarStack, CallStack) ?? false;
            }

            return false;
        }

        public void RetryCurrentGoal(int level)
        {
            Object o;

            while (CurrVarStack.Count != 0)
            {
                o = CurrVarStack.Pop();

                if (o is Variable)
                {
                    ((Variable)o).Unbind();
                }
            }
        }

        public BaseTerm GetVariable(string s)
        {
            return Solution1.GetVar(s);
        }

        public void SetVariable(BaseTerm t, string s)
        {
            Solution1.SetVar(s, t);
        }

        public void EraseVariables()
        {
            Solution1.Clear();
        }

        public void RegisterVarNonSingleton(string s)
        {
            Solution1.RegisterVarNonSingleton(s);
        }

        public void ReportSingletons(ClauseNode c, int lineNo, ref bool firstReport)
        {
            Solution1.ReportSingletons(c, lineNo, ref firstReport);
        }

        public void SetProfiling(bool mode)
        {
            Profiling = mode;
        }

        public int ElapsedTime() // returns numer of milliseconds since last Call
        {
            long prevStartTime = StartTime == -1 ? DateTime.Now.Ticks : StartTime;

            return (int)((StartTime = DateTime.Now.Ticks) - prevStartTime) / 10000;
        }

        public long ProcessorTime() // returns numer of milliseconds since last Call
        {
            long prevProcTime = ProcTime == 0 ? Stopwatch.GetTimestamp() : ProcTime;

            return (long)(1000 * ((ProcTime = Stopwatch.GetTimestamp()) - prevProcTime) / (double)Stopwatch.Frequency);
        }

        public long ClockTicksMSecs()
        {
            return DateTime.Now.Ticks / 10000;
        }

        public void Consult(string stream, string streamName = null)
        {
            bool csharpStringsSave = CsharpStrings;
            // string as ISO-style charcode lists or as C# strings

            try
            {
                ConsultedFiles.Clear();
                PredTable.Consult(stream, streamName);
            }
            finally
            {
                CsharpStrings = csharpStringsSave;
                LastConsulted = DateTime.Now;
            }
        }

        public void ConsultFromString(string prologCode, string codeTitle = null)
        {
            Consult(prologCode, codeTitle);
        }

        public void SetStringStyle(BaseTerm t)
        {
            string arg = t.FunctorToString;

            if (!(arg == "csharp" || arg == "iso"))
            {
                IO.ThrowRuntimeException($"Illegal argument '{t}' for setstringstyle/1 -- must be 'iso' or 'csharp'",
                    CurrVarStack,
                    t);
            }

            CsharpStrings = arg == "csharp";
        }

        private void SetSwitch(string switchName, ref bool switchVar, bool mode)
        {
            bool current = switchVar;

            if (current == (switchVar = mode))
            {
                IO.Message($"{switchName} already {(mode ? "on" : "off")}");
            }
            else
            {
                IO.Message($"{switchName} switched {(mode ? "on" : "off")}");
            }
        }

        public class ChoicePoint
        {
            protected bool active;

            public TermNode PrevGoal { get; }

            protected TermNode goalListHead;
            protected ClauseNode nextClause; // next clause to be tried for goalListHead

            public ChoicePoint(TermNode goal, ClauseNode nextClause, TermNode prevGoal)
            {
                goalListHead = goal;
                this.nextClause = nextClause;
                active = true;
                this.PrevGoal = prevGoal;
            }

            public TermNode GoalListHead => goalListHead;

            public ClauseNode NextClause
            {
                get => nextClause;
                set => nextClause = value;
            }

            public bool IsActive => active;

            public void Kill()
            {
                active = false;
            }

            public override String ToString()
            {
                return $"choicepoint\r\ngoal {goalListHead}\r\nclause {nextClause}\r\nactive {active}";
            }
        }

        public class CallReturn : TermNode
        {
            public CallReturn(TermNode goal) : base(null, null, null, 0)
            {
                SavedGoal = goal;
            }

            public TermNode SavedGoal { get; }
        }

        public class VarStack : Stack<object>
        {
            public OperatorDescr CommaOpDescr;
            public OpDescrTriplet CommaOpTriplet;
            public OperatorDescr SemiOpDescr;
            public int varNoMax;
            public int verNoMax;

            public int CurrUnifyCount { get; set; }

            public void NextUnifyCount()
            {
                CurrUnifyCount++;
            }

            public void DisableChoices(int n)
            {
                int i = Count;

                foreach (object o in this) // works its way down from the top !!!
                {
                    if (i-- == n)
                    {
                        return;
                    }

                    (o as ChoicePoint)?.Kill();
                }
            }
        }

        public interface IVarValue
        {
            string Name { get; }
            ITermNode Value { get; }
            string DataType { get; }
        }

        public class VarValue : IVarValue
        {
            public string name;
            public BaseTerm value;

            public VarValue(string name, BaseTerm value)
            {
                this.name = name;
                this.value = value;
                IsSingleton = true;
            }

            public bool IsSingleton { get; set; }
            public string Name => name;
            public ITermNode Value => value;
            public string DataType => value.TermType.ToString().ToLower();

            public override string ToString()
            {
                if (!value.IsVar)
                {
                    bool mustPack = value.Precedence >= 700;

                    return $"{name} = {value.ToString().Packed(mustPack)}";
                }

                return null;
            }
        }

        public class VarValues : Dictionary<string, VarValue>
        {
            public BaseTerm GetValue(string name)
            {
                VarValue result;
                TryGetValue(name, out result); // result is null if value not found

                return result == null ? null : result.value;
            }
        }

        public interface ISolution
        {
            IEnumerable<IVarValue> VarValuesIterator { get; }
            bool IsLast { get; }
            bool Solved { get; }
        }

        // contains the answer to a query or the response to a history command
        public class Solution : ISolution
        {
            private readonly PrologEngine engine;

            public string msg;
            public VarValues variables;

            public Solution(PrologEngine engine)
            {
                this.engine = engine;
                variables = new VarValues();
                VarValuesIterator = GetEnumerator();
                Solved = true;
                msg = null;
            }

            public PrologException Error { get; set; }
            public bool Solved { get; set; }

            public bool IsLast { get; set; }
            public IEnumerable<IVarValue> VarValuesIterator { get; }

            public IEnumerable<IVarValue> GetEnumerator()
            {
                foreach (IVarValue varValue in variables.Values)
                {
                    if (engine.Halted)
                    {
                        yield break;
                    }

                    yield return varValue;
                }
            }

            public void Clear()
            {
                variables.Clear();
            }

            public void SetMessage(string msg)
            {
                this.msg = msg;
            }

            public void SetMessage(string msg, params object[] args)
            {
                SetMessage(string.Format(msg, args));
            }

            public void ResetMessage()
            {
                msg = null;
            }

            public void SetVar(string name, BaseTerm value)
            {
                variables[name] = new VarValue(name, value);
            }

            public void RegisterVarNonSingleton(string name)
            {
                variables[name].IsSingleton = false;
            }

            public void ReportSingletons(ClauseNode c, int lineNo, ref bool firstReport)
            {
                List<string> singletons = new List<string>();

                foreach (VarValue var in variables.Values)
                {
                    if (var.IsSingleton)
                    {
                        singletons.Add(var.Name);
                    }
                }

                if (singletons.Count == 0)
                {
                    return;
                }

                if (firstReport)
                {
                    IO.WriteLine();
                    firstReport = false;
                }

                IO.Write($"    Warning: '{c.Head.Name}' at line {lineNo} has {(singletons.Count == 1 ? "a " : null)}singleton variable{(singletons.Count == 1 ? null : "s")} [");

                bool first = true;

                foreach (string name in singletons)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        IO.Write(", ");
                    }

                    IO.Write(name);
                }

                IO.WriteLine("]");
            }

            public BaseTerm GetVar(string name)
            {
                return variables.GetValue(name);
            }

            public override string ToString()
            {
                if (engine.Halted)
                {
                    return null;
                }

                if (msg != null)
                {
                    return msg;
                }

                double totSecs = engine.ProcessorTime() / 1000.0;

                string time = engine.EventDebug || totSecs < 0.1 ? "" : $" ({totSecs:f3} s)";

                if (!Solved)
                {
                    return NO + time;
                }

                StringBuilder sb = new StringBuilder();

                foreach (VarValue varValue in variables.Values)
                {
                    if (!varValue.value.IsVar && varValue.name[0] != '_')
                    {
                        sb.AppendFormat("{0}", varValue);
                    }
                }

                return (sb.Length == 0 ? YES : sb.ToString()) + time;
            }
        }

        private class GlobalTermsTable
        {
            private readonly Dictionary<string, int> counterTable;
            private readonly Dictionary<string, BaseTerm> globvarTable;

            public GlobalTermsTable()
            {
                counterTable = new Dictionary<string, int>();
                globvarTable = new Dictionary<string, BaseTerm>();
            }

            public void getctr(string a, out int value)
            {
                if (!counterTable.TryGetValue(a, out value))
                {
                    IO.ThrowRuntimeException($"Value of counter '{a}' is not set", null, null);
                }
            }

            public void setctr(string a, int value)
            {
                counterTable[a] = value;
            }

            public void incctr(string a)
            {
                int value;

                if (counterTable.TryGetValue(a, out value))
                {
                    counterTable[a] = value + 1;
                }
                else
                {
                    IO.ThrowRuntimeException($"Value of counter '{a}' is not set", null, null);
                }
            }

            public void decctr(string a)
            {
                int value;

                if (counterTable.TryGetValue(a, out value))
                {
                    counterTable[a] = value - 1;
                }
                else
                {
                    IO.ThrowRuntimeException($"Value of counter '{a}' is not set", null, null);
                }
            }

            public void getvar(string name, out BaseTerm value)
            {
                if (!globvarTable.TryGetValue(name, out value))
                {
                    IO.ThrowRuntimeException($"Value of '{name}' is not set", null, null);
                }
            }

            public void setvar(string name, BaseTerm value)
            {
                globvarTable[name] = value;
            }
        }

        private enum Status { NextCatchId, CompareIds, NextGoalNode, TestGoalNode, TryMatch }
    }
}