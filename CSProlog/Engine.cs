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

        private OpenFiles openFiles;

        private PrologParser parser;
        private string query;
        private int queryTimeout; // maximum Number of milliseconds that a command may run -- 0 means unlimited
        private bool redo; // set by CanBacktrack if a choice point was found
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
            openFiles = new OpenFiles();
            TryCatchId = 0;

            Error = false;
            Halted = false;
            Trace1 = false;
            EventDebug = false;
            StartTime = -1;
            ProcTime = 0;
            currentFileReader = null;
            currentFileWriter = null;
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
                        } while (!Halted && CanBacktrack(false));
                    }
                    else // history command
                    {
                        Solution1.IsLast = true;

                        yield return Solution1;
                    }
                }
                finally
                {
                    PostQueryTidyUp(); // close all potentially open files and SQL connections

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

        public void PostQueryTidyUp()
        {
            openFiles.CloseAllOpenFiles();
            currentFileReader = null;
            currentFileWriter = null;
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
            // variables declaration used in goal loop to save stack space
            int stackSize;
            TermNode currClause = null;
            BaseTerm cleanClauseHead;
            BI builtinId;
            BaseTerm t;
            TermNode saveGoal = null;
            TermNode p;
            TermNode pHead;
            TermNode pTail;
            TermNode tn0;
            TermNode tn1;
            redo = false; // set by CanBacktrack if a choice point was found

            while (goalListHead != null) // consume the last of goalNodes until it is exhausted
            {
                if (UserInterrupted)
                {
                    throw new AbortQueryException(goalListHead.Term);
                }

                if (goalListHead.Term is TryCatchTerm)
                {
                    if (goalListHead.Term is TryOpenTerm)
                    {
                        CatchIdStack.Push(((TryOpenTerm)goalListHead.Term)
                            .Id); // CATCH-id of corresponding CATCH-clause(s) now on top
                        goalListHead = goalListHead.NextGoal;

                        continue;
                    }

                    if (goalListHead.Term is CatchOpenTerm)
                    {
                        if (((CatchOpenTerm)goalListHead.Term).SeqNo == 0) // only once:
                        {
                            CatchIdStack.Pop(); // CATCH-id of CATCH-clause enclosing this TRY/CATCH now on top
                        }

                        while (goalListHead.Term != TC_CLOSE)
                        {
                            goalListHead = goalListHead.NextGoal;
                        }

                        continue;
                    }

                    if (goalListHead.Term == TC_CLOSE)
                    {
                        goalListHead = goalListHead.NextGoal;

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

                    CallStack.Pop();

                    goalListHead = sp.NextGoal;

                    continue;
                }

                stackSize = CurrVarStack.Count; // varStack reflects the current program state

                // FindPredicateDefinition tries to find in the program the predicate definition for the
                // functor+arity of goalNode.Term. This definition is stored in goalListHead.NextClause
                if (findFirstClause)
                {
                    // CALL, REDO
                    //if (reporting)
                    //    Debugger(goalListHead, currClause, false);

                    if (!goalListHead.FindPredicateDefinition(PredTable)) // undefined predicate
                    {
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

                currClause = goalListHead.NextClause; // the first or next clause of the predicate definition

                saveGoal = goalListHead; // remember the original saveGoal (which may be NextGoal-ed, see below)

                if (currClause.NextClause != null) // no redo possible => fail, make explicit when tracing
                {
                    CurrVarStack.Push(currentCp = new ChoicePoint(goalListHead, currClause.NextClause));
                }

                // instantiations must be retained for clause body -> create newVars
                cleanClauseHead = currClause.Head.Copy(CurrVarStack); 

                // CALL, REDO
                if (Reporting)
                {
                    Debugger(saveGoal, currClause, false);
                }

                this.ExecutionDetails?.CurrentGoalBeforeUnify(goalListHead, currClause);
                int currVarStackCount = this.CurrVarStack.Count;

                // UNIFICATION of the current goal and the (clause of the) predicate that matches it
                if (cleanClauseHead.Unify(goalListHead.Term, CurrVarStack))
                {
                    this.ExecutionDetails?.AfterUnify(CurrVarStack, currVarStackCount, true);

                    currClause = currClause.NextNode; // body - if any - of the matching predicate definition clause

                    if (Reporting)
                    {
                        Debugger(saveGoal, currClause, false);
                    }

                    // FACT
                    if (currClause == null) // body is null, so matching was against a fact
                    {
                        goalListHead = goalListHead.NextGoal;

                        findFirstClause = true;
                    }
                    // BUILT-IN
                    else if ((builtinId = currClause.BuiltinId) != BI.none)
                    {
                        if (builtinId == BI.call)
                        {
                            t = goalListHead.Head.Arg(0);

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

                            tn0 = t.ToGoalList(stackSize, goalListHead.Level + 1);

                            CallReturn callReturn = new CallReturn(saveGoal);
                            CallStack.Push(callReturn);

                            if (Reporting)
                            {
                                tn0.Append(callReturn);
                            }

                            goalListHead = goalListHead == null ? tn0 : tn0.Append(goalListHead.NextGoal);
                            findFirstClause = true;
                        }
                        else if (builtinId == BI.or)
                        {
                            tn1 = goalListHead.Head.Arg(1).ToGoalList(stackSize, goalListHead.Level);
                            CurrVarStack.Push(new ChoicePoint(goalListHead == null
                                ? tn1
                                : tn1.Append(goalListHead.NextGoal), null));

                            tn0 = goalListHead.Head.Arg(0).ToGoalList(stackSize, goalListHead.Level);
                            goalListHead = goalListHead == null ? tn0 : tn0.Append(goalListHead.NextGoal);
                            findFirstClause = true;
                        }
                        else if (builtinId == BI.cut)
                        {
                            CurrVarStack.DisableChoices(goalListHead.Term.TermId);
                            goalListHead = goalListHead.NextGoal;
                            findFirstClause = true;
                        }
                        else if (builtinId == BI.fail)
                        {
                            if (!(redo = CanBacktrack()))
                            {
                                return false;
                            }
                        }
                        else if (DoBuiltin(builtinId, out findFirstClause))
                        {
                        }
                        else if (!(redo = CanBacktrack()))
                        {
                            return false;
                        }
                    }
                    // PREDICATE RULE
                    else // replace goal by body of matching clause of defining predicate
                    {
                        pHead = null;
                        pTail = null;
                        BaseTerm currTerm;

                        while (currClause != null)
                        {
                            currTerm = currClause.Term;

                            if (currTerm is TryOpenTerm)
                            {
                                ((TryOpenTerm)currTerm).Id = ++TryCatchId;
                                p = new TermNode(currTerm, null, 0);
                            }
                            else if (currTerm is CatchOpenTerm)
                            {
                                ((CatchOpenTerm)currTerm).Id = TryCatchId; // same id as for corresponding TRY
                                p = new TermNode(currTerm.Copy(false, CurrVarStack), null, 0);
                            }
                            else if (currTerm is Cut)
                            {
                                p = new TermNode(new Cut(currTerm.Symbol, stackSize), null,
                                    goalListHead.Level + 1); // save the pre-unification state
                            }
                            else // Copy (false): keep the varNo constant over all terms of the predicate head+body
                                 // (otherwise each term would get new variables, independent of their previous incarnations)
                            {
                                p = new TermNode(currTerm.Copy(false, CurrVarStack), currClause.PredDescr,
                                    goalListHead.Level + 1); // gets the newVar version
                            }

                            if (pHead == null)
                            {
                                pHead = p;
                            }
                            else
                            {
                                pTail.NextGoal = p;
                            }

                            pTail = p;
                            currClause = currClause.NextNode;
                        }

                        CallReturn callReturn = new CallReturn(saveGoal);
                        CallStack.Push(callReturn);

                        if (Reporting)
                        {
                            pTail.NextGoal = callReturn;
                            pTail = pTail.NextNode;
                        }

                        pTail.NextGoal = goalListHead.NextGoal;
                        goalListHead = pHead; // will never be a spypoint
                        findFirstClause = true;
                    }
                }
                else
                {
                    this.ExecutionDetails?.AfterUnify(CurrVarStack, currVarStackCount, false);

                    if (!(redo = CanBacktrack())) // unify failed - try backtracking
                    {
                        return false;
                    }
                }
            } // end of while

            return true;
        }

        private void InsertCutFail()
        {
            ClauseNode fail = new ClauseNode(BaseTerm.FAIL, null);
            fail.NextGoal = goalListHead.NextGoal;
            ClauseNode cut = new ClauseNode(BaseTerm.CUT, null) { NextGoal = fail };
            goalListHead = cut;
        }

        private bool CanBacktrack() // returns true if choice point was found
        {
            return CanBacktrack(true);
        }

        private bool CanBacktrack(bool local) // local = false if user wants more (so as not to trigger the debugger)
        {
            Object o;
            ChoicePoint cp;

            findFirstClause = false; // to prevent resetting to the first clause upon re-entering ExecuteGoalList

            while (CurrVarStack.Count != 0)
            {
                o = CurrVarStack.Pop();

                if (o is Variable)
                {
                    ((Variable)o).Unbind();
                }
                else if (o is ChoicePoint && ((ChoicePoint)o).IsActive)
                {
                    goalListHead = (cp = (ChoicePoint)o).GoalListHead; // this was the goal we wanted to prove ...

                    if (cp.NextClause == null) // no next predicate clause ...
                    {
                        findFirstClause = true; // ... so find predicate belonging to the goal last head
                    }
                    else
                    {
                        goalListHead.NextClause = cp.NextClause; // ... and this is next predicate clause to be tried
                    }

                    return true;
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

                        if (goalListHead.Term is CatchOpenTerm)
                        {
                            t = (CatchOpenTerm)goalListHead.Term;
                            status = Status.CompareIds;
                        }
                        else
                        {
                            status = Status.NextGoalNode;
                        }

                        break;

                    case Status.NextGoalNode:
                        goalListHead = goalListHead.NextGoal;
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

            redo = false;

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

        public void Consult(string fileName)
        {
            bool csharpStringsSave = CsharpStrings;
            // string as ISO-style charcode lists or as C# strings

            try
            {
                ConsultedFiles.Clear();
                PredTable.Consult(fileName);
            }
            finally
            {
                CsharpStrings = csharpStringsSave;
            }
        }

        public void Consult(Stream stream, string streamName = null)
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
            using (MemoryStream ms = new MemoryStream(Encoding.UTF8.GetBytes(prologCode)))
            {
                Consult(ms, codeTitle);
            }
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
            protected TermNode goalListHead;
            protected ClauseNode nextClause; // next clause to be tried for goalListHead

            public ChoicePoint(TermNode goal, ClauseNode nextClause)
            {
                goalListHead = goal;
                this.nextClause = nextClause;
                active = true;
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
            public CallReturn(TermNode goal)
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

        public class OpenFiles : Dictionary<string, FileTerm>
        {
            public FileTerm GetFileReader(string fileName)
            {
                FileTerm ft;

                TryGetValue(fileName.ToLower(), out ft);

                return ft;
            }

            public FileTerm GetFileWriter(string fileName)
            {
                FileTerm ft;

                TryGetValue(fileName.ToLower(), out ft);

                return ft;
            }

            public void CloseAllOpenFiles()
            {
                foreach (FileTerm ft in Values)
                {
                    ft.Close();
                }

                Clear();
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