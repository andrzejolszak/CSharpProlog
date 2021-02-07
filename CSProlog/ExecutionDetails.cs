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
using System.Linq;
using static Prolog.PrologEngine;
using static Prolog.PrologEngine.BaseParser;

namespace Prolog
{
    public class ExecutionDetails
    {


        public List<(string, Symbol)> CallHistory { get; private set; } = new List<(string, Symbol)>(1000);

        public string CallHistoryString => string.Join(Environment.NewLine, this.CallHistory.Select(x => x.Item1));

        public string CallHistoryStringWithLines => string.Join(Environment.NewLine, this.CallHistory.Select(x => x.Item1 + " [ln " + x.Item2.LineNoAdjusted + "]"));

        public string CallHistoryStringWithLinesLast10 => $"...{Environment.NewLine}" + string.Join(Environment.NewLine, this.CallHistory.TakeLast(10).Select(x => x.Item1.Trim() + " [ln " + x.Item2.LineNoAdjusted + "]"));

        public void Reset()
        {
            this.CallHistory.Clear();
        }

        internal void FactCall(int level, string goalListHead, Symbol symbol)
        {
            this.CallHistory.Add((new string(' ', level) + "Call: " + goalListHead, symbol));
        }

        internal void Exit(TermNode savedGoal)
        {
            this.CallHistory.Add((new string(' ', savedGoal.Level) + "Exit: " + savedGoal.Head, savedGoal.Head.Symbol));
        }

        internal void CallCall(CallReturn callReturn)
        {
            this.CallHistory.Add((new string(' ', callReturn.SavedGoal.Level) + "Call: " + callReturn.SavedGoal.Head, callReturn.SavedGoal.Head.Symbol));
        }

        internal void FailCall(int level, string goalListHead, Symbol symbol)
        {
            this.CallHistory.Add((new string(' ', level) + "Call: " + goalListHead, symbol));
        }

        internal void PredicateRuleCall(CallReturn callReturn)
        {
            this.CallHistory.Add((new string(' ', callReturn.SavedGoal.Level) + "Call: " + callReturn.SavedGoal.Head, callReturn.SavedGoal.Head.Symbol));
        }

        internal void Failed(TermNode saveGoal)
        {
            this.CallHistory.Add((new string(' ', saveGoal.Level) + "Fail: " + saveGoal.Head, saveGoal.Head.Symbol));
        }

        internal void Failed(int level, string saveGoal, Symbol symbol)
        {
            this.CallHistory.Add((new string(' ', level) + "Fail: " + saveGoal, symbol));
        }

        internal void BuiltInCall(TermNode saveGoal)
        {
            this.CallHistory.Add((new string(' ', saveGoal.Level) + "Call: " + saveGoal.Head, saveGoal.Head.Symbol));
        }

        internal void Redo(TermNode callerGoal)
        {
            this.CallHistory.Add((new string(' ', callerGoal.Level) + "Redo: " + callerGoal.Head, callerGoal.Head.Symbol));
        }

        internal void NextSolution(TermNode prevGoal)
        {
            this.CallHistory.Add((new string(' ', prevGoal.Level) + "Next: " + prevGoal.Head, prevGoal.Head.Symbol));
        }
    }
}