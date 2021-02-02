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
using System.Text;
using static Prolog.PrologEngine;

namespace Prolog
{
    public class ExecutionDetails
    {


        public List<string> CallHistory { get; private set; } = new List<string>(1000);

        public string CallHistoryString => "\r\n" + string.Join("\r\n", this.CallHistory);

        public List<string> CurrentTermHistory { get; private set; } = new List<string>(1000);

        public string CurrentTermHistoryString => "\r\n" + string.Join("\r\n", this.CurrentTermHistory);

        public void Reset()
        {
            this.CallHistory.Clear();
            this.CurrentTermHistory.Clear();
        }

        internal void CurrentGoalBeforeUnify(TermNode goal, TermNode targetClause)
        {
            if ((goal.Head.Name == "true/0" && targetClause.Head.Name == "true/0")
                || (goal.Head.Name == "fail/0" && targetClause.Head.Name == "fail/0"))
            {
                this.CurrentTermHistory.Add($"?: {targetClause.Head} [ln " + goal.Head.Symbol.LineNoAdjusted + "]");
                return;
            }

            int line = targetClause.BuiltinId == BI.none ? targetClause.Head.Symbol.LineNoAdjusted : goal.Head.Symbol.LineNoAdjusted;
            string postfix = targetClause.NextNode == null ? "." : (targetClause.NextNode.Head == null ? "" : " :- ...");
            this.CurrentTermHistory.Add($"?: {goal} = {targetClause.Head}{postfix} [ln {line}]");
        }

        internal void AfterUnify(VarStack varStack, int varStackCountPreUnify, bool unified, bool isFailUnification)
        {
            if (!unified)
            {
                this.CurrentTermHistory.Add("   -> No");
            }
            else
            {
                StringBuilder yes = new StringBuilder(isFailUnification ? "   -> Fail" : "   -> Yes");
                if (varStackCountPreUnify < varStack.Count)
                {
                    yes.Append(": ");

                    object[] asArray = varStack.ToArray();
                    for (int i = varStack.Count - varStackCountPreUnify - 1; i >= 0; i--)
                    {
                        yes.Append((asArray[i] as BaseTerm).ToWriteString(0) + ", ");
                    }

                    yes.Remove(yes.Length - 2, 2);
                }

                this.CurrentTermHistory.Add(yes.ToString());
            }
        }

        internal void FactCall(int level, string goalListHead)
        {
            this.CallHistory.Add(new string(' ', level) + "Call: " + goalListHead);
        }

        internal void Exit(TermNode savedGoal)
        {
            this.CallHistory.Add(new string(' ', savedGoal.Level) + "Exit: " + savedGoal.Head);
        }

        internal void CallCall(CallReturn callReturn)
        {
            this.CallHistory.Add(new string(' ', callReturn.SavedGoal.Level) + "Call: " + callReturn.SavedGoal.Head);
        }

        internal void FailCall(int level, string goalListHead)
        {
            this.CallHistory.Add(new string(' ', level) + "Call: " + goalListHead);
        }

        internal void PredicateRuleCall(CallReturn callReturn)
        {
            this.CallHistory.Add(new string(' ', callReturn.SavedGoal.Level) + "Call: " + callReturn.SavedGoal.Head);
        }

        internal void Failed(TermNode saveGoal)
        {
            this.CallHistory.Add(new string(' ', saveGoal.Level) + "Fail: " + saveGoal.Head);
        }

        internal void Failed(int level, string saveGoal)
        {
            this.CallHistory.Add(new string(' ', level) + "Fail: " + saveGoal);
        }

        internal void BuiltInCall(TermNode saveGoal)
        {
            this.CallHistory.Add(new string(' ', saveGoal.Level) + "Call: " + saveGoal.Head);
        }

        internal void Redo(TermNode callerGoal)
        {
            this.CallHistory.Add(new string(' ', callerGoal.Level) + "Redo: " + callerGoal.Head);
        }

        internal void NextSolution(TermNode prevGoal)
        {
            this.CallHistory.Add(new string(' ', prevGoal.Level) + "Next: " + prevGoal.Head);
        }
    }
}