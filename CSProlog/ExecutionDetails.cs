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
using System.Text;
using static Prolog.PrologEngine;

namespace Prolog
{
    public class ExecutionDetails
    {
        public event CurrentTerm OnCurrentTermChanged;

        public List<string> CurrentTermHistory { get; private set; } = new List<string>(1000);

        public string CurrentTermHistoryString => "\r\n" + string.Join("\r\n", this.CurrentTermHistory);

        public void Reset()
        {
            this.CurrentTermHistory.Clear();
        }

        public void CurrentGoalBeforeUnify(TermNode goal, TermNode targetClause)
        {
            this.CurrentTermHistory.Add($"?: {goal} = {targetClause.Head}");
            this.OnCurrentTermChanged?.Invoke(targetClause);
        }

        public void AfterUnify(VarStack varStack, int varStackCountPreUnify, bool unified)
        {
            if (!unified)
            {
                this.CurrentTermHistory.Add("   -> No");
            }
            else
            {
                StringBuilder yes = new StringBuilder("   -> Yes");
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
    }
}