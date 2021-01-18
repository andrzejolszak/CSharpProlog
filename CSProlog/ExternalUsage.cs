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

namespace Prolog
{
    public class SolutionSet
    {
        private readonly List<Solution> solutionSet;
        private Solution currVarSet;

        public SolutionSet()
        {
            solutionSet = new List<Solution>();
            Success = false;
            ErrMsg = null;
        }

        public bool Success { get; internal set; }

        public string ErrMsg { get; internal set; }

        public bool HasError => ErrMsg != null;

        public List<PrologEngine.PrologException> Errors { get; } = new List<PrologEngine.PrologException>();

        public Solution this[int i] => solutionSet[i];

        internal void CreateVarSet()
        {
            solutionSet.Add(currVarSet = new Solution());
        }

        internal void AddToVarSet(string name, string type, string value)
        {
            currVarSet.Add(name, type, value);
            Success = true;
        }

        public override string ToString()
        {
            if (ErrMsg != null)
            {
                return ErrMsg;
            }

            if (Success)
            {
                if (solutionSet.Count == 0)
                {
                    return "yes";
                }

                StringBuilder sb = new StringBuilder();
                int i = 0;
                foreach (Solution s in solutionSet)
                {
                    sb.AppendLine("Solution {0}:\r\n{1}", ++i, s.ToString());
                }

                return sb.ToString();
            }

            return "no";
        }
    }

    public class Solution // a solution is a set of variables
    {
        private readonly List<Variable> variables;

        public Solution()
        {
            variables = new List<Variable>();
        }

        public IEnumerable<Variable> NextVariable
        {
            get
            {
                foreach (Variable v in variables)
                {
                    yield return v;
                }
            }
        }

        internal void Add(string name, string type, string value)
        {
            variables.Add(new Variable(name, type, value));
        }

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();

            foreach (Variable v in variables)
            {
                sb.AppendLine(v.ToString());
            }

            return sb.ToString();
        }
    }

    public class Variable
    {
        public Variable(string name, string type, string value)
        {
            Name = name;
            Type = type;
            Value = value;
        }

        public string Name { get; }
        public string Type { get; }
        public string Value { get; }

        public override string ToString()
        {
            return $"{Name} ({Type}) = {Value}";
        }
    }

    public partial class PrologEngine
    {
        // Store solutions in an GetAllSolutions class
        public SolutionSet GetAllSolutions(string query)
        {
            return GetAllSolutions(query, 0);
        }

        public SolutionSet GetAllSolutions(string query, int maxSolutionCount)
        {
            Error = false;
            Halted = false;

            SolutionSet solutions = new SolutionSet();
            this.ExecutionDetails?.Reset();

            try
            {
                Query = query + (query.EndsWith(".") ? null : "."); // append a dot if necessary
                int i = 0;
                bool found = false;
                bool varFound = false;

                foreach (Solution s in SolutionIterator)
                {
                    if (Error)
                    {
                        if (s.Error != null)
                        {
                            solutions.Errors.Add(s.Error);
                        }

                        solutions.ErrMsg = s.ToString();

                        break;
                    }

                    if (!found && !s.Solved)
                    {
                        break;
                    }

                    solutions.Success = true;
                    bool firstVar = true;

                    foreach (IVarValue varValue in s.VarValuesIterator)
                    {
                        if (varValue.DataType == "none")
                        {
                            break;
                        }

                        if (firstVar)
                        {
                            firstVar = false;
                            solutions.CreateVarSet();
                        }

                        solutions.AddToVarSet(varValue.Name, varValue.DataType, varValue.Value.ToString());
                        varFound = true;
                    }

                    if (++i == maxSolutionCount || !varFound)
                    {
                        break;
                    }

                    found = true;
                }
            }
            catch (Exception e)
            {
                solutions.ErrMsg = e.Message;
            }

            return solutions;
        }
    }
}