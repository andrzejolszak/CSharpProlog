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
using System.IO;
using System.Text;

namespace Prolog
{
        public class SolutionSet
    {
        public string Query { get; internal set; }

        public bool Success { get; internal set; }

        public string ErrMsg { get; internal set; }

        public bool HasError => ErrMsg != null;
        private List<Solution> solutionSet;
        public List<PrologEngine.RuntimeException> Errors { get; } = new List<PrologEngine.RuntimeException>();

        private Solution currVarSet;

        public SolutionSet()
        {
            solutionSet = new List<Solution>();
            Success = false;
            ErrMsg = null;
        }

        internal void CreateVarSet()
        {
            solutionSet.Add(currVarSet = new Solution());
        }

        internal void AddToVarSet(string name, string type, string value)
        {
            currVarSet.Add(name, type, value);
            Success = true;
        }

        public IEnumerable<Solution> NextSolution
        {
            get
            {
                foreach (Solution s in solutionSet)
                    yield return s;
            }
        }

        public Solution this[int i] => solutionSet[i];

        public override string ToString()
        {
            if (ErrMsg != null)
                return ErrMsg;

            if (Success)
            {
                if (solutionSet.Count == 0)
                    return "yes";
                else
                {
                    StringBuilder sb = new StringBuilder();
                    int i = 0;
                    foreach (Solution s in solutionSet)
                        sb.AppendLine("Solution {0}:\r\n{1}", ++i, s.ToString());

                    return sb.ToString();
                }
            }
            else
                return "no";
        }
    }
    
        public class Solution // a solution is a set of variables
    {
        private List<Variable> variables;

        public Solution()
        {
            variables = new List<Variable>();
        }

        internal void Add(string name, string type, string value)
        {
            variables.Add(new Variable(name, type, value));
        }

        public IEnumerable<Variable> NextVariable
        {
            get
            {
                foreach (Variable v in variables)
                    yield return v;
            }
        }

        public Variable this[int i] => variables[i];

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();

            foreach (Variable v in variables)
                sb.AppendLine(v.ToString());

            return sb.ToString();
        }
    }
    
        public class Variable
    {
        public string Name { get; }
        public string Type { get; }
        public string Value { get; }

        public Variable(string name, string type, string value)
        {
            this.Name = name;
            this.Type = type;
            this.Value = value;
        }

        public override string ToString()
        {
            return $"{Name} ({Type}) = {Value}";
        }
    }
    
    public partial class PrologEngine
    {
        // Store solutions in an GetAllSolutions class
        public SolutionSet GetAllSolutions(string sourceFileName, string query)
        {
            return GetAllSolutions(sourceFileName, query, 0);
        }

        public SolutionSet GetAllSolutions(string sourceFileName, string query, int maxSolutionCount)
        {
            error = false;
            Halted = false;

            SolutionSet solutions = new SolutionSet();

            try
            {
                if (sourceFileName != null) Consult(sourceFileName);

                Query = solutions.Query = query + (query.EndsWith(".") ? null : "."); // append a dot if necessary
                int i = 0;
                bool found = false;
                bool varFound = false;

                foreach (PrologEngine.Solution s in SolutionIterator)
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
                    else if (!found && !s.Solved)
                        break;

                    solutions.Success = true;
                    bool firstVar = true;

                    foreach (PrologEngine.IVarValue varValue in s.VarValuesIterator)
                    {
                        if (varValue.DataType == "none") break;

                        if (firstVar)
                        {
                            firstVar = false;
                            solutions.CreateVarSet();
                        }

                        solutions.AddToVarSet(varValue.Name, varValue.DataType, varValue.Value.ToString());
                        varFound = true;
                    }

                    if (++i == maxSolutionCount || !varFound) break;

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
