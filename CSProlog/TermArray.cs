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

// Arrays: not in this version. The whole concept needs to be thought over more carefully.

namespace Prolog
{
    public partial class PrologEngine
    {
        // untested code
    /*    public class TermArray
        {
            private int[] dimensions;
            // Any array (i.e. with an arbitrary number of subscripts) is mapped to a one-dimensional 
            // one (baseArray). In doing so, it is possible to accomodate arrays with an arbitrary 
            // number of subscripts.
            private BaseTerm[] baseArray;
            public int Rank => dimensions.Length;
            public string Name { get; }

            // TODO: remove
 /*           public TermArray(string name, int[] dimensions)
            {
                this.Name = name;
                this.dimensions = dimensions;
                int length = 1;

                for (int i = 0; i < dimensions.Length; i++)
                {
                    if (dimensions[i] <= 0)
                        IO.ErrorRuntime( "Dimension {0} of array '{1}' has illegal value {2}", i, name, dimensions[i]);

                    length *= dimensions[i];
                }

                baseArray = new BaseTerm[length];
            }
            
            public BaseTerm GetEntry(int[] subscripts) // subscripts are zero-based
            {
                return baseArray[CalculateOffset(subscripts)];
            }

            public void SetEntry(int[] subscripts, BaseTerm t)
            {
                baseArray[CalculateOffset(subscripts)] = t;
            }

            // CalculateOffset calculates the mapping of [i1, i2, ..., iN] to the index in this 1-D array
            private int CalculateOffset(int[] subscripts) // f(i1, i2, ..., iN) => 0 .. d1*d2*...*dN-1
            {
                for (int i = 0; i < subscripts.Length; i++)
                    if (subscripts[i] < 0 || subscripts[i] >= dimensions[i])
                        IO.ErrorRuntime(string.Format("Value of index {0} is {1} but must be in the range 0..{2}",
                          i, subscripts[i], dimensions[i] - 1), null, null);

                int offset = subscripts[0];

                for (int i = 1; i < subscripts.Length; i++)
                    offset = offset * dimensions[i] + subscripts[i];

                return offset;
            }
        }
    */
        /*public class ArrayVariable : NamedVariable
        {
            private TermArray ta;
            private List<BaseTerm> subscripts;

            public ArrayVariable(Symbol symbol, string name, TermArray ta, ListTerm subscripts)
                : base(symbol, ta.Name)
            {
                this.ta = ta;
                this.name = ta.Name;
                this.subscripts = subscripts.ToTermList();

                if (this.subscripts.Count != ta.Rank)
                    IO.ErrorRuntime("Wrong number of subscripts for '{0}': expected {1}, got {2}",
                      name, ta.Rank, this.subscripts.Count);
            }

            public override string ToWriteString(int level)
            {
                StringBuilder sb = new StringBuilder(name);
                bool first = true;

                foreach (BaseTerm t in subscripts)
                {
                    if (first) first = false; else sb.Append(CommaAtLevel(level));

                    sb.Append((t.IsGround ? t.Eval() : t).ToString()); // at end of query vs. any other situation
                }

                return sb.ToString();
            }
        }*/

    }
}
