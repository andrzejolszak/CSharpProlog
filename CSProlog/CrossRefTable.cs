using System;
using System.Collections.Generic;

namespace Prolog
{
    /* _______________________________________________________________________________________________
      |                                                                                               |
      |  C#Prolog -- Copyright (C) 2007-2014 John Pool -- j.pool@ision.nl                             |
      |                                                                                               |
      |  This library is free software; you can redistribute it and/or modify it under the terms of   |
      |  the GNU General Public License as published by the Free Software Foundation; either version  |
      |  2 of the License, or any later version.                                                      |
      |                                                                                               |
      |  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;    |
      |  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    |
      |  See the GNU General Public License for details, or enter 'license.' at the command prompt.   |
      |_______________________________________________________________________________________________|
    */

    // PrologParser Generator version 4.0 -- Date/Time: 22-12-2010 8:42:54

    public partial class PrologEngine
    {
        // CrossRefTable implements a cross reference table for the predicates making up the 'program'.
        // Each predicate corresponds to a row. The entries in the row are the predicates that are called.
        // CalculateClosure () calculates the indirect calls (i.e. if A calls B and B calls C directly,
        // then the closure would calculate the indirect call A -> C.
        // Direct calls have an entry value 'false', indirect calls an entry value 'true'.
        public class CrossRefTable : Dictionary<Tuple<PredicateDescr, PredicateDescr>, bool?>
        {
            private readonly List<PredicateDescr> axis; // predicate names sorted alphabetically
            private bool? result;

            public CrossRefTable()
            {
                axis = new List<PredicateDescr>();
                ReverseDirectRefIndex = new Dictionary<PredicateDescr, List<PredicateDescr>>();
            }

            public Dictionary<PredicateDescr, List<PredicateDescr>> ReverseDirectRefIndex { get; }

            private int dimension => axis.Count;

            // used only when registering the direct calls, not for the closure (indirect calls)
            public bool? this[PredicateDescr row, PredicateDescr col]
            {
                set
                {
                    this[CompoundKey(row, col)] = false; // i.e. a direct call

                    if (!ReverseDirectRefIndex.ContainsKey(col))
                    {
                        ReverseDirectRefIndex[col] = new List<PredicateDescr>();
                    }

                    ReverseDirectRefIndex[col].Add(row);
                }

                get
                {
                    if (TryGetValue(CompoundKey(row, col), out result))
                    {
                        return result;
                    }

                    return null;
                }
            }

            // used only when calculating the indirect calls (CalculateClosure)
            public bool? this[int i, int j]
            {
                set
                {
                    Tuple<PredicateDescr, PredicateDescr> key = CompoundKey(axis[i], axis[j]);
                    bool? result;

                    // do not overwrite 'false', as this would hide the fact that a direct call exists
                    if (!TryGetValue(key, out result))
                    {
                        this[key] = true;
                    }
                }

                get
                {
                    if (TryGetValue(CompoundKey(axis[i], axis[j]), out result))
                    {
                        return result;
                    }

                    return null;
                }
            }

            public void Reset()
            {
                Clear();
                axis.Clear();
                ReverseDirectRefIndex.Clear();
            }

            public void AddPredicate(PredicateDescr pd)
            {
                // add predicate only if not already present
                int i = axis.BinarySearch(pd);

                if (i < 0)
                {
                    axis.Insert(~i, pd); // keep range sorted
                }
            }

            // Warshall algorithm -- Journal of the ACM, Jan. 1962, pp. 11-12
            // This algorithm calculates the indirect calls.
            public void CalculateClosure()
            {
                int i, j, k;

                for (i = 0; i < dimension; i++)
                    for (j = 0; j < dimension; j++)
                    {
                        if (this[j, i] != null)
                        {
                            for (k = 0; k < dimension; k++)
                            {
                                if (this[i, k] != null)
                                {
                                    this[j, k] = true; // 'true' to indicate entry is indirect call (result of closure)
                                }
                            }
                        }
                    }
            }

            private Tuple<PredicateDescr, PredicateDescr> CompoundKey(PredicateDescr row, PredicateDescr col)
            {
                return Tuple.Create(row, col);
            }
        }

        public partial class PredicateDescr : IComparable<PredicateDescr>
        {
            public int CompareTo(PredicateDescr pd)
            {
                int result = Functor.CompareTo(pd.Functor);

                if (result == 0)
                {
                    return Arity.CompareTo(pd.Arity);
                }

                return result;
            }

            protected bool Equals(PredicateDescr other)
            {
                return string.Equals(Functor, other.Functor) && Arity == other.Arity &&
                       IsPredefined == other.IsPredefined && string.Equals(Module, other.Module) &&
                       string.Equals(DefinitionFile, other.DefinitionFile);
            }

            public override bool Equals(object obj)
            {
                if (ReferenceEquals(null, obj))
                {
                    return false;
                }

                if (ReferenceEquals(this, obj))
                {
                    return true;
                }

                if (obj.GetType() != GetType())
                {
                    return false;
                }

                return Equals((PredicateDescr)obj);
            }

            public override int GetHashCode()
            {
                unchecked
                {
                    int hashCode = Functor != null ? Functor.GetHashCode() : 0;
                    hashCode = (hashCode * 397) ^ Arity;
                    hashCode = (hashCode * 397) ^ IsPredefined.GetHashCode();
                    hashCode = (hashCode * 397) ^ (Module != null ? Module.GetHashCode() : 0);
                    hashCode = (hashCode * 397) ^ (DefinitionFile != null ? DefinitionFile.GetHashCode() : 0);
                    return hashCode;
                }
            }
        }
    }
}