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

#define old // old: working correctly, but still under construction

using System;
using System.Collections.Generic;
using System.Text;
using static Prolog.PrologEngine.BaseParser;

namespace Prolog
{
    public partial class PrologEngine
    {
        public class BaseRepFactor
        {
            protected BaseRepFactor(BaseTerm bindVar, BaseTerm minLenTerm, BaseTerm maxLenTerm)
            {
                this.bindVar = (Variable)bindVar;
                this.minLenTerm = minLenTerm;
                this.maxLenTerm = maxLenTerm;
            }

            public Variable bindVar { get; }
            public BaseTerm minLenTerm { get; }
            public BaseTerm maxLenTerm { get; }
            public int MinRangeLen => minLenTerm.IsVar ? 0 : minLenTerm.To<int>();
            public int MaxRangeLen => maxLenTerm.IsVar ? PrologParser.Infinite : maxLenTerm.To<int>();

            public override string ToString()
            {
                string range = null;

                if (MinRangeLen == MaxRangeLen)
                {
                    if (MinRangeLen == 0)
                    {
                        range = null; // {0,0} => {}
                    }
                    else
                    {
                        range = $"{{{minLenTerm}}}";
                    }
                }
                else if (minLenTerm.IsInteger)
                {
                    int minLen = minLenTerm.To<int>();

                    if (MaxRangeLen == PrologParser.Infinite)
                    {
                        if (minLen == 0)
                        {
                            range = "{*}";
                        }
                        else if (minLen == 1)
                        {
                            range = "{+}";
                        }
                    }
                    else if (minLen == 0 && maxLenTerm.IsInteger && maxLenTerm.To<int>() == 1)
                    {
                        range = "{?}";
                    }
                }
                else if (MinRangeLen == 0 && !minLenTerm.IsVar)
                {
                    if (MaxRangeLen == PrologParser.Infinite && !maxLenTerm.IsVar)
                    {
                        range = PrologParser.ELLIPSIS; // any length, {0,Infinite} => '..'
                    }
                    else
                    {
                        range = $"{{,{maxLenTerm}}}";
                    }
                }
                else if (MaxRangeLen == PrologParser.Infinite && !maxLenTerm.IsVar)
                {
                    range = $"{{{minLenTerm},}}";
                }
                else if (minLenTerm.IsVar && maxLenTerm.IsVar && // both symbolic, and identical
                         ((Variable)minLenTerm).IsUnifiedWith((Variable)maxLenTerm))
                {
                    range = $"{{{minLenTerm}}}";
                }

                if (range == null)
                {
                    range = $"{{{minLenTerm},{maxLenTerm}}}";
                }

                if (bindVar != null)
                {
                    if (range == null)
                    {
                        range = bindVar.ToString();
                    }
                    else
                    {
                        range = $"{bindVar}!{range}";
                    }
                }

                return range;
            }
        }

        public class ListPatternElem : CompoundTerm
        {
            private const int TARGETOFFSET = 4;
            public override bool IsEvaluatable => false;
#if old
            public bool IsNegSearch { get; }
            public BaseTerm MinLenTerm => CompoundArgs[0];
            public BaseTerm MaxLenTerm => CompoundArgs[1];
            public BaseTerm RangeBindVar => CompoundArgs[2];
            public int MinRangeLen => MinLenTerm.IsVar ? 0 : MinLenTerm.To<int>();
            public int MaxRangeLen => MaxLenTerm.IsVar ? PrologParser.Infinite : MaxLenTerm.To<int>();
#else
      public bool IsNegSearch { get { return altSearchTerms.isNegSearch; } }
      public BaseTerm MinLenTerm { get { return acrossRepFactor.minLenTerm; } }
      public BaseTerm MaxLenTerm { get { return acrossRepFactor.maxLenTerm; } }
      public BaseTerm RangeBindVar { get { return acrossRepFactor.bindVar; } }
      public bool HasDownRepFactor { get { return downRepFactor == null; } }
      public int MinRangeLen { get { return acrossRepFactor.MinRangeLen; } }
      public int MaxRangeLen { get { return acrossRepFactor.MaxRangeLen; } }
      public bool HasVariableLength { get { return acrossRepFactor.HasVariableLength; } }
#endif
            public BaseTerm AltListBindVar => CompoundArgs[3];

            public BaseTerm[] AltSearchTerms => CompoundArgs;

            // no simple way to return args[4..args.Length-1]
            public bool HasAltListBindVar => CompoundArgs[3] != null;

            public bool HasSearchTerm => CompoundArgs[4] != null;

            // MinRangeTermLen: the minimum number of elements that a match must have, i.e. the
            // sum of the MinRangeLen + (1 if there is a SearchTerm, 0 otherwise)
            public int MinRangeTermLen => MinRangeLen + (HasSearchTerm ? 1 : 0);

            // MaxRangeTermLen: analogous
            public int MaxRangeTermLen => SaveAdd(MaxRangeLen, HasSearchTerm ? 1 : 0);

            // MinRemMatchLen and MaxRemMatchLen are calculated on the fly during the matching process
            /*
                  // For future enhancement (don't forget CopyEx())
                  public ListPatternElem (DownRepFactor downRepFactor, DisjunctiveSearchTerm altSearchTerms, AcrossRepFactor acrossRepFactor)
                    : base ("RANGE")  // used by CopyEx
                  {
                    this.downRepFactor = downRepFactor;
                    this.altSearchTerms = altSearchTerms;
                    this.acrossRepFactor = acrossRepFactor;

                    int n = TARGETOFFSET + (altSearchTerms == null ? 1 : altSearchTerms.Count);
                    args = new BaseTerm [n];

                    args [0] = HasAcrossRepFactor ? acrossRepFactor.minLenTerm : DecimalTerm.ZERO;
                    args [1] = HasAcrossRepFactor ? acrossRepFactor.maxLenTerm : DecimalTerm.ZERO;
                    args [2] = HasAcrossRepFactor ? acrossRepFactor.bindVar : null;
                    args [3] = HasAltSearchTerms ? altSearchTerms.bindVar : null;

                    int i = TARGETOFFSET;

                    if (altSearchTerms != null)
                      foreach (BaseTerm t in altSearchTerms.alternatives)
                        args [i++] = t;

                    isNegSearch = (altSearchTerms == null) ? false : altSearchTerms.isNegSearch;
                  }
            */

            public ListPatternElem(Symbol symbol, BaseTerm[] a, bool isNegSearch)
                : base(symbol, "RANGE", a) // used by CopyEx
            {
#if old
                IsNegSearch = isNegSearch;
#endif
            }

            public ListPatternElem(Symbol symbol, BaseTerm minLenTerm, BaseTerm maxLenTerm, BaseTerm rangeBindVar,
                BaseTerm altListVar, List<SearchTerm> altSearchTerms, bool isNegSearch, bool hasDownRepFactor)
                : base(symbol)
            {
                int n = TARGETOFFSET + (altSearchTerms == null ? 1 : altSearchTerms.Count);
                CompoundArgs = new BaseTerm[n];
                CompoundArgs[0] = minLenTerm;
                CompoundArgs[1] = maxLenTerm;
                CompoundArgs[2] = rangeBindVar;
                CompoundArgs[3] = altListVar;

                int i = TARGETOFFSET;

                if (altSearchTerms != null)
                {
                    foreach (SearchTerm t in altSearchTerms)
                    {
                        CompoundArgs[i++] = t.term;
                    }
                }

#if old
                IsNegSearch = isNegSearch;
#endif
            }

            public override bool Unify(BaseTerm t, VarStack varStack)
            {
                varStack.NextUnifyCount();

                if (!((t = t.ChainEnd()) is ListPatternElem))
                {
                    return false; // should never occur
                }
#if old
                if (IsNegSearch != ((ListPatternElem)t).IsNegSearch)
                {
                    return false;
                }
#endif
                for (int i = 0; i < arity; i++)
                {
                    if (!((CompoundArgs[i] == null && t.Args[i] == null) ||
                          CompoundArgs[i].Unify(t.Args[i], varStack)))
                    {
                        return false;
                    }
                }

                return true;
            }

            public static int
                SaveAdd(int m, int n) // n, m > 0; adding without overflow. n+m <= inf -> n <= inf-m must hold
            {
                return n > PrologParser.Infinite - m ? PrologParser.Infinite : n + m;
            }

            public override string ToWriteString(int level)
            {
#if old
                StringBuilder sb = new StringBuilder(IsNegSearch ? "~" : null);
#else
        StringBuilder sb = new StringBuilder ();
#endif

                string range;

                if (MinRangeLen == MaxRangeLen)
                {
                    if (MinRangeLen == 0)
                    {
                        range = null; // {0,0} => {}
                    }
                    else
                    {
                        range = $"{{{MinLenTerm}}}";
                    }
                }
                else if (MinRangeLen == 0 && !MinLenTerm.IsVar)
                {
                    if (MaxRangeLen == PrologParser.Infinite && !MaxLenTerm.IsVar)
                    {
                        range = PrologParser.ELLIPSIS; // any length, {0,Infinite} => '..'
                    }
                    else
                    {
                        range = $"{{,{MaxLenTerm}}}";
                    }
                }
                else if (MaxRangeLen == PrologParser.Infinite && !MaxLenTerm.IsVar)
                {
                    range = $"{{{MinLenTerm},}}";
                }
                else if (MinLenTerm.IsVar && MaxLenTerm.IsVar && // both symbolic, and identical
                         ((Variable)MinLenTerm).IsUnifiedWith((Variable)MaxLenTerm))
                {
                    range = $"{{{MinLenTerm}}}";
                }
                else
                {
                    range = $"{{{MinLenTerm},{MaxLenTerm}}}";
                }

                if (RangeBindVar != null)
                {
                    if (range == null)
                    {
                        range = RangeBindVar.ToString();
                    }
                    else
                    {
                        range = $"{RangeBindVar}{range}";
                    }
                }

                if (!HasSearchTerm)
                {
                    return range;
                }

                if (HasAltListBindVar)
                {
                    sb.AppendFormat("{0}!", AltListBindVar);
                }

                sb.Append(Arg(TARGETOFFSET));

                for (int k = TARGETOFFSET + 1; k < Arity; k++)
                {
                    sb.AppendFormat("|{0}", Arg(k));
                }

                if (range == null)
                {
                    return sb.ToString();
                }

                return $"{range},{SpaceAtLevel(level)}{sb}";
            }

            public override void TreePrint(int level, PrologEngine e)
            {
                e.WriteLine("0}{1}", Spaces(2 * level), this);
            }
        }

        public class ListPatternTerm : AltListTerm
        {
            private BaseTerm[] pattern; // rangeTerms; pattern is searched ...
            private List<BaseTerm> target; // ... in target

            public ListPatternTerm(Symbol symbol, BaseTerm[] a)
                : base(symbol, PrologParser.LISTPATOPEN, PrologParser.LISTPATCLOSE, a)
            {
            }

            public override bool IsProperList => false;
            public override bool IsPartialList => false;
            public override bool IsPseudoList => false;
            public override bool IsProperOrPartialList => false;
            public override bool IsEvaluatable => false;

            public override bool Unify(BaseTerm t, VarStack varStack)
            {
                if ((t = t.ChainEnd()) is Variable) // t not unified
                {
                    ((Variable)t).Bind(this, varStack);
                    varStack.Push(t);

                    return true;
                }

                if (t is ListPatternTerm && arity == t.Arity) // two ListPatternTerms match if their rangeTerms match
                {
                    for (int i = 0; i < arity; i++)
                    {
                        if (!CompoundArgs[i].Unify(t.Args[i], varStack))
                        {
                            return false;
                        }
                    }

                    return true;
                }

                if (t is ListTerm)
                {
                    pattern = CompoundArgs; // pattern is searched ...
                    target = ((ListTerm)t).ToList(); // ... in target
                    int ip = 0;
                    int it = 0;

                    return UnifyTailEx(ip, it, varStack);
                }

                return false;
            }

            // each call processes one element of pattern[]
            private bool UnifyTailEx(int ip, int it, VarStack varStack)
            {
                ListPatternElem e = (ListPatternElem)pattern[ip];
                Variable rangeSpecVar = (Variable)e.RangeBindVar;
                NodeIterator subtreeIterator;
                int k;
                int marker;
                ListTerm RangeList = null;
                BaseTerm tail = null;
                int minLen; // minimum required range length (number of range elements)
                int maxLen; // maximum possible ...

                if (!DoLowerAndUpperboundChecks(ip, it, out minLen, out maxLen))
                {
                    return false;
                }

                // scan the minimal number of range elements. Add them to the range list,
                // i.e. the last variable (if present) preceding the '{ , }'
                for (int i = 0; i < minLen; i++)
                {
                    if ((k = it + i) >= target.Count)
                    {
                        return false;
                    }

                    AppendToRangeList(ref RangeList, rangeSpecVar, target[k], ref tail);
                }

                marker = varStack.Count; // register the point to which we must possibly undo unifications

                if (e.HasSearchTerm)
                {
                    // scan the elements up to the maximum range length, and the element immediately thereafter
                    for (int i = minLen; i <= maxLen; i++)
                    {
                        BaseTerm t = null;
                        k = it + i;

                        if (k == target.Count)
                        {
                            return false;
                        }

                        bool negSearchSucceeded = true; // iff none of the alternative term matches the target term

                        for (int j = 4;
                            j < e.Args.Length;
                            j++) // scan all AltSearchTerm alternatives (separated by '|')
                        {
                            BaseTerm searchTerm = e.AltSearchTerms[j];
                            t = target[k];
                            AltLoopStatus status = AltLoopStatus.TryNextAlt;

                            status =
                                TryOneAlternative(ip, varStack, e, k, marker, RangeList, i, t, ref negSearchSucceeded,
                                    searchTerm);

                            if (status == AltLoopStatus.MatchFound)
                            {
                                return true;
                            }

                            if (status == AltLoopStatus.Break)
                            {
                                break;
                            }
                        }

                        // at this point sufficient alternatives have been tried
                        if (e.IsNegSearch && negSearchSucceeded) // none of the terms matched => ok if binding succeeds
                        {
                            if (!TryBindingAltListVarToMatch(e.AltListBindVar, t,
                                    varStack) || // bind the AltListBindVar to the match
                                !TryBindingRangeRelatedVars(e, i, RangeList, varStack)
                            ) // bind the range to the range variables
                            {
                                return false; // binding failed
                            }

                            if (ip == pattern.Length - 1) // this was the last pattern element
                            {
                                if (k == target.Count - 1) // both pattern and target exhausted
                                {
                                    return true;
                                }
                            }
                            else if (UnifyTailEx(ip + 1, k + 1, varStack)) // now deal with the rest
                            {
                                return true;
                            }
                        }
                        else if (i < maxLen) // append the rejected term to the range list and go try the next term
                        {
                            AppendToRangeList(ref RangeList, rangeSpecVar, t, ref tail);
                        }
                    }
                }
                else // a range without a subsequent search term (so followed by another range or end of pattern)
                {
                    for (int i = minLen; i <= maxLen; i++)
                    {
                        k = it + i;

                        if (k == target.Count) // ok, target[k] does not exist, end of target hit
                        {
                            return TryBindingRangeRelatedVars(e, i, RangeList, varStack); // i is actual range length
                        }

                        // k is ok
                        if (i < maxLen)
                        {
                            AppendToRangeList(ref RangeList, rangeSpecVar, target[k], ref tail);
                        }

                        // now deal with the rest
                        if (TryBindingRangeRelatedVars(e, i, RangeList, varStack) &&
                            UnifyTailEx(ip + 1, k, varStack))
                        {
                            return true;
                        }
                    }
                }

                // If we arrive here, no matching term was found in or immediately after the permitted range.
                // Therefore, undo any unifications made locally in this method and return with failure.
                if (marker != 0)
                {
                    UnbindToMarker(varStack, marker);
                }

                return false;
            }

            private AltLoopStatus TryOneAlternative(int ip, VarStack varStack, ListPatternElem e, int k,
                int marker, ListTerm RangeList, int i, BaseTerm t, ref bool negSearchSucceeded, BaseTerm searchTerm)
            {
                bool unified = searchTerm.Unify(t, varStack);

                if (e.IsNegSearch) // none of the terms in the inner loop may match. ~(a | b | c) = ~a & ~b & ~c
                {
                    if (unified) // ... no point in investigating the other alternatives if one does
                    {
                        negSearchSucceeded = false;

                        return AltLoopStatus.Break; // don't try the other alternatives
                    }

                    return AltLoopStatus.TryNextDown; // non of the downranges matches may lead to a success
                }

                if (unified && // we found a match. Unify, and
                    TryBindingAltListVarToMatch(e.AltListBindVar, t,
                        varStack) && // bind the AltListBindVar to the match
                    TryBindingRangeRelatedVars(e, i, RangeList, varStack)) // bind the range to the range variables
                {
                    if (ip == pattern.Length - 1) // this was the last pattern element
                    {
                        if (k == target.Count - 1) // both pattern and target exhausted
                        {
                            return AltLoopStatus.MatchFound;
                        }
                    }
                    else if (UnifyTailEx(ip + 1, k + 1, varStack)) // now deal with the rest
                    {
                        return AltLoopStatus.MatchFound;
                    }
                }

                // if we arrive here, it was not possible to ...
                // (1) ... unify the range's SearchTerm with the target element, or
                // (2) ... unify the range variable with the range list, or
                // (3) ... successfully process the rest of the pattern and target
                // Now unbind and try matching with the next target element
                UnbindToMarker(varStack, marker);

                return AltLoopStatus.TryNextDown; // try the next downrange match
            }

            // calculated the upper and lower bounds imposed by the remainder of the pattern
            private bool DoLowerAndUpperboundChecks(int ip, int it, out int minLen, out int maxLen)
            {
                ListPatternElem e = (ListPatternElem)pattern[ip];
                int minRangeLen = e.MinRangeLen; // min and max range length ...
                int maxRangeLen = e.MaxRangeLen; // ... as specified in ListPatternElem
                int termLen = e.HasSearchTerm ? 1 : 0; // take the search term into account if present

                // calculated the minimum required and maximum possible range length,
                // given the rest of the pattern and the remaining part of the target

                int minRequiredForRestOfRanges = 0;
                int maxPossibleForRestOfRanges = 0;

                // calculated the minimum and maximum remaining pattern length (and
                // hence the number of target terms that should still be available)
                for (int i = ip + 1; i < pattern.Length; i++)
                {
                    minRequiredForRestOfRanges += (e = (ListPatternElem)pattern[i]).MinRangeTermLen;
                    maxPossibleForRestOfRanges = ListPatternElem.SaveAdd(maxPossibleForRestOfRanges, e.MaxRangeTermLen);
                }

                // calculate the number of target terms that are actually available
                int remainingTargetLength =
                    target.Count - it; // 'it' is the index of the first of the remaining target terms
                // calculate the lower bound for the number of target terms to be consumed by this range
                int minRequiredRangeLen = Math.Max(0, remainingTargetLength - termLen - maxPossibleForRestOfRanges);
                // calculate the upper bound for the number of target terms to be consumed by this range
                int maxPossibleRangeLen = remainingTargetLength - termLen - minRequiredForRestOfRanges;

                // the bounds just calculated must be adjusted with the values specified in the range descriptor itself.
                // the lower bound should always at least be equal to minRequiredRangeLen, so take the highest value
                minLen = Math.Max(minRangeLen, minRequiredRangeLen);
                // the upper bound should always at most be equal to maxPossibleRangeLen, so take the lowest value
                maxLen = Math.Min(maxRangeLen, maxPossibleRangeLen);

                return minLen <= maxLen; // fail if we ended up with a zero-length range
            }

            // try to bind the variable associated with the list of search alternatives
            // (t1|t2|...|tn) to the target term found matching one of these alternatives
            private bool TryBindingAltListVarToMatch(BaseTerm AltListVar, BaseTerm searchTerm, VarStack varStack)
            {
                if (AltListVar == null)
                {
                    return true;
                }

                return AltListVar.Unify(searchTerm, varStack);
            }

            // try to bind the range specification variable (if present) to the range list.
            // if the minimun or the maximum range length was a variable, then bind it to the actual length just found
            private bool TryBindingRangeRelatedVars(ListPatternElem g, int rangeLength, ListTerm RangeList,
                VarStack varStack)
            {
                if (g.MinLenTerm.IsVar)
                {
                    g.MinLenTerm.Unify(new DecimalTerm(Symbol, rangeLength), varStack);
                }

                if (g.MaxLenTerm.IsVar)
                {
                    g.MaxLenTerm.Unify(new DecimalTerm(Symbol, rangeLength), varStack);
                }

                if (g.RangeBindVar != null)
                {
                    if (RangeList == null)
                    {
                        RangeList = EMPTYLIST;
                    }

                    if (!g.RangeBindVar.Unify(RangeList, varStack))
                    {
                        return false; // alas, the same range var was apparently used & bound earlier in the pattern
                    }
                }

                return true;
            }

            private void
                AppendToRangeList(ref ListTerm RangeList, BaseTerm rangeSpecVar, BaseTerm t,
                    ref BaseTerm tail) // append t to RangeList
            {
                if (rangeSpecVar == null)
                {
                    return; // no point in constructing a range list if it won't be bound later
                }

                if (RangeList == null)
                {
                    RangeList = new ListTerm(t.Symbol, t);
                    tail = RangeList;
                }
                else
                {
                    tail.SetArg(1, new ListTerm(t.Symbol, t));
                    tail = tail.Arg(1);
                }
            }

            public override string ToWriteString(int level)
            {
                StringBuilder sb = new StringBuilder(PrologParser.LISTPATOPEN + SpaceAtLevel(level));
                bool first = true;

                foreach (ListPatternElem e in CompoundArgs)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        sb.Append(CommaAtLevel(level));
                    }

                    sb.AppendPacked(e.ToWriteString(level),
                        e.HasSearchTerm ? e.AltSearchTerms[4].FunctorIsBinaryComma : false);
                }

                sb.Append(SpaceAtLevel(level) + PrologParser.LISTPATCLOSE);

                return sb.ToString();
            }

            public override void TreePrint(int level, PrologEngine engine)
            {
                string margin = Spaces(2 * level);

                engine.WriteLine("{0}{1}", margin, PrologParser.LISTPATOPEN);

                BaseTerm t = ChainEnd();

                foreach (ListPatternElem e in CompoundArgs)
                {
                    engine.WriteLine("  {0}{1}", margin, e);
                }

                engine.WriteLine("{0}{1}", margin, PrologParser.LISTPATCLOSE);
            }

            private enum AltLoopStatus { Break, MatchFound, TryNextDown, TryNextAlt }
        }

        public class SearchTerm
        {
            public SearchTerm(BaseTerm term)
            {
                this.term = term;
            }

            public BaseTerm term { get; }
        }
    }
}