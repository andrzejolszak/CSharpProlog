using System;
using Xunit;
using Prolog;

namespace CSPrologTest
{
    public class DCGTest
    {
        [Fact]
        public void ExpandTerm()
        {
            @"expand_term((aaa --> ['a', 's']), X), assertz(X), aaa(['a', 's'], [])".True();
            @"expand_term((aaa --> ""as""), X), assertz(X), aaa(""as"", [])".True();
            @"expand_term((aaa --> ""as""), X), assertz(X), aaa(""afs"", [])".False();

            @"expand_term((aaa(R) --> [a, s], [R]), X), assertz(X), aaa(d, [a, s, d], Rem)".True();
            @"expand_term((aaa(R) --> [cow, eats], [R]), X), assertz(X), aaa(grass, [cow, eats, grass], Rem)".True();
            @"expand_term((aaa(R) --> [cow, eats], {R = grass}), X), assertz(X), aaa(grass, [cow, eats], Rem)".True();

            @"aaa(grass, [cow, eats], Rem)".True(@"aaa(R) --> [cow, eats], {R = grass}.");

            //@"expand_term((aaa(R) --> ""as"", [R]), X), assertz(X), aaa(F, ""asd"", Rem)".True();
        }

        [Fact]
        public void SimpleExample()
        {
            // TODO: https://www.amzi.com/manuals/amzi/pro/ref_dcg.htm
            string dcg = @"
sentence(s(S,V,O)) --> subject(S), verb(V), object(O).

subject(sb(M,N)) -->  modifier(M),  noun(N).

object(ob(M,N)) -->  modifier(M),  noun(N).

modifier(m(the)) --> [the].

noun(n(dog)) --> [dog].
noun(n(cow)) --> [cow].

verb(v(chases)) --> [chases].
verb(v(eats)) --> [eats].
";
        }
    }
}
