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

            @"aaa(grass, [cow, eats], _)".True(@"aaa(R) --> [cow, eats], {R = grass}.");
            @"aaa(grass, X, _), X = [cow, eats]".True(@"aaa(R) --> [cow, eats], {R = grass}.");
            @"aaa(X, [cow, eats], _), X = grass".True(@"aaa(R) --> [cow, eats], {R = grass}.");
            @"aaa(X, Y, _), X = grass, Y = [cow, eats]".True(@"aaa(R) --> [cow, eats], {R = grass}.");
        }

        [Fact]
        public void Example1()
        {
            string consult = @"
sentence -->
  subject,
  verb,
  object.

subject -->
  modifier,
  noun.

object -->
  modifier,
  noun.

modifier --> [the].

noun --> [cat].
noun --> [mouse].
noun --> [polar, bear].

verb --> [chases].
verb --> [eats].
";

            @"sentence([the, cat, chases, the, mouse], _)".True(consult);
            @"sentence([the, cat, chases, the, cat], _)".True(consult);
            @"sentence([the, polar, bear, eats, the, cat], _)".True(consult);
        }

        [Fact]
        public void Example2()
        {
            string consult = @"
sentence(s(S,V,O)) --> subject(S), verb(V), object(O).

subject(sb(M,N)) -->  modifier(M),  noun(N).

object(ob(M,N)) -->  modifier(M),  noun(N).

modifier(m(the)) --> [the].

noun(n(dog)) --> [dog].
noun(n(cow)) --> [cow].

verb(v(chases)) --> [chases].
verb(v(eats)) --> [eats].
";

            @"sentence(s(sb(m(the),n(dog)),v(chases),ob(m(the),n(cow))), [the, dog, chases, the, cow], _)".True(consult);
        }

        [Fact]
        public void Example3()
        {
            string consult = @"
sentence --> [a];[b].
";

            @"sentence([a], _)".True(consult);
            @"sentence([b], _)".True(consult);
        }

        [Fact]
        public void Example4()
        {
            string consult = @"
sentence --> [a], [b] ; [c], [d] ; [e].
";

            @"sentence([a, b], _)".True(consult);
            @"sentence([c, d], _)".True(consult);
            @"sentence([e], _)".True(consult);
        }


        [Fact]
        public void Example5()
        {
            string consult = @"
sentence --> [a], [X], {X = b ; X = c}, [d].
";

            @"sentence([a, b, d], _)".True(consult);
            @"sentence([a, c, d], _)".True(consult);
        }

        [Fact]
        public void ExampleCut()
        {
            string consult = @"
sentence(X) --> [a], !, {X = 1}.
sentence(X) --> [a], {X = 2}.
";

            @"sentence(1, [a], _)".True(consult);
            @"sentence(2, [a], _)".False(consult);
        }

        [Fact]
        public void ExampleEos()
        {
            string consult = @"
eos([], []).

sentence --> [a], sentence.
sentence --> eos.
";

            @"sentence([a], _)".True(consult);
            @"sentence([a, a, a], _)".True(consult);
            @"sentence([], _)".True(consult);
        }

        [Fact]
        public void ExampleRemainder()
        {
            string consult = @"
remainder(List, List, []).

sentence(X) --> [a], remainder(X).
";

            @"sentence([], [a], _)".True(consult);
            @"sentence([b, c], [a, b, c], _)".True(consult);
        }

        [Fact]
        public void ExampleString()
        {
            string consult = @"
dcg_string([]) -->
    [].
dcg_string([H|T]) -->
    [H],
    dcg_string(T).

sentence(X) --> dcg_string(X), [c].
";

            @"sentence([], [c], _)".True(consult);
            @"sentence([a, b], [a, b, c], _)".True(consult);
        }

        [Fact]
        public void ExampleStringWithout()
        {
            string consult = @"
string_without(End, Codes) -->
    { string(End), !,
      string_codes(End, EndCodes)
    },
    list_string_without(EndCodes, Codes).
string_without(End, Codes) -->
    list_string_without(End, Codes).

list_string_without(Not, [C|T]) -->
    [C],
    { \+ memberchk(C, Not)
    }, !,
    list_string_without(Not, T).
list_string_without(_, []) -->
    [].

sentence(X) --> string_without([c], X), [c].
";

            @"sentence([], [c], _)".True(consult);
            @"sentence([a, b], [a, b, c], _)".True(consult);
        }
    }
}
