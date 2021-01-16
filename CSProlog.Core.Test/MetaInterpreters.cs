using Xunit;
using Xunit.Abstractions;

namespace CSPrologTest
{
    public class MetaInterpreters
    {
        [Fact]
        public void Background()
        {
            string consult = @"
natnum(0).
natnum(s(X)) :- natnum(X).";

            "Goal = natnum(0), Goal".True(consult);
            "Goal = natnum(s(0)), Goal".True(consult);
            "Goal = natnum(s(0)), call(Goal)".True(consult);
        }

        [Fact]
        public void Vanilla()
        {
            string consult = @"
single(1).
singleRule(X) :- X = 1.
singleRuleCall(X) :- single(X).
multiRule(0).
multiRule(X) :- X = 1.
multiRuleRec(X, (0 + 1)) :- true.
multiRuleRec(X, N) :- multiRuleRec(X, (N + 1)).

natnum(0).
natnum(s(X)) :- natnum(X), true, Y = Z.

mi1(true).
mi1((A,B)) :-
        mi1(A),
        mi1(B).
mi1(Goal) :-
        Goal \= true,
        Goal \= (_,_),
        clause(Goal, Body),
        mi1(Body).";

            //"mi1(single(X)), X = 1".True(consult);
            //"mi1(singleRule(X)), X = 1".True(consult);
            //"mi1(singleRuleCall(X)), X = 1".True(consult);
            //"mi1(multiRule(X)), X = 1".True(consult);
            //"mi1(multiRuleRec(5, 0)), X = 5".True(consult);
            //"mi1(multiRuleRec(X, 0)), X = 5".True(consult);
            //"mi1(natnum(0))".True(consult);
            //"mi1(natnum(s(0)))".True(consult);
            "mi1(natnum(s(s(0))))".True(consult, new Prolog.ExecutionDetails());
            //"mi1(natnum(s(s(s(0)))))".True(consult);
            //"mi1(natnum(X)), X = 0".True(consult);
            //"mi1(natnum(X)), X = s(0)".True(consult);
        }

        [Fact(Skip = "hangs")]
        public void VanillaCleanRepresentation()
        {
            string consult = @"
natnum(0).
natnum(s(X)) :- natnum(X).

mi_clause(G, Body) :-
        clause(G, B),
        defaulty_better(B, Body).

defaulty_better(true, true).
defaulty_better((A,B), (BA,BB)) :-
        defaulty_better(A, BA),
        defaulty_better(B, BB).
defaulty_better(G, g(G)) :-
        G \= true,
        G \= (_,_).";

            "mi_clause(natnum(0), true).".True(consult);
        }

    }
}