using System;
using FluentAssertions;
using Prolog;
using Xunit;

namespace CSPrologTest
{
    public class PrologEngineTest
    {
        [Fact]
        public void ConsultFromString_GetOneSolution()
        {
            PrologEngine prolog = new PrologEngine();
            // 'socrates' is human.
            prolog.ConsultFromString("human(socrates).");
            // 'R2-D2' is droid.
            prolog.ConsultFromString("droid(r2d2).");
            // human is bound to die.
            prolog.ConsultFromString("mortal(X) :- human(X).");

            // Question: Shall 'socrates' die?
            PrologEngine.ISolution solution1 = prolog.GetFirstSolution("mortal(socrates).");
            Assert.True(solution1.Solved); // = "True" (Yes)

            // Question: Shall 'R2-D2' die?
            PrologEngine.ISolution solution2 = prolog.GetFirstSolution("mortal(r2d2).");
            Assert.False(solution2.Solved); // = "False" (No)
        }

        [Fact]
        public void ConsultFromString_GetAllSolutions_Adhoc()
        {
            PrologEngine prolog = new PrologEngine();

            // 'socrates' is human.
            prolog.ConsultFromString("human(socrates).");
            // 'R2-D2' is droid.
            prolog.ConsultFromString("droid(r2d2).");
            // human is bound to die.
            prolog.ConsultFromString("mortal(X) :- human(X).");

            prolog.GetFirstSolution("listing.");

            SolutionSet solutionset1 = prolog.GetAllSolutions("human(H)");

            Assert.True(solutionset1.Success);
            if (solutionset1.Success)
            {
                Solution s = solutionset1[0];
                foreach (Variable v in s.NextVariable)
                {
                    Console.WriteLine("{0} ({1}) = {2}", v.Name, v.Type, v.Value);
                }
            }
        }


        [Fact]
        public void ExecutionDetails()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(@"
person(alice).
person(bob).

ppp(Z, Y) :- Z = Y.
ppp(X) :- 
    X = 1.

a(X):-b(X).
b(X):-d(X).
b(X):-e(X).
d(1).
e(2).
a(X):-c(X).
c(X):-f(X).
f(3).
");

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("person(X)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: person({X}) = person(alice) [ln 2]
   -> Yes: {X=alice}
?: person({X}) = person(bob) [ln 3]
   -> Yes: {X=bob}");

            ss = prolog.GetAllSolutions("person(bob)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: person(bob) = person(alice) [ln 2]
   -> No
?: person(bob) = person(bob) [ln 3]
   -> Yes");

            ss = prolog.GetAllSolutions("person(nope)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: person(nope) = person(alice) [ln 2]
   -> No
?: person(nope) = person(bob) [ln 3]
   -> No");

            ss = prolog.GetAllSolutions("ppp(1)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: ppp(1) = ppp({X}) [ln 6]
   -> Yes: {X=1}
?: {X=1}=1 = {X}={X} [ln 51]
   -> Yes: {X=1}");

            ss = prolog.GetAllSolutions("ppp(5, 6)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: ppp(5, 6) = ppp({Z}, {Y}) [ln 5]
   -> Yes: {Z=5}, {Y=6}
?: {Z=5}={Y=6} = {X}={X} [ln 51]
   -> No");

            ss = prolog.GetAllSolutions("a(3)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: a(3) = a({X}) [ln 9]
   -> Yes: {X=3}
?: b({X=3}) = b({X}) [ln 10]
   -> Yes: {X=3}
?: d({X=3}) = d(1) [ln 12]
   -> No
?: b({X=3}) = b({X}) [ln 11]
   -> Yes: {X=3}
?: e({X=3}) = e(2) [ln 13]
   -> No
?: a(3) = a({X}) [ln 14]
   -> Yes: {X=3}
?: c({X=3}) = c({X}) [ln 15]
   -> Yes: {X=3}
?: f({X=3}) = f(3) [ln 16]
   -> Yes");

            ss = prolog.GetAllSolutions("a(X), X=3", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: a({X}), {X}=3 = a({X}) [ln 9]
   -> Yes: {X={X}}
?: b({X={X}}), {X}=3 = b({X}) [ln 10]
   -> Yes: {X={X}}
?: d({X={X}}), {X}=3 = d(1) [ln 12]
   -> Yes: {X=1}
?: {X=1}=3 = {X}={X} [ln 51]
   -> No
?: b({X={X}}), {X}=3 = b({X}) [ln 11]
   -> Yes: {X={X}}
?: e({X={X}}), {X}=3 = e(2) [ln 13]
   -> Yes: {X=2}
?: {X=2}=3 = {X}={X} [ln 51]
   -> No
?: a({X}), {X}=3 = a({X}) [ln 14]
   -> Yes: {X={X}}
?: c({X={X}}), {X}=3 = c({X}) [ln 15]
   -> Yes: {X={X}}
?: f({X={X}}), {X}=3 = f(3) [ln 16]
   -> Yes: {X=3}
?: {X=3}=3 = {X}={X} [ln 51]
   -> Yes: {X=3}");

            ss = prolog.GetAllSolutions("a(X)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: a({X}) = a({X}) [ln 9]
   -> Yes: {X={X}}
?: b({X={X}}) = b({X}) [ln 10]
   -> Yes: {X={X}}
?: d({X={X}}) = d(1) [ln 12]
   -> Yes: {X=1}
?: b({X={X}}) = b({X}) [ln 11]
   -> Yes: {X={X}}
?: e({X={X}}) = e(2) [ln 13]
   -> Yes: {X=2}
?: a({X}) = a({X}) [ln 14]
   -> Yes: {X={X}}
?: c({X={X}}) = c({X}) [ln 15]
   -> Yes: {X={X}}
?: f({X={X}}) = f(3) [ln 16]
   -> Yes: {X=3}");
        }
    }
}