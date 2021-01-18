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

            SolutionSet solutionset1 = prolog.GetAllSolutions(null, "human(H)");

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
ppp(X) :- X = 1.

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
            ss = prolog.GetAllSolutions(null, "person(X)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: person({X}) = person(alice)
   -> Yes: {X=alice}
?: person({X}) = person(bob)
   -> Yes: {X=bob}");

            ss = prolog.GetAllSolutions(null, "person(bob)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: person(bob) = person(alice)
   -> No
?: person(bob) = person(bob)
   -> Yes");

            ss = prolog.GetAllSolutions(null, "person(nope)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: person(nope) = person(alice)
   -> No
?: person(nope) = person(bob)
   -> No");

            ss = prolog.GetAllSolutions(null, "ppp(1)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: ppp(1) = ppp({X})
   -> Yes: {X=1}
?: {X=1}=1 = {X}={X}
   -> Yes: {X=1}");

            ss = prolog.GetAllSolutions(null, "ppp(5, 6)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: ppp(5, 6) = ppp({Z}, {Y})
   -> Yes: {Z=5}, {Y=6}
?: {Z=5}={Y=6} = {X}={X}
   -> No");

            ss = prolog.GetAllSolutions(null, "a(3)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: a(3) = a({X})
   -> Yes: {X=3}
?: b({X=3}) = b({X})
   -> Yes: {X=3}
?: d({X=3}) = d(1)
   -> No
?: b({X=3}) = b({X})
   -> Yes: {X=3}
?: e({X=3}) = e(2)
   -> No
?: a(3) = a({X})
   -> Yes: {X=3}
?: c({X=3}) = c({X})
   -> Yes: {X=3}
?: f({X=3}) = f(3)
   -> Yes");

            ss = prolog.GetAllSolutions(null, "a(X), X=3", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: a({X}), {X}=3 = a({X})
   -> Yes: {X={X}}
?: b({X={X}}), {X}=3 = b({X})
   -> Yes: {X={X}}
?: d({X={X}}), {X}=3 = d(1)
   -> Yes: {X=1}
?: {X=1}=3 = {X}={X}
   -> No
?: b({X={X}}), {X}=3 = b({X})
   -> Yes: {X={X}}
?: e({X={X}}), {X}=3 = e(2)
   -> Yes: {X=2}
?: {X=2}=3 = {X}={X}
   -> No
?: a({X}), {X}=3 = a({X})
   -> Yes: {X={X}}
?: c({X={X}}), {X}=3 = c({X})
   -> Yes: {X={X}}
?: f({X={X}}), {X}=3 = f(3)
   -> Yes: {X=3}
?: {X=3}=3 = {X}={X}
   -> Yes: {X=3}");

            ss = prolog.GetAllSolutions(null, "a(X)", 5);
            prolog.ExecutionDetails.CurrentTermHistoryString.Should().Be(
                @"
?: a({X}) = a({X})
   -> Yes: {X={X}}
?: b({X={X}}) = b({X})
   -> Yes: {X={X}}
?: d({X={X}}) = d(1)
   -> Yes: {X=1}
?: b({X={X}}) = b({X})
   -> Yes: {X={X}}
?: e({X={X}}) = e(2)
   -> Yes: {X=2}
?: a({X}) = a({X})
   -> Yes: {X={X}}
?: c({X={X}}) = c({X})
   -> Yes: {X={X}}
?: f({X={X}}) = f(3)
   -> Yes: {X=3}");

        }
    }
}