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

        }
    }
}