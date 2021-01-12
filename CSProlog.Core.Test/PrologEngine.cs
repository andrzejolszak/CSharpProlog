using System;
using Xunit;
using Prolog;

namespace CSPrologTest
{
    public class PrologEngineTest
    {
        [Fact]
        public void ConsultFromString_GetOneSolution()
        {
            var prolog = new PrologEngine(persistentCommandHistory: false);

            // 'socrates' is human.
            prolog.ConsultFromString("human(socrates).");
            // 'R2-D2' is droid.
            prolog.ConsultFromString("droid(r2d2).");
            // human is bound to die.
            prolog.ConsultFromString("mortal(X) :- human(X).");

            // Question: Shall 'socrates' die?
            var solution1 = prolog.GetFirstSolution(query: "mortal(socrates).");
            Assert.True(solution1.Solved); // = "True" (Yes)

            // Question: Shall 'R2-D2' die?
            var solution2 = prolog.GetFirstSolution(query: "mortal(r2d2).");
            Assert.False(solution2.Solved); // = "False" (No)
        }

        [Fact]
        public void ConsultFromString_GetAllSolutions_Adhoc()
        {
            var prolog = new PrologEngine(persistentCommandHistory: false);

            // 'socrates' is human.
            prolog.ConsultFromString("human(socrates).");
            // 'R2-D2' is droid.
            prolog.ConsultFromString("droid(r2d2).");
            // human is bound to die.
            prolog.ConsultFromString("mortal(X) :- human(X).");

            prolog.GetFirstSolution(query: "listing.");

            SolutionSet solutionset1 = prolog.GetAllSolutions(null, "human(H)");

            Assert.True(solutionset1.Success);
            if (solutionset1.Success)
            {
                var s = solutionset1[0];
                foreach (Variable v in s.NextVariable)
                    Console.WriteLine(string.Format("{0} ({1}) = {2}", v.Name, v.Type, v.Value));

            }

        }
    }
}
