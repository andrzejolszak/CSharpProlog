using System;
using FluentAssertions;
using Prolog;
using Xunit;

namespace CSPrologTest
{
    public class PrologEngineTest
    {
        private const string _execDetailsConsult = @"
person(alice).
person(bob).

ppp(Z, Y) :- Z = Y.
ppp(X) :- 
    X = 1.

a(X):-true,b(X).
b(X):-d(X),true.
b(X):-e(X).
d(1).
e(2).
a(X):-var(Z), c(X).
c(X):-call(f(X)).
f(3).

foo1 :- bar1, car1, dar1.
bar1 :- far1; % ln 19
    true. % ln 20
car1 :- ear1(1).
ear1(X) :- true.
far1 :- 
    fail.
dar1 :- fail.
";

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
        public void ExecutionDetails1()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("foo1", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: foo1
 Call: bar1
  Call: far1
   Call: fail
   Fail: fail
  Fail: far1
 Redo: bar1
  Call: true
  Exit: true
 Exit: bar1
 Call: car1
  Call: ear1(1)
  Exit: ear1(1)
 Exit: car1
 Call: dar1
  Call: fail
  Fail: fail
 Fail: dar1
Fail: foo1");
        }

        [Fact]
        public void ExecutionDetails2()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("person(X)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }

        [Fact]
        public void ExecutionDetails3()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("person(bob)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }

        [Fact]
        public void ExecutionDetails4()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("person(nope)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }

        [Fact]
        public void ExecutionDetails5()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("ppp(1)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }

        [Fact]
        public void ExecutionDetails6()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("ppp(5, 6)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }

        [Fact]
        public void ExecutionDetails7()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("a(3)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }

        [Fact]
        public void ExecutionDetails8()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("a(X), X=3", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }

        [Fact]
        public void ExecutionDetails9()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("a(X)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"");
        }
    }
}