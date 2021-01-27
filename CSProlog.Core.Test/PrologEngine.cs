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

ppp(1, 2).
ppp(Z, Y) :- Z = Y.
ppp(X) :- 
    X = 1.

a(X):-true,b(X).
b(X):-d(X),true.
b(X):-e(X).
d(1).
d(2) :- 1 = X, 2 = Y, X = Y.
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

natnum(0).
natnum(s(X)) :- 
    natnum(X),
    true,
    Y = Z.

sentence --> [a], [b] ; [c], [d] ; [e].

banan(X) :- apple(3), pineapple(Y), X = Y.
apple(X) :- 1=1.
pineapple(1) :- 2=2.
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
            // SWI-compliant
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
            // SWI-compliant
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: person({X})
Exit: person({X=alice})
Next: person({X})
Call: person({X})
Exit: person({X=bob})");
        }

        [Fact]
        public void ExecutionDetails3()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("person(bob)", 5);
            // SWI-compliant
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: person(bob)
Exit: person(bob)");
        }

        [Fact]
        public void ExecutionDetails4()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("person(nope)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: person(nope)
Fail: person(nope)");
        }

        [Fact]
        public void ExecutionDetails5()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("ppp(1)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: ppp(1)
 Call: {X=1}=1
 Exit: {X=1}=1
Exit: ppp(1)");
        }

        [Fact]
        public void ExecutionDetails6()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("ppp(5, 6)", 5);
            ss.Success.Should().BeFalse();
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: ppp(5, 6)
 Call: {5}={6}
 Fail: {5}={6}
Fail: ppp(5, 6)");
        }

        [Fact]
        public void ExecutionDetails7()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("a(3)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call:a(3)
 Call:true
 Exit:true
 Call:b(3)
  Call:d(3)
  Fail:d(3)
 Redo:b(3)
  Call:e(3)
  Fail:e(3)
 Fail:b(3)
Redo:a(3)
 Call:var(_10402)
 Exit:var(_10402)
 Call:c(3)
  Call:f(3)
  Exit:f(3)
 Exit:c(3)
Exit:a(3)");
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
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call:a(_7856)
 Call:true
 Exit:true
 Call:b(_7856)
  Call:d(_7856)
  Exit:d(1)
  Call:true
  Exit:true
 Exit:b(1)
Exit:a(1)
Call:1=3
Fail:1=3
Redo:d(_7856)
  Call:_8184=1
  Exit:1=1
  Call:_8186=2
  Exit:2=2
  Call:1=2
  Fail:1=2
 Fail:d(_7856)
Redo:b(_7856)
  Call:e(_7856)
  Exit:e(2)
 Exit:b(2)
Exit:a(2)
Call:2=3
Fail:2=3
Redo:a(_7856)
  Call:var(_8310)
  Exit:var(_8310)
  Call:c(_7856)
   Call:f(_7856)
   Exit:f(3)
  Exit:c(3)
 Exit:a(3)
 Call:3=3
 Exit:3=3");
        }

        [Fact]
        public void ExecutionDetails10()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("natnum(s(s(s(0))))", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
 Call:natnum(s(s(s(0))))
  Call:natnum(s(s(0)))
   Call:natnum(s(0))
    Call:natnum(0)
    Exit:natnum(0)
    Call:true
    Exit:true
    Call:true
    Exit:true
   Exit:natnum(s(0))
   Call:true
   Exit:true
   Call:true
   Exit:true
  Exit:natnum(s(s(0)))
  Call:true
  Exit:true
  Call:true
  Exit:true
 Exit:natnum(s(s(s(0))))");
        }

        [Fact]
        public void ExecutionDetails11()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("sentence([e], _)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call:sentence([e], _6500)
 Call:[e]=[a|_6772]
 Fail:[e]=[a|_6772]
Redo:sentence([e], _6500)
 Call:[e]=[c|_6772]
 Fail:[e]=[c|_6772]
Redo:sentence([e], _6500)
 Call:[e]=[e|_6500]
 Exit:[e]=[e]
Exit:sentence([e], [])");
        }

        [Fact]
        public void ExecutionDetails12()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("banan(1)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: banan(1)
 Call: apple(3)
  Call: 1=1
  Exit: 1=1
 Exit: apple(3)
 Call: pineapple({Y=1})
  Call: 2=2
  Exit: 2=2
 Exit: pineapple({Y=1})
 Call: {X=1}={Y=1}
 Exit: {X=1}={Y=1}
Exit: banan(1)");
        }
    }
}