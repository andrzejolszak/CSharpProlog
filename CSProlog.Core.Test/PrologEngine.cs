using System;
using FluentAssertions;
using Prolog;
using Xunit;

namespace CSPrologTest
{
    public class PrologEngineTest
    {
        private const string _execDetailsConsult = @"
person(alice). % 1
person(bob). % 2
% 3
ppp(1, 2). % 4
ppp(Z, Y) :- Z = Y. % 5
ppp(X) :-  % 6
    X = 1. % 7
% 8
a(X):-true,b(X). % 9
b(X):-d(X),true. % 10
b(X):-e(X). % 11
d(1). % 12
d(2) :- 1 = X, 2 = Y, X = Y. % 13
e(2). % 14
a(X):-var(Z), c(X). % 15
c(X):-call(f(X)). % 16
f(3). % 17
% 18
foo1 :- bar1, car1, dar1. % 19
bar1 :- far1; % 20
    true. % 21
car1 :- ear1(1). % 22
ear1(X) :- true. % 23
far1 :-  % 24
    fail. % 25
dar1 :- fail. % 26
% 27
natnum(0). % 28
natnum(s(X)) :- % 29 
    natnum(X), % 30
    true, % 31
    Y = Z. % 32
% 33
sentence --> [a], [b] ; [c], [d] ; [e]. % 34
% 35
banan(X) :- apple(3), pineapple(Y), X = Y. % 36
apple(X) :- 1=1. % 37
pineapple(1) :- 2=2. % 38
% 39
oor(X) :- 2=3 ; 4=5 ; X=1. % 40
% 41
oor2(X) :- oor3(X) ; 4=5 ; X=1. % 42
oor3(5). % 43
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
Fail: foo1".Trim());

            prolog.ExecutionDetails.CallHistoryStringWithLines.Should().Be(@"
Call: foo1 [ln 0]
 Call: bar1 [ln 20]
  Call: far1 [ln 21]
   Call: fail [ln 26]
   Fail: fail [ln 26]
  Fail: far1 [ln 21]
 Redo: bar1 [ln 20]
  Call: true [ln 22]
  Exit: true [ln 22]
 Exit: bar1 [ln 20]
 Call: car1 [ln 20]
  Call: ear1(1) [ln 23]
  Exit: ear1(1) [ln 23]
 Exit: car1 [ln 20]
 Call: dar1 [ln 20]
  Call: fail [ln 27]
  Fail: fail [ln 27]
 Fail: dar1 [ln 20]
Fail: foo1 [ln 0]".Trim());
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
Exit: person({X=bob})".Trim());

            prolog.ExecutionDetails.CallHistoryStringWithLines.Should().Be(@"
Call: person({X}) [ln 0]
Exit: person({X=alice}) [ln 0]
Next: person({X}) [ln 0]
Call: person({X}) [ln 0]
Exit: person({X=bob}) [ln 0]".Trim());
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
Exit: person(bob)".Trim());

            prolog.ExecutionDetails.CallHistoryStringWithLines.Should().Be(@"
Call: person(bob) [ln 0]
Exit: person(bob) [ln 0]".Trim());
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
Fail: person(nope)".Trim());
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
Exit: ppp(1)".Trim());

            prolog.ExecutionDetails.CallHistoryStringWithLines.Should().Be(@"
Call: ppp(1) [ln 0]
 Call: {X=1}=1 [ln 8]
 Exit: {X=1}=1 [ln 8]
Exit: ppp(1) [ln 0]".Trim());
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
 Call: {Z=5}={Y=6}
 Fail: {Z=5}={Y=6}
Fail: ppp(5, 6)".Trim());
        }

        [Fact]
        public void ExecutionDetails7()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("a(3)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: a(3)
 Call: true
 Exit: true
 Call: b({X=3})
  Call: d({X=3})
  Fail: d({X=3})
 Redo: b({X=3})
  Call: e({X=3})
  Fail: e({X=3})
 Fail: b({X})
Redo: a(3)
 Call: var({Z})
 Exit: var({Z})
 Call: c({X=3})
  Call: call(f({X=3}))
   Call: f({X=3})
   Exit: f({X=3})
  Exit: call(f({X=3}))
 Exit: c({X=3})
Exit: a(3)".Trim());
        }

        [Fact(Skip = "TODO")]
        public void ExecutionDetails8()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("a(X), X=3", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: a({X})
 Call: true
 Exit: true
 Call: b({X={X}})
  Call: d({X={X}})
  Exit: d({X={X=1}})
  Call: true
  Exit: true
 Exit: b({X={X=1}})
Exit: a({X=1})
Call: {X=1}=3
Fail: {X=1}=3
".Trim());
        }

        [Fact(Skip = "TODO")]
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
 Exit:3=3".Trim());
        }

        [Fact]
        public void ExecutionDetails10()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("natnum(s(s(s(0))))", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: natnum(s(s(s(0))))
 Call: natnum({X=s(s(0))})
  Call: natnum({X=s(0)})
   Call: natnum({X=0})
   Exit: natnum({X=0})
   Call: true
   Exit: true
   Call: {Y}={Z}
   Exit: {Y={Z}}={Z}
  Exit: natnum({X=s(0)})
  Call: true
  Exit: true
  Call: {Y}={Z}
  Exit: {Y={Z}}={Z}
 Exit: natnum({X=s(s(0))})
 Call: true
 Exit: true
 Call: {Y}={Z}
 Exit: {Y={Z}}={Z}
Exit: natnum(s(s(s(0))))".Trim());
        }

        [Fact(Skip = "TODO")]
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
Exit:sentence([e], [])".Trim());
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
Exit: banan(1)".Trim());

            prolog.ExecutionDetails.CallHistoryStringWithLines.Should().Be(@"
Call: banan(1) [ln 0]
 Call: apple(3) [ln 37]
  Call: 1=1 [ln 38]
  Exit: 1=1 [ln 38]
 Exit: apple(3) [ln 37]
 Call: pineapple({Y=1}) [ln 37]
  Call: 2=2 [ln 39]
  Exit: 2=2 [ln 39]
 Exit: pineapple({Y=1}) [ln 37]
 Call: {X=1}={Y=1} [ln 37]
 Exit: {X=1}={Y=1} [ln 37]
Exit: banan(1) [ln 0]".Trim());
        }

        [Fact]
        public void ExecutionDetails13()
        {
            PrologEngine prolog = new PrologEngine(new ExecutionDetails());

            prolog.ConsultFromString(_execDetailsConsult);

            SolutionSet ss = null;
            ss = prolog.GetAllSolutions("oor(1)", 5);
            prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: oor(1)
 Call: 2=3
 Fail: 2=3
Redo: oor(1)
 Call: 4=5
 Fail: 4=5
Redo: oor(1)
 Call: {X=1}=1
 Exit: {X=1}=1
Exit: oor(1)".Trim());

            prolog.ExecutionDetails.CallHistoryStringWithLines.Should().Be(@"
Call: oor(1) [ln 0]
 Call: 2=3 [ln 41]
 Fail: 2=3 [ln 41]
Redo: oor(1) [ln 0]
 Call: 4=5 [ln 41]
 Fail: 4=5 [ln 41]
Redo: oor(1) [ln 0]
 Call: {X=1}=1 [ln 41]
 Exit: {X=1}=1 [ln 41]
Exit: oor(1) [ln 0]".Trim());
        }

            [Fact]
            public void ExecutionDetails14()
            {
                PrologEngine prolog = new PrologEngine(new ExecutionDetails());

                prolog.ConsultFromString(_execDetailsConsult);

                SolutionSet ss = null;
                ss = prolog.GetAllSolutions("oor2(1)", 5);
                prolog.ExecutionDetails.CallHistoryString.Should().Be(@"
Call: oor2(1)
 Call: oor3({X=1})
 Fail: oor3({X=1})
Redo: oor2(1)
 Call: 4=5
 Fail: 4=5
Redo: oor2(1)
 Call: {X=1}=1
 Exit: {X=1}=1
Exit: oor2(1)".Trim());
            }
    }
}