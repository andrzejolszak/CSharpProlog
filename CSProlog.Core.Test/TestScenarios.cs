using Xunit;
using Xunit.Abstractions;

namespace CSPrologTest
{
    public class TestScenarios
    {
        private readonly ITestOutputHelper _output;

        public TestScenarios(ITestOutputHelper output)
        {
            _output = output;
        }

        [Theory]
        [InlineData("a :- true.", "a")]
        [InlineData("a :- X=2.", "a")]
        [InlineData("a :- 1=2.", "\\+a")]
        [InlineData("a :- b, c, d. b. c:-true. d:-2=2.", "a")]
        [InlineData("a(X) :- X=2.", "a(2)")]
        [InlineData("a :- X=2, b(X). b(X) :- X=2.", "a")]
        [InlineData("a :- b(X), X=2. b(X) :- X=2.", "a")]
        [InlineData("a :- X=1, b(X). b(X) :- X=2.", "\\+a")]
        [InlineData("a :- b(X), X=1. b(X) :- X=2.", "\\+a")]
        [InlineData("a(X) :- X=2.", "\\+a(1)")]
        [InlineData("a :- \\+ fail.", "a")]
        [InlineData("a :- fail.", "\\+a")]
        [InlineData("a :- X=2, X=3.", "\\+a")]
        [InlineData("a :- b(X), c(X). b(X) :- X=3. c(X) :- X=2.", "\\+a")]
        [InlineData("a :- b(2), b(3). b(X) :- Y=X.", "a")]
        [InlineData("a :- b(2, Y), b(3, Z), Y=Z. b(X, Y) :- Y=X.", "\\+a")]
        [InlineData("a :- b(3, Y), b(3, Z), Y=Z. b(X, Y) :- Y=X.", "a")]
        public void TrivialCases(string test, string query)
        {
            query.True(test);
        }

        [Theory]
        //[InlineData(@"a :- ""str"" = ['s', 't', 'r'].", "a")]
        [InlineData(@"a :- [s, t, r] = ['s', 't', 'r'].", "a")]
        //[InlineData(@"a :- ""str"" = [s, t, r].", "a")]
        [InlineData(@"a(X) :- ""str"" = X.", @"a(""str"")")]
        [InlineData("a :- var(X).", "a")]
        [InlineData("a :- nonvar(X).", "\\+a")]
        [InlineData("a :- var(1).", "\\+a")]
        [InlineData("a :- nonvar(1).", "a")]
        [InlineData("a :- X = 2, var(X).", "\\+a")]
        [InlineData("a :- X = Y, var(X).", "a")]
        public void BuiltIns(string test, string query)
        {
            query.True(test);
        }

        [Theory]
        [InlineData("T: call(!)")]
        [InlineData("F: call(fail)")]
        [InlineData("F: call((fail, _X))")]
        [InlineData("F: call((fail, call(1)))")]
        [InlineData("R: call(_X)")]
        [InlineData("R: call(1)")]
        [InlineData("R: call((fail, 1))")]
        [InlineData("R: call((1; true))")]
        public void call(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData("T: !")]
        [InlineData("F: (!,fail;true)")]
        [InlineData("T: (call(!),fail;true)")]
        [InlineData("T: call(!),fail;true")]
        public void cut(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData("T: ';'(true, fail)")]
        [InlineData("F: ';'((!, fail), true)")]
        [InlineData("T: ';'(!, call(3))")]
        [InlineData("T: ';'((X=1, !), X=2)")]
        [InlineData("T: X=1; X=2")]
        [InlineData("T: (X=1; X=2), X=2")]
        [InlineData("F: (fail; fail), true")]
        [InlineData("T: X=2, (Y=3 ; Y=2), X=Y")]
        [InlineData("F: X=2, (Y=3 ; Y=1), X=Y")]
        public void or(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData("T: true")]
        [InlineData("F: fail")]
        public void trueFail(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData("F: X=1, var(X)")]
        [InlineData("T: var(X), X=1")]
        [InlineData("T: (var(X), X=1), Y=2")]
        [InlineData("T: (var(X)), X=1")]
        [InlineData("F: fail, call(3)")]
        [InlineData("F: nofoo(X), call(X)")]
        [InlineData("T: X = true, call(X)")]
        public void and(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData("T: '=\\='(0,1)")]
        [InlineData("F: '=\\='(1.0,1)")]
        [InlineData("F: '=\\='(3 * 2,7 - 1)")]
        [InlineData("R: '=\\='(N,5)")]
        [InlineData("R: '=\\='(floot(1),5)")]
        public void ari_cmp(string test)
        {
            test.Evaluate();
        }
    }
}