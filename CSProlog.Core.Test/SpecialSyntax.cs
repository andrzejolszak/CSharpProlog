using Xunit;
using Xunit.Abstractions;

namespace CSPrologTest
{
    public class SpecialSyntax
    {
        [Fact]
        public void FailIfUndefined()
        {
            "fooBar".Error();
            "fooBar".False(":-fail_if_undefined(fooBar/0).");
        }

        [Fact(Skip = "This is not available")]
        public void IsOnLists()
        {
            "X is [1,2] + [a,b],  X = [(1,2), (2,b)]".True();
            "X is [1,2] * [a,b],  X = [(1,a), (1,b), (1,c), (2,a), (2,b),(2, c)]".True();
        }

        [Fact]
        public void Unicode()
        {
            "'åæø'".True("'åæø'.");
        }

        [Fact]
        public void Wraps()
        {
            "X = [: 1, 2, 3, 4 :]".True();
        }

        [Fact]
        public void ListPatternTerms()
        {
            "[1,2,3,4] = [! .., L !], L = 4".True();
            "[! .., xxx, S{2,5}, xxx, ..!] = [a, xxx, 1,2,3,4, xxx, 5], S = [1, 2, 3, 4]".True();
            "[! .., color=C, ..!] = [length=12, height=78, color=red, weight = 100], C = red".True();
            "x( [1, 2, 3, p(9), 4], Z), Z = 9".True("x([! .., p(X)|q(X), .. !], X).");
            "[! X{,5}, Y!a|b|c, .. !] = [1, 2, 4 ,b , 5,6], X = [1, 2, 4], Y = b".True();
            "[!.., N, L{N}, N, ..!] = [1,7,5,4,8,7,6,5,4,9], N = 4, L = [8, 7, 6, 5]".True();
            "[! .., p, Z!~a|b|c, .. !] = [a, p, b, p, a, p, e, p, q, r], Z = e".True();
        }
    }
}