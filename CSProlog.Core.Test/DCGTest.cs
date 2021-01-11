using System;
using Xunit;
using Prolog;

namespace CSPrologTest
{
    public class DCGTest
    {
        [Fact]
        public void Simple()
        {
            @"phrase(a(X), ""as""), X = 5.".True(@"a(5) --> ""as"".");
        }

        [Fact]
        public void Simple2()
        {
            @"phrase(a(X), ['a', 's']), X = 5.".True(@"a(5) --> ['a', 's'].");
        }

        [Fact]
        public void ExpandTerm()
        {
            @"expand_term((a --> ['a', 's']), X)".True();
            @"expand_term((a --> ""as""), X)".True();
        }
    }
}
