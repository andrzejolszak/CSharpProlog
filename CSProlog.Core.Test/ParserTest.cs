using FluentAssertions;
using Xunit;
using Xunit.Abstractions;
using static Prolog.PrologEngine;

namespace CSPrologTest
{
    public class ParserTest
    {
        private readonly ITestOutputHelper _output;

        public ParserTest(ITestOutputHelper output)
        {
            this._output = output;
        }

        [Theory]
        [InlineData(":-(foo,true)", "foo.")]
        [InlineData(":-(foo,true)", "foo .")]
        [InlineData(":-(foo,true)", "'foo'.")]
        public void ParsesSymbols(string expected, string test)
        {
            //expected.CanParse();
            test.CanParse();
        }

        [Theory]
        [InlineData("a§b.")]
        [InlineData("1ab.")]
        [InlineData("1ab,.")]
        [InlineData("-f_-_.")]
        [InlineData("-abc.")]
        [InlineData("aab/.")]
        [InlineData("aab/-1.")]
        [InlineData("aab/.1.")]
        [InlineData("aab/1()")]
        [InlineData("%This line is commented out: a :- b.")]
        public void ParsesSymbolsFalsePositives(string test)
        {
            test.CanParse();
        }

        [Theory]
        [InlineData(":-(a,Foobar123_)", "a :- Foobar123_.")]
        [InlineData(":-(a,_)", "a :- _ .")]
        [InlineData(":-(a,__)", "a :- __ .")]
        [InlineData(":-(a,A__a___)", "a :- A__a___ .")]
        public void ParsesVariables(string expected, string test)
        {
            //expected.CanParse();
            test.CanParse();
        }

        [Theory]
        [InlineData("foobar123_d.", 1, 1, 13)]
        [InlineData("\r\n\r\nfoobar123_d.\r\n", 3, 1, 1)]
        [InlineData("   foobar123_d.   ", 1, 4, 19)]
        public void PopulatesSourceInfo(string test, int lineNo, int startCol, int endCol)
        {
            PredicateDescr d = test.CanParse();
            d.ClauseList.Term.Symbol.LineNo.Should().Be(lineNo);
            d.ClauseList.Term.Symbol.Start.Should().Be(startCol);
            d.ClauseList.Term.Symbol.Final.Should().Be(endCol);
        }

        [Theory]
        [InlineData(":-(struct,true)", "struct().")]
        [InlineData(":-(struct(abba),true)", "struct(abba).")]
        [InlineData(":-(struct(abba,babba,Variable),true)", "struct(abba, babba,Variable ).")]
        [InlineData(":-(struct(innerStruct(a,b,c)),true)", "struct(innerStruct(a,b,c)).")]
        [InlineData(":-(struct(abba,babba,Variable,innerStruct(a,b,c)),true)", "struct(abba, babba,Variable ,innerStruct(a,b,c)).")]
        [InlineData(":-(struct,true)", "(struct()).")]
        public void ParsesStructureCall(string expected, string test)
        {
            //expected.CanParse();
            test.CanParse();
        }

        [Theory]
        [InlineData(":-(+(atom,Var),true)", "atom + Var.")]
        [InlineData(":-(+(atom,Var),true)", "+(atom,Var).")]
        [InlineData(":-(+(atom,Var),true)", "'+'(atom,Var).")]
        [InlineData(":-(b(a,c),true)", "a b c.")]
        [InlineData(":-(-=>(atom,Var),true)", "atom -=> Var.")]
        [InlineData(":-(+(a,b),true)", "a+b.")]
        [InlineData(":-(+(+(a,b),c),true)", "a+b+c.")]
        [InlineData(":-(+(+(a,b),c),true)", "(a+b+c).")]
        [InlineData(":-(+(aaa,+(b,c)),true)", "aaa+(b+c).")]
        [InlineData(":-(+(+(a,b),c),true)", "(a+b)+c.")]
        [InlineData(":-(+(a,*(b,c)),true)", "a+b*c.")]
        [InlineData(":-(+(*(a,b),c),true)", "a*b+c.")]
        [InlineData(":-(+(a,b),true)", "(a+b).")]
        [InlineData(":-(+(a,b),true)", " ( a  +  b ) .")]
        [InlineData(":-(->(a,b),true)", "a->b.")]
        [InlineData(":-(=(a,b),true)", "a=b.")]
        [InlineData(":-(*(+(a,b),c),true)", " (a+b)*c.")]
        [InlineData(":-(*(+(a,b),c),true)", " ( a  +  b )  *  c .")]
        [InlineData(":-(+(a,b),true)", "'+'(a,b).")]
        [InlineData(":-(+(a,b),true)", "('+'(a,b)).")]
        [InlineData(":-(*(a,b),true)", "*(a,b).")]
        [InlineData(":-(*(a,b),true)", "'*'(a,b).")]
        [InlineData(":-(*(+(a,b),c),true)", "'*'('+'(a,b),c).")]
        [InlineData(":-(xor(a,b),true)", "a xor b.")]
        [InlineData(":-(xor_new(a,b),true)", "a xor_new b.")]
        [InlineData(":-(dynamic(/(foo,2)),true)", ":- dynamic( foo/2 ).")]
        [InlineData(":-(ensure_loaded(testEnsureLoaded.pl),true)", ":- ensure_loaded('testEnsureLoaded.pl').")]
        public void ParsesStructureBinaryOperator(string expected, string test)
        {
            //expected.CanParse();
            test.CanParse();
        }

        [Theory]
        [InlineData(":-(a,b)", "a :- b.")]
        [InlineData(":-(a,=(X,b))", "a :- X = b.")]
        [InlineData(":-(a,=(X,b))", "a :- X=b.")]
        [InlineData(":-(a,,(b,,(c,d)))", "a :- b, c, d.")]
        public void ParsesPredicates(string expected, string test)
        {
            //expected.CanParse();
            test.CanParse();
        }

        [Theory]
        [InlineData(":-(a,b)", "%This is a line comment\r\na :- b.")]
        [InlineData(":-(a,b)", "  %This is a line comment\r\n   a :- b.")]
        [InlineData(":-(a,b)", "a :- b. %This is a line comment\r\n")]
        [InlineData(":-(a,b)", "/**/ a :- b.")]
        [InlineData(":-(a,b)", "/* This is a block comment */ a :- b.")]
        [InlineData(":-(a,b)", "/*This is\r\n a multi\r\nline comment\r\n*/\r\na :- b.")]
        public void ParsesCommentedCode(string expected, string test)
        {
            //expected.CanParse();
            test.CanParse();
        }

        [Theory]
        [InlineData("a1. a2. a3.")]
        [InlineData("foobar123_.\r\n a(Var) :- true. ")]
        [InlineData("atom. atom.")]
        [InlineData("aar. a :- atom.")]
        [InlineData("aar. struct(Param, param2).")]
        [InlineData("foobar123_.\r\n aar=2. struct(Param, param2).")]
        public void ParsesMany(string test)
        {
            test.CanParse();
        }

        [Theory]
        [InlineData("'a§b'.")]
        [InlineData("a1.")]
        [InlineData("a -_w_> a.")]
        [InlineData("a op->erator a.")]
        [InlineData("a1 :- b.")]
        [InlineData("a2 :- b, c.")]
        [InlineData("a3 :- b, c, d.")]
        [InlineData("a4 :- b=c .")]
        [InlineData("a5 :- b(c).")]
        [InlineData("a6 :- b(c,d).")]
        [InlineData("a7 :- b(c,d, e).")]
        [InlineData("  b  . ")]
        [InlineData(" b(c,d).")]
        [InlineData(" b(c,d, e).")]
        [InlineData("a+b.")]
        [InlineData("a+b :- a=s, a.")]
        [InlineData("a+b. c.")]
        [InlineData("a+b. c. d:-e. \r\n f:-g=h.")]
        [InlineData("a :-a=s,true,1+2.")]
        [InlineData("a :- a=s, true,1+2.")]
        [InlineData("a(c+d).")]
        [InlineData("a(a, c+d).")]
        [InlineData("a(a, b, c+d, (e+1)).")]
        [InlineData("a(a, b, c+d) :- a=s, true,1+2.")]
        [InlineData(":- op(900, xfx, =>).")]
        [InlineData(":- op(900, fx, [abba]).")]
        [InlineData(":- op(900, xfx, [=>]).")]
        [InlineData(":- op(900, xfx, [=>, -->>, -+->]).")]
        [InlineData("call(true).")]
        [InlineData("a(X) :- X = true, call(X).")]
        [InlineData("a :- [a, 2, B, c_].")]
        [InlineData("a :- [].")]
        [InlineData("a :-  [  ]  .")]
        [InlineData("a :-  [ a, 2 ] .")]
        [InlineData("a :- [B].")]
        [InlineData("a :- [b;c].")]
        [InlineData("a :- [b+c].")]
        [InlineData("a :- X = [], Y = [a], Z = [a, b, 1], [C, a] = [2, a], C=X.")]
        [InlineData("a :- [[a, 2], B].")]
        [InlineData("a :- [[a, 2], B, c].")]
        [InlineData("a :- [[a, 2], [B], []].")]
        [InlineData("a :- [b|[]].")]
        [InlineData("a :- [[]|b].")]
        [InlineData("a :- [b|[c]].")]
        [InlineData("a :- [b|c].")]
        [InlineData("a :- [a, b|c].")]
        [InlineData("a :- [[]|[]].")]
        [InlineData("a([a+b,c|d]) :- [X,Y|Z] = [1,2,3,4,5,6].")]
        [InlineData("a :- a ; b.")]
        [InlineData("a :- a ; b ; c.")]
        [InlineData("a :- a ; b , c, d;e;f;g,h.")]
        [InlineData("a :- (a ; b), c, d, (e;f;f).")]
        [InlineData("a :- (e;f;(g,h), (i;j; [true])).")]
        [InlineData("a :- (1+2=3 -> call(true) ; call(false)).")]
        [InlineData("a :- ((1+2==3) -> call(true) ; call(false)).")]
        [InlineData("a :- {a}.")]
        [InlineData("a :- {a, b}.")]
        [InlineData("a :- {a; b}.")]
        [InlineData("a :- {a+b}.")]
        [InlineData("a :- {a, b, c}.")]
        [InlineData("a :- {}.")]
        [InlineData("a :- !,a.")]
        [InlineData("a :- a,!.")]
        [InlineData("a :- a,!,c.")]
        [InlineData("a :- fail.")]
        [InlineData("a :- X is 2+2.")]
        [InlineData("a :- X is 2.")]
        [InlineData("a :- X is 'is'.")]
        [InlineData("a :- X is xor.")]
        [InlineData("a :- X is +.")]
        [InlineData("a :- X is is.")]
        [InlineData("a :- {x:1, y:2}.")]
        [InlineData("a :- point{x:1, y:2}.")]
        [InlineData("a :- \"abc d - 1,\'. :- sd\".")]
        [InlineData("a :- \"\".")]
        [InlineData("a :- \" \".")]
        [InlineData("a :- X=1.\r\n a :- X=2. \r\n b :- true. \r\n a :- X=3. a() :- X=4. a(1,2):-X=5. a(1):-X=5. a((a,b,c)):-X=5. a.")]
        [InlineData("a :- X = 2.1234.")]
        [InlineData("a :- (1 + 2) * 2.")]
        [InlineData("a :- 2.0 + 40.")]
        [InlineData("a :- -1 + 2.")]
        [InlineData("a :- -12.23230 -40.")]
        [InlineData("a :- -12.23230 - 40.")]
        [InlineData("a :- -12.23230 -4.001.")]
        [InlineData("a :- -12.23230 - 0.001.")]
        [InlineData("a :- 0.00001 + -40.")]
        [InlineData("a :- X = 1 + +29.")]
        [InlineData("a :- X = 1 + (-40) + -(40) + (1).")]
        [InlineData("a :- X=''.")]
        [InlineData("a :- X='single quoted string', nl.")]
        [InlineData("a :- \\+(a = b, c = d).")]
        [InlineData("a :- (a = b, c = d ; p = t).")]
        [InlineData("a :- \\+ a.")]
        [InlineData("a :- \\+a.")]
        [InlineData("a :- \\+ (a).")]
        [InlineData("a :- \\+ (a,b,c).")]
        [InlineData("a :- \\+(a,b,c).")]
        [InlineData("a :- + (a,b).")]
        [InlineData("a :- +(a,b).")]
        [InlineData("a :- f (a,b).")]
        [InlineData("a :- f(a,b).")]
        [InlineData("a :- a().")]
        [InlineData("a :- (a()).")]
        [InlineData("a/0.")]
        [InlineData("a/1.")]
        [InlineData("a :- 1 + (2).")]
        [InlineData("a :- 1 + -(2).")]
        [InlineData("a :- 1 -(2).")]
        [InlineData("a :- 1 - (2).")]
        [InlineData("a. a :- X=2. a:-true.")]
        [InlineData("a :- X=2.")]
        [InlineData("aaa(X) :- X=1. bbb(X,a(b),1+3):-true. a([1,3|b]). bbb(X,2,X). aaa(5) :- fail. aaa(Y) :- Y=a.")]
        [InlineData("a. a :- fail. a :- X=1,true.")]
        [InlineData("a :- asserta((bar(X) :- X)), clause(bar(X), B). a(b) :- c.")]
        [InlineData("a(GAR, foobar(DEDAL), [KAIN, ABEL], banan) :-GAR = IAR, IAR = [DEDAL], DEDAL = abc.")]
        [InlineData("a :- atom_chars(A,['p','r','o','l','o','g']), A = 'prolog'.")]
        [InlineData("a :- atom_chars([],L), L=['[',']'].")]
        [InlineData("a :- ['n', s, 3] = [n|[Y, Z]], Y = s, Z = 3.")]
        [InlineData("a :- bagof(f(X,Y),(X=a;Y=b),L), L = [f(a, _), f(_, b)].")]
        [InlineData("a :- bagof(X,Y^Z,L).")]
        [InlineData("a :- findall(X+Y,(X=1),S), S=[1+_].")]
        [InlineData("a :- '->'(true, (X=1; X=2)), (X=1;X=2).")]
        [InlineData("a :- X = 1 + 2, 'is'(Y, X * 3), X = 1+2, Y=9.")]
        [InlineData("a :- once(!), (X=1; X=2), (X=1;X=2).")]
        [InlineData("a :- ((X=1, !); X=2), X=1.")]
        [InlineData("a :- (!; call(3)).")]
        [InlineData("a :- retract((atom(_) :- X == '[]')).")]
        [InlineData("a :- setof(X, X ^ (true; 4), L).")]
        [InlineData("a(F) :- F =..[a|F2].")]
        [InlineData("a :- '='(f(A, B, C), f(g(B, B), g(C, C), g(D, D))), A=g(g(g(D, D), g(D, D)), g(g(D, D), g(D, D))), B=g(g(D, D), g(D, D)), C=g(D, D).")]
        [InlineData("a :- once(!).")]
        [InlineData("a :- (X=1; X=2), (X=1;X=2).")]
        [InlineData("a :- (X=1; X=2).")]
        [InlineData("a :- (X=1;X=2).")]
        [InlineData("a :- (X=1;X=2); (a).")]
        [InlineData("a :- (X=1; X=2), (a).")]
        [InlineData("a :- X = +(1,2), Y = + (1,2), X=Y.")]
        [InlineData("a :- X = +(1,2,3,4), Y = + (1,2,3,4), X=Y.")]
        [InlineData("aaa(X) :- X=1. bbb(X,Y,Z):-true. aaa(Y) :- fail. aaa(Y) :- Y=a.")]
        public void CanParsePredicates(string test)
        {
            test.CanParse();
        }
    }
}