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
            _output = output;
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
        [InlineData("foobar123_d.", 1, 0, 12)]
        [InlineData("\r\n\r\nfoobar123_d.\r\n", 3, 4, 16)]
        [InlineData("   foobar123_d.   ", 1, 3, 15)]
        public void PopulatesSourceInfo(string test, int lineNo, int start, int final)
        {
            PredicateDescr d = test.CanParse();
            d.ClauseList.Term.Symbol.LineNoAdjusted.Should().Be(lineNo);
            d.ClauseList.Term.Symbol.StartAdjusted.Should().Be(start);
            d.ClauseList.Term.Symbol.FinalAdjusted.Should().Be(final);
            d.ClauseList.Term.Symbol.Class.Should().Be(BaseParser.SymbolClass.Id);
            test.Substring(start, final - start).Should().Be("foobar123_d.");
        }

        [Theory]
        [InlineData(":-(struct,true)", "struct().")]
        [InlineData(":-(struct(abba),true)", "struct(abba).")]
        [InlineData(":-(struct(abba,babba,Variable),true)", "struct(abba, babba,Variable ).")]
        [InlineData(":-(struct(innerStruct(a,b,c)),true)", "struct(innerStruct(a,b,c)).")]
        [InlineData(":-(struct(abba,babba,Variable,innerStruct(a,b,c)),true)",
            "struct(abba, babba,Variable ,innerStruct(a,b,c)).")]
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
        [InlineData(
            "a :- X=1.\r\n a :- X=2. \r\n b :- true. \r\n a :- X=3. a() :- X=4. a(1,2):-X=5. a(1):-X=5. a((a,b,c)):-X=5. a.")]
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
        [InlineData(
            "a :- '='(f(A, B, C), f(g(B, B), g(C, C), g(D, D))), A=g(g(g(D, D), g(D, D)), g(g(D, D), g(D, D))), B=g(g(D, D), g(D, D)), C=g(D, D).")]
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

        [Fact]
        public void MultiHeadRecursive()
        {
            string def = @"
%% This is a comment
% This is a comment line 2
fooBar(X).

% Baz comm
baz(a).

baz(F).

concatenate([], L, L).

% More comment
% Second line
concatenate([X|L1], L2, [X|L3]) :-
    concatenate(L1, L2, L3).";

            "concatenate([], [], [])".True(def);
            "concatenate([a,b], [c,d], [a,b,c,d])".True(def);
            "concatenate([a,b], [], [a,b])".True(def);
            "concatenate([], [c], [c])".True(def);
            "concatenate([], [c,d], [c,d])".True(def);
            "concatenate([s], [], [s])".True(def);
            "concatenate([a,b], [], [a,b])".True(def);
        }

        [Fact]
        public void MultiHeadRecursiveCall()
        {
            string def = @"
concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-
    concatenate(L1, L2, L3).";

            "call(concatenate([], [], []))".True(def);
            "call(concatenate([a,b], [c,d], [a,b,c,d]))".True(def);
            "call(concatenate([a,b], [], [a,b]))".True(def);
            "call(concatenate([], [c], [c]))".True(def);
            "call(concatenate([], [c,d], [c,d]))".True(def);
            "call(concatenate([s], [], [s]))".True(def);
            "call(concatenate([a,b], [], [a,b]))".True(def);
        }

        [Fact]
        public void SubAtom()
        {
            //TODO
            "sub_atom(abracadabra, 0, 5, _, S2), S2='abrac'".True();

            @"test([])".False(@"test(X):- '\='(X, []).");
            @"test(1)".True(@"test(X):- '\='(X, []).");
            @"true".True(@"test(X):- X \= [].");
            @"atom(atom)".True(
                "foo(GAR, foobar(DEDAL), [KAIN, ABEL], banan) :- GAR = IAR, IAR = [DEDAL], DEDAL = abc.");

            /*
            'sub_atom', [sub_atom(abracadabra, _, 5, 0, S2), [[S2<-- 'dabra']]]).
'sub_atom', [sub_atom(abracadabra, 3, Length, 3, S2), [[Length<-- 5, S2<-- 'acada']]]).
'sub_atom', [sub_atom(abracadabra, Before, 2, After, ab),

                     [[Before <-- 0, After <-- 9],
                     [Before<-- 7, After<-- 2]]]).
'sub_atom', [sub_atom('Banana', 3, 2, _, S2), [[S2<-- 'an']]]).
'sub_atom', [sub_atom('charity', Before, 3, After, S2),

            [[Before <-- 0, After <-- 4, S2 <-- 'cha'],
                     [Before<-- 1, After<-- 3, S2<-- 'har'],
                     [Before<-- 2, After<-- 2, S2<-- 'ari'],
                     [Before<-- 3, After<-- 1, S2<-- 'rit'],
                     [Before<-- 4, After<-- 0, S2<-- 'ity']]]).
'sub_atom', [sub_atom('ab', Before, Length, After, Sub_atom),

                    [[Before <-- 0, Length <-- 0, After <-- 2, Sub_atom <-- ''],
                     [Before<-- 0, Length<-- 1, After<-- 1, Sub_atom<-- 'a'],
                     [Before<-- 0, Length<-- 2, After<-- 0, Sub_atom<-- 'ab'],
                     [Before<-- 1, Length<-- 0, After<-- 1, Sub_atom<-- ''],
                     [Before<-- 1, Length<-- 1, After<-- 0, Sub_atom<-- 'b'],
                     [Before<-- 2, Length<-- 0, After<-- 0, Sub_atom<-- '']]]).
'sub_atom', [sub_atom(Banana, 3, 2, _, S2), instantiation_error]).
'sub_atom', [sub_atom(f(a), 2, 2, _, S2), type_error(atom, f(a))]).
'sub_atom', [sub_atom('Banana', 4, 2, _, 2), type_error(atom, 2)]).
'sub_atom', [sub_atom('Banana', a, 2, _, S2), type_error(integer, a)]).
'sub_atom', [sub_atom('Banana', 4, n, _, S2), type_error(integer, n)]).
'sub_atom', [sub_atom('Banana', 4, _, m, S2), type_error(integer, m)]).
             */
        }

        [Fact]
        public void SetPrologFlag()
        {
            "set_prolog_flag(unknown, fail), current_prolog_flag(unknown, V), V=fail".True();
            /*
'set_prolog_flag', [set_prolog_flag(X, warning), instantiation_error]).
'set_prolog_flag', [set_prolog_flag(5, decimals), type_error(atom,5)]).
'set_prolog_flag', [set_prolog_flag(date, 'July 1999'), domain_error(prolog_flag,date)]).
'set_prolog_flag', [set_prolog_flag(debug, no), domain_error(flag_value,debug+no)]).
'set_prolog_flag', [set_prolog_flag(max_arity, 40), permission_error(modify, flag, max_arity)]).
'set_prolog_flag', [set_prolog_flag(double_quotes, atom), success]).
%'set_prolog_flag', [X = ""fred"", [[X <-- fred]]]).
%Use read/2 because the fileContents predicate is already parsed.
'set_prolog_flag', [read(""\""fred\"". "", X), [[X<-- fred]]]).
'set_prolog_flag', [set_prolog_flag(double_quotes, chars), success]).
%'set_prolog_flag', [X = ""fred"", [[X<-- [f, r, e, d]]]]).
'set_prolog_flag', [read(""\""fred\"". "", X), [[X<-- [f, r, e, d]]]]).
'set_prolog_flag', [set_prolog_flag(double_quotes, codes), success]).
%'set_prolog_flag', [X = ""fred"", [[X<-- [102,114,101,100]]]]).
'set_prolog_flag', [read(""\""fred\"". "", X), [[X<-- [102,114,101,100]]]]).
             */
        }

        [Fact]
        public void NumberChars()
        {
            //TODO
            "number_chars(33,L), L = ['3', '3']".True();
            "number_chars(33,['3','3'])".True();
            "number_chars(33.0,L), L =['3', '3', '.', '0]".True();
            "number_chars(X,['3','.','3','E','+','0']), X=3.3".True();
            "number_chars(3.3,['3','.','3'])".True();

            string d = @"'number_chars', [number_chars(A,['-','2','5']), [[A <-- (-25)]]]).
'number_chars', [number_chars(A,['\n',' ','3']), [[A <-- 3]]]).
'number_chars', [number_chars(A,['3',x]), syntax_error(_)]).
'number_chars', [number_chars(A,['0',x,f]), [[A <-- 15]]]).
'number_chars', [number_chars(A,['0','''','A']), [[A <-- 65]]]).
'number_chars', [number_chars(A,['4','.','2']), [[A <-- 4.2]]]).
'number_chars', [number_chars(A,['4','2','.','0','e','-','1']), [[A <-- 4.2]]]).
'number_chars', [number_chars(A,L), instantiation_error]).
'number_chars', [number_chars(a,L), type_error(number, a)]).
'number_chars', [number_chars(A,4), type_error(list, 4)]).
'number_chars', [number_chars(A,['4',2]), type_error(character, 2)]).
%'number_codes', [number_codes(33,L), [[L <-- [0'3,0'3]]]]).
%'number_codes', [number_codes(33,[0'3,0'3]), success]).
'number_codes', [number_codes(33.0,L), [[L <-- [51,51,46,48]]]]) :- intAndFloatAreDifferent.
%'number_codes', [number_codes(33.0,[0'3,0'3,0'.,0'0]), success]) :- intAndFloatAreDifferent.
%'number_codes', [number_codes(A,[0'-,0'2,0'5]), [[A <-- (-25)]]]).
%'number_codes', [number_codes(A,[0' ,0'3]), [[A <-- 3]]]).
%'number_codes', [number_codes(A,[0'0,0'x,0'f]), [[A <-- 15]]]).
%'number_codes', [number_codes(A,[0'0,39,0'a]), [[A <-- 97]]]).
%'number_codes', [number_codes(A,[0'4,0'.,0'2]), [[A <-- 4.2]]]).
%'number_codes', [number_codes(A,[0'4,0'2,0'.,0'0,0'e,0'-,0'1]), [[A <-- 4.2]]]).
'number_codes', [number_codes(A,L), instantiation_error]).
'number_codes', [number_codes(a,L), type_error(number,a)]).
'number_codes', [number_codes(A,4), type_error(list,4)]).
%'number_codes', [number_codes(A,[ 0'1, 0'2, '3']), representation_error(character_code)]).";
        }

        [Fact]
        public void AtomCodes()
        {
            //TODO
            "atom_codes('',L), L=[]".True();

            /*
%'atom_codes', [atom_codes([],L), [[L <-- [ 0'[, 0'] ]]]]).
'atom_codes', [atom_codes('''',L), [[L <-- [ 39 ]]]]).
%'atom_codes', [atom_codes('iso',L), [[L <-- [ 0'i, 0's, 0'o ]]]]).
%'atom_codes', [atom_codes(A,[ 0'p, 0'r, 0'o, 0'l, 0'o, 0'g]), [[A <-- 'prolog']]]).
%'atom_codes', [atom_codes('North',[0'N | L]), [[L <-- [0'o, 0'r, 0't, 0'h]]]]).
%'atom_codes', [atom_codes('iso',[0'i, 0's]), failure]).
'atom_codes', [atom_codes(A,L), instantiation_error]).
'atom_codes', [atom_codes(f(a),L), type_error(atom,f(a))]).
%'atom_codes', [atom_codes(A, 0'x), type_error(list,0'x)]).
%'atom_codes', [atom_codes(A,[ 0'i, 0's, o]), representation_error(character_code)]).
             */
        }
    }
}