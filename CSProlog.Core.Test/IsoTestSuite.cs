using Xunit;

namespace CSPrologTest
{
    public class IsoTestSuite
    {
        [Fact]
        public void Abolish()
        {
            //"abolish(abolish/1)".Error();
            "abolish(foo/a)".Error();
            //"abolish(foo/(-1))".Error();
            //"current_prolog_flag(max_arity,A), X is A + 1, abolish(foo/X)".Error();
            "abolish(5/2)".Error();
        }

        [Fact]
        public void And()
        {
            "X=1, var(X)".False();
            "var(X), X=1".True();
            "fail, call(3)".False();
            "nofoo(X), call(X)".False();
            "X = true, call(X)".True();
        }

        [Fact]
        public void Arg()
        {
            "arg(1,foo(a,b),a)".True();
            "arg(1,foo(a,b),X), X=a".True();
            "arg(1,foo(a,b),X)".True();
            "arg(1,foo(X,b),a), X=a".True();
            "arg(1,foo(X,b),a)".True();
            "arg(2,foo(a, f(X,b), c), f(a, Y)), X=a, Y=b".True();
            "arg(1,foo(X,b),Y), X=Y".True();
            "arg(1,foo(X,b),Y), X=a, Y=a".True();
            "arg(1,foo(a,b),b)".False();
            "arg(0,foo(a,b),foo)".False();
            "arg(3,foo(3,4),N)".False();
            "arg(X,foo(a,b),a)".Error();
            "arg(0,atom,A)".Error();
            "arg(0,3,A)".Error();
            //"arg(-3,foo(a,b),A)".Error();
            "arg(a,foo(a,b),X)".Error();
        }

        [Fact]
        public void ArithDiff()
        {
            "'=\\='(0,1)".True();
            "'=\\='(1.0,1)".False();
            "'=\\='(3 * 2,7 - 1)".False();
            "'=\\='(N,5)".Error();
            "'=\\='(floot(1),5)".Error();
        }

        [Fact]
        public void ArithEq()
        {
            "'=:='(0,1)".False();
            "'=:='(1.0,1)".True();
            "'=:='(3 * 2,7 - 1)".True();
            "'=:='(N,5)".Error();
            "'=:='(floot(1),5)".Error();
        }

        [Fact]
        public void ArithGt()
        {
            "'>'(0,1)".False();
            "'>'(1.0,1)".False();
            "'>'(3*2,7-1)".False();
            "'>'(X,5)".Error();
            "'>'(2 + floot(1),5)".Error();
        }

        [Fact]
        public void ArithGtEq()
        {
            "'>='(0,1)".False();
            "'>='(1.0,1)".True();
            "'>='(3*2,7-1)".True();
            "'>='(X,5)".Error();
            "'>='(2 + floot(1),5)".Error();
        }

        [Fact]
        public void ArithLt()
        {
            "'<'(0,1)".True();
            "'<'(1.0,1)".False();
            "'<'(3*2,7-1)".False();
            "'<'(X,5)".Error();
            "'<'(2 + floot(1),5)".Error();
        }

        [Fact]
        public void ArithLtEq()
        {
            "'=<'(0,1)".True();
            "'=<'(1.0,1)".True();
            "'=<'(3*2,7-1)".True();
            "'=<'(X,5)".Error();
            "'=<'(2 + floot(1),5)".Error();
        }

        [Fact]
        public void Asserta()
        {
            "asserta((bar(X) :- X)), clause(bar(X), B)".True();
            "asserta(_)".Error();
            //"asserta(4)".Error();
            //"asserta((foo :- 4))".Error();
            //"asserta((atom(_) :- true))".Error();
        }

        [Fact]
        public void Assertz()
        {
            "assertz((foo(X) :- X -> call(X)))".True();
            "assertz(_)".Error();
            //"assertz(4)".Error();
            //"assertz((foo :- 4))".Error();
            //"assertz((atom(_) :- true))".Error();
        }

        [Fact]
        public void Atom()
        {
            "atom(atom)".True("foo(GAR, foobar(DEDAL), [KAIN, ABEL], banan) :- GAR = IAR, IAR = [DEDAL], DEDAL = abc.");
            "atom('string')".True();
            "atom(Var)".False();
            "atom(a(b))".False();
            "atom([])".True();
            "atom(6)".False();
            "atom(3.3)".False();
        }

        [Fact]
        
        public void AtomChars()
        {
            "atom_chars('',L), L=[]".True();
            "atom_chars('iso',L), L=['i', 's', 'o']".True();
            "atom_chars(A,['p','r','o','l','o','g']), A = 'prolog'".True();
            "atom_chars('iso',['i','s'])".False();
            //"atom_chars(A,L)".Error();
            //"atom_chars(A,[a,E,c])".Error();
            //"atom_chars(A,[a,b|L])".Error();
            //"atom_chars(f(a),L)".Error();
            //"atom_chars(A,iso)".Error();
            //"atom_chars(A,[a,f(b)])".Error();
            "atom_chars(X,['1','2']), Y is X + 1".Error();
            "atom_chars('North',['N'|X]), X = ['o','r','t','h']".True();
            "atom_chars([],L), L=['[',']']".True();
            //"atom_chars('''',L), L=['''']".True();
        }

        [Fact]
        
        public void AtomCodes()
        {
            // TODO
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

        [Fact]
        public void ListSyntax()
        {
            "[1, 2, 3] = [X|Y], X = 1, Y = [2, 3]".True();
            "[1, s] = [X|Y], X = 1, Y = [s]".True();
            "[1, s] = [X|[Y]], X = 1, Y = s".True();
            "[1, s, 3] = [X|[Y, Z]], X = 1, Y = s, Z = 3".True();
            "['N', s, 3] = ['N'|[Y, Z]], Y = s, Z = 3".True();
            "[n, s, 3] = ['n'|[Y, Z]], Y = s, Z = 3".True();
            "['n', s, 3] = [n|[Y, Z]], Y = s, Z = 3".True();
        }

        [Fact]
        
        public void AtomConcat()
        {
            "atom_concat('hello',' world',A), A = 'hello world'".True();
            "atom_concat('hello','world',A), A = helloworld".True();
            "atom_concat(T,' world','small world'), T = 'small'".True();
            "atom_concat('small', T, 'small world'), T = ' world'".True();
            "atom_concat(hello,' world','small world')".False();
            //"atom_concat(T1,T2,'hello'),T1 = he, T2=llo".True();
            //"atom_concat(A1,'iso',A3)".Error();
            //"atom_concat('iso',A2,A3)".Error();
            //"atom_concat(f(a),'iso',A3)".Error();
            //"atom_concat('iso',f(a),A3)".Error();
            //"atom_concat(A1,A2,f(a))".Error();
        }

        [Fact]
        
        public void AtomLength()
        {
            "atom_length('enchanted evening', N), N = 17".True();
            "atom_length('', N), N=0".True();
            "atom_length('scarlet', 5)".False();
            //"atom_length(Atom, 4)".Error();
            //"atom_length(1.23, 4)".Error();
            //"atom_length(atom, '4')".Error();
        }

        [Fact]
        public void Atomic()
        {
            "atomic(atom)".True();
            "atomic(a(b))".False();
            "atomic(Var)".False();
            "atomic([])".True();
            "atomic(6)".True();
            "atomic(3.3)".True();
        }

        [Fact]
        
        public void Bagof()
        {
            "bagof(X,(X=1;X=2),L), L = [1, 2]".True();
            "bagof(X,(X=1;X=2),X), X = [1, 2]".True();
            //"bagof(X,(X=Y;X=Z),L)".True();
            "bagof(X,fail,L)".False();
            "bagof(1,(Y=1;Y=2),L), L = [1, 1]".True();
            "bagof(f(X,Y),(X=a;Y=b),L), L = [f(a, _), f(_, b)]".True();
            //"bagof(X,Y^((X=1,Y=1);(X=2,Y=2)),S), S = [1, 2]".True();
            //"bagof(X,Y^((X=1;Y=1);(X=2,Y=2)),S), S = [1, _, 2]".True();
            //"bagof(X,(Y^(X=1;Y=1);X=3),S)), S = [3]".True();
            //"bagof(X,(X=Y;X=Z;Y=1),L), (L = [Y, Z]; (L=[_], Y=1))".True();
            "bagof(X,Y^Z,L)".Error();
            "bagof(X,1,L)".Error();
            "findall(X,call(4),S)".Error();
        }

        [Fact]
        public void Call()
        {
            "call(!)".True();
            "call(fail)".False();
            "call((fail, X))".False();
            "call((fail, call(1)))".False();
            //"call((write(3), X))".Error();
            "call((write(3), call(1)))".Error();
            //            "call(X)".Error();
            "call(1)".Error();
            "call((fail, 1))".False();
            "call((write(3), 1))".Error();
            "call((1; true))".Error();
        }

        [Fact]
        
        public void CatchAndThrow()
        {
            "catch(true, C, write('something')), throw(blabla)".Error();
            // "catch(foobar(abc,L), error, fail)".False();
        }

        [Fact]
        
        public void CharCode()
        {
            "char_code(a,Code)".True();
            "char_code(Char,99)".True();
            "char_code(b,98)".True();
            "char_code('ab',Code)".Error();
            "char_code(a,x)".Error();
            "char_code(Char,Code)".Error();
            "char_code(Char,-2)".Error();
        }

        [Fact]
        public void Clause()
        {
            "clause(x,Body)".False();
            "clause(_,B)".Error();
            //"clause(4,B)".Error();
            //"clause(f(_),5)".Error();
            //"clause(atom(_),Body)".Error();
        }

        [Fact]
        public void Compound()
        {
            "compound(33.3)".False();
            "compound(-33.3)".False();
            "compound(-a)".True();
            "compound(_)".False();
            "compound(a)".False();
            "compound(a(b))".True();
            "compound([a])".True();
        }

        [Fact]
        public void CopyTerm()
        {
            "copy_term(X,Y)".True();
            "copy_term(X,3)".True();
            "copy_term(_,a)".True();
            "copy_term(a+X,X+b), X = a".True();
            "copy_term(_,_)".True();
            "copy_term(X+X+Y,A+B+B), A = B".True();
            "copy_term(X+X+Y,A+B+B), A=1, B=1".True();
            "copy_term(a,a)".True();
            "copy_term(a,b)".False();
            "copy_term(f(a),f(X)), X=a".True();
            "copy_term(a+X,X+b),copy_term(a+X,X+b)".False();
        }

        [Fact]
        
        public void CurrentIO()
        {
            "exists(current_input/1)".True();
            "exists(current_output/1)".True();
        }

        [Fact]
        
        public void CurrentPredicate()
        {
            //"current_predicate(current_predicate/1)".False();
            //"current_predicate(score/3)".True();
            "current_predicate(copy_term/2)".True();
            "functor(copy_term(a,a), Name, _),current_predicate(Name/2)".True();
            //"current_predicate(4)".Error();
            //"current_predicate(dog)".Error();
            //"current_predicate(0/dog)".Error();
        }

        [Fact]
        
        public void CurrentPrologFlag()
        {
            //"current_prolog_flag(debug, off)".True();
            //"set_prolog_flag(unknown, warning), current_prolog_flag(unknown, warning)".True();
            //"set_prolog_flag(unknown, warning), current_prolog_flag(unknown, error)".False();
            //"current_prolog_flag(debug, V), V=off".True();
            //"current_prolog_flag(5, V)".Error();
            //"current_prolog_flag(warning, V)".Error();
        }

        [Fact]
        public void Cut()
        {
            "!".True();
            //"!,fail;true".False();
            "call(!),fail;true".True();
        }

        [Fact]
        public void Fail()
        {
            "fail".False();
            "undef_pred".False();
            @"\+ fail".True();
            //"set_prolog_flag(unknown, fail), undef_pred".False();
            //"set_prolog_flag(unknown, warning), undef_pred".False();
        }

        [Fact]
        public void Findall()
        {
            "findall(X,(X=1 ; X=2),S), S=[1,2]".True();
            "findall(X+Y,(X=1),S), S=[1+_]".True();
            "findall(X,fail,L), L=[]".True();
            "findall(X,(X=1 ; X=1),S), S=[1,1]".True();
            "findall(X,(X=2 ; X=1),[1,2])".False();
            "findall(X,(X=1 ; X=2),[X,Y]), X=1, Y=2".True();
            //"findall(X,Goal,S)".Error();
            "findall(X,4,S)".Error();
            "findall(X,call(1),S)".Error();
        }

        [Fact]
        public void Float()
        {
            "float(3.3)".True();
            "float(-3.3)".True();
            "float(3)".False();
            "float(atom)".False();
            "float(X)".False();
        }

        [Fact]
        public void Functor()
        {
            "functor(foo(a,b,c),foo,3)".True();
            "functor(foo(a,b,c),X,Y), X=foo, Y=3".True();
            "functor(X,foo,3), X=foo(A, B, C)".True();
            "functor(X,foo,0), X=foo".True();
            "functor(mats(A,B),A,B), A=mats, B=2".True();
            "functor(foo(a),foo,2)".False();
            "functor(foo(a),fo,1)".False();
            "functor(1,X,Y), X=1, Y=0".True();
            "functor(X,1.1,0), X=1.1".True();
            "functor([_|_],'.',2)".True();
            "functor([],[],0)".True();
            "functor(X, Y, 3)".Error();
            "functor(X, foo, N)".Error();
            "functor(X, foo, a)".Error();
            "functor(F, 1.5, 1)".Error();
            "functor(F,foo(a),1)".Error();
            //"current_prolog_flag(max_arity,A), X is A + 1, functor(T, foo, X)".Error();
            "functor(T, foo, -1)".Error();
        }

        [Fact]
        public void IfThen()
        {
            "'->'(true, true)".True();
            "'->'(true, fail)".False();
            "'->'(fail, true)".False();
            "'->'(true, X=1), X=1".True();
            "'->'((X=1; X=2), true), X=1".True();
            "'->'(true, (X=1; X=2)), (X=1;X=2)".True();
        }

        [Fact]
        public void IfThenElse()
        {
            "'->'(true, true); fail".True();
            "'->'(fail, true); true".True();
            "'->'(true, fail); fail".False();
            "'->'(fail, true); fail".False();
            "('->'(true, X=1); X=2), X=1".True();
            "('->'(fail, X=1); X=2), X=2".True();
            "('->'(true, (X=1; X=2)); true), (X=1 ; X=2)".True();
            "('->'((X=1; X=2), true); true), X=1".True();
        }

        [Fact]
        public void Integer()
        {
            "integer(3)".True();
            "integer(-3)".True();
            "integer(3.3)".False();
            "integer(X)".False();
            "integer(atom)".False();
        }

        [Fact]
        public void Is()
        {
            "'is'(Result,3 + 11.0), Result=14.0".True();
            "X = 1 + 2, 'is'(Y, X * 3), X = 1+2, Y=9".True();
            "'is'(foo,77)".False();
            "'is'(77, N)".Error();
            "'is'(77, foo)".Error();
            "'is'(X,float(3)), X=3.0".True();
        }

        [Fact]
        public void NoVar()
        {
            "nonvar(33.3)".True();
            "nonvar(foo)".True();
            "nonvar(Foo)".False();
            "foo=Foo,nonvar(Foo), Foo=foo".True();
            "nonvar(_)".False();
            "nonvar(a(b))".True();
        }

        [Fact]
        public void NotProvable()
        {
            @"\+(true)".False();
            @"\+(!)".False();
            //@"(X=1;X=2), \+((!,fail)), (X=1;X=2)".True();
            @"\+(4 = 5)".True();
            @"\+(3)".Error();
            //@"\+((!,fail))".True();
            //@"\+(X)".Error();
        }

        [Fact]
        public void NotUnify()
        {
            @"'\='(1,1)".False();
            @"'\='(X,1)".False();
            @"'\='(X,Y)".False();
            @"'\='(X,Y),'\='(X,abc)".False();
            @"'\='(f(X,def),f(def,Y))".False();
            //@"'\='(1,2)".True();
            @"'\='(1,1.0)".False();
            //@"'\='(g(X),f(f(X)))".True();
            //@"'\='(f(X,1),f(a(X)))".True();
            //@"'\='(f(X,Y,X),f(a(X),a(Y),Y,2))".True();
            "test([])".False(@"test(X):- '\='(X, []).");
            //"test(1)".True(@"test(X):- '\='(X, []).");
            //"true".True(@"test(X):- X \= [].");
        }

        [Fact]
        public void Number()
        {
            "number(3)".True();
            "number(3.3)".True();
            "number(-3)".True();
            "number(a)".False();
            "number(X)".False();
        }

        [Fact]
        
        public void NumberChars()
        {
            // TODO
            "number_chars(33,L), L = ['3', '3']".True();
            "number_chars(33,['3','3'])".True();
            //"number_chars(33.0,L), L =['3', '3', '.', '0]".True();
            "number_chars(X,['3','.','3','E','+','0']), X=3.3".True();
            "number_chars(3.3,['3','.','3'])".True();

            var d = @"'number_chars', [number_chars(A,['-','2','5']), [[A <-- (-25)]]]).
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
        public void Once()
        {
            "once(!)".True();
            "once(!), (X=1; X=2), (X=1;X=2)".True();
            "once(repeat)".True();
            "once(fail)".False();
            "once(3)".Error();
            //"once(X)".Error();
        }

        [Fact]
        public void Or()
        {
            "true; fail".True();
            //"(!, fail); true".False();
            "(!; call(3))".Error();
            "((X=1, !); X=2), X=1".True();
            "(X=1; X=2)".True();
        }

        [Fact]
        
        public void Repeat()
        {
            "repeat,!,fail".False();
        }

        [Fact]
        public void Retract()
        {
            "retract((4 :- X))".False();
            "retract((atom(_) :- X == '[]'))".False();
        }

        [Fact]
        public void SetPrologFlag()
        {
            //"set_prolog_flag(unknown, fail), current_prolog_flag(unknown, V), V=fail".True();
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
        
        public void Setof()
        {
            "setof(X, (X = 1; X=2),L), L=[1, 2]".True();
            "setof(X, (X = 1; X=2),X), X=[1, 2]".True();
            "setof(X, (X = 2; X=1),L), L=[1, 2]".True();
            "setof(X, (X = 2; X=2),L), L=[2]".True();
            "setof(X, fail, L)".False();
            "setof(1, (Y = 2; Y=1),L), ((L=[1],Y=1);(L=[1],Y=2))".True();
            "setof(f(X, Y), (X = a;Y = b), L), L=[f(_,b), f(a,_)]".True();
            //"setof(X, Y ^ ((X = 1, Y = 1);(X=2,Y=2)),S), S=[1, 2]".True();
            //"setof(X, Y ^ ((X = 1; Y=1);(X=2,Y=2)),S), S=[_, 1, 2]".True();
            //"set_prolog_flag(unknown, warning), setof(X, (Y ^ (X = 1; Y = 1); X = 3),S), S=[3]".True();
            //"set_prolog_flag(unknown, warning), setof(X, Y ^ (X = 1; Y = 1; X = 3),S), S=[_,1,3]".True();
            //"setof(X, (X = Y; X=Z;Y=1),L), (L=[Y,Z]; (L=[_],Y=1))".True();
            "setof(X, X ^ (true; 4), L)".Error();
            "setof(X, 1, L)".Error();
        }

        [Fact]
        
        public void SubAtom()
        {
            // TODO
            "sub_atom(abracadabra, 0, 5, _, S2), S2='abrac'".True();

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
        public void TermDiff()
        {
            @"'\=='(1,1)".False();
            @"'\=='(X, X)".False();
            @"'\=='(1,2)".True();
            @"'\=='(X,1)".True();
            @"'\=='(X, Y)".True();
            @"'\=='(_, _)".True();
            @"'\=='(X, a(X))".True();
            @"'\=='(f(a), f(a))".False();
        }

        [Fact]
        public void TermEq()
        {
            "'=='(1,1)".True();
            "'=='(X, X)".True();
            "'=='(1,2)".False();
            "'=='(X,1)".False();
            "'=='(X, Y)".False();
            "'=='(_, _)".False();
            "'=='(X, a(X))".False();
            "'=='(f(a), f(a))".True();
        }

        [Fact]
        public void TermGt()
        {
            "'@>'(1.0,1)".False();
            "'@>'(aardvark, zebra)".False();
            "'@>'(short, short)".False();
            "'@>'(short, shorter)".False();
            "'@>'(foo(b), foo(a))".True();
            "'@>'(X, X)".False();
            "'@>'(foo(a, X), foo(b, Y))".False();

            //"'@>='(1.0,1)".True();
            "'@>='(aardvark, zebra)".False();
            "'@>='(short, short)".True();
            "'@>='(short, shorter)".False();
            "'@>='(foo(b), foo(a))".True();
            "'@>='(X, X)".True();
            "'@>='(foo(a, X), foo(b, Y))".False();
        }

        [Fact]
        public void TermLt()
        {
            //"'@<'(1.0,1)".False();
            "'@<'(aardvark, zebra)".True();
            "'@<'(short, short)".False();
            "'@<'(short, shorter)".True();
            "'@<'(foo(b), foo(a))".False();
            "'@<'(X, X)".False();
            "'@<'(foo(a, X), foo(b, Y))".True();

            //"'@=<'(1.0,1)".True();
            "'@=<'(aardvark, zebra)".True();
            "'@=<'(short, short)".True();
            "'@=<'(short, shorter)".True();
            "'@=<'(foo(b), foo(a))".False();
            "'@=<'(X, X)".True();
            "'@=<'(foo(a, X), foo(b, Y))".True();
        }

        [Fact]
        public void True()
        {
            "true".True();
        }

        [Fact]
        public void Unify()
        {
            "'='(1,1)".True();
            "1=1".True();
            "'='(X,1)".True();
            "'='(X,Y), X=a, Y='a'".True();
            "'='(X, Y), '='(X, abc), (X=abc; Y=abc)".True();
            "'='(f(X, def), f(def, Y)), (X=def; Y=def)".True();
            "'='(1,2)".False();
            //"'='(1,1.0)".True();
            "'='(g(X), f(f(X)))".False();
            "'='(f(X, 1), f(a(X)))".False();
            "'='(f(X, Y, X), f(a(X), a(Y), Y, 2))".False();
            "'='(f(A, B, C), f(g(B, B), g(C, C), g(D, D))), A=g(g(g(D, D), g(D, D)), g(g(D, D), g(D, D))), B=g(g(D, D), g(D, D)), C=g(D, D)".True();
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
    }
}