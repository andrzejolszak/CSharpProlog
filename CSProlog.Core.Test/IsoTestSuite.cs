using Xunit;

namespace CSPrologTest
{
    public class IsoTestSuite
    {
        [Theory]
        [InlineData(@"R: abolish(abolish/1)")]
        [InlineData(@"R: abolish(foo/a)")]
        [InlineData(@"R: abolish(foo/(-1))")]
        [InlineData(@"R: current_prolog_flag(max_arity,A), X is A + 1, abolish(foo/X)")]
        [InlineData(@"R: abolish(5/2)")]
        public void Abolish(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: X=1, var(X)")]
        [InlineData(@"T: var(X), X=1")]
        [InlineData(@"F: fail, call(3)")]
        [InlineData(@"F: nofoo(X), call(X)")]
        [InlineData(@"T: X = true, call(X)")]
        public void And(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: arg(1,foo(a,b),a)")]
        [InlineData(@"T: arg(1,foo(a,b),X), X=a")]
        [InlineData(@"T: arg(1,foo(a,b),X)")]
        [InlineData(@"T: arg(1,foo(X,b),a), X=a")]
        [InlineData(@"T: arg(1,foo(X,b),a)")]
        [InlineData(@"T: arg(2,foo(a, f(X,b), c), f(a, Y)), X=a, Y=b")]
        [InlineData(@"T: arg(1,foo(X,b),Y), X=Y")]
        [InlineData(@"T: arg(1,foo(X,b),Y), X=a, Y=a")]
        [InlineData(@"F: arg(1,foo(a,b),b)")]
        [InlineData(@"F: arg(0,foo(a,b),foo)")]
        [InlineData(@"F: arg(3,foo(3,4),N)")]
        [InlineData(@"R: arg(X,foo(a,b),a)")]
        [InlineData(@"R: arg(0,atom,A)")]
        [InlineData(@"R: arg(0,3,A)")]
        [InlineData(@"R: arg(-3,foo(a,b),A)")]
        [InlineData(@"R: arg(a,foo(a,b),X)")]
        public void Arg(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: '=\\='(0,1)")]
        [InlineData(@"F: '=\\='(1.0,1)")]
        [InlineData(@"F: '=\\='(3 * 2,7 - 1)")]
        [InlineData(@"R: '=\\='(N,5)")]
        [InlineData(@"R: '=\\='(floot(1),5)")]
        public void ArithDiff(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: '=:='(0,1)")]
        [InlineData(@"T: '=:='(1.0,1)")]
        [InlineData(@"T: '=:='(3 * 2,7 - 1)")]
        [InlineData(@"R: '=:='(N,5)")]
        [InlineData(@"R: '=:='(floot(1),5)")]
        public void ArithEq(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: '>'(0,1)")]
        [InlineData(@"F: '>'(1.0,1)")]
        [InlineData(@"F: '>'(3*2,7-1)")]
        [InlineData(@"R: '>'(X,5)")]
        [InlineData(@"R: '>'(2 + floot(1),5)")]
        public void ArithGt(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: '>='(0,1)")]
        [InlineData(@"T: '>='(1.0,1)")]
        [InlineData(@"T: '>='(3*2,7-1)")]
        [InlineData(@"R: '>='(X,5)")]
        [InlineData(@"R: '>='(2 + floot(1),5)")]
        public void ArithGtEq(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: '<'(0,1)")]
        [InlineData(@"F: '<'(1.0,1)")]
        [InlineData(@"F: '<'(3*2,7-1)")]
        [InlineData(@"R: '<'(X,5)")]
        [InlineData(@"R: '<'(2 + floot(1),5)")]
        public void ArithLt(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: '=<'(0,1)")]
        [InlineData(@"T: '=<'(1.0,1)")]
        [InlineData(@"T: '=<'(3*2,7-1)")]
        [InlineData(@"R: '=<'(X,5)")]
        [InlineData(@"R: '=<'(2 + floot(1),5)")]
        public void ArithLtEq(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: asserta((bar(X) :- X)), clause(bar(X), B)")]
        [InlineData(@"R: asserta(_)")]
        [InlineData(@"R: asserta(4)")]
        [InlineData(@"R: asserta((foo :- 4))")]
        [InlineData(@"R: asserta((atom(_) :- true))")]
        public void Asserta(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: assertz((foo(X) :- X -> call(X)))")]
        [InlineData(@"R: assertz(_)")]
        [InlineData(@"R: assertz(4)")]
        [InlineData(@"R: assertz((foo :- 4))")]
        [InlineData(@"R: assertz((atom(_) :- true))")]
        public void Assertz(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: atom('string')")]
        [InlineData(@"F: atom(Var)")]
        [InlineData(@"F: atom(a(b))")]
        [InlineData(@"T: atom([])")]
        [InlineData(@"F: atom(6)")]
        [InlineData(@"F: atom(3.3)")]
        public void Atom(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: atom_chars('',L), L=[]")]
        [InlineData(@"T: atom_chars('iso',L), L=['i', 's', 'o']")]
        [InlineData(@"T: atom_chars(A,['p','r','o','l','o','g']), A = 'prolog'")]
        [InlineData(@"F: atom_chars('iso',['i','s'])")]
        [InlineData(@"R: atom_chars(A,L)")]
        [InlineData(@"R: atom_chars(A,[a,E,c])")]
        [InlineData(@"R: atom_chars(A,[a,b|L])")]
        [InlineData(@"R: atom_chars(f(a),L)")]
        [InlineData(@"R: atom_chars(A,iso)")]
        [InlineData(@"R: atom_chars(A,[a,f(b)])")]
        [InlineData(@"R: atom_chars(X,['1','2']), Y is X + 1")]
        [InlineData(@"T: atom_chars('North',['N'|X]), X = ['o','r','t','h']")]
        [InlineData(@"T: atom_chars([],L), L=['[',']']")]
        [InlineData(@"T: atom_chars('''',L), L=['''']")]
        public void AtomChars(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: [1, 2, 3] = [X|Y], X = 1, Y = [2, 3]")]
        [InlineData(@"T: [1, s] = [X|Y], X = 1, Y = [s]")]
        [InlineData(@"T: [1, s] = [X|[Y]], X = 1, Y = s")]
        [InlineData(@"T: [1, s, 3] = [X|[Y, Z]], X = 1, Y = s, Z = 3")]
        [InlineData(@"T: ['N', s, 3] = ['N'|[Y, Z]], Y = s, Z = 3")]
        [InlineData(@"T: [n, s, 3] = ['n'|[Y, Z]], Y = s, Z = 3")]
        [InlineData(@"T: ['n', s, 3] = [n|[Y, Z]], Y = s, Z = 3")]
        public void ListSyntax(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: atom_concat('hello',' world',A), A = 'hello world'")]
        [InlineData(@"T: atom_concat('hello','world',A), A = helloworld")]
        [InlineData(@"T: atom_concat(T,' world','small world'), T = 'small'")]
        [InlineData(@"T: atom_concat('small', T, 'small world'), T = ' world'")]
        [InlineData(@"F: atom_concat(hello,' world','small world')")]
        [InlineData(@"T: atom_concat(T1,T2,'hello'),T1 = he, T2=llo")]
        [InlineData(@"R: atom_concat(A1,'iso',A3)")]
        [InlineData(@"R: atom_concat('iso',A2,A3)")]
        [InlineData(@"R: atom_concat(f(a),'iso',A3)")]
        [InlineData(@"R: atom_concat('iso',f(a),A3)")]
        [InlineData(@"R: atom_concat(A1,A2,f(a))")]
        public void AtomConcat(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: atom_length('enchanted evening', N), N = 17")]
        [InlineData(@"T: atom_length('', N), N=0")]
        [InlineData(@"F: atom_length('scarlet', 5)")]
        [InlineData(@"R: atom_length(Atom, 4)")]
        [InlineData(@"R: atom_length(1.23, 4)")]
        [InlineData(@"R: atom_length(atom, '4')")]
        public void AtomLength(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: atomic(atom)")]
        [InlineData(@"F: atomic(a(b))")]
        [InlineData(@"F: atomic(Var)")]
        [InlineData(@"T: atomic([])")]
        [InlineData(@"T: atomic(6)")]
        [InlineData(@"T: atomic(3.3)")]
        public void Atomic(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: bagof(X,(X=1;X=2),L), L = [1, 2]")]
        [InlineData(@"T: bagof(X,(X=1;X=2),X), X = [1, 2]")]
        [InlineData(@"T: bagof(X,(X=Y;X=Z),L)")]
        [InlineData(@"F: bagof(X,fail,L)")]
        [InlineData(@"T: bagof(1,(Y=1;Y=2),L), L = [1, 1]")]
        [InlineData(@"T: bagof(f(X,Y),(X=a;Y=b),L), L = [f(a, _), f(_, b)]")]
        [InlineData(@"T: bagof(X,Y^((X=1,Y=1);(X=2,Y=2)),S), S = [1, 2]")]
        [InlineData(@"T: bagof(X,Y^((X=1;Y=1);(X=2,Y=2)),S), S = [1, _, 2]")]
        [InlineData(@"T: bagof(X,(Y^(X=1;Y=1);X=3),S)), S = [3]")]
        [InlineData(@"T: bagof(X,(X=Y;X=Z;Y=1),L), (L = [Y, Z]; (L=[_], Y=1))")]
        [InlineData(@"R: bagof(X,Y^Z,L)")]
        [InlineData(@"R: bagof(X,1,L)")]
        [InlineData(@"R: findall(X,call(4),S)")]
        public void Bagof(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: call(!)")]
        [InlineData(@"F: call(fail)")]
        [InlineData(@"F: call((fail, X))")]
        [InlineData(@"F: call((fail, call(1)))")]
        [InlineData(@"R: call((write(3), X))")]
        [InlineData(@"R: call((write(3), call(1)))")]
        [InlineData(@"R: call(X)")]
        [InlineData(@"R: call(1)")]
        [InlineData(@"F: call((fail, 1))")]
        [InlineData(@"R: call((write(3), 1))")]
        [InlineData(@"R: call((1; true))")]
        public void Call(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"R: catch(true, C, write('something')), throw(blabla)")]
        [InlineData(@"F: catch(foobar(abc,L), error, fail)")]
        public void CatchAndThrow(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: char_code(a,Code)")]
        [InlineData(@"T: char_code(Char,99)")]
        [InlineData(@"T: char_code(b,98)")]
        [InlineData(@"R: char_code('ab',Code)")]
        [InlineData(@"R: char_code(a,x)")]
        [InlineData(@"R: char_code(Char,Code)")]
        [InlineData(@"R: char_code(Char,-2)")]
        public void CharCode(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: clause(x,Body)")]
        [InlineData(@"R: clause(_,B)")]
        [InlineData(@"R: clause(4,B)")]
        [InlineData(@"R: clause(f(_),5)")]
        [InlineData(@"R: clause(atom(_),Body)")]
        [InlineData(@"F: clause(natnum, X)")]
        [InlineData(@"T: clause(natnum(0), true)")]
        [InlineData(@"T: clause(natnum(1), true)")]
        [InlineData(@"T: clause(natnum(X), true), X = 0")]
        [InlineData(@"T: clause(natnum(X), true), X = 1")]
        [InlineData(@"T: clause(natnum(X), natnum(Z)), X = s(Z)")]
        [InlineData(@"T: clause(natnum(X), F), X = s(Z)")]
        [InlineData(@"T: clause(natnum(s(X)), natnum(X))")]
        [InlineData(@"T: clause(natnum(s(Y)), natnum(Y))")]
        [InlineData(@"T: clause(natnum(s(1)), natnum(1))")]
        [InlineData(@"T: clause(natnum(s(X, 1)), (natnum(X),natnum(1)))")]
        [InlineData(@"T: clause(natnum(s(X, Y)), (natnum(X),natnum(1))), Y = 1")]
        public void Clause(string test)
        {
            string consult =
@"
natnum(0) :- true.
natnum(1).
natnum(s(X)) :- natnum(X).
natnum(s(X, 1)) :- natnum(X), natnum(1).
";
            test.Evaluate(consult);
        }

        [Theory]
        [InlineData(@"F: compound(33.3)")]
        [InlineData(@"F: compound(-33.3)")]
        [InlineData(@"T: compound(-a)")]
        [InlineData(@"F: compound(_)")]
        [InlineData(@"F: compound(a)")]
        [InlineData(@"T: compound(a(b))")]
        [InlineData(@"T: compound([a])")]
        public void Compound(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: copy_term(X,Y)")]
        [InlineData(@"T: copy_term(X,3)")]
        [InlineData(@"T: copy_term(_,a)")]
        [InlineData(@"T: copy_term(a+X,X+b), X = a")]
        [InlineData(@"T: copy_term(_,_)")]
        [InlineData(@"T: copy_term(X+X+Y,A+B+B), A = B")]
        [InlineData(@"T: copy_term(X+X+Y,A+B+B), A=1, B=1")]
        [InlineData(@"T: copy_term(a,a)")]
        [InlineData(@"F: copy_term(a,b)")]
        [InlineData(@"T: copy_term(f(a),f(X)), X=a")]
        [InlineData(@"F: copy_term(a+X,X+b),copy_term(a+X,X+b)")]
        public void CopyTerm(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: exists(current_input/1)")]
        [InlineData(@"T: exists(current_output/1)")]
        public void CurrentIO(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: current_predicate(current_predicate/1)")]
        [InlineData(@"T: current_predicate(score/3)")]
        [InlineData(@"T: current_predicate(copy_term/2)")]
        [InlineData(@"T: functor(copy_term(a,a), Name, _),current_predicate(Name/2)")]
        [InlineData(@"R: current_predicate(4)")]
        [InlineData(@"R: current_predicate(dog)")]
        [InlineData(@"R: current_predicate(0/dog)")]
        public void CurrentPredicate(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: current_prolog_flag(debug, off)")]
        [InlineData(@"T: set_prolog_flag(unknown, warning), current_prolog_flag(unknown, warning)")]
        [InlineData(@"F: set_prolog_flag(unknown, warning), current_prolog_flag(unknown, error)")]
        [InlineData(@"T: current_prolog_flag(debug, V), V=off")]
        [InlineData(@"R: current_prolog_flag(5, V)")]
        [InlineData(@"R: current_prolog_flag(warning, V)")]
        public void CurrentPrologFlag(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: !")]
        [InlineData(@"F: !,fail;true")]
        [InlineData(@"T: call(!),fail;true")]
        public void Cut(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: fail")]
        [InlineData(@"F: undef_pred")]
        [InlineData(@"T: \+ fail")]
        [InlineData(@"F: set_prolog_flag(unknown, fail), undef_pred")]
        [InlineData(@"F: set_prolog_flag(unknown, warning), undef_pred")]
        public void Fail(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: findall(X,(X=1 ; X=2),S), S=[1,2]")]
        [InlineData(@"T: findall(X+Y,(X=1),S), S=[1+_]")]
        [InlineData(@"T: findall(X,fail,L), L=[]")]
        [InlineData(@"T: findall(X,(X=1 ; X=1),S), S=[1,1]")]
        [InlineData(@"F: findall(X,(X=2 ; X=1),[1,2])")]
        [InlineData(@"T: findall(X,(X=1 ; X=2),[X,Y]), X=1, Y=2")]
        [InlineData(@"R: findall(X,Goal,S)")]
        [InlineData(@"R: findall(X,4,S)")]
        [InlineData(@"R: findall(X,call(1),S)")]
        public void Findall(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: float(3.3)")]
        [InlineData(@"T: float(-3.3)")]
        [InlineData(@"F: float(3)")]
        [InlineData(@"F: float(atom)")]
        [InlineData(@"F: float(X)")]
        public void Float(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: functor(foo(a,b,c),foo,3)")]
        [InlineData(@"T: functor(foo(a,b,c),X,Y), X=foo, Y=3")]
        [InlineData(@"T: functor(X,foo,3), X=foo(A, B, C)")]
        [InlineData(@"T: functor(X,foo,0), X=foo")]
        [InlineData(@"T: functor(mats(A,B),A,B), A=mats, B=2")]
        [InlineData(@"F: functor(foo(a),foo,2)")]
        [InlineData(@"F: functor(foo(a),fo,1)")]
        [InlineData(@"T: functor(1,X,Y), X=1, Y=0")]
        [InlineData(@"T: functor(X,1.1,0), X=1.1")]
        [InlineData(@"T: functor([_|_],'.',2)")]
        [InlineData(@"T: functor([],[],0)")]
        [InlineData(@"R: functor(X, Y, 3)")]
        [InlineData(@"R: functor(X, foo, N)")]
        [InlineData(@"R: functor(X, foo, a)")]
        [InlineData(@"R: functor(F, 1.5, 1)")]
        [InlineData(@"R: functor(F,foo(a),1)")]
        [InlineData(@"R: current_prolog_flag(max_arity,A), X is A + 1, functor(T, foo, X)")]
        [InlineData(@"R: functor(T, foo, -1)")]
        public void Functor(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: '->'(true, true)")]
        [InlineData(@"F: '->'(true, fail)")]
        [InlineData(@"F: '->'(fail, true)")]
        [InlineData(@"T: '->'(true, X=1), X=1")]
        [InlineData(@"T: '->'((X=1; X=2), true), X=1")]
        [InlineData(@"T: '->'(true, (X=1; X=2)), (X=1;X=2)")]
        public void IfThen(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: '->'(true, true); fail")]
        [InlineData(@"T: '->'(fail, true); true")]
        [InlineData(@"F: '->'(true, fail); fail")]
        [InlineData(@"F: '->'(fail, true); fail")]
        [InlineData(@"T: ('->'(true, X=1); X=2), X=1")]
        [InlineData(@"T: ('->'(fail, X=1); X=2), X=2")]
        [InlineData(@"T: ('->'(true, (X=1; X=2)); true), (X=1 ; X=2)")]
        [InlineData(@"T: ('->'((X=1; X=2), true); true), X=1")]
        public void IfThenElse(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: integer(3)")]
        [InlineData(@"T: integer(-3)")]
        [InlineData(@"F: integer(3.3)")]
        [InlineData(@"F: integer(X)")]
        [InlineData(@"F: integer(atom)")]
        public void Integer(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: 'is'(Result,3 + 11.0), Result=14.0")]
        [InlineData(@"T: X = 1 + 2, 'is'(Y, X * 3), X = 1+2, Y=9")]
        [InlineData(@"F: 'is'(foo,77)")]
        [InlineData(@"R: 'is'(77, N)")]
        [InlineData(@"R: 'is'(77, foo)")]
        [InlineData(@"T: 'is'(X,float(3)), X=3.0")]
        public void Is(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: nonvar(33.3)")]
        [InlineData(@"T: nonvar(foo)")]
        [InlineData(@"F: nonvar(Foo)")]
        [InlineData(@"T: foo=Foo,nonvar(Foo), Foo=foo")]
        [InlineData(@"F: nonvar(_)")]
        [InlineData(@"T: nonvar(a(b))")]
        public void NoVar(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: \+(true)")]
        [InlineData(@"F: \+(!)")]
        [InlineData(@"T: (X=1;X=2), \+((!,fail)), (X=1;X=2)")]
        [InlineData(@"T: \+(4 = 5)")]
        [InlineData(@"R: \+(3)")]
        [InlineData(@"T: \+((!,fail))")]
        [InlineData(@"R: \+(X)")]
        public void NotProvable(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: '\='(1,1)")]
        [InlineData(@"F: '\='(X,1)")]
        [InlineData(@"F: '\='(X,Y)")]
        [InlineData(@"F: '\='(X,Y),'\='(X,abc)")]
        [InlineData(@"F: '\='(f(X,def),f(def,Y))")]
        [InlineData(@"T: '\='(1,2)")]
        [InlineData(@"F: '\='(1,1.0)")]
        [InlineData(@"T: '\='(g(X),f(f(X)))")]
        [InlineData(@"T: '\='(f(X,1),f(a(X)))")]
        [InlineData(@"T: '\='(f(X,Y,X),f(a(X),a(Y),Y,2))")]
        public void NotUnify(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: number(3)")]
        [InlineData(@"T: number(3.3)")]
        [InlineData(@"T: number(-3)")]
        [InlineData(@"F: number(a)")]
        [InlineData(@"F: number(X)")]
        public void Number(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: once(!)")]
        [InlineData(@"T: once(!), (X=1; X=2), (X=1;X=2)")]
        [InlineData(@"T: once(repeat)")]
        [InlineData(@"F: once(fail)")]
        [InlineData(@"R: once(3)")]
        [InlineData(@"R: once(X)")]
        public void Once(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: true; fail")]
        [InlineData(@"F: (!, fail); true")]
        [InlineData(@"R: (!; call(3))")]
        [InlineData(@"T: ((X=1, !); X=2), X=1")]
        [InlineData(@"T: (X=1; X=2)")]
        public void Or(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: repeat,!,fail")]
        public void Repeat(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: retract((4 :- X))")]
        [InlineData(@"F: retract((atom(_) :- X == '[]'))")]
        public void Retract(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: setof(X, (X = 1; X=2),L), L=[1, 2]")]
        [InlineData(@"T: setof(X, (X = 1; X=2),X), X=[1, 2]")]
        [InlineData(@"T: setof(X, (X = 2; X=1),L), L=[1, 2]")]
        [InlineData(@"T: setof(X, (X = 2; X=2),L), L=[2]")]
        [InlineData(@"F: setof(X, fail, L)")]
        [InlineData(@"T: setof(1, (Y = 2; Y=1),L), ((L=[1],Y=1);(L=[1],Y=2))")]
        [InlineData(@"T: setof(f(X, Y), (X = a;Y = b), L), L=[f(_,b), f(a,_)]")]
        [InlineData(@"T: setof(X, Y ^ ((X = 1, Y = 1);(X=2,Y=2)),S), S=[1, 2]")]
        [InlineData(@"T: setof(X, Y ^ ((X = 1; Y=1);(X=2,Y=2)),S), S=[_, 1, 2]")]
        [InlineData(@"T: set_prolog_flag(unknown, warning), setof(X, (Y ^ (X = 1; Y = 1); X = 3),S), S=[3]")]
        [InlineData(@"T: set_prolog_flag(unknown, warning), setof(X, Y ^ (X = 1; Y = 1; X = 3),S), S=[_,1,3]")]
        [InlineData(@"T: setof(X, (X = Y; X=Z;Y=1),L), (L=[Y,Z]; (L=[_],Y=1))")]
        [InlineData(@"R: setof(X, X ^ (true; 4), L)")]
        [InlineData(@"R: setof(X, 1, L)")]
        public void Setof(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: '\=='(1,1)")]
        [InlineData(@"F: '\=='(X, X)")]
        [InlineData(@"T: '\=='(1,2)")]
        [InlineData(@"T: '\=='(X,1)")]
        [InlineData(@"T: '\=='(X, Y)")]
        [InlineData(@"T: '\=='(_, _)")]
        [InlineData(@"T: '\=='(X, a(X))")]
        [InlineData(@"F: '\=='(f(a), f(a))")]
        public void TermDiff(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: '=='(1,1)")]
        [InlineData(@"T: '=='(X, X)")]
        [InlineData(@"F: '=='(1,2)")]
        [InlineData(@"F: '=='(X,1)")]
        [InlineData(@"F: '=='(X, Y)")]
        [InlineData(@"F: '=='(_, _)")]
        [InlineData(@"F: '=='(X, a(X))")]
        [InlineData(@"T: '=='(f(a), f(a))")]
        public void TermEq(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: '@>'(1.0,1)")]
        [InlineData(@"F: '@>'(aardvark, zebra)")]
        [InlineData(@"F: '@>'(short, short)")]
        [InlineData(@"F: '@>'(short, shorter)")]
        [InlineData(@"T: '@>'(foo(b), foo(a))")]
        [InlineData(@"F: '@>'(X, X)")]
        [InlineData(@"F: '@>'(foo(a, X), foo(b, Y))")]
        [InlineData(@"T: '@>='(1.0,1)")]
        [InlineData(@"F: '@>='(aardvark, zebra)")]
        [InlineData(@"T: '@>='(short, short)")]
        [InlineData(@"F: '@>='(short, shorter)")]
        [InlineData(@"T: '@>='(foo(b), foo(a))")]
        [InlineData(@"T: '@>='(X, X)")]
        [InlineData(@"F: '@>='(foo(a, X), foo(b, Y))")]
        public void TermGt(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"F: '@<'(1.0,1)")]
        [InlineData(@"T: '@<'(aardvark, zebra)")]
        [InlineData(@"F: '@<'(short, short)")]
        [InlineData(@"T: '@<'(short, shorter)")]
        [InlineData(@"F: '@<'(foo(b), foo(a))")]
        [InlineData(@"F: '@<'(X, X)")]
        [InlineData(@"T: '@<'(foo(a, X), foo(b, Y))")]
        [InlineData(@"T: '@=<'(1.0,1)")]
        [InlineData(@"T: '@=<'(aardvark, zebra)")]
        [InlineData(@"T: '@=<'(short, short)")]
        [InlineData(@"T: '@=<'(short, shorter)")]
        [InlineData(@"F: '@=<'(foo(b), foo(a))")]
        [InlineData(@"T: '@=<'(X, X)")]
        [InlineData(@"T: '@=<'(foo(a, X), foo(b, Y))")]
        public void TermLt(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: true")]
        public void True(string test)
        {
            test.Evaluate();
        }

        [Theory]
        [InlineData(@"T: '='(1,1)")]
        [InlineData(@"T: 1=1")]
        [InlineData(@"T: '='(X,1)")]
        [InlineData(@"T: '='(X,Y), X=a, Y='a'")]
        [InlineData(@"T: '='(X, Y), '='(X, abc), (X=abc; Y=abc)")]
        [InlineData(@"T: '='(f(X, def), f(def, Y)), (X=def; Y=def)")]
        [InlineData(@"F: '='(1,2)")]
        [InlineData(@"T: '='(1,1.0)")]
        [InlineData(@"F: '='(g(X), f(f(X)))")]
        [InlineData(@"F: '='(f(X, 1), f(a(X)))")]
        [InlineData(@"F: '='(f(X, Y, X), f(a(X), a(Y), Y, 2))")]
        [InlineData(
            @"T: '='(f(A, B, C), f(g(B, B), g(C, C), g(D, D))), A=g(g(g(D, D), g(D, D)), g(g(D, D), g(D, D))), B=g(g(D, D), g(D, D)), C=g(D, D)")]
        public void Unify(string test)
        {
            test.Evaluate();
        }
    }
}