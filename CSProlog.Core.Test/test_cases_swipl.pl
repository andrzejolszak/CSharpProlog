% Test cases from the ISO standard
% !! a lot of different tests here (some of them skipped by swi)

:- discontiguous((stc/4, etc/4, atc/4, program/2, macro/2)).

% Legend for test Type: !! document this
% 
% iso_doc -- test case in the ISO standard
% eddbali -- test case in A Ed-Dbali's test suite %'
% sics    -- additional test case added at SICS
% pts     -- test case added by Péter Szabó !! more

%%%% pts %%%% Dat: this solution would rerder the tests into stc, etc, atc --
%   we don't want this, we'd like to keep the original order

% test_case(Label, Type,    Goal,                           Result,                         Prerequisites and side
%                                                                               effects).

% test_case(foo:5, pts, ((X=5 ; X=6; X=7 ; X=8)), {[X <-- 9], [X <-- 5], ...}, []).

% 6.3.3.1
test_case(term_sx: 1, iso_doc,  read(_),                        success,                        [txtin('f(x,y).')]).
test_case(term_sx: 2, iso_doc,  read(_),                        success,                        [txtin('f(:-, ;, [:-, :-|:-]).')]).
test_case(term_sx: 3, iso_doc,  read(_),                        syntax,                         [txtin('f(,,a).')]).
test_case(term_sx: 4, iso_doc,  read(_),                        syntax,                         [txtin('[a,,|v].')]).
test_case(term_sx: 5, iso_doc,  read(_),                        syntax,                         [txtin('[a,b|,].')]).
test_case(term_sx: 6, iso_doc,  read(_),                        success,                        [txtin("f(',',a).")]).
test_case(term_sx: 7, iso_doc,  read(_),                        success,                        [txtin("[a,','|v].")]).
test_case(term_sx: 8, iso_doc,  read(_),                        success,                        [txtin("[a,b|','].")]).

% 6.3.4
test_case(ops: 1, iso_doc,      read(_),	    		syntax,				[prog(fxops), txtin('fx fx 1.')]).
test_case(ops: 2, iso_doc,      read(_),	    		success,			[prog(fxops), txtin('fx (fx 1).')]).
test_case(ops: 3, iso_doc,      read(_),	    		syntax,				[prog(fxops), txtin('1 xf xf.')]).
test_case(ops: 4, iso_doc,      read(_),	    		success,			[prog(fxops), txtin('(1 xf) xf.')]).
test_case(ops: 5, iso_doc,      read(_),	    		syntax,				[prog(fxops),
	  			    						 txtin('1 xfx 2 xfx 3.')]).
test_case(ops: 6, iso_doc,      read(_),	    		success,			[prog(fxops),
	  			    						 txtin('(1 xfx 2) xfx 3.')]).
test_case(ops: 7, iso_doc,      read(_),	    		success,			[prog(fxops),
	  			    						 txtin('1 xfx (2 xfx 3).')]).
test_case(ops: 8, iso_doc,      (read(TU), read(TB)),		[[TU == TB]], 			[prog(fyops),
	  	                     			     			 txtin('fy fy 1. fy (fy 1).')]).
test_case(ops: 9, iso_doc,      (read(TU), read(TB)),		[[TU == TB]],			[prog(fyops),
	  	                     			     			 txtin(['1 xfy  2 xfy 3. ',
	  	                     			     			       '1 xfy (2 xfy 3).'])]).
test_case(ops:10, iso_doc,      (read(TU), read(TB)),		[[TU == TB]],			[prog(fyops),
	  	                     			     			 txtin(['1 xfy  2 yfx 3. ',
	  	                     			     			       '1 xfy (2 yfx 3).'])]).
test_case(ops:11, iso_doc,      (read(TU), read(TB)),		[[TU == TB]],			[prog(fyops),
	  	                     			     			 txtin('fy 2 yf. fy (2 yf).')]).
test_case(ops:12, iso_doc,      (read(TU), read(TB)),		[[TU == TB]],  			[prog(fyops),
	  	                     			     			 txtin('1 yf yf. (1 yf) yf.')]).
test_case(ops:13, iso_doc,      (read(TU), read(TB)),		[[TU == TB]],			[prog(fyops),
				    			    			 txtin([' 1 yfx 2  yfx 3. ',
					       '(1 yfx 2) yfx 3.'])]).

/* % pts: start of skipped tests
*/ % pts: end of skipped tests
% 6.3.4.3
test_case(ops:14, iso_doc,      read(T),	    		[[T <-- -(1,2)]],		[txtin('-(1,2).')]).
test_case(ops:15, iso_doc,      read(T),	    		[[T <-- -((1,2))]],		[txtin('- (1,2).')]).

% 6.3.5.1
test_case(list: 1, iso_doc,     read(T),                        [[T <-- .(a, [])]],             [txtin('[a].')]).
test_case(list: 2, iso_doc,     read(T),                        [[T <-- .(a, .(b, []))]],	[txtin('[a,b].')]).
test_case(list: 3, iso_doc,     read(T),                        [[T <-- .(a, b)]],              [txtin('[a|b].')]).

% 6.3.6.1
test_case(curly: 1, iso_doc,    read(T),                        [[T <-- '{}'(a)]],             	[txtin('{a}.')]).
test_case(curly: 2, iso_doc,    read(T),                        [[T <-- '{}'(','(a,b))]],	[txtin('{a,b}.')]).

% 6.3.7.1
test_case(dbl_qts: 1, iso_doc,  (read(Goal), call(Goal)),	success,			[double_quotes_ex1,
					 set_double_quotes(chars)]).
test_case(dbl_qts: 2, iso_doc,  (read(Goal), call(Goal)),	success,			[double_quotes_ex1,
					 set_double_quotes(codes)]).
test_case(dbl_qts: 3, iso_doc,  (read(Goal), call(Goal)),	success,			[double_quotes_ex1,
					 set_double_quotes(atom)]).
test_case(dbl_qts: 4, iso_doc,  (read(Goal), call(Goal)),	success,			[double_quotes_ex2,
					 set_double_quotes(chars)]).
test_case(dbl_qts: 5, iso_doc,  (read(Goal), call(Goal)),	success,			[double_quotes_ex2,
					 set_double_quotes(codes)]).
test_case(dbl_qts: 6, iso_doc,  (read(Goal), call(Goal)),	success,			[double_quotes_ex2,
					 set_double_quotes(atom)]).
test_case(dbl_qts: 7, sics,  read(T),			[[atom_chars('jim', T)]],	[txtin('"jim".'),
					 set_double_quotes(chars)]).
test_case(dbl_qts: 8, sics,  read(T),			[[atom_codes('jim', T)]],	[txtin('"jim".'),
					 set_double_quotes(codes)]).
test_case(dbl_qts: 9, sics,  read(T),			[['jim'== T]],			[txtin('"jim".'),
					 set_double_quotes(atom)]).
test_case(dbl_qts:10, sics,  read(T),			[[ T <-- [] ]],			[txtin('"".'),
					 set_double_quotes(chars)]).
test_case(dbl_qts:11, sics,  read(T),			[[ T <-- [] ]],			[txtin('"".'),
					 set_double_quotes(codes)]).
test_case(dbl_qts:12, sics,  read(T),			[[ T <-- '' ]],			[txtin('"".'),
					 set_double_quotes(atom)]).


% 7.8.1.4
test_case(true: 1, iso_doc,     true,                           success,                        []).

% 7.8.2.4
test_case(fail: 1, iso_doc,     fail,                           failure,                        []).

% 7.8.3.4
test_case(call: 1, iso_doc,     call(!),                        success,                        []).
test_case(call: 2, iso_doc,     call(fail),                     failure,                        []).
test_case(call: 3, iso_doc,     call((fail, _X)),               failure,                        []).
test_case(call: 4, iso_doc,     call((fail, call(1))),          failure,                        []).
test_case(call: 5, iso_doc,     call(b(_)),                     inst,                           [prog(call),txtout(rx('_.*'))]).
test_case(call: 6, iso_doc,     call(b(3)),                     type(callable,(write(3),3)),    [prog(call),
					 variant(literally_iso,[
					   no -txtout(''),
					   yes-txtout('3')])
					]).
                                                                                % ERROR in the std:
                                                                                % prescribes: txtout('3').
                                                                                % Cf. call:14
test_case(call: 7, iso_doc,     (Z=!, call((Z=!, a(X), Z))),    [[Z <-- !, X <-- 1]],           [prog(call)]).
test_case(call: 8, iso_doc,     (call((Z=!, a(X), Z))),         [[Z <-- !, X <-- 1],
                                                 [Z <-- !, X <-- 2]],           [prog(call)]).
test_case(call: 9, iso_doc,     call((write(3), _X)),           inst,                           [txtout('3')]).
test_case(call:10, iso_doc,     call((write(3), call(1))),      type(callable,1),               [txtout('3')]).
test_case(call:11, iso_doc,     call(_X),                       inst,                           []).
test_case(call:12, iso_doc,     call(1),                        type(callable,1),               []).
test_case(call:13, iso_doc,     call((fail, 1)),                type(callable,(fail,1)),        []).
test_case(call:14, iso_doc,     call((write(3), 1)),            type(callable,(write(3),1)),    [txtout('')]).
test_case(call:15, iso_doc,     call((1; true)),                type(callable,(1;true)),        []).

% 7.8.4.4
test_case(cut: 1, iso_doc,      !,                              success,                        []).
test_case(cut: 2, iso_doc,      (!,fail;true),                  failure,                        []).
test_case(cut: 3, iso_doc,      (call(!),fail;true),            success,                        []).
test_case(cut: 4, iso_doc,      (twice(_), !,
                 write('Forwards '), fail),     failure,                        [prog(cut),txtout('C Forwards ')]).
test_case(cut: 5, iso_doc,      ((!; write('No ')),
                 write('Cut disjunction '),
                 fail),                         failure,                        [txtout('Cut disjunction ')]).
test_case(cut: 6, iso_doc,      (twice(_), (write('No '); !),
                 write('Cut '), fail),          failure,                        [prog(cut),
                                                                                 txtout('C No Cut Cut ')]).
test_case(cut: 7, iso_doc,      (twice(_),
                 (!, fail ;write('No '))),      failure,                        [prog(cut),txtout('C ')]).
test_case(cut: 8, iso_doc,      (twice(X), call(X),
                 write('Forwards '), fail),     failure,                        [prog(cut),txtout(
                                                                                  'C Forwards Moss Forwards ')]).
test_case(cut: 9, iso_doc,      (goal(X), call(X),
                 write('Forwards '), fail),     failure,                        [prog(cut),txtout(
                                                                                  'C Forwards Three Forwards ')]).
test_case(cut:10, iso_doc,      (twice(_), \+(\+(!)),
                 write('Forwards '), fail),     failure,                        [prog(cut),txtout(
                                                                                  'C Forwards Moss Forwards ')]).
test_case(cut:11, iso_doc,      (twice(_), once(!),
                 write('Forwards '), fail),     failure,                        [prog(cut),txtout(
                                                                                  'C Forwards Moss Forwards ')]).
test_case(cut:12, iso_doc,      (twice(_), call(!),
                 write('Forwards '), fail),     failure,                        [prog(cut),txtout(
                                                                                  'C Forwards Moss Forwards ')]).

% 7.8.5.4
test_case(and:1, iso_doc,       ','(X=1, var(X)),               failure,                        []).
test_case(and:2, iso_doc,       ','(var(X), X=1),               [[X <-- 1]],                    []).
test_case(and:3, iso_doc,       ','(X = true, call(X)),         [[X <-- true]],                 []).

% 7.8.6.4
test_case(or:1, iso_doc,        ';'(true, fail),                success,                        []).
test_case(or:2, iso_doc,        ';'((!, fail), true),           failure,                        []).
test_case(or:3, iso_doc,        ';'(!, call(3)),                success,                        []).
test_case(or:4, iso_doc,        ';'((X=1, !), X=2),             [[X <-- 1]],                    []).
test_case(or:5, iso_doc,        ';'(X=1, X=2),                  [[X <-- 1],
                                                 [X <-- 2]],                    []).

% 7.8.7.4
test_case(if_then:1, iso_doc,   '->'(true, true),               success,                        []).
test_case(if_then:2, iso_doc,   '->'(true, fail),               failure,                        []).
test_case(if_then:3, iso_doc,   '->'(fail, true),               failure,                        []).
test_case(if_then:4, iso_doc,   '->'(true, X=1),                [[X <-- 1]],                    []).
test_case(if_then:5, iso_doc,   '->'(';'(X=1, X=2), true),       [[X <-- 1]],                   []).
test_case(if_then:6, iso_doc,   '->'(true, ';'(X=1, X=2)),      [[X <-- 1],
                                                 [X <-- 2]],                    []).

% 7.8.8.4
test_case(ifthels:1, iso_doc,   ';'('->'(true, true), fail),    success,                        []).
test_case(ifthels:2, iso_doc,   ';'('->'(fail, true), true),    success,                        []).
test_case(ifthels:3, iso_doc,   ';'('->'(true, fail), fail),    failure,                        []).
test_case(ifthels:4, iso_doc,   ';'('->'(fail, true), fail),    failure,                        []).
test_case(ifthels:5, iso_doc,   ';'('->'(true, X=1), X=2),      [[X <-- 1]],                    []).
test_case(ifthels:6, iso_doc,   ';'('->'(fail, X=1), X=2),      [[X <-- 2]],                    []).
test_case(ifthels:7, iso_doc,   ';'('->'(true, ';'(X=1, X=2)),
                    true),                      [[X <-- 1],
                                                 [X <-- 2]],                    []).
test_case(ifthels:8, iso_doc,   ';'('->'(';'(X=1, X=2), true),
                    true),                      [[X <-- 1]],                    []).
test_case(ifthels:9, iso_doc,   ';'('->'(','(!, fail), true),
                    true),                      success,                        []).

% 7.8.9.4
test_case(catch:1, iso_doc,     catch(foo(5),test(Y), true),    [[Y <-- 10]],                   [prog(catch)]).
test_case(catch:2, iso_doc,     catch(bar(3),Z,true),           [[Z <-- 3]],                    [prog(catch)]).
test_case(catch:3, iso_doc,     catch(true,_,3),                success,                        []).
test_case(catch:4, iso_doc,     (catch(true,_C,write(demoen)),
                 throw(bla)),                   variant(literally_iso,[
						  no- user_error(bla),
						  yes-system]),	                [txtout('')]).
                                                  % system error ?!
test_case(catch:5, iso_doc,     catch(car(_X),Y,true),          [[Y <-- 1]],                    [prog(catch)]).
test_case(catch:6, iso_doc,     catch(
                 number_chars(_X,['1',a,'0']),
                 error(syntax_error(_),_),
                 fail),                         failure,                        []).
test_case(catch:7, iso_doc,     catch(g, C, write(h1)),         [[C <-- c]],                    [prog(catch),txtout('h1')]).
test_case(catch:8, iso_doc,     catch(coo(_X), Y, true),        [[Y <-- error(
                                                   instantiation_error,@@)]],   [prog(catch)]).

% 8.2.1.4
test_case(unify: 1, iso_doc,    '='(1,1),                       success,                        []).
test_case(unify: 2, iso_doc,    '='(X,1),                       [[X <-- 1]],                    []).
test_case(unify: 3, iso_doc,    '='(X,Y),                       [[X <-- Y]],                    []).
test_case(unify: 4, iso_doc,    '='(_,_),                       success,                        []).
test_case(unify: 5, iso_doc,    ('='(X,Y),'='(X,abc)),          [[X <-- abc,
                                                  Y <-- abc]],                  []).
test_case(unify: 6, iso_doc,    '='(f(X,def),f(def,Y)),         [[X <-- def,
                                                  Y <-- def]],                  []).
test_case(unify: 7, iso_doc,    '='(1,2),                       failure,                        []).
test_case(unify: 8, iso_doc,    '='(1,1.0),                     failure,                        []).
test_case(unify: 9, iso_doc,    '='(g(X),f(f(X))),              failure,                        []).
test_case(unify:10, iso_doc,    '='(f(X,1),f(a(X))),            failure,                        []).
test_case(unify:11, iso_doc,    '='(f(X,Y,X),f(a(X),a(Y),Y,2)), failure,                        []).
test_case(unify:12, iso_doc,    '='(X,a(X)),                    sto([[X <-- Y@a(Y)]]),          []).
test_case(unify:13, iso_doc,    '='(f(X,1),f(a(X),2)),          sto(failure),                   []).
test_case(unify:14, iso_doc,    '='(f(1,X,1),f(2,a(X),2)),      sto(failure),                   []).
test_case(unify:15, iso_doc,    '='(f(1,X),f(2,a(X))),          sto(failure),                   []).
test_case(unify:16, iso_doc,    '='(f(X,Y,X,1),
                    f(a(X),a(Y),Y,2)),          sto(failure),                   []).

% 8.2.2.4
test_case(uni_woc: 1, iso_doc,  unify_with_occurs_check(1, 1),  success,                        []).
test_case(uni_woc: 2, iso_doc,  unify_with_occurs_check(X, 1),  [[X <-- 1]],                    []).
test_case(uni_woc: 3, iso_doc,  unify_with_occurs_check(X, Y),  [[X <-- Y]],                    []).
test_case(uni_woc: 4, iso_doc,  unify_with_occurs_check(_,_),   success,                        []).
test_case(uni_woc: 5, iso_doc,  (unify_with_occurs_check(X,Y),
                 unify_with_occurs_check(
                   X, abc)),                    [[X <-- abc,
                                                  Y <-- abc]],                  []).
test_case(uni_woc: 6, iso_doc,  unify_with_occurs_check(
                  f(X,def),f(def,Y)),           [[X <-- def,
                                                  Y <-- def]],                  []).
test_case(uni_woc: 7, iso_doc,  unify_with_occurs_check(1,2),   failure,                        []).
test_case(uni_woc: 8, iso_doc,  unify_with_occurs_check(1,1.0), failure,                        []).
test_case(uni_woc: 9, iso_doc,  unify_with_occurs_check(
                  g(X),f(f(X))),                failure,                        []).
test_case(uni_woc:10, iso_doc,  unify_with_occurs_check(
                  f(X,1),f(a(X))),              failure,                        []).
test_case(uni_woc:11, iso_doc,  unify_with_occurs_check(
                  f(X,Y,X),f(a(X),a(Y),Y,2)),   failure,                        []).
test_case(uni_woc:12, iso_doc,  unify_with_occurs_check(
                  X,a(X)),                      failure,                        []).
test_case(uni_woc:13, iso_doc,  unify_with_occurs_check(
                  f(X,1),f(a(X),2)),            failure,                        []).
test_case(uni_woc:14, iso_doc,  unify_with_occurs_check(
                  f(1,X,1),f(2,a(X),2)),        failure,                        []).
test_case(uni_woc:15, iso_doc,  unify_with_occurs_check(
                  f(1,X),f(2,a(X))),            failure,                        []).
test_case(uni_woc:16, iso_doc,  unify_with_occurs_check(
                  f(X,Y,X,1),f(a(X),a(Y),Y,2)), failure,                        []).

% 8.2.3.4
test_case(not_uni: 1, iso_doc,  '\\='(1,1),                     failure,                        []).
test_case(not_uni: 2, iso_doc,  \=(_X,1),                       failure,                        []).
test_case(not_uni: 3, iso_doc,  '\\='(_X,_Y),                   failure,                        []).
test_case(not_uni: 4, iso_doc,  \=(_,_),                        failure,                        []).
test_case(not_uni: 5, iso_doc,  \=(f(_X,def),f(def,_Y)),        failure,                        []).
test_case(not_uni: 6, iso_doc,  '\\='(1,2),                     success,                        []).
test_case(not_uni: 7, iso_doc,  \=(1,1.0),                      success,                        []).
test_case(not_uni: 8, iso_doc,  '\\='(g(X),f(f(X))),            success,                        []).
test_case(not_uni: 9, iso_doc,  \=(f(X,1),f(a(X))),             success,                        []).
test_case(not_uni:10, iso_doc,  '\\='(f(X,Y,X),
                      f(a(X),a(Y),Y,2)),        success,                        []).
test_case(not_uni:11, iso_doc,  \=(X,a(X)),                     sto(failure),                   []).
test_case(not_uni:12, iso_doc,  '\\='(f(X,1),f(a(X),2)),        sto(success),                   []).
test_case(not_uni:13, iso_doc,  '\\='(f(1,X,1),f(2,a(X),2)),    sto(success),                   []).
test_case(not_uni:14, iso_doc,  \=(f(1,X),f(2,a(X))),           sto(success),                   []).
test_case(not_uni:15, iso_doc,  '\\='(f(X,Y,X,1),
                      f(a(X),a(Y),Y,2)),        sto(success),                   []).

% 8.3.1.4
test_case(var:1, iso_doc,       var(foo),                       failure,                        []).
test_case(var:2, iso_doc,       var(_Foo),                      success,                        []).
test_case(var:3, iso_doc,       (foo = Foo, var(Foo)),          failure,                        []).
test_case(var:4, iso_doc,       var(_),                         success,                        []).

% 8.3.2.4
test_case(atom:1, iso_doc,      atom(atom),                     success,                        []).
test_case(atom:2, iso_doc,      atom('string'),                 success,                        []).
test_case(atom:3, iso_doc,      atom(a(b)),                     failure,                        []).
test_case(atom:4, iso_doc,      atom(_Var),                     failure,                        []).
test_case(atom:5, iso_doc,      atom([]),                       success,                        []).
test_case(atom:6, iso_doc,      atom(6),                        failure,                        []).
test_case(atom:7, iso_doc,      atom(3.3),                      failure,                        []).

% 8.3.3.4
test_case(integer:1, iso_doc,   integer(3),                     success,                        []).
test_case(integer:2, iso_doc,   integer(-3),                    success,                        []).
test_case(integer:3, iso_doc,   integer(3.3),                   failure,                        []).
test_case(integer:4, iso_doc,   integer(_X),                    failure,                        []).
test_case(integer:5, iso_doc,   integer(atom),                  failure,                        []).

% 8.3.4.4
test_case(float:1, iso_doc,     float(3.3),                     success,                        []).
test_case(float:2, iso_doc,     float(-3.3),                    success,                        []).
test_case(float:3, iso_doc,     float(3),                       failure,                        []).
test_case(float:4, iso_doc,     float(atom),                    failure,                        []).
test_case(float:5, iso_doc,     float(_X),                      failure,                        []).

% 8.3.5.4
test_case(atomic:1, iso_doc,    atomic(atom),                   success,                        []).
test_case(atomic:2, iso_doc,    atomic(a(b)),                   failure,                        []).
test_case(atomic:3, iso_doc,    atomic(_Var),                   failure,                        []).
test_case(atomic:4, iso_doc,    atomic(6),                      success,                        []).
test_case(atomic:5, iso_doc,    atomic(3.3),                    success,                        []).

% 8.3.6.4
test_case(compound:1, iso_doc,  compound(33.3),                 failure,                        []).
test_case(compound:2, iso_doc,  compound(-33.3),                failure,                        []).
test_case(compound:3, iso_doc,  compound(-a),                   success,                        []).
test_case(compound:4, iso_doc,  compound(_),                    failure,                        []).
test_case(compound:5, iso_doc,  compound(a),                    failure,                        []).
test_case(compound:6, iso_doc,  compound(a(b)),                 success,                        []).
test_case(compound:7, iso_doc,  compound([]),                   failure,                        []).
test_case(compound:8, iso_doc,  compound([a]),                  success,                        []).

% 8.3.7.4
test_case(nonvar:1, iso_doc,    nonvar(33.3),                   success,                        []).
test_case(nonvar:2, iso_doc,    nonvar(foo),                    success,                        []).
test_case(nonvar:3, iso_doc,    nonvar(_Foo),                   failure,                        []).
test_case(nonvar:4, iso_doc,    (foo = Foo, nonvar(Foo)),       success,                        []).
test_case(nonvar:5, iso_doc,    nonvar(_),                      failure,                        []).
test_case(nonvar:6, iso_doc,    nonvar(a(b)),                   success,                        []).

% 8.3.8.4
test_case(number:1, iso_doc,    number(3),                      success,                        []).
test_case(number:2, iso_doc,    number(3.3),                    success,                        []).
test_case(number:3, iso_doc,    number(-3),                     success,                        []).
test_case(number:4, iso_doc,    number(a),                      failure,                        []).
test_case(number:5, iso_doc,    number(_X),                     failure,                        []).

% 8.4.1.4
test_case(trm_cmp: 1, iso_doc,  '@=<'(1.0, 1),                  success,                        []).
test_case(trm_cmp: 2, iso_doc,  '@<'(1.0, 1),                   success,                        []).
test_case(trm_cmp: 3, iso_doc,  '\\=='(1, 1),                   failure,                        []).
test_case(trm_cmp: 4, iso_doc,  '@=<'(aardvark, zebra),         success,                        []).
test_case(trm_cmp: 5, iso_doc,  '@=<'(short, short),            success,                        []).
test_case(trm_cmp: 6, iso_doc,  '@=<'(short, shorter),          success,                        []).
test_case(trm_cmp: 7, iso_doc,  '@>='(short, shorter),          failure,                        []).
test_case(trm_cmp: 8, iso_doc,  '@<'(foo(a,b), north(a)),       failure,                        []).
test_case(trm_cmp: 9, iso_doc,  '@>'(foo(b), foo(a)),           success,                        []).
test_case(trm_cmp:10, iso_doc,  '@<'(foo(a, _X), foo(b, _Y)),   success,                        []).
test_case(trm_cmp:11, iso_doc,  '@<'(foo(_X, a), foo(_Y, b)),   succfail,                       []).
test_case(trm_cmp:12, iso_doc,  '@=<'(X, X),                    success,                        []).
test_case(trm_cmp:13, iso_doc,  '=='(X, X),                     success,                        []).
test_case(trm_cmp:14, iso_doc,  '@=<'(_X, _Y),                  succfail,                       []).
test_case(trm_cmp:15, iso_doc,  '=='(_X, _Y),                   failure,                        []).
test_case(trm_cmp:16, iso_doc,  \==(_, _),                      success,                        []).
test_case(trm_cmp:17, iso_doc,  '=='(_, _),                     failure,                        []).
test_case(trm_cmp:18, iso_doc,  '@=<'(_, _),                    succfail,                       []).
test_case(trm_cmp:19, iso_doc,  '@=<'(foo(_X, a), foo(_Y, b)),  succfail,                       []).

% 8.5.1.4
test_case(functor: 1, iso_doc,  functor(foo(a,b,c), foo, 3),    success,                        []).
test_case(functor: 2, iso_doc,  functor(foo(a,b,c),X,Y),        [[X <-- foo, Y <-- 3]],         []).
test_case(functor: 3, iso_doc,  functor(X,foo,3),               [[X <-- foo(_A,_B,_C)]],        []).
test_case(functor: 4, iso_doc,  functor(X,foo,0),               [[X <-- foo]],                  []).
test_case(functor: 5, iso_doc,  functor(mats(A,B),A,B),         [[A <-- mats,B <-- 2]],         []).
test_case(functor: 6, iso_doc,  functor(foo(a),foo,2),          failure,                        []).
test_case(functor: 7, iso_doc,  functor(foo(a),fo,1),           failure,                        []).
test_case(functor: 8, iso_doc,  functor(1,X,Y),                 [[X <-- 1,Y <-- 0]],            []).
test_case(functor: 9, iso_doc,  functor(X,1.1,0),               [[X <-- 1.1]],                  []).
test_case(functor:10, iso_doc,  functor([_|_],'.',2),           success,                        []).
test_case(functor:11, iso_doc,  functor([],[],0),               success,                        []).
test_case(functor:12, iso_doc,  functor(_X, _Y, 3),             inst,                           []).
test_case(functor:13, iso_doc,  functor(_X, foo, _N),           inst,                           []).
test_case(functor:14, iso_doc,  functor(_X, foo, a),            type(integer,a),                []).
test_case(functor:15, iso_doc,  functor(_F, 1.5, 1),            type(atom,1.5),                 []).
test_case(functor:16, iso_doc,  functor(_F,foo(a),1),           type(atomic,foo(a)),            []).
test_case(functor:17, iso_doc,  (current_prolog_flag(
                   max_arity, A), X is A+1,
                 functor(_T, foo, X)),          repr(max_arity),                []).
test_case(functor:18, iso_doc,  functor(_T, foo, -1),           dom(not_less_than_zero,-1),     []).

% 8.5.2.4
test_case(arg: 1, iso_doc,      arg(1, foo(a,b), a),            success,                        []).
test_case(arg: 2, iso_doc,      arg(1,foo(a,b),X),              [[X <-- a]],                    []).
test_case(arg: 3, iso_doc,      arg(1,foo(X,b),a),              [[X <-- a]],                    []).
test_case(arg: 4, iso_doc,      arg(1,foo(X,b),Y),              [[Y <-- X]],                    []).
test_case(arg: 5, iso_doc,      arg(1,foo(a,b),b),              failure,                        []).
test_case(arg: 6, iso_doc,      arg(0,foo(a,b),foo),            failure,                        []).
test_case(arg: 7, iso_doc,      arg(3,foo(3,4),_N),             failure,                        []).
test_case(arg: 8, iso_doc,      arg(_X,foo(a,b),a),             inst,                           []).
test_case(arg: 9, iso_doc,      arg(1,_X,a),                    inst,                           []).
test_case(arg:10, iso_doc,      arg(0,atom,_A),                 type(compound, atom),           []).
test_case(arg:11, iso_doc,      arg(0,3,_A),                    type(compound, 3),              []).
test_case(arg:12, iso_doc,      arg(1,foo(X),u(X)),             sto([[X<--Y@u(Y)]]),            []).

test_case(arg:13, eddbali,      arg(-3,foo(a,b),_A),            dom(not_less_than_zero,-3),     []).
test_case(arg:14, eddbali,      arg(a,foo(a,b),_X),             type(integer, a),               []).
test_case(arg:15, eddbali,      arg(2,foo(a,f(X,b),c), f(a,Y)), [[X <-- a, Y <-- b]],           []).
test_case(arg:16, sics,      arg(1,3,_A),                    type(compound, 3),              []).

% 8.5.3.4
test_case(univ: 1, iso_doc,     '=..'(foo(a,b), [foo,a,b]),     success,                        []).
test_case(univ: 2, iso_doc,     '=..'(X, [foo,a,b]),            [[X <-- foo(a,b)]],             []).
test_case(univ: 3, iso_doc,     '=..'(foo(a,b), L),             [[L <-- [foo,a,b]]],            []).
test_case(univ: 4, iso_doc,     '=..'(foo(X,b), [foo,a,Y]),     [[X <-- a,Y <-- b]],            []).
test_case(univ: 5, iso_doc,     '=..'(1, [1]),                  success,                        []).
test_case(univ: 6, iso_doc,     '=..'(foo(a,b), [foo,b,a]),     failure,                        []).
test_case(univ: 7, iso_doc,     '=..'(_X, _Y),                  inst,                           []).
test_case(univ: 8, iso_doc,     '=..'(_X, [foo,a|_Y]),          inst,                           []).
test_case(univ: 9, iso_doc,     '=..'(_X, [foo|bar]),           type(list,[foo|bar]),           []).
test_case(univ:10, iso_doc,     '=..'(_X, [_Foo,bar]),          inst,                           []).
test_case(univ:11, iso_doc,     '=..'(_X, [3,1]),               type(atom,3),                   []).
test_case(univ:12, iso_doc,     '=..'(_X, [1.1,foo]),           type(atom,1.1),                 []).
test_case(univ:13, iso_doc,     '=..'(_X, [a(b),1]),            type(atom,a(b)),                []).
test_case(univ:14, iso_doc,     '=..'(_X, 4),                   type(list,4),                   []).
test_case(univ:15, iso_doc,     '=..'(f(X), [f,u(X)]),          sto([[X <-- Y@u(Y)]]),          []).

test_case(univ:16, sics,     '=..'(_X, [f(a)]),              type(atomic, f(a)),             []).
test_case(univ:17, sics,     '=..'(_X, []),                  dom(non_empty_list, []),        []).
test_case(univ:18, sics,     '=..'(_X, [f|L]),               repr(max_arity),                [list_of(N, 1, L)]) :-
	context_info(max_arity, Max), N is Max+1.


% 8.5.4.4
test_case(cp_term:1, iso_doc,   copy_term(_X,_Y),               success,                        []).
test_case(cp_term:2, iso_doc,   copy_term(_X,3),                success,                        []).
test_case(cp_term:3, iso_doc,   copy_term(_,a),                 success,                        []).
test_case(cp_term:4, iso_doc,   copy_term(a+X,X+b),             [[X <-- a]],                    []).
test_case(cp_term:5, iso_doc,   copy_term(_,_),                 success,                        []).
test_case(cp_term:6, iso_doc,   copy_term(X+X+_Y,A+B+B),        [[A <-- B]],                    []).
test_case(cp_term:7, iso_doc,   copy_term(a,b),                 failure,                        []).
test_case(cp_term:8, iso_doc,   (copy_term(a+X,X+b),
                 copy_term(a+X,X+b)),           failure,                        []).
test_case(cp_term:9, iso_doc,   copy_term(demoen(X,X),
                          demoen(Y,f(Y))),      sto([[Y<--Z@f(Z)]]),            []).

% 8.6.1.4
test_case(is: 1, iso_doc,       'is'(Result,3 + 11.0),          [[Result <-- 14.0]],            []).
test_case(is: 2, iso_doc,       (X = 1+2, 'is'(Y, X*3)),        [[X <-- 1+2, Y <-- 9]],         []).
test_case(is: 3, iso_doc,       'is'(3, 3),                     success,                        []).
test_case(is: 4, iso_doc,       'is'(3, 3.0),                   failure,                        []).
test_case(is: 5, iso_doc,       'is'(foo, 77),                  failure,                        []).
test_case(is: 6, iso_doc,       'is'(77, _N),                   inst,                           []).

% 8.7.1.4
test_case(ari_cmp: 1, iso_doc,  '=:='(0,1),                     failure,                        []).
test_case(ari_cmp: 2, iso_doc,  '=\\='(0, 1),                   success,                        []).
test_case(ari_cmp: 3, iso_doc,  '<'(0, 1),                      success,                        []).
test_case(ari_cmp: 4, iso_doc,  '>'(0, 1),                      failure,                        []).
test_case(ari_cmp: 5, iso_doc,  '>='(0, 1),                     failure,                        []).
test_case(ari_cmp: 6, iso_doc,  '=<'(0, 1),                     success,                        []).
test_case(ari_cmp: 7, iso_doc,  '=:='(1.0, 1),                  success,                        []).
test_case(ari_cmp: 8, iso_doc,  =\=(1.0, 1),                    failure,                        []).
test_case(ari_cmp: 9, iso_doc,  '<'(1.0, 1),                    failure,                        []).
test_case(ari_cmp:10, iso_doc,  '>'(1.0, 1),                    failure,                        []).
test_case(ari_cmp:11, iso_doc,  '>='(1.0, 1),                   success,                        []).
test_case(ari_cmp:12, iso_doc,  '=<'(1.0, 1),                   success,                        []).
test_case(ari_cmp:13, iso_doc,  '=:='(3*2, 7-1),                success,                        []).
test_case(ari_cmp:14, iso_doc,  '=\\='(3*2, 7-1),               failure,                        []).
test_case(ari_cmp:15, iso_doc,  '<'(3*2, 7-1),                  failure,                        []).
test_case(ari_cmp:16, iso_doc,  '>'(3*2, 7-1),                  failure,                        []).
test_case(ari_cmp:17, iso_doc,  '>='(3*2, 7-1),                 success,                        []).
test_case(ari_cmp:18, iso_doc,  '=<'(3*2, 7-1),                 success,                        []).
test_case(ari_cmp:19, iso_doc,  '=:='(_X, 5),                   inst,                           []).
test_case(ari_cmp:20, iso_doc,  =\=(_X, 5),                     inst,                           []).
test_case(ari_cmp:21, iso_doc,  '<'(_X, 5),                     inst,                           []).
test_case(ari_cmp:22, iso_doc,  '>'(_X, 5),                     inst,                           []).
test_case(ari_cmp:23, iso_doc,  '>='(_X, 5),                    inst,                           []).
test_case(ari_cmp:24, iso_doc,  '=<'(_X, 5),                    inst,                           []).

% 8.8.1.4
test_case(clause: 1, iso_doc,   clause(cat,true),               success,                        [prog(clause)]).
test_case(clause: 2, iso_doc,   clause(dog,true),               success,                        [prog(clause)]).
test_case(clause: 3, iso_doc,   clause(legs(I,6),Body),         [[Body <-- insect(I)]],         [prog(clause)]).
test_case(clause: 4, iso_doc,   clause(legs(C,7),Body),         [(D= @C)^
                                                 [Body <-- (call(D),call(D))]], [prog(clause)]).
test_case(clause: 5, iso_doc,   clause(insect(I),T),            [[I <-- ant, T <-- true],
                                                 [I <-- bee, T <-- true]],      [prog(clause)]).
test_case(clause: 6, iso_doc,   clause(x,_Body),                failure,                        []).
test_case(clause: 7, iso_doc,   clause(_,_B),                   inst,                           []).
test_case(clause: 8, iso_doc,   clause(4,_B),                   type(callable,4),               []).
test_case(clause: 9, iso_doc,   clause(elk(_N),_Body),          perm(access,private_procedure,
                                                     @elk/1),                   [prog(clause)]).
test_case(clause:10, iso_doc,   clause(atom(_),_Body),          perm(access,private_procedure,
                                                     @atom/1),                  []).
test_case(clause:11, iso_doc,   clause(legs(A,6),insect(f(A))), sto([[A <-- Y@f(Y)]]),          [prog(clause)]).

test_case(clause:12, eddbali,   clause(f(_),5),                 type(callable,5),               []).


% 8.8.2.4
test_case(cur_prd: 1, iso_doc,  current_predicate(dog/0),       success,                        [prog(clause)]).
test_case(cur_prd: 2, iso_doc,  current_predicate(
                     current_predicate/1),      failure,                        []).
test_case(cur_prd: 3, iso_doc,  current_predicate(elk/Arity),   [[Arity <-- 1]],                [prog(clause)]).
test_case(cur_prd: 4, iso_doc,  current_predicate(foo/_A),      failure,                        [prog(clause)]).
test_case(cur_prd: 5, iso_doc,  current_predicate(Name/1),      variant(modules, [
						 yes - {[Name <-- elk],
							[Name <-- insect]},
						 no  - {[Name <-- elk],
							[Name <-- insect], ...}
			 ]),            [prog(clause)]).
test_case(cur_prd: 6, iso_doc,  current_predicate(4),           type(predicate_indicator,@4),   []).

test_case(cur_prd: 7, eddbali,  current_predicate(dog),         type(predicate_indicator,@dog), []).
test_case(cur_prd: 8, eddbali,  current_predicate(0/dog),       type(predicate_indicator,
                                                          @0/dog),              []).
test_case(cur_prd: 9, eddbali,  current_predicate(P),           variant(modules, [
                                                  yes - {[P<--cat/0],
							 [P<--dog/0],
							 [P<--elk/1],
							 [P<--legs/2],
							 [P<--insect/1]},
						  no  - {[P<--cat/0],
							 [P<--dog/0],
							 [P<--elk/1],
							 [P<--legs/2],
							 [P<--insect/1], ...}
			 ]),            [prog(clause)]).

% 8.9.1.4
test_case(asserta: 1, iso_doc,  asserta(legs(octopus, 8)),      success,                        [prog(dynpreds1),dynp(dynpreds2)]).
test_case(asserta: 2, iso_doc,  asserta(
                (legs(A,4):-animal(A))),        success,                        [prog(dynpreds2),dynp(dynpreds3)]).
test_case(asserta: 3, iso_doc,  asserta((foo(X) :- X,call(X))), success,                        [prog(dynpreds3),dynp(dynpreds4)]).
test_case(asserta: 4, iso_doc,  asserta(_),                     inst,                           []).
test_case(asserta: 5, iso_doc,  asserta(4),                     type(callable,4),               []).
test_case(asserta: 6, iso_doc,  asserta((foo :- 4)),            type(callable,4),               []).
test_case(asserta: 7, iso_doc,  asserta((atom(_) :- true)),     perm(modify,static_procedure,
                                                     @atom/1),                  []).

test_case(asserta: 8, eddbali,  (asserta(insct(bee)), insct(X),
                 asserta(insct(ant)),insct(Y)), [[X <-- bee, Y <-- ant],
                                                 [X <-- bee, Y <-- bee]],       []).

% 8.9.2.4
test_case(assertz: 1, iso_doc,  assertz(legs(spider, 8)),       success,                        [prog(dynpreds4),dynp(dynpreds5)]).
test_case(assertz: 2, iso_doc,  assertz((legs(B, 2):-bird(B))), success,                        [prog(dynpreds5),dynp(dynpreds6)]).
test_case(assertz: 3, iso_doc,  assertz((foo(X):-X->call(X))),  success,                        [prog(dynpreds6),dynp(dynpreds7)]).
test_case(assertz: 4, iso_doc,  assertz(_),                     inst,                           []).
test_case(assertz: 5, iso_doc,  assertz(4),                     type(callable,4),               []).
test_case(assertz: 6, iso_doc,  assertz((foo :- 4)),            type(callable,4),               []).
test_case(assertz: 7, iso_doc,  assertz((atom(_) :- true)),     perm(modify,static_procedure,
                                                     @atom/1),                  []).
% 8.9.3.4
test_case(retract: 1, iso_doc,  retract(legs(octopus, 8)),      success,                        [prog(dynpreds7),dynp(dynpreds8)]).
test_case(retract: 2, iso_doc,  retract(legs(spider, 6)),       failure,                        [prog(dynpreds8),dynp(dynpreds8)]).
test_case(retract: 3, iso_doc,  retract((legs(X, 2) :- T)),     [[T <-- bird(X)]],              [prog(dynpreds8),dynp(dynpreds9)]).
test_case(retract: 4, iso_doc,  retract((legs(X, Y) :- Z)),     [[Y <-- 4, Z <-- animal(X)],
                                                 [Y <-- 6, Z <-- insect(X)],
                                                 [X <-- spider, Y <-- 8,
                                                  Z <-- true]],                 [prog(dynpreds9),dynp(dynpreds10)]).
test_case(retract: 5, iso_doc,  retract((legs(_X, _Y) :- _Z)),  failure,                        [prog(dynpreds10)]).
test_case(retract: 6, iso_doc,  (retract(insect(I)), write(I),
                 retract(insect(bee))),         [[I<--ant]],                    [prog(dynpreds10),txtout('antbee'),
                                                                                 dynp(dynpreds11)]).
test_case(retract: 7, iso_doc,  retract((foo(A) :- A,call(A))), sto(undefined),			[prog(dynpreds11)]).
                                                % too complicated to describe
                                                % the effect of cyclic terms
                                                % plus module qualification...
test_case(retract: 8, iso_doc,  retract((foo(C) :- A -> B)),    [(D = @C)^
                                                 [A<--call(D), B<--call(D)]],   [prog(dynpreds11),
                                                                                 dynp(dynpreds12)]).
test_case(retract: 9, iso_doc,  retract((_X :- in_eec(_Y))),    inst,                           []).
test_case(retract:10, iso_doc,  retract((4 :- _X)),             type(callable,4),               []).
test_case(retract:11, iso_doc,  retract((atom(X) :- X =='[]')), perm(modify,static_procedure,
                                                     @atom/1),                  []).

% 8.9.4.4
% !! SWI-Prolog
test_case(abolish: 1, iso_doc,  abolish(foo/2),                 success,                        []).
test_case(abolish: 2, iso_doc,  abolish(foo/_),                 inst,                           []).
test_case(abolish: 3, iso_doc,  abolish(foo),                   type(predicate_indicator,@foo), []).
test_case(abolish: 4, iso_doc,  abolish(foo(X)),                type(predicate_indicator,
                                                     @foo(X)),                  []).
test_case(abolish: 5, iso_doc,  abolish(abolish/1),             perm(modify,static_procedure,
                                                     @abolish/1),               []).

test_case(abolish: 6, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
% !! SWI-Prolog
test_case(abolish: 61, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
test_case(abolish: 62, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
test_case(abolish: 63, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
test_case(abolish: 64, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
test_case(abolish: 65, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
test_case(abolish: 66, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
test_case(abolish: 67, eddbali,  abolish(foo/1),                 success,                        [prog(abolish)]).
test_case(abolish: 7, eddbali,  (insect(X), abolish(insect/1)), [[X <-- ant],
                                                 [X <-- bee]],                  [prog(abolish)]).
% !! SWI-Prolog: segfault
%test_case(abolish: 8, eddbali,  abolish(foo/_),                 inst,                           [prog(abolish)]).
%test_case(abolish: 9, eddbali,  abolish(bar/1),                 perm(modify,static_procedure,
%                                                     @bar/1),                   [prog(abolish)]).
test_case(abolish:10, eddbali,  abolish(foo/a),                 type(integer,a),                []).
test_case(abolish:11, eddbali,  abolish(foo/(-1)),              dom(not_less_than_zero,-1),     []).
test_case(abolish:12, eddbali,  abolish(foo/X),                 repr(max_arity),                [over_arity(X)]).
test_case(abolish:13, eddbali,  abolish(5/2),                   type(atom,5),                   []).
% !! SWI-Prolog: segfault
%test_case(abolish:14, eddbali,  abolish(insect),                type(predicate_indicator,
%                                                     @insect),                  [prog(abolish)]).

% 8.10.1.4
test_case(findall: 1, iso_doc,  findall(X,(X=1;X=2),S),         [[S <-- [1,2]]],                []).
test_case(findall: 2, iso_doc,  findall(X+_Y,(X=1),S),          [[S <-- [1+_]]],                []).
test_case(findall: 3, iso_doc,  findall(_X,fail,L),             [[L <-- []]],                   []).
test_case(findall: 4, iso_doc,  findall(X,(X=1;X=1),S),         [[S <-- [1,1]]],                []).
test_case(findall: 5, iso_doc,  findall(X,(X=2;X=1),[1,2]),     failure,                        []).
test_case(findall: 6, iso_doc,  findall(X,(X=1;X=2),[X,Y]),     [[X <-- 1, Y <-- 2]],           []).
test_case(findall: 7, iso_doc,  findall(_X,_Goal,_S),           inst,                           []).
test_case(findall: 8, iso_doc,  findall(_X,4,_S),               type(callable,4),               []).
test_case(findall: 9, sics,  findall(X,X=1,[A|1]),           type(list,[A|1]),               []).

% 8.10.2.4
test_case(bagof: 1, iso_doc,    bagof(X,(X=1;X=2),S),           [[S <-- [1, 2]]],               []).
test_case(bagof: 2, iso_doc,    bagof(X,(X=1;X=2),X),           [[X <-- [1, 2]]],               []).
test_case(bagof: 3, iso_doc,    bagof(X,(X=Y;X=Z),S),           [[S <-- [Y, Z]]],               []).
test_case(bagof: 4, iso_doc,    bagof(_X,fail,_S),              failure,                        []).
test_case(bagof: 5, iso_doc,    bagof(1,(Y=1;Y=2),L),           {[L <-- [1], Y <-- 2],
                                                 [L <-- [1], Y <-- 1]},         []).
test_case(bagof: 6, iso_doc,    bagof(f(X,Y),(X=a;Y=b),L),      [[L <-- [f(a, _), f(_, b)]]],   []).
test_case(bagof: 7, iso_doc,    bagof(X,Y^((X=1,Y=1)
                          ;(X=2,Y=2)),S),       [[S <-- [1, 2]]],               []).
test_case(bagof: 8, iso_doc,    bagof(X,Y^((X=1;Y=1)
                          ;(X=2,Y=2)),S),       [[S <-- [1, _, 2]]],            []).
test_case(bagof: 9, iso_doc,    bagof(X,(Y^(X=1;Y=1);X=3),S),
                                                variant(caret, [undef-
						[[S <-- [3], Y <-- _]], % no ^/2
			def-
						{[S <-- [1,3] ],
						 [S <-- [_], Y <-- 1]}  % ^ /2 exists
							       ]),     		[pre(set_prolog_flag(unknown, fail))]).
test_case(bagof:10, iso_doc,    bagof(X,(X=Y;X=Z;Y=1),S),       {[S <-- [Y, Z]],
                                                 [S <-- [_], Y <-- 1]},         []).
test_case(bagof:11, iso_doc,    bagof(X,a(X,Y),L),              [[L <-- [1, 2], Y <-- f(_)]],   [prog([a(1,f(_)),a(2,f(_))])]).
test_case(bagof:12, iso_doc,    bagof(X,b(X,Y),L),              {[L <-- [1, 1, 2], Y <-- 1],
                                                 [L <-- [1, 2, 2], Y <-- 2]},   [prog([b(1,1), b(1,1), b(1,2),
                                                                                       b(2,1), b(2,2), b(2,2)])]).
test_case(bagof:13, iso_doc,    bagof(_X,_Y^_Z,_L),             inst,                           []).
test_case(bagof:14, iso_doc,    bagof(_X,1,_L),                 type(callable, 1),              []).

% 8.10.3.4
test_case(setof: 1, iso_doc,    setof(X,(X=1;X=2),S),           [[S <-- [1, 2]]],               []).
test_case(setof: 2, iso_doc,    setof(X,(X=1;X=2),X),           [[X <-- [1, 2]]],               []).
test_case(setof: 3, iso_doc,    setof(X,(X=2;X=1),S),           [[S <-- [1, 2]]],               []).
test_case(setof: 4, iso_doc,    setof(X,(X=2;X=2),S),           [[S <-- [2]]],                  []).
test_case(setof: 5, iso_doc,    setof(X,(X=Y;X=Z),S),           ([[S <-- [Z, Y]]];
                                                 [[S <-- [Y, Z]]]),             []).
test_case(setof: 6, iso_doc,    setof(_X,fail,_S),              failure,                        []).
test_case(setof: 7, iso_doc,    setof(1,(Y=1;Y=2),L),           {[L <-- [1], Y <-- 1],
                                                 [L <-- [1], Y <-- 2]},         []).
test_case(setof: 8, iso_doc,    setof(f(X,Y),(X=a;Y=b),L),      [[L <-- [f(_, b),f(a, _)]]],    []).
test_case(setof: 9, iso_doc,    setof(X,Y^((X=1,Y=1)
                          ;(X=2,Y=2)),S),       [[S <-- [1, 2]]],               []).
test_case(setof:10, iso_doc,    setof(X,Y^((X=1;Y=1)
                          ;(X=2,Y=2)),S),       [[S <-- [_, 1, 2]]],            []).
test_case(setof:11, iso_doc,    setof(X,((Y^(X=1;Y=1));X=3),S),   variant(caret, [undef-
% Dat: bad parens: test_case(setof:11, iso_doc,    setof(X,(Y^((X=1;Y=1);X=3)),S),   variant(caret, [undef-
						[[S <-- [3], Y <-- _]], % no ^
			def-
						{[S <-- [1,3] ],
						 [S <-- [_], Y <-- 1]}  % ^ /2 exists
			]),     	[pre(set_prolog_flag(unknown, fail))]).
test_case(setof:12, iso_doc,    setof(X,(X=Y;X=Z;Y=1),S),       [[S <-- ([Y, Z];[Z, Y])],
                                                 [S <-- [_], Y <-- 1]],         []).
test_case(setof:13, iso_doc,    setof(X,a(X,Y),L),              [[L <-- [1, 2], Y <-- f(_)]],   [prog([(a(1,f(_)) :- true),
                                                                                       (a(2,f(_)) :- true)])]).
test_case(setof:14, iso_doc,    setof(X,my_member(X,[f(U,b),
                                  f(V,c)]),L),  [[L <-- ([f(U,b),f(V,c)]
                                                        ;[f(V,c),f(U,b)])]],    [prog(member)]).
test_case(setof:15, iso_doc,    setof(X,my_member(X,[f(U,b),
                                  f(V,c)]),
                      [f(a,c),f(a,b)]),         ([[U <-- a, V <-- a]]
                                                ;failure),                      [prog(member)]).
test_case(setof:16, iso_doc,    setof(X,my_member(X,[f(b,U),
                                  f(c,V)]),
                      [f(b,a),f(c,a)]),         [[U <-- a, V <-- a]],           [prog(member)]).
test_case(setof:17, iso_doc,    setof(X,
                  my_member(X,[V,U,f(U),f(V)]),L), [[ L <-- ([U,V,f(U),f(V)]
                                                         ;[V,U,f(V),f(U)])]],   [prog(member)]).
test_case(setof:18, iso_doc,    setof(X,
                  my_member(X,[V,U,f(U),f(V)]),
                  [a,b,f(a),f(b)]),             ([[ U <-- a, V <-- b]]
                                                ;[[ U <-- b, V <-- a]]),        [prog(member)]).
test_case(setof:19, iso_doc,    setof(X,
                  my_member(X,[V,U,f(U),f(V)]),
                  [a,b,f(b),f(a)]),             failure,                        [prog(member)]).
test_case(setof:20, iso_doc,    setof(X,
                  exists(U,V)^my_member(X,[V,U,f(U),f(V)]),
                  [a,b,f(a),f(b)]),             success,                        [prog(member)]).
test_case(setof:21, iso_doc,    setof(X,b(X,Y),L),              {[L <-- [1, 2], Y <-- 1],
                                                 [L <-- [1, 2], Y <-- 2]},      [prog([b(1,1), b(1,1), b(1,2),
                                                                                       b(2,1), b(2,2), b(2,2)])]).
test_case(setof:22, iso_doc,    setof(X-Xs,
                  Y^setof(Y,b(X,Y),Xs),L),      [[L <-- [1-[1,2],2-[1,2]]]],    [prog([b(1,1), b(1,1), b(1,2),
                                                                                       b(2,1), b(2,2), b(2,2)])]).
test_case(setof:23, iso_doc,    setof(X-Xs,
                  setof(Y,b(X,Y),Xs),L),        [[Y <-- _,
                                                  L <-- [1-[1,2],2-[1,2]]]],    [prog([b(1,1), b(1,1), b(1,2),
                                                                                       b(2,1), b(2,2), b(2,2)])]).
test_case(setof:24, iso_doc,    setof(X-Xs,
                  bagof(Y,d(X,Y),Xs),L),        [[Y <-- _,
                                                  L <-- [1-[1,2,1],
                                                         2-[2,1,2]]]],          [prog([d(1,1), d(1,2), d(1,1),
                                                                                       d(2,2), d(2,1), d(2,2)])]).

test_case(setof:25, eddbali,    setof(f(X,Y),X=Y,[f(g(Z),Z)]),  sto(undefined),                 []).
test_case(setof:26, eddbali,    setof(X, X^(true; 4),_L),       type(callable,(true;4)),   []).
test_case(setof:27, sics,    setof(_X,A^A^1,_L),             type(callable,1),               []).
test_case(setof:28, sics,    setof(X,X=1,[1|A]),             [[A <-- []]],                   []).
test_case(setof:29, sics,    setof(X,X=1,[A|1]),             type(list,[A|1]),               []).

% 8.11.1
test_case(currinp: 1, sics,  current_input(_S),              success,                        []).
test_case(currinp: 2, sics,  current_input(foo),             dom(stream,foo),                []).
test_case(currinp: 3, sics,  current_input(S),               failure,                        [pre(current_output(S))]).
test_case(currinp: 4, sics,  current_input(S),               dom(stream,S),                  [closed_instream(S,[])]).
test_case(currinp: 5, sics,  current_input(S),               success,                        [pre(current_input(S))]).

% 8.11.2
test_case(currout: 1, sics,  current_output(_S),             success,                        []).
test_case(currout: 2, sics,  current_output(foo),            dom(stream,foo),                []).
test_case(currout: 3, sics,  current_output(S),              failure,                        [pre(current_input(S))]).
test_case(currout: 4, sics,  current_output(S),              dom(stream,S),                  [closed_outstream(S,[])]).
test_case(currout: 5, sics,  current_output(S),              success,                        [pre(current_output(S))]).

% 8.11.3
test_case(set_in: 1, sics,   set_input(S),                   success,                        [pre(current_input(S))]).
test_case(set_in: 2, sics,   set_input(_S),                  inst,                           []).
test_case(set_in: 3, sics,   set_input(foo),                 dom(stream_or_alias,foo),       []).
test_case(set_in: 4, sics,   set_input(S),                   exist(stream,S),                [closed_instream(S,[])]).
test_case(set_in: 5, sics,   set_input(S),                   perm(input,stream,S),           [pre(current_output(S))]).

% 8.11.4
test_case(set_out: 1, sics,  set_output(S),                  success,                        [pre(current_output(S))]).
test_case(set_out: 2, sics,  set_output(_S),                 inst,                           []).
test_case(set_out: 3, sics,  set_output(foo),                dom(stream_or_alias,foo),       []).
test_case(set_out: 4, sics,  set_output(S),                  exist(stream,S),                [closed_outstream(S,[])]).
test_case(set_out: 5, sics,  set_output(S),                  perm(output,stream,S),          [pre(current_input(S))]).

% 8.11.5.4
test_case(open: 1, iso_doc,     open('roger_data', read, D,
                     [type(binary)]),           [[at_end_of_stream(D)]],        [makebin(roger_data, [])]).
test_case(open: 2, iso_doc,     open('scowen', write, D,
                     [alias(editor)]),          [[stream_property(D,
                                                        alias(editor)) ]],      [checktxt(scowen,'')]).
test_case(open: 3, iso_doc,     open('dave', read, DD, []),     [[read(DD, foo),
                                                  at_end_of_stream(DD)]],	[maketxt(dave, 'foo.')]).

test_case(open: 4, sics,     open(_,read,_),                 inst,                           []).
test_case(open: 5, sics,     open(f,_,_),                    inst,                           []).
test_case(open: 6, sics,     open(f,write,_,_),              inst,                           []).
test_case(open: 7, sics,     open(f,write,_,[type(text)|_]), inst,                           []).
test_case(open: 8, sics,     open(f,write,_,[type(text),_]), inst,                           []).
test_case(open: 9, sics,     open(f,1,_),                    type(atom,1),                   []).
test_case(open:10, sics,     open(f,write,_,type(text)),     type(list,type(text)),          []).
test_case(open:11, sics,     open(f,write, bar),             type(variable,bar),             []).
test_case(open:12, sics,     open(foo(1,2),write,_),         dom(source_sink,foo(1,2)),      []).
test_case(open:13, sics,     open('foo',red,_),              dom(io_mode,red),               []).
test_case(open:14, sics,     open(foo,write,_,[bar]),        dom(stream_option,bar),         []).
test_case(open:15, sics,     open('nonexistent',read,_),     exist(source_sink,@@),          []).
test_case(open:16, sics,     open(bar,write,_,[alias(a)]),   perm(open,source_sink,
                                                     alias(a)),                 [pre(open(foo,write,_,[alias(a)])),
                                                                                 clean(foo)]).
/* [PM] 3.8.4 Was
test_case(open:17, sics,     open('/dev/tty',read,_,
                     [reposition(true)]),       perm(open,source_sink,
                                                     reposition(true)),[]).
*/
/* [PM] 3.8.4 /dev/tty does not exist on Windows and not when building
   via rsh. Expect therefore existence error as an alternative. */
test_case(open:17, sics,     open('/dev/tty',read,_,
                     [reposition(true)]),       ( perm(open,source_sink,
                                                       reposition(true))
                                                ; exist(source_sink,_)
                                                ),                              []).

% 8.11.6
test_case(close: 1, sics,    close(S),                       success,                        [pre(open('foo',write,S)),
                                                                                 checktxt(foo,'')]).
test_case(close: 2, sics,    close(_),                       inst,                           []).
test_case(close: 3, sics,    close(S,_),                     inst,                           [pre(current_input(S))]).
test_case(close: 4, sics,    close(S,[force(true)|_]),       inst,                           [pre(current_input(S))]).
test_case(close: 5, sics,    close(S,[force(true),_]),       inst,                           [pre(current_input(S))]).
test_case(close: 6, sics,    close(S,foo),                   type(list,foo),                 [pre(current_input(S))]).
test_case(close: 7, sics,    close(S,[foo]),                 dom(close_option,foo),          [pre(current_input(S))]).
test_case(close: 8, sics,    close(foo),                     dom(stream_or_alias,foo),       []).
test_case(close: 9, sics,    close(S),                       exist(stream,S),                [closed_outstream(S, [])]).

% 8.11.7
test_case(flush_o: 1, sics,  flush_output(S),                success,                        [pre((open('foo', write, S),
                                                                                      write(S, foo))),
                                                                                 checktxt(foo,'foo')]).
test_case(flush_o: 2, sics,  flush_output(foo),              dom(stream_or_alias,foo),       []).
test_case(flush_o: 3, sics,  flush_output(_S),               inst,                           []).
test_case(flush_o: 4, sics,  flush_output(S),                exist(stream,S),                [closed_outstream(S, [])]).
test_case(flush_o: 5, sics,  flush_output(S),           	perm(output,stream,S),          [maketxt(foo,''),
                                                                                 pre(open(foo,read,S))]).
test_case(flush_o: 6, sics,  flush_output(st_o),             success,                        [txtout(st_o,'','')]).

% 8.11.8.4
test_case(strprop: 1, iso_doc,  stream_property(S,
				file_name(F)),	{[S <-- S1, F <-- @@],
                                                 [S <-- S2, F <-- @@]}, 	[maketxt(foo,''),
					 pre((open(foo, read, S1),
					      open(bar, write, S2))),
					 clean(bar)]).
test_case(strprop: 2, iso_doc,   stream_property(S, output), 	{[S <-- FOut ],
                                                 [S <-- COut], ...}, 		[pre((open(bar, write, FOut),
					      current_output(COut))),
					 clean(bar)]).

test_case(strprop: 3, sics,  stream_property(foo,_S), 	dom(stream,foo),		[]).
test_case(strprop: 4, sics,  stream_property(_S,foo), 	dom(stream_property,foo),      	[]).
% vvv !! aprolog has eof_action(eof_mode), reposition(true)
test_case(strprop: 5, sics,  stream_property(S, P),          {[P <-- input],
						 [P <-- alias(user_input)],
						 [P <-- eof_action(reset)],
						 [P <-- mode(read)],
						 [P <-- reposition(false)],
						 [P <-- type(text)], ...},    	[pre(current_input(S))]).
						% 7.10.2.13, Table 40
test_case(strprop: 6, sics,  stream_property(S, P),          {[P <-- output],
						 [P <-- alias(user_output)],
						 [P <-- eof_action(reset)],
						 [P <-- mode(append)],
						 [P <-- reposition(false)],
						 [P <-- type(text)], ...},     	[pre(current_output(S))]).
test_case(strprop: 7, sics,  stream_property(_S,
				type(binary)),	failure,			[]).

test_case(atendst: 8, sics,  at_end_of_stream(_S),  		inst,                   	[]).
test_case(atendst: 9, sics,  at_end_of_stream(foo), 		dom(stream_or_alias,foo),	[]).
test_case(atendst:10, sics,  at_end_of_stream(S), 		exist(stream,S),     		[closed_outstream(S,[])]).
test_case(atendst:11, sics,  at_end_of_stream(st_i), 	success,	     		[txtin(st_i,'','')]).
test_case(atendst:12, sics,  at_end_of_stream(st_i), 	failure,	     		[txtin(st_i,'a','a')]).
test_case(atendst:13, sics,  at_end_of_stream(st_i), 	success,	     		[binin(st_i,[],[])]).
test_case(atendst:14, sics,  at_end_of_stream(st_i), 	failure,	     		[binin(st_i,[0],[0])]).

% 8.11.9
% !! SWI-Prolog !! global problem with stream redirection, seems to truncate swipl.test_results
%test_case(setstrp: 1, sics,  set_stream_position(_S,Pos), 	inst,                   	[stream_position(Pos)]).
%test_case(setstrp: 2, sics,  set_stream_position(S,_Pos), 	inst,                   	[pre(current_input(S))]).
%test_case(setstrp: 3, sics,  set_stream_position(foo,Pos), 	dom(stream_or_alias,foo), 	[stream_position(Pos)]).
%test_case(setstrp: 4, sics,  set_stream_position(S,Pos), 	exist(stream,S), 		[stream_position(Pos),
%					 closed_outstream(S,[])]).
%test_case(setstrp: 5, sics,  set_stream_position(S,foo), 	dom(stream_position,foo), 	[pre(current_input(S))]).
%test_case(setstrp: 6, sics,  set_stream_position(S,Pos), 	perm(reposition,stream,S), 	[pre((open(foo,write,FS),
%					  stream_property(FS,position(Pos)),
%					  current_input(S))),
%					 clean(foo)]).
% missing: set_stream_position/2 functionality test


% 8.12.1.4
test_case(getchar: 1, iso_doc,  get_char(Char),        		[[Char <-- 'q']],  		[txtin('qwerty','werty')]).
test_case(getcode: 2, iso_doc,  get_code(Code),        		[[Code <-- 0'q]],  		[txtin('qwerty', 'werty')]).
test_case(getchar: 3, iso_doc,  get_char(st_i, Char),  		[[Char <-- 'q']],  		[txtin(st_i, 'qwerty','werty')]).
test_case(getcode: 4, iso_doc,  get_code(st_i, Code),  		[[Code <-- 0'q]],  		[txtin(st_i, 'qwerty','werty')]).
test_case(getchar: 5, iso_doc,  get_char(st_i, Char),  		[[Char <-- '''']], 		[txtin(st_i,
					      "'qwerty'","qwerty'")]).
% vvv pts: swi-prolog !!
%stc(getcode: 6, get_code(st_i, Code),  		[[Code <-- 0'\']],  		[txtin(st_i,
test_case(getcode: 6, iso_doc,  get_code(st_i, Code),  		[[Code <-- 39]],  		[txtin(st_i,
%stc(getcode: 6, get_code(st_i, Code),  		[[Code <-- 0'\']],  		[txtin(st_i,
					      "'qwerty'","qwerty'")]).
test_case(getchar: 7, iso_doc,  get_char(st_i, p),     		failure,           		[txtin(st_i, 'qwerty','werty')]).
test_case(getcode: 8, iso_doc,  get_code(st_i, 0'p),   		failure,           		[txtin(st_i, 'qwerty','werty')]).
test_case(getchar: 9, iso_doc,  get_char(st_i, Char),  		[[Char <-- end_of_file]], 	[txtin(st_i,'',{past})]).
test_case(getcode:10, iso_doc,  get_code(st_i, Code),  		[[Code <-- -1]], 		[txtin(st_i,'',{past})]).
test_case(getchar:11, iso_doc,  get_char(user_output, _), 	perm(input,stream,user_output), []).
test_case(getcode:12, iso_doc,  get_code(user_output, _), 	perm(input,stream,user_output), []).

test_case(getchar:13, sics,  get_char(_, _),        		inst,                           []).
test_case(getchar:14, sics,  get_char(1),           		type(in_character,1),   	[]).
test_case(getchar:15, sics,  get_char(user_input,1),		type(in_character,1),   	[]).
test_case(getchar:16, sics,  get_char(foo,_),       		dom(stream_or_alias,foo),	[]).
test_case(getchar:17, sics,  get_char(S,_), 			exist(stream,S), 		[closed_outstream(S,[])]).
test_case(getchar:18, sics,  get_char(S,_), 			perm(input,stream,S),		[pre(current_output(S))]).
test_case(getchar:19, sics,  get_char(_), 			perm(input,binary_stream,S), 	[binin([]),
					 pre(current_input(S))]).
test_case(getchar:20, sics,  get_char(_),			perm(input,past_end_of_stream,
						     S),		 	[txtin('',{past}),
					 pre(current_input(S)),
					 pre(get_char(_))]).
test_case(getchar:21, sics,  (get_char(S, C1),
		 get_char(S, C2)), 		[[C1 <-- end_of_file,
						  C2 <-- end_of_file]], 	[maketxt(t, ''),
					 pre(open(t, read, S,
					      [eof_action(eof_code)]))]).
test_case(getchar:22, sics,  get_char(S, _),	 		repr(character), 		[makebin(t,[0]),
					 pre(open(t, read, S))]).

test_case(getcode:23, sics,  get_code(_, _),        		inst,                           []).
test_case(getcode:24, sics,  get_code(p),           		type(integer,p),                []).
test_case(getcode:25, sics,  get_code(user_input,p),		type(integer,p),                []).
test_case(getcode:26, sics,  get_code(-2),          		repr(in_character_code),        []).
test_case(getcode:27, sics,  get_code(foo,_),       		dom(stream_or_alias,foo),       []).
test_case(getcode:28, sics,  get_code(S,_), 			exist(stream,S), 		[closed_instream(S,[])]).
test_case(getcode:29, sics,  get_code(S,_), 			perm(input,stream,S),		[pre(current_output(S))]).
test_case(getcode:30, sics,  get_code(_), 			perm(input,binary_stream,_S),	[binin([])]).
test_case(getcode:31, sics,  get_code(_),			perm(input,past_end_of_stream,
						     S),		 	[txtin('',{past}),
					 pre(current_input(S)),
					 pre(get_code(_))]).
test_case(getcode:32, sics,  (get_code(S, C1),
		 get_code(S, C2)), 		[[C1 <-- -1,
						  C2 <-- -1]], 			[maketxt(t, ''),
					 pre(open(t, read, S,
					      [eof_action(eof_code)]))]).
test_case(getcode:33, sics,  get_code(S, _),	 		repr(character), 		[makebin(t,[0]),
					 pre(open(t, read, S))]).

% 8.12.2.4
test_case(peekchr: 1, iso_doc,  peek_char(Char),      		[[Char <-- 'q']],  		[txtin('qwerty', 'qwerty')]).
test_case(peekcde: 2, iso_doc,  peek_code(Code),      		[[Code <-- 0'q]],  		[txtin('qwerty', 'qwerty')]).
test_case(peekchr: 3, iso_doc,  peek_char(st_i, Char),		[[Char <-- 'q']],  		[txtin(st_i, 'qwerty','qwerty')]).
test_case(peekcde: 4, iso_doc,  peek_code(st_i, Code),		[[Code <-- 0'q]],  		[txtin(st_i, 'qwerty','qwerty')]).
test_case(peekchr: 5, iso_doc,  peek_char(st_i, Char),		[[Char <-- '''']], 		[txtin(st_i,
					      "'qwerty'","'qwerty'")]).
% vvv pts: swi-prolog !!
%stc(peekcde: 6, peek_code(st_i, Code),		[[Code <-- 0'\']],  		[txtin(st_i,
test_case(peekcde: 6, iso_doc,  peek_code(st_i, Code),		[[Code <-- 39]],  		[txtin(st_i,
%stc(peekcde: 6, peek_code(st_i, Code),		[[Code <-- 0'\']],  		[txtin(st_i,
					      "'qwerty'","'qwerty'")]).
test_case(peekchr: 7, iso_doc,  peek_char(st_i, p),   		failure,           		[txtin(st_i, 'qwerty','qwerty')]).
test_case(peekcde: 8, iso_doc,  peek_code(st_i, 0'p), 		failure,           		[txtin(st_i, 'qwerty','qwerty')]).
test_case(peekchr: 9, iso_doc,  peek_char(st_i, Char), 		[[Char <-- end_of_file]], 	[txtin(st_i,'','')]).
test_case(peekcde:10, iso_doc,  peek_code(st_i, Code), 		[[Code <-- -1]], 		[txtin(st_i,'','')]).
test_case(peekchr:11, iso_doc,  peek_char(s, _Char), 		perm(input,past_end_of_stream,
						     _S), 			[txtin(s, '', {past}),
					 pre(get_char(s, _))]).
test_case(peekchr:12, iso_doc,  peek_char(user_output, _), 	perm(input,stream,user_output), []).
test_case(peekcde:13, iso_doc,  peek_code(user_output, _), 	perm(input,stream,user_output), []).

test_case(peekchr:14, sics,  peek_char(_, _),      		inst,                           []).
test_case(peekchr:15, sics,  peek_char(1),         		type(in_character,1),   	[]).
test_case(peekchr:16, sics,  peek_char(user_input,1),	type(in_character,1),         	[]).
test_case(peekchr:17, sics,  peek_char(foo,_),     		dom(stream_or_alias,foo),	[]).
test_case(peekchr:18, sics,  peek_char(S,_), 		exist(stream,S), 		[closed_instream(S,[])]).
test_case(peekchr:19, sics,  peek_char(S,_), 		perm(input,stream,S),		[pre(current_output(S))]).
test_case(peekchr:20, sics,  peek_char(s,_), 		perm(input,binary_stream,s), 	[binin(s, [], [])]).
test_case(peekchr:21, sics,  (peek_char(S, C1),
		 peek_char(S, C1),
		 peek_char(S, C2)), 		[[C1 <-- end_of_file,
						  C2 <-- end_of_file]], 	[maketxt(t, ''),
					 pre(open(t, read, S))]).
test_case(peekchr:22, sics,  peek_char(S, _), 		repr(character), 		[makebin(t, [0]),
					 pre(open(t, read, S))]).

test_case(peekcde:23, sics,  peek_code(_, _),      		inst,                           []).
test_case(peekcde:24, sics,  peek_code(p),         		type(integer,p),                []).
test_case(peekcde:25, sics,  peek_code(user_input,p),	type(integer,p),      		[]).
test_case(peekcde:26, sics,  peek_code(-2),        		repr(in_character_code), 	[]).
test_case(peekcde:27, sics,  peek_code(foo,_),     		dom(stream_or_alias,foo),	[]).
test_case(peekcde:28, sics,  peek_code(S,_), 		exist(stream,S), 		[closed_instream(S,[])]).
test_case(peekcde:29, sics,  peek_code(S,_), 		perm(input,stream,S),		[pre(current_output(S))]).
test_case(peekcde:30, sics,  peek_code(S,_), 		perm(input,binary_stream, S), 	[binin([]), pre(current_input(S))]).
test_case(peekcde:31, sics,  peek_code(_), 			perm(input,past_end_of_stream,
						     S), 			[txtin('',{past}),
					 pre(current_input(S)),
					 pre(get_code(_))]).
test_case(peekcde:32, sics,  (peek_code(C1), peek_code(C2)),	[[C1 <-- -1, C2 <-- -1]], 	[txtin('')]).
test_case(peekcde:33, sics,  peek_code(S, _), 		repr(character), 		[makebin(t, [0]),
					 pre(open(t, read, S))]).

% 8.12.3.4
test_case(putchar: 1, iso_doc,  put_char(t),   			success,        		[txtout('qwer','qwert')]).
test_case(putchar: 2, iso_doc,  put_char(st_o, 'A'),		success,   			[txtout(st_o,'qwer','qwerA')]).
test_case(putcode: 3, iso_doc,  put_code(0't), 			success,        		[txtout('qwer','qwert')]).
test_case(putcode: 4, iso_doc,  put_code(st_o, 0't),		success,   			[txtout(st_o,'qwer','qwert')]).
test_case(putchar: 5, iso_doc,  (nl, put_char(a)),		success,     			[txtout('qwer','qwer\na')]).
test_case(putchar: 6, iso_doc,  (nl(st_o), put_char(st_o, a)), 	success,			[txtout(st_o,'qwer', 'qwer\na')]).
test_case(putchar: 7, iso_doc,  put_char(my_file, _C), 		inst, 				[txtout(my_file,'','')]).
test_case(putchar: 8, iso_doc,  put_char(st_o, 'ty'), 		type(character, ty), 		[txtout(st_o,'','')]).
test_case(putcode: 9, iso_doc,  put_code(my_file, _C), 		inst, 				[txtout(my_file,'','')]).
test_case(putcode:10, iso_doc,  put_code(st_o, 'ty'), 		type(integer, ty), 		[txtout(st_o,'','')]).
test_case(nl:     11, iso_doc,  nl(_Sr),       			inst,            		[]).
test_case(nl:     12, iso_doc,  nl(user_input),			perm(output,stream,user_input), []).
test_case(putchar:13, sics,  put_char(_S,t),			inst,            		[]).
test_case(putchar:14, sics,  put_char(_C),  			inst,				[]).
test_case(putchar:15, sics,  put_char(S,a), 			exist(stream,S), 		[closed_outstream(S,[])]).
test_case(putchar:16, sics,  put_char(S,a), 			perm(output,stream,S),		[pre(current_input(S))]).
test_case(putchar:17, sics,  put_char(a), 			perm(output,binary_stream,S), 	[binout([]),
					 pre(current_output(S))]).
test_case(putcode:18, sics,  put_code(_S,0't),      		inst,            		[]).
test_case(putcode:19, sics,  put_code(_C),          		inst,            		[]).
test_case(putcode:20, sics,  put_code(S,0'a), 		exist(stream,S), 		[closed_outstream(S,[])]).
test_case(putcode:21, sics,  put_code(S,0'a), 		perm(output,stream,S),		[pre(current_input(S))]).
test_case(putcode:22, sics,  put_code(S,0'a), 		perm(output,binary_stream,S), 	[pre(open(t,write,S,
						  [type(binary)])),
					 clean(t)]).
test_case(putcode:23, sics,  put_code(-1),  			repr(character_code), 		[]).
test_case(putcode:24, sics,  put_code(foo,1),       		dom(stream_or_alias,foo),	[]).

% 8.13.1.4
test_case(getbyte: 1, iso_doc,  get_byte(Byte),			[[Byte <-- 113]],  		[binin([113,119,101,114],
					       [119,101,114])]).
test_case(getbyte: 2, iso_doc,  get_byte(st_i, Byte), 		[[Byte <-- 113]],  		[binin(st_i, [113,119,101,114],
					       [119,101,114])]).
test_case(getbyte: 3, iso_doc,  get_byte(st_i, 114), 		failure,           		[binin(st_i,
					       [113,119,101,114,116,121],
					       [119,101,114,116,121])]).
test_case(getbyte: 4, iso_doc,  get_byte(st_i, Byte), 		[[Byte <-- -1]],   		[binin(st_i, [], {past})]).
test_case(getbyte: 5, iso_doc,  get_byte(user_output, _Byte), 	perm(input,stream,user_output), []).
test_case(getbyte: 6, sics,  get_byte(_, _),        		inst,                           []).
test_case(getbyte: 7, sics,  get_byte(p),      		type(in_byte,p), 		[binin([])]).
test_case(getbyte: 8, sics,  get_byte(-2),      		type(in_byte,-2), 		[binin([])]).
test_case(getbyte: 9, sics,  get_byte(foo,_),       		dom(stream_or_alias,foo),	[]).
test_case(getbyte:10, sics,  get_byte(S,_), 			exist(stream,S), 		[closed_instream(S,
							 [type(binary)])]).
test_case(getbyte:11, sics,  get_byte(S,_), 			perm(input,stream,S), 		[pre(current_output(S))]).
test_case(getbyte:12, sics,  get_byte(_), 			perm(input,text_stream,S), 	[txtin(''),
					 pre(current_input(S))]).
test_case(getbyte:13, sics,  (get_byte(_), get_byte(_)), 	perm(input,past_end_of_stream,
						     S), 			[binin([], {past}),
					 pre(current_input(S))]).

% 8.13.2.4
test_case(peekbte: 1, iso_doc,  peek_byte(Byte),      		[[Byte <-- 113]],	 	[binin([113,119,101,114],
					       [113,119,101,114])]).
test_case(peekbte: 2, iso_doc,  peek_byte(st_i, Byte),		[[Byte <-- 113]], 		[binin(st_i,
					       [113,119,101,114],
					       [113,119,101,114])]).
test_case(peekbte: 3, iso_doc,  peek_byte(st_i, 114), 		failure, 			[binin(st_i,
					       [113,119,101,114],
					       [113,119,101,114])]).
test_case(peekbte: 4, iso_doc,  peek_byte(st_i, Byte),		[[Byte <-- -1]], 		[binin(st_i, [], [])]).
test_case(peekbte: 5, iso_doc,  peek_byte(user_output, _Byte), 	perm(input,stream,user_output), []).
test_case(peekbte: 6, sics,  peek_byte(_, _),      		inst,                           []).
test_case(peekbte: 7, sics,  peek_byte(p),     		type(in_byte,p), 		[binin([])]).
test_case(peekbte: 8, sics,  peek_byte(-2),     		type(in_byte,-2), 		[binin([])]).
test_case(peekbte: 9, sics,  peek_byte(foo,_),     		dom(stream_or_alias,foo),	[]).
test_case(peekbte:10, sics,  peek_byte(S,_), 		exist(stream,S), 		[closed_instream(S,
							 [type(binary)])]).
test_case(peekbte:11, sics,  peek_byte(S,_),			perm(input,stream,S),		[pre(current_output(S))]).
test_case(peekbte:12, sics,  peek_byte(_),	 		perm(input,text_stream,S), 	[txtin(''),
					 pre(current_input(S))]).
test_case(peekbte:13, sics,  (get_byte(_), peek_byte(_)),	perm(input,
						     past_end_of_stream, S),	[binin([],{past}),
					 pre(current_input(S))]).

% 8.13.3.4
test_case(putbyte: 1, iso_doc,  put_byte(84),         		success,			variant(literally_iso, [
				          no  - [binout([113,119,101,114],
						[113,119,101,114,84])],
					  yes - [binout([113,119,101,114],
						[113,119,101,114,116])]
							       ])).
test_case(putbyte: 2, iso_doc,  put_byte(st_o, 116),   		success,			[binout(st_o, [113,119,101,114],
						[113,119,101,114,116])]).
test_case(putbyte: 3, iso_doc,  put_byte(my_file, _C), 		inst, 				[binout(my_file,[],[])]).

test_case(putbyte: 4, iso_doc,  put_byte(my_file, 'ty'), 	type(byte, ty), 		[binout(my_file,[],[])]).
                % ISO: user_output
test_case(putbyte: 5, sics,  put_byte(_S, 118),     		inst,            		[]).
test_case(putbyte: 6, sics,  put_byte(_C), 			inst,            		[binout([])]).
test_case(putbyte: 7, sics,  put_byte(S, 77), 		exist(stream,S), 		[closed_outstream(S,
					[type(binary)])]).
test_case(putbyte: 8, sics,  put_byte(S, 99), 		perm(output,stream,S), 		[binin([]),
					 pre(current_input(S))]).
test_case(putbyte: 9, sics,  put_byte(99),		 	perm(output,text_stream,S), 	[pre(current_output(S))]).
test_case(putbyte:10, sics,  put_byte(-1), 			type(byte, -1), 		[binout([])]).
test_case(putbyte:11, sics,  put_byte(_S,1),        		inst,            		[]).
test_case(putbyte:12, sics,  put_byte(foo,1),       		dom(stream_or_alias, foo), 	[]).

% 8.14.1.4

test_case(read: 1, iso_doc,     read(T),			[[T <-- term1]],		[txtin('term1. term2. ...',
					       ' term2. ...')]).
test_case(read: 2, iso_doc,     read(st_o, term1),		success,			[txtin(st_o,'term1. term2. ...',
					       ' term2. ...')]).
test_case(read: 3, iso_doc,     read_term(st_o, T,
		[variables(VL),
		 variable_names(VN),
		 singletons(VS)]),		[[ T <-- foo(X1+X2,X1+X3),
						   VL <-- [X1,X2,X3],
						   VN <-- ['A'=X1,'Roger'=X2],
						   VS <-- ['Roger'=X2] ]],	[txtin(st_o,
					       ['foo(A+Roger,A+_). ',
						'term2. ...'],
					       ' term2. ...')]).
test_case(read: 4, iso_doc,     read(4.1),			failure,			[txtin('3.1.  term2. ...',
					       '  term2. ...')]).
test_case(read: 5, iso_doc,     read(_T),			syntax,				[txtin('foo 123. term2. ...',
					       ' term2. ...')]).
%stc(read: 6,    read(_T),			syntax,				[txtin('3.1',{past})]).
test_case(read: 6, iso_doc,     read(_T),			syntax,				[txtin('3.1','')]).

test_case(read: 7, sics,     read_term(T, [singletons(S)]),	[[ T <-- foo(bar), S <-- [] ]],	[txtin('foo( bar).')]).
test_case(read: 8, sics,     read(_,_),			inst,				[]).
test_case(read: 9, sics,     read_term(user_input,_,_),	inst,				[]).
test_case(read:10, sics,     read_term(user_input,_,
			  [variables(_)|_]),	inst,				[]).
test_case(read:11, sics,     read_term(user_input,_,
			  [variables(_),_]),	inst,				[]).
test_case(read:12, sics,     read(foo,_),			dom(stream_or_alias,foo),	[]).
test_case(read:13, sics,     read_term(user_input,_,bar),	type(list,bar),			[]).
test_case(read:14, sics,     read_term(user_input,_,[bar]),	dom(read_option,bar),	[]).
test_case(read:15, sics,     read_term(user_output,_,[]),	perm(input,stream,user_output),	[]).
test_case(read:16, sics,     read(T),			[[ T <-- end_of_file ]],	[txtin('',{past})]).
test_case(read:17, sics,     read_term(S,_,[]), 		exist(stream,S), 		[closed_instream(S,[])]).
test_case(read:18, sics,     read_term(_,[]), 		perm(input,binary_stream,S), 	[binin([]),
				 pre(current_input(S))]).
% !! SWI-Prolog
%test_case(read:19, sics,     read(_),	 		perm(input,binary_stream,S), 	[binin([]),
%							 pre(current_input(S))]).
%test_case(read:20, sics,     read_term(S,_,[]),		perm(input,past_end_of_stream,
%				     S), [txtin('',{past}),
%					 pre(current_input(S)),
%					 pre(get_code(_))]).
test_case(read:21, sics,     read_term(_,[]),		repr(max_arity),	  	[txtin_foo256]).
%atc(read:22,    read_term(_,[]),		syntax,			  	[txtin("'a.", {past})]).
test_case(read:22, sics,     read_term(_,[]),		syntax,			  	[txtin("'a.", '')]).
test_case(read:23, sics,     read(X),                        [[X <-- M]],                    [txtin_maxmin(max_integer, M)]).
test_case(read:24, sics,     read(X),                        [[X <-- M]],                    [txtin_maxmin(min_integer, M)]).

% TODO: read(X), get_code(C) with input a.%

% 8.14.2.4
test_case(write: 1, iso_doc,    write_term(S, [1,2,3], []),	success,			[txtout('[1,2,3]'),
					 pre(current_output(S))]).
% stc(write: 2,	write_canonical([1,2,3]),	success,			[txtout('.(1,.(2,.(3,[])))')]).
test_case(write: 2, iso_doc,    write_canonical([1,2,3]),	success,			[txtout(rx(
					   ["\\(\\.\\|'\\.'\\)",
					    "(1,\\1(2,\\1(3,\\[\\])))"]))]).
					% this nice regexp is just to
					% allow . to be written as '.'
test_case(write: 3, iso_doc,    write_term(S, '1<2', []),	success,			[txtout('1<2'),
					 pre(current_output(S))]).
test_case(write: 4, iso_doc,    writeq(S, '1<2'),		success,			[txtout("'1<2'"),
					 pre(current_output(S))]).
test_case(write: 5, iso_doc,    writeq('$VAR'(0)),		success,			[txtout('A')]).
test_case(write: 6, iso_doc,    write_term(S,'$VAR'(1),
		          [numbervars(false)]),	success,			[txtout('$VAR(1)'),
					 pre(current_output(S))]).
test_case(write: 7, iso_doc,    write_term(S,'$VAR'(51),
			   [numbervars(true)]),	success,			[txtout('Z1'),
					 pre(current_output(S))]).

test_case(write: 8, sics,    write(_S, foo),			inst,				[]).
test_case(write: 9, sics,    write_term(foo, _Opts),		inst,				[]).
test_case(write:10, sics,    write_term(user_output,
			     foo, _Opts),	inst,				[]).
test_case(write:11, sics,    write_term(foo,
	    	   [quoted(true)|_Opts]),	inst,				[]).
test_case(write:12, sics,    write_term(user_output, foo,
	    	   [quoted(true)|_Opts]),	inst,				[]).
test_case(write:13, sics,    write_term(foo,
	    	   [quoted(true),_Opts]),	inst,				[]).
test_case(write:14, sics,    write_term(user_output, foo,
	    	   [quoted(true),_Opts]),	inst,				[]).
test_case(write:15, sics,    write_term(user_output, 1, 2),	type(list,2),			[]).
test_case(write:16, sics,    write_term(1, [quoted(true)
	   		      |foo]),		type(list,[quoted(true)|foo]),	[]).
test_case(write:17, sics, 	write(foo, 1),			dom(stream_or_alias,foo),	[]).
test_case(write:18, sics,    write_term(1, [quoted(true),
	   		      foo]),		dom(write_option,foo),		[]).
test_case(write:19, sics,    write(S,a), 			exist(stream,S), 		[closed_outstream(S,[])]).
test_case(write:20, sics,    write(S,a), 			perm(output,stream,S),		[pre(current_input(S))]).
test_case(write:21, sics,    write(a), 			perm(output,binary_stream,S), 	[binout([]),
					 pre(current_output(S))]).

% 8.14.3.1

test_case(op: 1, iso_doc, 	op(30, xfy, ++),		success,			[post(current_op(30,xfy,++)),
					 post(op(0,xfy,++))]).
test_case(op: 2, iso_doc, 	op(0 , xfy, ++),		success,			[pre(op(30,xfy,++)),
					 post(\+current_op(_,_,++))]).
test_case(op: 3, iso_doc, 	op(max, xfy, ++),		type(integer,max),		[]).
test_case(op: 4, iso_doc, 	op(-30, xfy, ++),		dom(operator_priority,-30),	[]).
test_case(op: 5, iso_doc, 	op(1201, xfy, ++),		dom(operator_priority,1201),	[]).
test_case(op: 6, iso_doc, 	op(30, _XFY, ++),		inst,				[]).
test_case(op: 7, iso_doc, 	op(30, yfy, ++),		dom(operator_specifier,yfy),	[]).
test_case(op: 8, iso_doc, 	op(30, xfy, 0),			type(list,0),			[]).
test_case(op: 9, iso_doc, 	(op(30, xfy, ++),
		 op(40, xfx, ++)),		success,			[post(current_op(40,xfx,++)),
					 post(op(0,xfx,++))]).
test_case(op:10, iso_doc, 	(op(30, xfy, ++),
	 	 op(50, yf, ++)),		perm(create,operator,++),	[post(current_op(30,xfy,++)),
					 post(op(0,xfy,++))]) :-
	\+ context_info(postfix_and_infix_op, allowed). % Dat: conditional to skip test case !! better methodology, don't skip
test_case(op:11, sics, 	op(_,xfx,++),			inst,				[]).
test_case(op:12, sics, 	op(100,xfx,_),			inst,				[]).
test_case(op:13, sics, 	op(100,xfx,[a|_]),		inst,				[]).
test_case(op:14, sics, 	op(100,xfx,[a,_]),		inst,				[]).
test_case(op:15, sics, 	op(100,200,[a]),		type(atom,200),			[]).
test_case(op:16, sics, 	op(100,f(1),[a]),		type(atom,f(1)),		[]).
test_case(op:17, sics, 	op(100,xfx,[a,a+b]),		type(atom,a+b),			[]).
test_case(op:18, sics, 	op(100,xfx,','),		perm(modify,operator,','),	[]).
test_case(op:19, sics, 	op(100,xfx,[a,',']),		perm(modify,operator,','),	[]).

% 8.14.4.4

test_case(currop: 1, iso_doc, 	current_op(P, xfy, OP),		{[ P <-- 1100, OP <-- (';')],
						 [ P <-- 1050, OP <-- ('->')],
						 [ P <-- 1000, OP <-- (',')],
						 [ P <-- 200, OP <-- ('^')],
						 ...},				[]).

test_case(currop: 2, sics, 	current_op(1201, _, _), 	dom(operator_priority,1201),	[]).
test_case(currop: 3, sics, 	current_op(_, yfy, _), 		dom(operator_specifier,yfy),	[]).
test_case(currop: 4, sics, 	current_op(_, 0, _), 		type(atom,0),                   []).
test_case(currop: 5, sics, 	current_op(_, _, 5), 		type(atom,5),			[]).

% 8.14.5.4

test_case(charcnv: 1, iso_doc,  char_conversion('&', ','),	[[(read((a,b)), get_char(' '),
						    get_char('&'))]],		[txtin('a&b. &'),clear_chconv]).
%%%% pts %%%% vvv change '\'' to '''' for XSB
test_case(charcnv: 2, iso_doc,  char_conversion('^', ''''),	[[(read('b+c'), get_char(' '),
						   get_char('^'))]],		[txtin('^b+c^. ^'),clear_chconv]).
test_case(charcnv: 3, iso_doc,  char_conversion('A', 'a'),	[[read('A+c'+a)]],		[txtin("'A+c'+A."),clear_chconv]).
test_case(charcnv: 4, iso_doc,  (read(X), read(Y), read(Z)),	[[ X <-- (a,a), Y <-- 'AAA',
						   Z <-- 'a,a' ]],		[txtin("A&A. 'AAA'. ^A&A^."),
					 pre((char_conversion('&', ','),
					     char_conversion('^', ''''),
					     char_conversion('A', 'a'))),
					 clear_chconv]).
test_case(charcnv: 5, iso_doc, 	char_conversion('&', '&'),	[[(read('&'),
						  \+ current_char_conversion(
			_,_))]],	[txtin("& ."),
					 pre(char_conversion('&', ','))]).

test_case(charcnv: 6, sics,  read(X),			[[ X <-- (0'%)+1 ]],		[txtin("0'%%1."),
					 some_chconvs]).
test_case(charcnv: 7, sics,  read(X),			[[ X <-- '%'+1 ]],		[txtin("'%'%1."),
					 some_chconvs]).
test_case(charcnv: 8, sics,  read(X),			[[ X <-- "%"+1 ]],		[txtin('"%"%1.'),
					 some_chconvs]).
test_case(charcnv: 9, sics,  read(X),			[[ X <-- '.'(1,!) ]],		[txtin('1.#.'),
					 pre(op(100,xfx,'.')),
					 post(op(0,xfx,'.')),
					 pre(char_conversion('#','!')),
					 clear_chconv]).
test_case(charcnv:10, sics,  read(X),			[[ X <--  'aa'+'bb^']],		[txtin("^aa'+'bb^'."),
					 some_chconvs]).
% !! SWI-Prolog
%test_case(charcnv:11, sics,  (set_prolog_flag(
%		     char_conversion, off),
%		 read(X), set_prolog_flag(
%		     char_conversion, on),
%		 read(Y)),			[[ X <--  (+), Y <-- (+)]],	[txtin("+ .% ."),
%					 some_chconvs]).
%test_case(charcnv:12, sics,  (read(X), read(Y)),		[[ X <--  '-'('.+'),
%						   Y <-- end_of_file]],		[txtin("- .% .",{past}),
%					 some_chconvs]).

% 8.14.6.4
test_case(crchcnv: 1, iso_doc,  current_char_conversion(C, a),	{[ C <-- 'A'],
						 [ C <-- Aacute]},		[txtin("'\\341\\'."),
					 pre((read(Aacute),
						 				  char_conversion('A', 'a'),
					  char_conversion(Aacute, 'a'))),
					 clear_chconv]).

% 8.15.1.4
test_case(not: 1, iso_doc,  	'\\+'(true),                    failure,                        []).
test_case(not: 2, iso_doc,  	'\\+'(!),                       failure,                        []).
test_case(not: 3, iso_doc,  	'\\+'((!,fail)),                success,                        []).
test_case(not: 4, iso_doc,  	((X=1;X=2),'\\+'((!,fail))),    [[X <-- 1],
            	                                 [X <-- 2]],                    []).
test_case(not: 5, iso_doc,  	'\\+'(4 = 5),                   success,                        []).
test_case(not: 6, iso_doc,  	'\\+'(3),                       type(callable,3),               []).
test_case(not: 7, iso_doc,  	'\\+'(_X),                      inst,                           []).
test_case(not: 8, iso_doc,  	'\\+'(X=f(X)),                  sto(failure),                   []).

% 8.15.2.4
test_case(once: 1, iso_doc,  	once(!),                        success,                        []).
test_case(once: 2, iso_doc,  	(once(!), (X=1; X=2)),          [[X <-- 1],[X <-- 2]],          []).
test_case(once: 3, iso_doc,  	once(repeat),                   success,                        []).
test_case(once: 4, iso_doc,  	once(fail),                     failure,                        []).
test_case(once: 5, iso_doc,  	once((X = f(X))),               sto([[X <-- V@f(V)]]),          []).

test_case(once: 6, eddbali,  	once(3),                        type(callable,3/*once(3)*/),    []).
test_case(once: 7, eddbali,  	once(_X),                       inst,                           []).


% 8.15.3.4
test_case(repeat: 1, iso_doc,  (repeat, write('hello '), fail), time_out,			[txtout(rx('hello hello .*'))]).
test_case(repeat: 2, iso_doc,  (repeat, !, fail),               failure,                	[]).

% 8.16.1.4
test_case(atomlen: 1, iso_doc,  atom_length(
		      'enchanted evening', N), 	[[N <-- 17]],           	[]).
test_case(atomlen: 2, iso_doc,  atom_length(
		      'enchanted\
 evening', N), 					[[N <-- 17]],           	[]).
test_case(atomlen: 3, iso_doc,  atom_length('', N),         	[[N <-- 0]],            	[]).
test_case(atomlen: 4, iso_doc,  atom_length('scarlet', 5),  	failure,                	[]).
test_case(atomlen: 5, iso_doc,  atom_length(_Atom, 4),      	inst,                   	[]).
test_case(atomlen: 6, iso_doc,  atom_length(1.23, 4),       	type(atom, 1.23),       	[]).
test_case(atomlen: 7, iso_doc,  atom_length(atom, '4'),     	type(integer, '4'),		[]).

test_case(atomlen: 8, eddbali,  atom_length(atom, -4),      	dom(not_less_than_zero,-4),	[]).
test_case(atomlen: 9, sics,  atom_length('Bartók Béla', L),  [[L <-- 11]],                   []).
test_case(atomlen:10, sics,  atom_length(_Atom, N),          inst,                   	[]) :-
	context_info(max_atom_length, Max), N is Max+1.
test_case(atomlen:11, sics,  atom_length(atom, N),           failure,                   	[]) :-
	context_info(max_atom_length, Max), N is Max+1.

% 8.16.2.4
test_case(atomcat: 1, iso_doc,  atom_concat('hello',
			    ' world', S3), 	[[S3 <-- 'hello world']],	[]).
test_case(atomcat: 2, iso_doc,  atom_concat(T, ' world',
			    'small world'), 	[[T <-- 'small']],      	[]).
test_case(atomcat: 3, iso_doc,  atom_concat('hello',' world',
			    'small world'), 	failure,                	[]).
test_case(atomcat: 4, iso_doc,  atom_concat(T1, T2, 'hello'),	[[T1 <-- '',T2 <-- 'hello'],
                                                 [T1 <-- 'h',T2 <-- 'ello'],
                                                 [T1 <-- 'he',T2 <-- 'llo'],
                                                 [T1 <-- 'hel',T2 <-- 'lo'],
                                                 [T1 <-- 'hell',T2 <-- 'o'],
                                                 [T1 <-- 'hello',T2 <-- '']], 	[]).
test_case(atomcat: 5, iso_doc,  atom_concat(small, _V2, _V4), 	inst,                 		[]).

test_case(atomcat: 6, eddbali,  atom_concat(_A, 'iso', _C), 	inst,                   	[]).
test_case(atomcat: 7, eddbali,  atom_concat('iso', _B, _C), 	inst,                   	[]).
test_case(atomcat: 8, eddbali,  atom_concat(f(a), 'iso', _C),	type(atom,f(a)),       		[]).
test_case(atomcat: 9, eddbali,  atom_concat('iso', f(a), _C),	type(atom,f(a)),       		[]).
test_case(atomcat:10, eddbali,  atom_concat(_A, _B, f(a)),  	type(atom,f(a)),        	[]).
test_case(atomcat:11, sics,  atom_concat('Bartók ', 'Béla', N), [[N <-- 'Bartók Béla']],     []).
test_case(atomcat:12, sics,  atom_concat(N, 'Béla', 'Bartók Béla'), [[N <-- 'Bartók ']],     []).
test_case(atomcat:13, sics,  atom_concat('Bartók ', N, 'Bartók Béla'), [[N <-- 'Béla']],     []).
test_case(atomcat:14, sics,  atom_concat(T1, T2, 'Pécs'),    [[T1 <-- '',T2 <-- 'Pécs'],
						 [T1 <-- 'P',T2 <-- 'écs'],
						 [T1 <-- 'Pé',T2 <-- 'cs'],
						 [T1 <-- 'Péc',T2 <-- 's'],
						 [T1 <-- 'Pécs',T2 <-- '']],    []).
% non-standard test
test_case(atomcat:15, sics,  atom_concat(LA,LA,_),		repr(exceeded_atom_size),       [long_atom(LA)]) :-
	context_info(max_atom_length, _).

% 8.16.3.4
test_case(subatom: 1, iso_doc,  sub_atom(abracadabra,0,5,X,S2), [X^[S2 <-- 'abrac']],   	[]).
test_case(subatom: 2, iso_doc,  sub_atom(abracadabra,X,5,0,S2), [X^[S2 <-- 'dabra']],   	[]).
test_case(subatom: 3, iso_doc,  sub_atom(abracadabra,3,
			 Length,3,S2), 		[[Length <-- 5,
                                                  S2 <-- 'acada']],     	[]).
test_case(subatom: 4, iso_doc,  sub_atom(abracadabra,
			 Before, 2, After, ab), [[Before <-- 0, After <-- 9],
						 [Before <-- 7, After <-- 2]], 	[]).
test_case(subatom: 5, iso_doc,  sub_atom('Banana',3,2,X,S2), 	[X^[S2 <-- 'an']],      	[]).
test_case(subatom: 6, iso_doc,  sub_atom('charity',B,3,A,S2), 	[A^B^[S2 <-- 'cha'],
                                                 A^B^[S2 <-- 'har'],
                                                 A^B^[S2 <-- 'ari'],
                                                 A^B^[S2 <-- 'rit'],
                                                 A^B^[S2 <-- 'ity']],   	[]).
test_case(subatom: 7, iso_doc,  sub_atom('ab', St, Len, X, A), 	[X^[St<--0, Len<--0, A<--''],
						 X^[St<--0, Len<--1, A<--'a'],
						 X^[St<--0, Len<--2, A<--'ab'],
						 X^[St<--1, Len<--0, A<--''],
						 X^[St<--1, Len<--1, A<--'b'],
						 X^[St<--2, Len<--0, A<--'']],	[]).

test_case(subatom: 8, eddbali,  sub_atom(_Banana, 3, 2, _, _S),	inst,                  		[]).
test_case(subatom: 9, eddbali,  sub_atom(f(a), 2, 2, _, _S2),  	type(atom,f(a)),        	[]).
test_case(subatom:10, eddbali,  sub_atom('Banana', 4, 2, _, 2),	type(atom,2),   		[]).
test_case(subatom:11, eddbali,  sub_atom('Banana', a, 2, _, _),	type(integer,a),		[]).
test_case(subatom:12, eddbali,  sub_atom('Banana', 4, n, _, _),	type(integer,n),		[]).
test_case(subatom:13, eddbali,  sub_atom('Banana', 4, _, m, _),	type(integer,m),		[]).
test_case(subatom:14, sics,  sub_atom('Banana', -2, 3, 4,_), dom(not_less_than_zero, -2),    []).
test_case(subatom:15, sics,  sub_atom('Banana', 2, -3, 4,_), dom(not_less_than_zero, -3),    []).
test_case(subatom:16, sics,  sub_atom('Banana', 2, 3, -4,_), dom(not_less_than_zero, -4),    []).
test_case(subatom:17, sics,  sub_atom('Banana', 2, 3, A, 'nan'), [[A<--1]],                  []).
test_case(subatom:18, sics,  sub_atom('Banana', B, 3, 1, 'nan'), [[B<--2]],                  []).
test_case(subatom:19, sics,  sub_atom('Banana', 2, L, 1, 'nan'), [[L<--3]],                  []).
test_case(subatom:20, sics,  sub_atom('Banana', 2, L, A, 'nan'), [[A<--1,L<--3]],            []).
test_case(subatom:21, sics,  sub_atom('Banana', B, L, 1, 'nan'), [[B<--2,L<--3]],            []).
test_case(subatom:22, sics,  sub_atom('Banana', 2, 3, 1, 'ana'), failure,                    []).
test_case(subatom:23, sics,  sub_atom('Banana', 2, 3, 2, 'nan'), failure,                    []).
test_case(subatom:24, sics,  sub_atom('Banana', 2, 3, 2, _), failure,                        []).
test_case(subatom:25, sics,  sub_atom('Banana', 2, 3, 1, 'anan'), failure,                   []).
test_case(subatom:26, sics,  sub_atom('Banana', 0, 7, 0, _), failure,                        []).
test_case(subatom:27, sics,  sub_atom('Banana', 7, 0, 0, _), failure,                        []).
test_case(subatom:28, sics,  sub_atom('Banana', 0, 0, 7, _), failure,                        []).
test_case(subatom:29, sics,  sub_atom(a, N, 1, 2, 1+2), type(atom,1+2),              []) :-
	context_info(max_atom_length, Max), N is Max+1.
test_case(subatom:30, sics,  sub_atom(a, N, 1, 2, 1+2),  type(atom,1+2),              []) :-
	context_info(max_atom_length, Max), N is Max+1.
test_case(subatom:31, sics,  sub_atom('Bartók Béla', 4, 2, A, S), [[A <-- 5,S <-- 'ók']],    []).
test_case(subatom:32, sics,  sub_atom('Bartók Béla', 4, L, 5, S), [[L <-- 2,S <-- 'ók']],    []).
test_case(subatom:33, sics,  sub_atom('Bartók Béla', B, 2, 5, S), [[B <-- 4,S <-- 'ók']],    []).
test_case(subatom:34, sics,  sub_atom('Pécs',B,2,A,S),       [[B <-- 0,A <-- 2,S <-- 'Pé'],
						 [B <-- 1,A <-- 1,S <-- 'éc'],
						 [B <-- 2,A <-- 0,S <-- 'cs']], []).
test_case(subatom:35, sics,  sub_atom(abracadabra,B,L,A,abra), [[B <-- 0,L <-- 4,A <-- 7],
						   [B <-- 7,L <-- 4,A <-- 0]],  []).
test_case(subatom:36, sics,  sub_atom(a, N, 1, 2, _), failure,			[]) :-
	context_info(max_atom_length, Max), N is Max+1.
test_case(subatom:37, sics,  sub_atom(a, 1, N, 2, _), failure,			[]) :-
	context_info(max_atom_length, Max), N is Max+1.
test_case(subatom:38, sics,  sub_atom(a, 1, 2, N, _), failure,			[]) :-
	context_info(max_atom_length, Max), N is Max+1.


% 8.16.4.4
test_case(atomchs: 1, iso_doc,  atom_chars('', L),           	[[L <-- []]],           	[]).
test_case(atomchs: 2, iso_doc,  atom_chars([], L),           	[[L <-- ['[',']']]],    	[]).
test_case(atomchs: 3, iso_doc,  atom_chars('''', L),         	[[L <-- ['''']]],       	[]).
test_case(atomchs: 4, iso_doc,  atom_chars('ant', L),        	[[L <-- ['a','n','t']]],	[]).
test_case(atomchs: 5, iso_doc,  atom_chars(Str, ['s','o','p']), [[Str <-- 'sop']],    		[]).
test_case(atomchs: 6, iso_doc,  atom_chars('North', ['N'|X]),	[[X <-- ['o','r','t','h']]],	[]).
test_case(atomchs: 7, iso_doc,  atom_chars('soap',
			   ['s','o','p']),	failure,           		[]).
test_case(atomchs: 8, iso_doc,  atom_chars(_X, _Y),          	inst,                   	[]).
test_case(atomchs: 9, eddbali,  atom_chars(_A, [a,_E,c]),    	inst,                   	[]).
test_case(atomchs:10, eddbali,  atom_chars(_A, [a,b|_L]),    	inst,                   	[]).
test_case(atomchs:11, eddbali,  atom_chars(f(a), _L),        	type(atom,f(a)),        	[]).
test_case(atomchs:12, eddbali,  atom_chars(_A, iso),         	type(list,iso), 		[]).
test_case(atomchs:13, eddbali,  atom_chars(_A, [a,f(b)]),    	type(character,f(b)), 		[]).
test_case(atomchs:14, sics,  atom_chars('Pécs', L),          [[L <-- ['P','é','c','s']]],    []).
test_case(atomchs:15, sics,  atom_chars(A,
			   ['P','é','c','s']),  [[A <-- 'Pécs']],               []).

% 8.16.5.4
test_case(atomcds: 1, iso_doc,  atom_codes('', L),           	[[L <-- []]],           	[]).
test_case(atomcds: 2, iso_doc,  atom_codes([], L),           	[[L <-- [0'[, 0']]]],   	[]).

% vvv pts: swi-prolog !!
%stc(atomcds: 3, atom_codes('''',L),          	[[L <-- [0'\' ]]],      	[]).
test_case(atomcds: 3, iso_doc,  atom_codes('''',L),          	[[L <-- [39 ]]],      	[]).

test_case(atomcds: 4, iso_doc,  atom_codes('ant', L),        	[[L <-- [0'a, 0'n, 0't]]], 	[]).
test_case(atomcds: 5, iso_doc,  atom_codes(Str,
			   [0's,0'o,0'p]),   	[[Str <-- 'sop']],      	[]).
test_case(atomcds: 6, iso_doc,  atom_codes('North',[0'N|X]), 	[[X <-- [0'o,0'r,0't,0'h]]],	[]).
test_case(atomcds: 7, iso_doc,  atom_codes('soap',
			   [0's, 0'o, 0'p]), 	failure,       			[]).
test_case(atomcds: 8, iso_doc,  atom_codes(_X, _Y),          	inst,                   	[]).
test_case(atomcds: 9, eddbali,  atom_codes(f(a), _L),        	type(atom,f(a)),        	[]).
test_case(atomcds:10, eddbali,  atom_codes(_, 0'x),          	type(list,0'x), 		[]).
test_case(atomcds:11, eddbali,  atom_codes(_A, [0'i,0's,-1]),	repr(character_code),      	[]).
test_case(atomcds:12, sics,  atom_codes('Pécs', C),          [[C <-- [0'P,0'é,0'c,0's]]],    []).
test_case(atomcds:13, sics,  atom_codes(A,
			   [0'P,0'é,0'c,0's]),  [[A <-- 'Pécs']],               []).
test_case(atomcds:14, sics,  atom_codes(_A,
			   [N]),                repr(character_code),           []) :-
	context_info(max_char_code, Max), N is Max+1.
%atc(atomcds:15, atom_codes(A,
%	         [0'\x10000000\,0'a,0'b]), 	[[A <-- '\x10000000\ab']],      []).

% vvv pts: swi-prolog !!
%atc(atomcds:15, atom_codes(A,
%	         [0'\xFF\,0'a,0'b]), 	        [[A <-- '\xFF\ab']],            []).

test_case(atomcds:16, sics,  atom_codes(_A, [a,b,c]),	(repr(character_code)
						;type(integer,@@)),          	[]).

% 8.16.6.4
test_case(charcde: 1, iso_doc,  char_code(a, Code),             [[Code <-- 0'a]],       	[]).
test_case(charcde: 2, iso_doc,  (char_code(Char, 99),
                   atom_codes(Atom, [99])),     [Ch^[Char <-- Ch,
                                                      Atom <-- Ch]],    	[]).
test_case(charcde: 3, iso_doc,  char_code(Char, 0'c),           [[Char <-- c]],         	[]).
test_case(charcde: 4, iso_doc,  char_code(Char, 163),           [[read(Char)]],    		[txtin("'\\243\\'.")]).
test_case(charcde: 5, iso_doc,  char_code(b, 0'b),              success,                	[]).
test_case(charcde: 6, iso_doc,  char_code('ab', _Code),         type(character, 'ab'),  	[]).
test_case(charcde: 7, iso_doc,  char_code(_Char, _Code),        inst,                   	[]).
test_case(charcde: 8, eddbali,  char_code(a, x),                type(integer, x),       	[]).
test_case(charcde: 9, eddbali,  char_code(_Char, -2),           repr(character_code),    	[]).
%atc(charcde:10, char_code(Char, 999),           [[read(Char)]],                 [txtin("'\\1747\\'.")]).       
%atc(charcde:11, char_code(Char, 0o177777),      [[read(Char)]],   		[txtin("'\\177777\\'.")]).     
%atc(charcde:12, char_code(Char, 0o7777777),     [[read(Char)]],   		[txtin("'\\7777777\\'.")]).    
%atc(charcde:13, char_code(Char, 0o377777777),   [[read(Char)]],   		[txtin("'\\377777777\\'.")]).  
%atc(charcde:14, char_code(Char, 0o17777777777), [[read(Char)]],   		[txtin("'\\17777777777\\'.")]).
test_case(charcde:15, sics,  (char_code(Char, 225),
		    atom_chars(Char, [Y])),     [[Char <-- á, Y <-- '\341\']],  []). 

% 8.16.7.4
test_case(numbchs: 1, iso_doc,  number_chars(33, L),       	[[L <-- ['3','3']]],    	[]).
test_case(numbchs: 2, iso_doc,  number_chars(33, ['3','3']),	success,	               	[]).
test_case(numbchs: 3, iso_doc,  number_chars(33.0, L),		[[number_chars(N, L), N=33.0]], []).
test_case(numbchs: 4, iso_doc,  number_chars(X,
		 ['3','.','3','E','+','0']),	[[X <-- 3.3]],          	[]).
test_case(numbchs: 5, iso_doc,  number_chars(3.3,  ['3'|_L]),	success,                	[]).
test_case(numbchs: 6, iso_doc,  number_chars(A, ['-','2','5']), [[A <-- -25]],          	[]).
test_case(numbchs: 7, iso_doc,  number_chars(A,['\n',' ','3']), [[A <-- 3]],            	[]).
test_case(numbchs: 8, iso_doc,  number_chars(_A, ['3',' ']),	syntax,        			[]).
test_case(numbchs: 9, iso_doc,  number_chars(A, ['0',x,f]),	[[A <-- 15]],           	[]).
test_case(numbchs:10, iso_doc,  number_chars(A, ['0','''',a]),  [[A <-- 0'a]],          	[]).
test_case(numbchs:11, iso_doc,  number_chars(A, ['4','.','2']), [[A <-- 4.2]],          	[]).
test_case(numbchs:12, iso_doc,  number_chars(A,
		['4','2','.','0','e','-','1']), [[A <-- 4.2]],          	[]).

test_case(numbchs:13, eddbali,  number_chars(_A, _L),      	inst,                        	[]).
test_case(numbchs:14, eddbali,  number_chars(a, _L),       	type(number, a),             	[]).
test_case(numbchs:15, eddbali,  number_chars(_A, 4),       	type(list, 4),       		[]).
test_case(numbchs:16, eddbali,  number_chars(_A, ['4',2]), 	type(character, 2),  		[]).

test_case(numbchs:17, sics,  number_chars(_A, [a|_L]),  	inst,                        	[]).
test_case(numbchs:18, sics,  number_chars(_A, [a,_L]),  	inst,                        	[]).

test_case(numbchs:19, sics,  number_chars(X, [' ', '0', 'o', '1', '1']), [[X <-- 9]],        []).
test_case(numbchs:20, sics,  number_chars(X, [' ', '0', 'x', '1', '1']), [[X <-- 17]],       []).
test_case(numbchs:21, sics,  number_chars(X, [' ', '0', 'b', '1', '1']), [[X <-- 3]],        []).
test_case(numbchs:22, sics,  number_chars(_X, ['0', 'o', '8']), syntax,                      []).
test_case(numbchs:23, sics,  number_chars(_X, ['0', 'b', '2']), syntax,                      []).
test_case(numbchs:24, sics,  number_chars(_X, ['0', 'x', 'g']), syntax,                      []).
test_case(numbchs:25, sics,  number_chars(_X, ['á']),        syntax,                         []).
test_case(numbchs:26, sics,  number_chars(_X, ['a']),        syntax,                         []).
test_case(numbchs:27, sics,  number_chars(_X,
		    ['0','x','0','.','0']),     syntax,                         []).

% 8.16.8.4
test_case(numbcds: 1, iso_doc,  number_codes(33, L),       	[[L <-- [0'3,0'3]]],    	[]).
test_case(numbcds: 2, iso_doc,  number_codes(33, [0'3,0'3]),	success,               		[]).
test_case(numbcds: 3, iso_doc,  number_codes(33.0, L),		[[number_codes(N, L), N=33.0]], []).
test_case(numbcds: 4, iso_doc,  number_codes(33.0, [0'3|_L]),	 success,                	[]).
test_case(numbcds: 5, iso_doc,  number_codes(A, [0'-,0'2,0'5]), [[A <-- -25]],          	[]).
test_case(numbcds: 6, iso_doc,  number_codes(A, [0' , 0'3]),    [[A <-- 3]],            	[]).
test_case(numbcds: 7, iso_doc,  number_codes(A, [0'0,0'x,0'f]), [[A <-- 15]],           	[]).

% vvv pts: swi-prolog !!
%stc(numbcds: 8, number_codes(A,[0'0,0'\',0'a]), [[A <-- 0'a]],          	[]).
test_case(numbcds: 8, iso_doc,  number_codes(A,[0'0,39,0'a]), [[A <-- 0'a]],          	[]).

test_case(numbcds: 9, iso_doc,  number_codes(A, [0'4,0'.,0'2]), [[A <-- 4.2]],          	[]).
test_case(numbcds:10, iso_doc,  number_codes(A, [
		 0'4,0'2,0'.,0'0,0'e,0'-,0'1]), [[A <-- 4.2]],          	[]).

test_case(numbcds:11, eddbali,  number_codes(_A, _L),      	inst,                   	[]).
test_case(numbcds:12, eddbali,  number_codes(a, _L),       	type(number,a), 		[]).
test_case(numbcds:13, eddbali,  number_codes(_A, 4),       	type(list,4),   		[]).
test_case(numbcds:14, eddbali,  number_codes(_A, [0'4,-1]),	repr(character_code),      	[]).

test_case(numbcds:15, sics,  number_codes(_A, [0'a|_L]),	inst,                   	[]).
test_case(numbcds:16, sics,  number_codes(_A, [0'a,_L]),	inst,                   	[]).

test_case(numbcds:17, sics,  (number_chars(X, [' ', '0', 'x', '1', '1', '1']),
		    number_codes(X, Y)), [[X <-- 273, Y <-- [50,55,51]]],       []).
test_case(numbcds:18, sics,  (number_chars(X, [' ', '0', 'o', '1', '1', '1']),
		    number_codes(X, Y)), [[X <-- 73, Y <-- [55,51]]],           []).
test_case(numbcds:19, sics,  (number_chars(X, [' ', '0', 'b', '1', '1', '1']),
		    number_codes(X, Y)), [[X <-- 7, Y <-- [55]]],               []).

% vvv pts  !! SWI-Prolog?!
%atc(numbcds:20, number_codes(N, "0'\\n"),       [[N <-- 10]],                   []).

test_case(numbcds:21, sics,  number_codes(_N, "ä"),          syntax,                         []).
test_case(numbcds:22, sics,  number_codes(_X,
		    [0'0,0'x,0'0,0'.,0'0]),     syntax,                         []).


% 8.17.1.4
test_case(setpflg: 1, iso_doc,  set_prolog_flag(unknown, fail), [[current_prolog_flag(unknown,
			      fail)]], 	[]).
test_case(setpflg: 2, iso_doc,  set_prolog_flag(_X, off),	inst,                  		[]).
test_case(setpflg: 3, iso_doc,  set_prolog_flag(5, decimals), 	type(atom,5),   		[]).
test_case(setpflg: 4, iso_doc,  set_prolog_flag(date,
				'July 1999'),   dom(prolog_flag,date),  	[]).
test_case(setpflg: 5, iso_doc,  set_prolog_flag(debug, trace),  dom(flag_value,debug+trace),    []).
test_case(setpflg: 6, eddbali,  set_prolog_flag(max_arity, 40), perm(modify,flag,max_arity),	[]).

% 8.17.2.4
test_case(curpflg: 1, iso_doc,  current_prolog_flag(debug,off), success,                	[]).
test_case(curpflg: 2, iso_doc,  current_prolog_flag(F, V), 	{[F<--debug, V<--off],
						 [F<--char_conversion, V<--on],
						 [F<--unknown, V<--error],...}, []).
test_case(curpflg: 3, iso_doc,  current_prolog_flag(5, _V), 	type(atom,5),   		[]).

test_case(curpflg: 4, eddbali,  set_prolog_flag(unknown,
				warning), 	[[current_prolog_flag(unknown,
			    warning)]], []).
test_case(curpflg: 5, eddbali,  (set_prolog_flag(unknown,
				warning),
		 current_prolog_flag(
		           unknown,error)),	failure,                	[]).
test_case(curpflg: 6, eddbali,  current_prolog_flag(debug, V),  [[V <-- off]],          	[]).
test_case(curpflg: 7, eddbali,  current_prolog_flag(warning,_), dom(prolog_flag,warning), 	[]).
test_case(curpflg: 8, eddbali,  current_prolog_flag(1+2, flag), type(atom,1+2), 		[]).

% missing: testing the effect of flag 'unknown'


% 8.17.3.4
% stc(halt: 1, 	halt,                           implementation_defined, 	[]).

% 8.17.4.4
% stc(halt1: 1, halt(1),                        implementation_defined, 	[]).
% !! SWI-Prolog
% test_case(halt1: 2, iso_doc,  	halt(a),                        type(integer,a),        	[]).
% test_case(halt1: 3, eddbali,  	halt(_),                        inst,                   	[]).


% 9.1.7

test_case(eval: 1, iso_doc, 	X is '+'(7, 35), 		[[ X <-- 42 ]],			[]).
test_case(eval: 2, iso_doc, 	X is '+'(0, 3+11), 		[[ X <-- 14 ]],			[]).
test_case(eval: 3, iso_doc, 	X is '+'(0, 3.2+11), 		[[ X <-- 14.2 ]],		[]).
test_case(eval: 4, iso_doc, 	_X is '+'(77, _N), 		inst,				[]).
%stc(eval: 5,	_X is '+'(foo, 77), 		type(number,foo),		[]).
test_case(eval: 5, iso_doc, 	_X is '+'(foo, 77), 		type(evaluable,foo/0),		[]).
test_case(eval: 6, iso_doc, 	X is '-'(7), 			[[ X <-- -7 ]],			[]).
test_case(eval: 7, iso_doc, 	X is '-'(3-11), 		[[ X <-- 8 ]],			[]).
test_case(eval: 8, iso_doc, 	X is '-'(3.2-11), 		[[ X <-- 7.8 ]],		[]).
test_case(eval: 9, iso_doc, 	_X is '-'(_N),	 		inst,				[]).
%stc(eval:10,	_X is '-'(foo), 		type(number,foo),		[]).
test_case(eval:10, iso_doc, 	_X is '-'(foo), 		type(evaluable,foo/0),		[]).
test_case(eval:11, iso_doc, 	X is '-'(7,35), 		[[ X <-- -28 ]],		[]).
test_case(eval:12, iso_doc, 	X is '-'(20,3+11), 		[[ X <-- 6 ]],			[]).
test_case(eval:13, iso_doc, 	X is '-'(0,3.2+11), 		[[ X <-- -14.2 ]],		[]).
test_case(eval:14, iso_doc, 	_X is '-'(77, _N), 		inst,				[]).
%stc(eval:15,	_X is '-'(foo, 77), 		type(number,foo),		[]).
test_case(eval:15, iso_doc, 	_X is '-'(foo, 77), 		type(evaluable,foo/0),		[]).
test_case(eval:16, iso_doc, 	X is '*'(7,35), 		[[ X <-- 245 ]],		[]).
test_case(eval:17, iso_doc, 	X is '*'(0,3+11), 		[[ X <-- 0 ]],			[]).
test_case(eval:18, iso_doc, 	X is '*'(1.5,3.2+11), 		[[ X =~~ 21.3 ]],		[]).

test_case(eval:19, iso_doc, 	_X is '*'(77, _N), 		inst,				[]).
%stc(eval:20,	_X is '*'(foo, 77), 		type(number,foo),		[]).
test_case(eval:20, iso_doc, 	_X is '*'(foo, 77), 		type(evaluable,foo/0),		[]).
%stc(eval:21,	X is '/'(7,35), 		[[ X <-- 0 ]],			[]).
test_case(eval:21, iso_doc, 	X is '//'(7,35), 		[[ X <-- 0 ]],			[]).
test_case(eval:22, iso_doc, 	X is '/'(7.0,35), 		[[ X <-- 0.2 ]],		[]).
%stc(eval:23,	X is '/'(140,3+11), 		[[ X <-- 10 ]],			[]).
test_case(eval:23, iso_doc, 	X is '//'(140,3+11), 		[[ X <-- 10 ]],			[]).
%stc(eval:24,	X is '/'(20.164,3.2+11),	[[ X =~~ 14.2 ]],		[]).
test_case(eval:24, iso_doc, 	X is '/'(20.164,3.2+11),	[[ X =~~ 1.42 ]],		[]).
%stc(eval:25,	_X is '/'(7,-3), 		undefined,			[]).
test_case(eval:25, iso_doc, 	X is '//'(7,-3), 		[[X <-- (-2;-3) ]],		[]).
%stc(eval:26,	_X is '/'(-7,3), 		undefined,			[]).
test_case(eval:26, iso_doc, 	X is '//'(-7,3), 		[[X <-- (-2;-3) ]],		[]).
test_case(eval:27, iso_doc, 	_X is '/'(77, _N), 		inst,				[]).
%stc(eval:28,	_X is '/'(foo, 77), 		type(number,foo),		[]).
test_case(eval:28, iso_doc, 	_X is '/'(foo, 77), 		type(evaluable,foo/0),		[]).
test_case(eval:29, iso_doc, 	_X is '/'(3, 0), 		eval(zero_divisor),		[]).
test_case(eval:30, iso_doc, 	X is mod(7,3),	 		[[ X <-- 1 ]],			[]).
test_case(eval:31, iso_doc, 	X is mod(0,3+11), 		[[ X <-- 0 ]],			[]).
test_case(eval:32, iso_doc, 	X is mod(7,-2),	 		[[ X <-- -1 ]],			[]).
test_case(eval:33, iso_doc, 	_X is mod(77, _N), 		inst,				[]).
%stc(eval:34,	_X is mod(foo, 77), 		type(number,foo),		[]).
test_case(eval:34, iso_doc, 	_X is mod(foo, 77), 		type(evaluable,foo/0),		[]).
test_case(eval:35, iso_doc, 	_X is mod(7.5, 2), 		type(integer,7.5),		[]).
test_case(eval:36, iso_doc, 	_X is mod(7, 0), 		eval(zero_divisor),		[]).
test_case(eval:37, iso_doc, 	X is floor(7.4), 		[[ X <-- 7 ]],			[]).
test_case(eval:38, iso_doc, 	X is floor(-0.4), 		[[ X <-- -1 ]],			[]).
test_case(eval:39, iso_doc, 	X is round(7.5), 		[[ X <-- 8 ]],			[]).
test_case(eval:40, iso_doc, 	X is round(7.6), 		[[ X <-- 8 ]],			[]).
test_case(eval:41, iso_doc, 	X is round(-0.6), 		[[ X <-- -1 ]],			[]).

test_case(eval:42, iso_doc, 	_X is round(_N), 		inst,				[]).
test_case(eval:43, iso_doc, 	X is ceiling(-0.5), 		[[ X <-- 0 ]],			[]).
test_case(eval:44, iso_doc, 	X is truncate(-0.5), 		[[ X <-- 0 ]],			[]).
%stc(eval:45,	_X is truncate(foo), 		type(number,foo),		[]).
test_case(eval:45, iso_doc, 	_X is truncate(foo), 		type(evaluable,foo/0),		[]).
test_case(eval:46, iso_doc, 	X is float(7), 			[[ X <-- 7.0 ]],		[]).
test_case(eval:47, iso_doc, 	X is float(7.3), 		[[ X <-- 7.3 ]],		[]).
%stc(eval:48,	X is float(5/3), 		[[ X <-- 1.0 ]],		[]).
test_case(eval:48, iso_doc, 	X is float(5//3), 		[[ X <-- 1.0 ]],		[]).
test_case(eval:49, iso_doc, 	_X is float(_N), 		inst,				[]).
%stc(eval:50,	_X is float(foo), 		type(number,foo),		[]).
test_case(eval:50, iso_doc, 	_X is float(foo), 		type(evaluable,foo/0),		[]).
test_case(eval:51, iso_doc, 	X is abs(7), 			[[ X <-- 7 ]],			[]).
test_case(eval:52, iso_doc, 	X is abs(3-11), 		[[ X <-- 8 ]],			[]).
test_case(eval:53, iso_doc, 	X is abs(3.2-11.0), 		[[ X <-- 7.8 ]],		[]).
test_case(eval:54, iso_doc, 	_X is abs(_N), 			inst,				[]).
%stc(eval:55,	_X is abs(foo), 		type(number,foo),		[]).
test_case(eval:55, iso_doc, 	_X is abs(foo), 		type(evaluable,foo/0),		[]).
test_case(eval:56, iso_doc, 	(current_prolog_flag(
	  	  max_integer, MI),
	  	 _X is '+'(MI,1)),		eval(int_overflow),		[]).
test_case(eval:57, iso_doc, 	(current_prolog_flag(
	  	  max_integer, MI),
	  	 _X is '-'('+'(MI,1),1)),	eval(int_overflow),		[]).
test_case(eval:58, iso_doc, 	(current_prolog_flag(
	  	  max_integer, MI),
	  	 _X is '-'(/*-1*/-2,MI)),	eval(int_overflow),		[]).
		 % ISO allows min_integer = -(max_integer + 1)
test_case(eval:59, iso_doc, 	(current_prolog_flag(
	  	  max_integer, MI),
	  	 _X is '*'(MI,2)),		eval(int_overflow),		[]).
test_case(eval:60, iso_doc, 	(current_prolog_flag(
	  	  max_integer, MI),
	  	 R is float(MI)*2,
	  	 _X is floor(R)),		eval(int_overflow),		[]).

% 9.3.1.4
test_case(pow: 1, iso_doc, 	X is '**'(5,3),			[[ X =~~ 125.0 ]], 		[]).
test_case(pow: 2, iso_doc, 	X is '**'(-5.0,3),		[[ X =~~ -125.0 ]], 		[]).
test_case(pow: 3, iso_doc, 	X is '**'(5,-1),		[[ X =~~ 0.2 ]], 		[]).
test_case(pow: 4, iso_doc, 	_X is '**'(77,_N),		inst,		 		[]).
test_case(pow: 5, iso_doc,  	_X is '**'(foo,2),		type(evaluable,foo/0),		[]).
test_case(pow: 6, iso_doc,  	X is '**'(5,3.0),		[[ X =~~ 125.0 ]], 		[]).
test_case(pow: 7, iso_doc,  	X is '**'(0,0.0),		[[ X =~~ 1.0 ]], 		[]).

% 9.3.2.4
test_case(sin: 1, iso_doc, 	X is sin(0.0),			[[ X <-- 0.0 ]], 		[]).
test_case(sin: 2, iso_doc, 	_X is sin(_N),			inst,		 		[]).
test_case(sin: 3, iso_doc, 	X is sin(0),			[[ X <-- 0.0 ]], 		[]).
test_case(sin: 4, iso_doc,  	_X is sin(foo),			type(evaluable,foo/0),		[]).
test_case(sin: 5, iso_doc,  	(PI is atan(1.0)*4,
		 X is sin(PI/2.0)),	   	[[  X =~~ 1, PI =~~ 3.14159 ]],	[]).

% 9.3.3.4
test_case(cos: 1, iso_doc, 	X is cos(0.0),			[[ X <-- 1.0 ]], 		[]).
test_case(cos: 2, iso_doc, 	_X is cos(_N),			inst,		 		[]).
test_case(cos: 3, iso_doc, 	X is cos(0),			[[ X <-- 1.0 ]], 		[]).
test_case(cos: 4, iso_doc,  	_X is cos(foo),			type(evaluable,foo/0),		[]).
test_case(cos: 5, iso_doc,  	(PI is atan(1.0)*4,
		 X is cos(PI/2.0)),	   	[[  X =~~ 0, PI =~~ 3.14159 ]],	[]).

% 9.3.4.4
test_case(atan: 1, iso_doc, 	X is atan(0.0),			[[ X <-- 0.0 ]], 		[]).
test_case(atan: 2, iso_doc,  	PI is atan(1.0)*4,	   	[[ PI =~~ 3.14159 ]],		[]).
test_case(atan: 3, iso_doc, 	_X is atan(_N),			inst,		 		[]).
test_case(atan: 4, iso_doc, 	X is atan(0),			[[ X <-- 0.0 ]], 		[]).
test_case(atan: 5, iso_doc,  	_X is atan(foo),		type(evaluable,foo/0),		[]).

% 9.3.5.4
test_case(exp1: 1, iso_doc, 	X is exp(0.0),			[[ X <-- 1.0 ]], 		[]).
%stc(exp1: 2, 	E is exp(1.0),		   	[[ E =~~ 2.7818 ]],		[]).
test_case(exp1: 2, iso_doc,  	E is exp(1.0),		   	[[ E =~~ 2.71828 ]],		[]).
test_case(exp1: 3, iso_doc, 	_X is exp(_N),			inst,		 		[]).
test_case(exp1: 4, iso_doc, 	X is exp(0),			[[ X <-- 1.0 ]], 		[]).
test_case(exp1: 5, iso_doc,  	_X is exp(foo),			type(evaluable,foo/0),		[]).

% 9.3.6.4
test_case(log: 1, iso_doc, 	X is log(1.0),			[[ X <-- 0.0 ]], 		[]).
%stc(log: 2, 	E is log(2.7818),	   	[[ E =~~ 1.0 ]],		[]).
test_case(log: 2, iso_doc,  	E is log(2.71828),	   	[[ E =~~ 1.0 ]],		[]).
test_case(log: 3, iso_doc, 	_X is log(_N),			inst,		 		[]).
test_case(log: 4, iso_doc, 	_X is log(0),			eval(undefined), 		[]).
test_case(log: 5, iso_doc,  	_X is log(foo),			type(evaluable,foo/0),		[]).
test_case(log: 6, iso_doc, 	_X is log(0.0),			eval(undefined), 		[]).

% 9.3.7.4
test_case(sqrt: 1, iso_doc, 	X is sqrt(0.0),			[[ X <-- 0.0 ]], 		[]).
test_case(sqrt: 2, iso_doc,  	E is sqrt(1),		   	[[ E <-- 1.0 ]],		[]).
test_case(sqrt: 3, iso_doc,  	E is sqrt(1.21),		[[ E =~~ 1.1 ]],		[]).
test_case(sqrt: 4, iso_doc, 	_X is sqrt(_N),			inst,		 		[]).
test_case(sqrt: 5, iso_doc, 	_X is sqrt(-1.0),		eval(undefined), 		[]).
test_case(sqrt: 6, iso_doc,  	_X is sqrt(foo),		type(evaluable,foo/0),		[]).

% 9.4.1.4
test_case(bit_rsh: 1, iso_doc, 	X is '>>'(16, 2),		[[ X <-- 4 ]], 			[]).
test_case(bit_rsh: 2, iso_doc, 	X is '>>'(19, 2),		[[ X <-- 4 ]], 			[]).
test_case(bit_rsh: 3, iso_doc, 	X is '>>'(-16, 2),		[[(compl2 -> X = -4 ; true)]],	[]).
test_case(bit_rsh: 4, iso_doc, 	_X is '>>'(77, _N),		inst,	 			[]).
%stc(bit_rsh: 5,_X is '>>'(foo, 2),		type(integer,foo),		[]).
test_case(bit_rsh: 5, iso_doc, 	_X is '>>'(foo, 2),		type(evaluable,foo/0),		[]).
test_case(bit_rsh: 6, sics, 	_X is '>>'(1.0, 2),		type(integer,1.0),		[]).

% 9.4.2.4
test_case(bit_lsh: 1, iso_doc, 	X is '<<'(16, 2),		[[ X <-- 64 ]], 		[]).
test_case(bit_lsh: 2, iso_doc, 	X is '<<'(19, 2),		[[ X <-- 76]], 			[]).
test_case(bit_lsh: 3, iso_doc, 	X is '<<'(-16, 2),		[[(compl2 -> X = -64 ; true)]],	[]).
test_case(bit_lsh: 4, iso_doc, 	_X is '<<'(77, _N),		inst,	 			[]).
%stc(bit_lsh: 5,_X is '<<'(foo, 2),		type(integer,foo),		[]).
test_case(bit_lsh: 5, iso_doc, 	_X is '<<'(foo, 2),		type(evaluable,foo/0),		[]).
test_case(bit_lsh: 6, sics, 	_X is '<<'(1.0, 2),		type(integer,1.0),		[]).

% 9.4.3.4
test_case(bit_and: 1, iso_doc, 	X is '/\\'(10, 12),		[[ X <-- 8 ]], 			[]).
test_case(bit_and: 2, iso_doc, 	X is /\(10, 12),		[[ X <-- 8 ]], 			[]).
test_case(bit_and: 3, iso_doc, 	X is '/\\'(17*256+125, 255),	[[ X <-- 125 ]], 		[]).
test_case(bit_and: 4, iso_doc, 	X is /\(-10, 12),		[[(compl2 -> X = 4 ; true)]],	[]).
test_case(bit_and: 5, iso_doc, 	_X is '/\\'(77, _N),		inst,	 			[]).
%stc(bit_and: 6,_X is '/\\'(foo, 2),		type(integer,foo),		[]).
test_case(bit_and: 6, iso_doc, 	_X is '/\\'(foo, 2),		type(evaluable,foo/0),		[]).
test_case(bit_and: 7, sics, 	_X is '/\\'(1.0, 2),		type(integer,1.0),		[]).

% 9.4.4.4
test_case(bit_or: 1, iso_doc, 	X is '\\/'(10, 12),		[[ X <-- 14 ]],			[]).
test_case(bit_or: 2, iso_doc, 	X is \/(10, 12),		[[ X <-- 14 ]],			[]).
test_case(bit_or: 3, iso_doc, 	X is '\\/'(125, 255),		[[ X <-- 255 ]], 		[]).
test_case(bit_or: 4, iso_doc, 	X is \/(-10, 12),		[[(compl2 -> X = -2 ; true)]],	[]).
test_case(bit_or: 5, iso_doc, 	_X is '\\/'(77, _N),		inst,	 			[]).
%stc(bit_or: 6,	_X is '\\/'(foo, 2),		type(integer,foo),		[]).
test_case(bit_or: 6, iso_doc, 	_X is '\\/'(foo, 2),		type(evaluable,foo/0),		[]).
test_case(bit_or: 7, sics, 	_X is '\\/'(1.0, 2),		type(integer,1.0),		[]).

% 9.4.5.4
test_case(bit_neg: 1, iso_doc, 	X is '\\'('\\'(10)),		[[ X <-- 10 ]],			[]).
test_case(bit_neg: 2, iso_doc, 	X is \(\(10)),			[[ X <-- 10 ]],			[]).
test_case(bit_neg: 4, iso_doc, 	X is \(10),			[[(compl2 -> X = -11 ; true)]],	[]).
test_case(bit_neg: 5, iso_doc, 	_X is '\\'(_N),			inst,	 			[]).
test_case(bit_neg: 6, sics, 	_X is '\\'(2.5),		type(integer,2.5),		[]).


% ---------------------------------------------------------------------------
% Programs and macros
% !! document this framework

% 6.3.4

program(fxops,[
(	:- op(100, fx, fx)
),(	:- op(100, xf, xf)
),(	:- op(100, xfx, xfx)
)]).

program(fyops,[
(	:- op(100, fy, fy)
),(	:- op(100, yf, yf)
),(	:- op(100, xfy, xfy)
),(	:- op(100, yfx, yfx)
)]).

% 6.3.7.1
macro(set_double_quotes(Flag), [pre((current_prolog_flag(double_quotes, Old),
				     set_prolog_flag(double_quotes, Flag))),
				post(set_prolog_flag(double_quotes, Old))]).

macro(double_quotes_ex1, txtin([
'(      current_prolog_flag(double_quotes, chars), atom_chars(\'jim\', "jim")',
';      current_prolog_flag(double_quotes, codes), atom_codes(\'jim\', "jim")',
';      current_prolog_flag(double_quotes, atom), \'jim\' == "jim"',
').'])).

macro(double_quotes_ex2, txtin([
'(      current_prolog_flag(double_quotes, chars), [] == ""',
';      current_prolog_flag(double_quotes, codes), [] == ""',
';      current_prolog_flag(double_quotes, atom), \'\' == ""',
').'])).

% 7.8.3.4
program(call,
[(
        b(X) :-
                Y = (write(X), X),
                call(Y)
),(
        a(1)
),(
        a(2)
)]).

% 7.8.4.4
program(cut,
[(
        twice(!) :- write('C ')
),(
        twice(true) :- write('Moss ')
),(
        goal((twice(_), !))
),(
        goal(write('Three '))
)]).

% 7.8.9.4
program(catch,
[(
        foo(X) :-
                Y is X * 2, throw(test(Y))
),(
        bar(X) :-
                X = Y, throw(Y)
),(
        coo(X) :-
                throw(X)
),(
        car(X) :-
                X = 1, throw(X)
),(
        g :-
                catch(p, _B, write(h2)),
                coo(c)
),(
        p
),(     p :-
                throw(b)
)]).

% 8.5.3.4
macro(list_of(N, A, L),
      [prog(build_list),pre(my_list_of(N,A,L))]).

program(build_list,
[(
        my_list_of(0, _, [])
),(
        my_list_of(N, A, [A|L]) :-
                  N > 0, N1 is N-1,
                  my_list_of(N1, A, L)
)]
       ).


% 8.8.1.4
program(clause, [
(       :- dynamic(cat/0)
),(     cat

),(     :- dynamic(dog/0)
),(     dog :- true

),(     elk(X) :- moose(X)

),(     :- dynamic(legs/2)
),(     legs(A,6) :- insect(A)
),(     legs(A,7) :- A, call(A)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)
)
        ]).

% 8.9.1.4
program(dynpreds1, [
(       :- dynamic(legs/2)
),(     legs(A, 6) :- insect(A)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)
)
]).

program(dynpreds2, [
(       :- dynamic(legs/2)
),(     legs(octopus, 8)
),(     legs(A, 6) :- insect(A)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)
)
]).

program(dynpreds3, [
(       :- dynamic(legs/2)
),(     legs(A, 4) :- animal(A)
),(     legs(octopus, 8)
),(     legs(A, 6) :- insect(A)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)
)
]).

program(dynpreds4, [
(       :- dynamic(legs/2)
),(     legs(A, 4) :- animal(A)
),(     legs(octopus, 8)
),(     legs(A, 6) :- insect(A)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
)
]).

% 8.9.2.4
program(dynpreds5, [
(       :- dynamic(legs/2)
),(     legs(A, 4) :- animal(A)
),(     legs(octopus, 8)
),(     legs(A, 6) :- insect(A)
),(     legs(spider, 8)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
)
]).

program(dynpreds6, [
(       :- dynamic(legs/2)
),(     legs(A, 4) :- animal(A)
),(     legs(octopus, 8)
),(     legs(A, 6) :- insect(A)
),(     legs(spider, 8)
),(     legs(B, 2) :- bird(B)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
)
]).

program(dynpreds7, [
(       :- dynamic(legs/2)
),(     legs(A, 4) :- animal(A)
),(     legs(octopus, 8)
),(     legs(A, 6) :- insect(A)
),(     legs(spider, 8)
),(     legs(B, 2) :- bird(B)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
),(     foo(X) :- call(@X) -> call(@X)
)
]).

% 8.9.3.4
program(dynpreds8, [
(       :- dynamic(legs/2)
),(     legs(A, 4) :- animal(A)
),(     legs(A, 6) :- insect(A)
),(     legs(spider, 8)
),(     legs(B, 2) :- bird(B)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
),(     foo(X) :- call(@X) -> call(@X)
)
]).

program(dynpreds9, [
(       :- dynamic(legs/2)
),(     legs(A, 4) :- animal(A)
),(     legs(A, 6) :- insect(A)
),(     legs(spider, 8)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
),(     foo(X) :- call(@X) -> call(@X)
)
]).

program(dynpreds10, [
(       :- dynamic(legs/2)

),(     :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
),(     foo(X) :- call(@X) -> call(@X)
)
]).

program(dynpreds11, [
(       :- dynamic(legs/2)

),(     :- dynamic(insect/1)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
),(     foo(X) :- call(@X) -> call(@X)
)
]).

program(dynpreds12, [
(       :- dynamic(legs/2)

),(     :- dynamic(insect/1)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(@X), call(@X)
)
]).

% 8.9.4.4
program(abolish, [
(       :- dynamic(insect/1)
),(     insect(ant)
),(     insect(bee)

),(     :- dynamic(foo/1)
),(     foo(X) :- call(X), call(X)
),(     foo(X) :- call(X) -> call(X)

),(     bar(X) :- true
)
                 ]).

macro(over_arity(N),
      [pre((current_prolog_flag(max_arity,A), N is A + 1))]).

% 8.10.3.4
program(member, [
(       my_member(X, [X|_])
),(     my_member(X, [_|L]) :-
                my_member(X, L)
)
                ]).

macro(stream_position(Pos), 
      [pre((open(bar,write,S,[reposition(true)]), stream_property(S, position(Pos)))), clean(bar)]).

macro(closed_instream(S, Opts),
      [maketxt(foo,''),pre((open('foo', read, S, Opts), close(S)))]).
macro(closed_outstream(S, Opts),
      [pre((open('foo', write, S, Opts), close(S))), clean(foo)]).

macro(txtin_foo256,
      txtin(['foo(',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,',
	'1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1).'])).

macro(clear_chconv,
      post((current_char_conversion(A,_B), char_conversion(A, A), fail; true))).

macro(some_chconvs, [pre((char_conversion('%', '+'), char_conversion('^', '\''))),clear_chconv]).

macro(long_atom(A),
      [prog(build_list),pre((my_list_of(Len,0'a,L),atom_codes(A, L)))]) :-
	context_info(max_atom_length, Max), Len is Max*2//3.

macro(txtin_maxmin(Type, Val), 
      [pre(((current_prolog_flag(Type, Val) -> true ; Val = 0), number_codes(Val, L), atom_codes(Atm, L))), txtin([Atm,'.'])]).
