% Written September 12th 2005 Markus Triska  triska@gmx.at
% Public domain code.

natnum1(0).
natnum1(s(X)) :-
	natnum1(X).


mi1(true).
mi1((A,B)) :-
	mi1(A),
	mi1(B).
mi1(G) :-
	G \= true,
	G \= (_,_),
	clause(G, Body),
	mi1(Body).

%complicated_clause(A) :-
%	goal1(A),
%	goal2(A),
%	goal3(A).

mi_clause(Goal, Body) :-
	clause(Goal, Body0),
	defaulty_better(Body0, Body).

defaulty_better(true, true).
defaulty_better((A,B), (BA,BB)) :-
	defaulty_better(A, BA),
	defaulty_better(B, BB).
defaulty_better(G, g(G)) :-
	G \= true,
	G \= (_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

natnum2(0).
natnum2(s(X)) :-
	g(natnum2(X)).

number_term(0, 0).
number_term(N, s(T)) :-
	N > 0,
	N1 is N - 1,
	number_term(N1, T).

mi2(true).
mi2((A,B)) :-
	mi2(A),
	mi2(B).
mi2(g(G)) :-
	clause(G, Body),
	mi2(Body).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_goal(natnum1(_)).

mi2_safe(true).
mi2_safe((A,B)) :-
	mi2_safe(A),
	mi2_safe(B).
mi2_safe(g(G)) :-
	( safe_goal(G) ->
		mi_clause(G, Body),
		mi2_safe(Body)
	;
		format("Sorry, you can't execute ~w\n", [G]),
		false
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declarative_false :-
	declarative_false,
	false.

mi3(true).
mi3((A,B)) :-
	mi3(B),
	mi3(A).
mi3(g(G)) :-
	clause(G, Body),
	mi3(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mi_list_clause(G, Ls) :-
	clause(G, Body),
	phrase(body_list(Body), Ls).


body_list(true)  --> [].
body_list((A,B)) -->
	body_list(A),
	body_list(B).
body_list(G) -->
	{ G \= true },
	{ G \= (_,_) },
	[G].

always_infinite :- always_infinite.

mi_list1([]).
mi_list1([G|Gs]) :-
	mi_list_clause(G, Body),
	mi_list1(Body),
	mi_list1(Gs).

mi_list2([]).
mi_list2([G0|Gs0]) :-
	mi_list_clause(G0, Body),
	append(Body, Gs0, Gs),
	mi_list2(Gs).


mi_ldclause(natnum(0), Rest, Rest).
mi_ldclause(natnum(s(X)), [natnum(X)|Rest], Rest).

mi_list3([]).
mi_list3([G0|Gs0]) :-
	mi_ldclause(G0, Remaining, Gs0),
	mi_list3(Remaining).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- op(750, xfy, =>).

mi_tree(true, true).
mi_tree((A,B), (TA,TB)) :-
	mi_tree(A, TA),
	mi_tree(B, TB).
mi_tree(g(G), TBody => G) :-
	mi_clause(G, Body),
	mi_tree(Body, TBody).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mi_limit(Goal, Max) :-
	mi_limit(Goal, Max, _).

mi_limit(true, N, N).
mi_limit((A,B), N0, N) :-
	mi_limit(A, N0, N1),
	mi_limit(B, N1, N).
mi_limit(g(G), N0, N) :-
	N0 > 0,
	N1 is N0 - 1,
	mi_clause(G, Body),
	mi_limit(Body, N1, N).


mi_id(Goal) :-
	between(0, infinite, N),
	mi_limit(Goal, N).

edge(a, b).
edge(b, a).
edge(b, c).

path(A, A, []).
path(A, C, [e(A,B)|Es]) :-
	edge(A, B),
	path(B, C, Es).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


occ(X, f(X)).

mi_occ(true).
mi_occ((A,B)) :-
	mi_occ(A),
	mi_occ(B).
mi_occ(g(G)) :-
	functor(G, F, Arity),
	functor(H, F, Arity),
	mi_clause(H, Body),
	unify_with_occurs_check(G, H),
	mi_occ(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mi_circ(true).
mi_circ((A,B)) :-
	mi_circ(A),
	mi_circ(B).
mi_circ(clause(A,B)) :-
	clause(A,B).
mi_circ(A \= B) :-
	A \= B.
mi_circ(G) :-
	G \= true,
	G \= (_,_),
	G \= (_\=_),
	G \= clause(_,_),
	clause(G, Body),
	mi_circ(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

as([]).
as([a]).			% redundant
as([a,a]).			% redundant
as([A|As]) :-
	A = a,			% test built-in =/2
	5 is 2 + 3,		% test built-in is/2
	1 > 0,			% test built-in >/2
	as(As).

redundant(Functor/Arity, Reds) :-
	functor(Term, Functor, Arity),
	findall(Term-Body, clause(Term, Body), Defs),
	setof(Red, Defs^redundant_(Defs, Red), Reds).

redundant_(Defs, Fact) :-
	select(Fact-true, Defs, Rest),
	once(provable(Fact, Rest)).


provable(true, _) :- !.
provable((G1,G2), Defs) :- !,
	provable(G1, Defs),
	provable(G2, Defs).
provable(BI, _) :-
	predicate_property(BI, built_in),
	!,
	call(BI).
provable(Goal, Defs) :-
	member(Def, Defs),
	copy_term(Def, Goal-Body),
	provable(Body, Defs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resstep_([A|As0], As) :-
	findall(Gs-G, (A = [G0|Rest]-G,mi_ldclause(G0,Gs,Rest)), As, As0).

mi_backtrack(G0) :- mi_backtrack_([[G0]-G0], G0).

mi_backtrack_([[]-G|_], G).
mi_backtrack_(Alts0, G) :-
	resstep_(Alts0, Alts1),
	mi_backtrack_(Alts1, G).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


dcgnumber(0).
dcgnumber(1).

expr(N)   --> [N], { dcgnumber(N) }.
expr(A+B) --> expr(A), [(+)], expr(B).


dcg_clause(expr(N),   [t(N),{dcgnumber(N)}]).
dcg_clause(expr(A+B), [l,nt(expr(A)),t(+),nt(expr(B))]).

mi_dcg(t(T), Rest, Rest, [T|Ts], Ts).
mi_dcg({Goal}, Rest, Rest, Ts, Ts) :-
	call(Goal).
mi_dcg(nt(NT), Rest0, Rest, Ts0, Ts) :-
	dcg_clause(NT, Body),
	mi_dcg_(Body, Rest0, Rest, Ts0, Ts).
mi_dcg(l, [_|Rest], Rest, Ts, Ts).

mi_dcg_([], Rest, Rest, Ts, Ts).
mi_dcg_([G|Gs], Rest0, Rest, Ts0, Ts) :-
	mi_dcg(G, Rest0, Rest1, Ts0, Ts1),
	mi_dcg_(Gs, Rest1, Rest, Ts1, Ts).

mi_dcg(NT, String) :-
	length(String, L),
	length(Rest0, L),
	mi_dcg_([nt(NT)], Rest0, _, String, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pe_expr(Expr, String) :-
	length(String, L),
	length(Rest0, L),
	pe_expr(Expr, Rest0, _, String, []).

pe_expr(N, Rest, Rest, Ts0, Ts) :-
	Ts0 = [N|Ts],
	dcgnumber(N).
pe_expr(A+B, [_|Rest0], Rest, Ts0, Ts) :-
	pe_expr(A, Rest0, Rest1, Ts0, Ts1),
	Ts1 = [+|Ts2],
	pe_expr(B, Rest1, Rest, Ts2, Ts).

sum_of_ones(0, [1]) :- !.
sum_of_ones(N0, [1,+|Rest]) :-
	N1 is N0 - 1,
	sum_of_ones(N1, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1) Ackermann function

ack0(0, N, s(N)).
ack0(s(M), 0, Res) :-
	ack0(M, s(0), Res).
ack0(s(M), s(N), Res) :-
	ack0(s(M), N, Res1),
	ack0(M, Res1, Res).

% 2) Make unifications explicit (can be automated)

ack1(A, B, C) :-
	A = 0,
	C = s(B).
ack1(A, B, C) :-
	A = s(M),
	B = 0,
	D = s(0),
	ack1(M, D, C).
ack1(A, B, C) :-
	A = s(M),
	B = s(N),
	ack1(A, N, Res1),
	ack1(M, Res1, C).

% 3) Change representation to list of goals (can be automated)

ack(ack(A,B,C), [A = 0, C = s(B)]).
ack(ack(A,B,C), [A = s(M), B = 0, D = s(0), ack(M,D,C)]).
ack(ack(A,B,C), [A = s(M), B = s(N), ack(A,N,Res1), ack(M,Res1,C)]).

% Unification over abstract parity domain {zero, one, even, odd}

unify(X, Y) :-
	nonvar(Y),
	Y = s(T),
	unify(P, T),
	(   P = zero, X = one
	;   P = one,  X = even
	;   P = even, X = odd
	;   P = odd,  X = even
	).
unify(zero, Y) :- Y == 0.
unify(even, even).
unify(odd, odd).
unify(zero, zero).
unify(one, one).

ack_fixpoint(Ack) :-
	ack_fixpoint([], Ack).

ack_fixpoint(Derived0, Derived) :-
	ack_derive(Derived0, New),
	(   New == [] ->
            Derived = Derived0
	;   append(New, Derived0, Derived1),
            ack_fixpoint(Derived1, Derived)
	).


ack_derive(Derived0, New) :-
	findall(D, (ack_derive_(Derived0, D), \+ memberchk(D, Derived0)), New).

ack_derive_(Derived0, Head) :-
	ack(Head, Body),
	maplist(ack_prove(Derived0), Body).

ack_prove(_, A = B) :- unify(A, B).
ack_prove(Derived, ack(A,B,C)) :- member(ack(A,B,C), Derived).
