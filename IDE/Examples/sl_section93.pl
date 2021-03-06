%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Section 9.3 of the book       %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: induce_spec/2                          %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(sl_library).


%%% 9.3  Top-down induction %%%

induce_spec(Examples,Clauses):-
  process_examples([],[],Examples,Clauses).

% process the examples
process_examples(Clauses,Done,[],Clauses).
process_examples(Cls1,Done,[Ex|Exs],Clauses):-
  process_example(Cls1,Done,Ex,Cls2),
  process_examples(Cls2,[Ex|Done],Exs,Clauses).

% process one example
process_example(Clauses,Done,+Example,Clauses):-
  covers(Clauses,Example).
process_example(Cls,Done,+Example,Clauses):-
  not covers(Cls,Example),
  generalise(Cls,Done,Example,Clauses).
process_example(Cls,Done,-Example,Clauses):-
  covers(Cls,Example),
  specialise(Cls,Done,Example,Clauses).
process_example(Clauses,Done,-Example,Clauses):-
  not covers(Clauses,Example).


% covers(Clauses,Ex) <- Ex can be proved from Clauses and
%                       background theory in max. 10 steps
covers(Clauses,Example):-
	prove_d(10,Clauses,Example).

prove_d(D,Cls,true):-!.
prove_d(D,Cls,(A,B)):-!,
	prove_d(D,Cls,A),
	prove_d(D,Cls,B).
prove_d(D,Cls,A):-
	D>0,D1 is D-1,
	copy_element((A:-B),Cls),	% make copy of clause
	prove_d(D1,Cls,B).
prove_d(D,Cls,A):-
	prove_bg(A).

prove_bg(true):-!.
prove_bg((A,B)):-!,
	prove_bg(A),
	prove_bg(B).
prove_bg(A):-
	bg((A:-B)),
	prove_bg(B).


% Specialisation by removing a refuted clause
specialise(Cls,Done,Example,Clauses):-
	false_clause(Cls,Done,Example,C),
	remove_one(C,Cls,Cls1),
	write('     ...refuted: '),write(C),nl,
	process_examples(Cls1,[],[-Example|Done],Clauses).

% false_clause(Cs,E,E,C) <- C is a false clause in the proof of E (or ok)
false_clause(Cls,Exs,true,ok):-!.
false_clause(Cls,Exs,(A,B),X):-!,
	false_clause(Cls,Exs,A,Xa),
	( Xa = ok   -> false_clause(Cls,Exs,B,X)
	; otherwise -> X = Xa
	).
false_clause(Cls,Exs,E,ok):-
	element(+E,Exs),!.
false_clause(Cls,Exs,A,ok):-
	bg((A:-B)),!.
false_clause(Cls,Exs,A,X):-
	copy_element((A:-B),Cls),
	false_clause(Cls,Exs,B,Xb),
	( Xb \= ok		-> X = Xb
	; otherwise		-> X = (A:-B)
	).


% Generalisation by adding a covering clause
generalise(Cls,Done,Example,Clauses):-
	search_clause(Done,Example,Cl),
	write('Found clause: '),write(Cl),nl,
	process_examples([Cl|Cls],[],[+Example|Done],Clauses).


% search_clause(Exs,E,C) <- C is a clause covering E and
%                           not covering negative examples
%                           (iterative deepening search)
search_clause(Exs,Example,Clause):-
	literal(Head,Vars),
	try((Head=Example)),
	search_clause(3,a((Head:-true),Vars),Exs,Example,Clause).

search_clause(D,Current,Exs,Example,Clause):-
	write(D),write('..'),
	search_clause_d(D,Current,Exs,Example,Clause),!.
search_clause(D,Current,Exs,Example,Clause):-
	D1 is D+1,
	!,search_clause(D1,Current,Exs,Example,Clause).

search_clause_d(D,a(Clause,Vars),Exs,Example,Clause):-
	covers_ex(Clause,Example,Exs),	% goal
	not((element(-N,Exs),covers_ex(Clause,N,Exs))),!.
search_clause_d(D,Current,Exs,Example,Clause):-
	D>0,D1 is D-1,
	specialise_clause(Current,Spec),
	search_clause_d(D1,Spec,Exs,Example,Clause).


% Extensional coverage
covers_ex((Head:-Body),Example,Exs):-
        try((Head=Example,covers_ex(Body,Exs))).

covers_ex(true,Exs):-!.
covers_ex((A,B),Exs):-!,
	covers_ex(A,Exs),
	covers_ex(B,Exs).
covers_ex(A,Exs):-
	element(+A,Exs).
covers_ex(A,Exs):-
	prove_bg(A).


% specialise_clause(C,S) <- S is a minimal specialisation
%                           of C under theta-subsumption
specialise_clause(Current,Spec):-
	add_literal(Current,Spec).
specialise_clause(Current,Spec):-
	apply_subs(Current,Spec).

add_literal(a((H:-true),Vars),a((H:-L),Vars)):-!,
	literal(L,LVars),
	proper_subset(LVars,Vars).
add_literal(a((H:-B),Vars),a((H:-L,B),Vars)):-
	literal(L,LVars),
	proper_subset(LVars,Vars).

apply_subs(a(Clause,Vars),a(Spec,SVars)):-
	copy_term(a(Clause,Vars),a(Spec,Vs)),
	apply_subs1(Vs,SVars).

apply_subs1(Vars,SVars):-
	unify_two(Vars,SVars).
apply_subs1(Vars,SVars):-
	subs_term(Vars,SVars).

unify_two([X|Vars],Vars):-
	element(Y,Vars),
	X=Y.
unify_two([X|Vars],[X|SVars]):-
	unify_two(Vars,SVars).

subs_term(Vars,SVars):-
	remove_one(X,Vars,Vs),
	term(Term,TVars),
	X=Term,
	append(Vs,TVars,SVars).


%%% Queries %%%

term(list([]),[]).
term(list([X|Y]),[item(X),list(Y)]).

%%%%%%%%%%%%%%%%%%  element/2  %%%%%%%%%%%%%%%%%%%%%%%%

% literal(element(X,Y),[item(X),list(Y)]).

% bg.

query1(Clauses):-
	induce_spec([+element(a,[a,b]),
 	            -element(x,[a,b]),
	             +element(b,[b]),
	             +element(b,[a,b])
	            ],Clauses).


%%%%%%%%%%%%%%%%%%  append/3   %%%%%%%%%%%%%%%%%%%%%%%

% literal(append(X,Y,Z),[list(X),list(Y),list(Z)]).

% bg.

query2(Clauses):-
	induce_spec([+append([],[b,c],[b,c]),
	             -append([],[a,b],[c,d]),
	             -append([a,b],[c,d],[c,d]),
	             -append([a],[b,c],[d,b,c]),
	             -append([a],[b,c],[a,d,e]),
	             +append([a],[b,c],[a,b,c])
	            ],Clauses).

%%%%%%%%%%%%%%%%%%   listnum/2    %%%%%%%%%%%%%%%%%%%%%%%

literal(listnum(X,Y),[list(X),list(Y)]).
literal(num(X,Y),[item(X),item(Y)]).

bg((num(1,one):-true)).
bg((num(2,two):-true)).
bg((num(3,three):-true)).
bg((num(4,four):-true)).
bg((num(5,five):-true)).

query3(Clauses):-
	induce_spec([+listnum([],[]),
 	            -listnum([one],[one]),
	             -listnum([1,two],[one,two]),
	             +listnum([1],[one]),
	             -listnum([five,two],[5,two]),
	             +listnum([five],[5])
	            ],Clauses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

