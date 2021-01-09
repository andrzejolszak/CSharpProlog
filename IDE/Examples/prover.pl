% anti-sequent theorem prover

% connectives
% --------------------------------------------
:- op(425, fy,-).	% negation
:- op(425, fy,~).	% goedel-negation
:- op(450,yfx,&).	% conjunction
:- op(475,yfx,v).	% disjunction
:- op(500,yfx,=>).	% implication
:- op(500,yfx,->).	% goedel-implication
% --------------------------------------------

% truth constants
% --------------------------------------------
truthConstant(f).   % false
truthConstant(i).   % intermediate truth valued
% --------------------------------------------

% atomicFormula(A) ... A is an atomic formula
% --------------------------------------------
atomicFormula(X) :- atom(X).
atomicFormula(X) :- truthConstant(X).
% --------------------------------------------

% literal(X) ... checks if X is a literal
% --------------------------------------------
literal(X)  :- atom(X).
literal(-X) :- atom(X). 
% --------------------------------------------

% designated(X) ... X is a designated truth value
% --------------------------------------------
designated(t).
% --------------------------------------------


% consistent(Gamma) ... Gamma is does not contain complementary literals
% --------------------------------------------
inconsistent(Gamma) :- member(P,Gamma), atom(P), member(-P,Gamma).
consistent(Gamma) :- not(inconsistent(Gamma)).
% --------------------------------------------

% only_literals(Gamma) ... Gamma contains only literals
% --------------------------------------------
not_only_literals(Gamma) :- member(X,Gamma), not(literal(X)).
only_literals(Gamma) :- not(not_only_literals(Gamma)).
% --------------------------------------------

% disjoint(Gamma,Delta) ... Gamma, Delta are disjoint
% --------------------------------------------
common_formula(Gamma,Delta) :- member(X,Gamma), member(X,Delta).
disjoint(Gamma,Delta) :- not(common_formula(Gamma,Delta)).
% --------------------------------------------


% proven(Gamma,Delta,N,Proof) ... Anti-sequent Gamma -: Delta can be proven in at most N steps,
%                                 Proof contains a representation of the proof.
% --------------------------------------------

% Axioms
% --------------------------------------------
% i is not designated:
proven(Gamma,Delta,N,axiom(Gamma,Delta)) :- 
	not(designated(i)), N >= 0,
	only_literals(Gamma), only_literals(Delta),
	disjoint(Gamma,Delta),
	not(member(f,Gamma)), not(member(-f,Delta)),
	consistent(Gamma),
	not(member(i,Gamma)), not(member(-i,Gamma)).
% i is designated:
proven(Gamma,Delta,N,axiom(Gamma,Delta)) :- 
	designated(i), N >= 0,
	only_literals(Gamma), only_literals(Delta),
	disjoint(Gamma,Delta),
	not(member(f,Gamma)), not(member(-f,Delta)),
	consistent(Delta),
	not(member(i,Delta)), not(member(-i,Delta)).
% --------------------------------------------


% => left - 1
% --------------------------------------------
proven(Gamma,Delta,N,impl_left_1(Psi => Phi,Gamma,Delta,P)) :-
	N > 0,
	select(Psi => Phi, Gamma, Gamma1),
	append(Delta,[Psi],Delta1),
	N1 is N - 1,
	proven(Gamma1,Delta1,N1,P).
% --------------------------------------------

% => left - 2
% --------------------------------------------
proven(Gamma,Delta,N,impl_left_2(Psi => Phi,Gamma,Delta,P)) :-
	N > 0,
	select(Psi => Phi, Gamma, Gamma1),
	append(Gamma1,[Phi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% => right
% --------------------------------------------
proven(Gamma,Delta,N,impl_right(Psi => Phi,Gamma,Delta,P)) :- 
	N > 0,
	select(Psi => Phi, Delta, Delta1),
	append(Gamma,[Psi],Gamma1),
	append(Delta1,[Phi],Delta2),
	N1 is N - 1,
	proven(Gamma1,Delta2,N1,P).
% --------------------------------------------

% & left 
% --------------------------------------------
proven(Gamma,Delta,N,conj_left(Psi & Phi,Gamma,Delta,P)) :- 
	N > 0,
	select(Psi & Phi, Gamma, Gamma1),
	append(Gamma1,[Psi,Phi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% & right - 1
% --------------------------------------------
proven(Gamma,Delta,N,conj_right_1(Psi & Phi,Gamma,Delta,P)) :-
	N > 0,
	select(Psi & Phi, Delta, Delta1),
	append(Delta1,[Psi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% & right - 2
% --------------------------------------------
proven(Gamma,Delta,N,conj_right_2(Psi & Phi,Gamma,Delta,P)) :-
	N > 0,
	select(Psi & Phi, Delta, Delta1),
	append(Delta1,[Phi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% v left - 1
% --------------------------------------------
proven(Gamma,Delta,N,disj_left_1(Psi v Phi,Gamma,Delta,P)) :-
	N > 0,
	select(Psi v Phi, Gamma, Gamma1),
	append(Gamma1,[Psi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% v left - 2
% --------------------------------------------
proven(Gamma,Delta,N,disj_left_2(Psi v Phi,Gamma,Delta,P)) :-
	N > 0,
	select(Psi v Phi, Gamma, Gamma1),
	append(Gamma1,[Phi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% v right
% --------------------------------------------
proven(Gamma,Delta,N,disj_right(Psi v Phi,Gamma,Delta,P)) :- 
	N > 0,
	select(Psi v Phi, Delta, Delta1),
	append(Delta1,[Psi,Phi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% connectives combined with negation
% --------------------------------------------

% -- left
% --------------------------------------------
proven(Gamma,Delta,N,neg_neg_left(-(-Psi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(-Psi), Gamma, Gamma1),
	append(Gamma1,[Psi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% -- right
% --------------------------------------------
proven(Gamma,Delta,N,neg_neg_right(-(-Psi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(-Psi), Delta, Delta1),
	append(Delta1,[Psi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% -& left - 1
% --------------------------------------------
proven(Gamma,Delta,N,neg_conj_left_1(-(Psi & Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi & Phi), Gamma, Gamma1),
	append(Gamma1,[-Psi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% -& left - 2
% --------------------------------------------
proven(Gamma,Delta,N,neg_conj_left_2(-(Psi & Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi & Phi), Gamma, Gamma1),
	append(Gamma1,[-Phi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% -& right 
% --------------------------------------------
proven(Gamma,Delta,N,neg_conj_right(-(Psi & Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi & Phi), Delta, Delta1),
	append(Delta1,[-Psi,-Phi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% -v left 
% --------------------------------------------
proven(Gamma,Delta,N,neg_disj_left(-(Psi v Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi v Phi), Gamma, Gamma1),
	append(Gamma1,[-Psi,-Phi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% -v right - 1 
% --------------------------------------------
proven(Gamma,Delta,N,neg_disj_right_1(-(Psi v Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi v Phi), Delta, Delta1),
	append(Delta1,[-Psi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% -v right - 2 
% --------------------------------------------
proven(Gamma,Delta,N,neg_disj_right_2(-(Psi v Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi v Phi), Delta, Delta1),
	append(Delta1,[-Phi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% -=> left 
% --------------------------------------------
proven(Gamma,Delta,N,neg_impl_left(-(Psi => Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi => Phi), Gamma, Gamma1),
	append(Gamma1,[Psi,-Phi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% -=> right - 1 
% --------------------------------------------
proven(Gamma,Delta,N,neg_impl_right_1(-(Psi => Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi => Phi), Delta, Delta1),
	append(Delta1,[Psi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% -=> right - 2
% --------------------------------------------
proven(Gamma,Delta,N,neg_impl_right_2(-(Psi => Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi => Phi), Delta, Delta1),
	append(Delta1,[-Phi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% Goedel connectives
% --------------------------------------------

% ~ left
% --------------------------------------------
proven(Gamma,Delta,N,gdlneg_left(~Psi,Gamma,Delta,P)) :- 
	N > 0,
	select(~Psi, Gamma, Gamma1),
	append(Gamma1,[-Psi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% ~ right
% --------------------------------------------
proven(Gamma,Delta,N,gdlneg_right(~Psi,Gamma,Delta,P)) :- 
	N > 0,
	select(~Psi, Delta, Delta1),
	append(Delta1,[-Psi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% -> left - 1
% --------------------------------------------
proven(Gamma,Delta,N,gdlimpl_left_1(Psi -> Phi,Gamma,Delta,P)) :- 
	N > 0,
	select(Psi -> Phi, Gamma, Gamma1),
	append(Delta,[Psi],Delta1),
	N1 is N - 1,
	proven(Gamma1,Delta1,N1,P).
% --------------------------------------------

% -> left - 2
% --------------------------------------------
proven(Gamma,Delta,N,gdlimpl_left_2(Psi -> Phi,Gamma,Delta,P)) :- 
	N > 0,
	select(Psi -> Phi, Gamma, Gamma1),
	append(Gamma1,[Phi],Gamma2),
	N1 is N - 1,
	proven(Gamma2,Delta,N1,P).
% --------------------------------------------

% -> right - 1
% --------------------------------------------
proven(Gamma,Delta,N,gdlimpl_right_1(Psi -> Phi,Gamma,Delta,P)) :- 
	N > 0,
	select(Psi -> Phi, Delta, Delta1),
	append(Gamma,[Psi],Gamma1),
	append(Delta1,[Phi],Delta2),
	N1 is N - 1,
	proven(Gamma1,Delta2,N1,P).
% --------------------------------------------

% -> right - 2
% --------------------------------------------
proven(Gamma,Delta,N,gdlimpl_right_2(Psi -> Phi,Gamma,Delta,P)) :- 
	N > 0,
	select(Psi -> Phi, Delta, Delta1),
	append(Gamma,[-Phi],Gamma1),
	append(Delta1,[-Psi],Delta2),
	N1 is N - 1,
	proven(Gamma1,Delta2,N1,P).
% --------------------------------------------

% Goedel connectives combined with negation
% --------------------------------------------

% -~ left 
% --------------------------------------------
proven(Gamma,Delta,N,neg_gdlneg_left(-(~Psi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(~Psi), Gamma, Gamma1),
	append(Delta,[-Psi],Delta1),
	N1 is N - 1,
	proven(Gamma1,Delta1,N1,P).
% --------------------------------------------

% -~ right
% --------------------------------------------
proven(Gamma,Delta,N,neg_gdlneg_right(-(~Psi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(~Psi), Delta, Delta1),
	append(Gamma,[-Psi],Gamma1),
	N1 is N - 1,
	proven(Gamma1,Delta1,N1,P).
% --------------------------------------------

% --> left 
% --------------------------------------------
proven(Gamma,Delta,N,neg_gdlimpl_left(-(Psi -> Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi -> Phi), Gamma, Gamma1),
	append(Gamma1,[-Phi],Gamma2),
	append(Delta,[-Psi],Delta1),
	N1 is N - 1,
	proven(Gamma2,Delta1,N1,P).
% --------------------------------------------

% --> right - 1
% --------------------------------------------
proven(Gamma,Delta,N,neg_gdlimpl_right_1(-(Psi -> Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi -> Phi), Delta, Delta1),
	append(Delta1,[-Phi],Delta2),
	N1 is N - 1,
	proven(Gamma,Delta2,N1,P).
% --------------------------------------------

% --> right - 2
% --------------------------------------------
proven(Gamma,Delta,N,neg_gdlimpl_right_2(-(Psi -> Phi),Gamma,Delta,P)) :- 
	N > 0,
	select(-(Psi -> Phi), Delta, Delta1),
	append(Gamma,[-Psi],Gamma1),
	N1 is N - 1,
	proven(Gamma1,Delta1,N1,P).
% --------------------------------------------

% prove(Gamma,Delta,N) ... Prove Gamma -: Delta in at most N steps and
% print the proof.
% --------------------------------------------
prove(Gamma,Delta,N) :- proven(Gamma,Delta,N,Proof), nl, output(Proof), nl.

output(axiom(G,D)) :- write('(axiom)   '),  
                      write(G), write(' -: '), write(D), nl.

output(X) :- 
	functor(X,Name,_), arg(1,X,F), arg(2,X,G), arg(3,X,D), arg(4,X,P),
	output(P), 
	write('-----------------------------------------------------------------------'), nl,
	write('('), write(Name), write(')   '),
	write(G), write(' -: '), write(D), 
	write('    on '), write(F),nl.
% --------------------------------------------

