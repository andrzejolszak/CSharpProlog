
headline:-
  wn('% -----------------------------------------------------------  %'),
  wn('%   decision under ambiguous beliefs by Prolog'),
  wn('% -----------------------------------------------------------  %'),
  h0.
h0:-
  wn('%  representative predicates in this program: '),
  wn('%  ----------------------------------------------------------------------- '),
  wn('%  implicate(X,Y):  implication mapping.'),
  wn('%  inverse_implication(X,Y):  inverse implication. '),
  wn('%  payoff(A,B,C):   (conservative/Choquet) expected utility representation. '),
  wn('%   w.r.t. belief function induced by implication mapping.'),
  wn('%   - A = choquet(F):   Choquet expected utility (CEU) for act F. '),
  wn('%   - A = choquet1(F):   equivalent representation of the CEU for F. '),
  wn('%   - A = conservative(F):   conservative utility, '),
  wn('%   - A = expected_conservative(F):   conservative expected utility, '),
  wn('%   i.e., additive decomposition (mean of min) for the Choquet integral, '),
  wn('%  reference: Mukerji(1997) and so on. Please execute references/0.'),
  wn('%  followings are the predicates of the previous version of this program. '),
  wn('%  ----------------------------------------------------------------------- '),
  wn('%  sbel(A,B,C):   belief function for sorted events. (bug fix.) '),
  wn('%  spos(A,B,C):   plausibility function. '),
  wn('%  mass(A,B,C):   bpa computed from bel via inversion. '),
  wn('%  modularity /1, /5:   checking modularity of the model. '),
  wn('%  update_mass(A,B,C):   updating bpa by Dempster-Shafer rule. '),
  wn('%  update_sbel(A,B,C):   updating bel by Dempster-Shafer rule. '),
  wn('%  update_spos(A,B,C):   updating pos by Dempster-Shafer rule. '),
  wn('%  update_bel_ul(A,B,C):   updating bel by upper-lower prob rule. '),
  wn('%  update_pos_ul(A,B,C):   updating pos by upper-lower prob rule. '),
  wn('%   h0.   this.').
me:-
  wn('%  file: belief01.pl'),
  wn('%  author: Kenryo INDO (Kanto Gakuen University)'),
  wn('%  created: 30 Nov 2001 (belief0.pl)'),
  wn('%  modified: 27-28 Feb 2003.(belief0.pl)'),
  wn('%  modified: 1 Mar 2003.(belief0.pl)'),
  wn('%  modified: 29 Jun 2003.'),
  wn('%  imported:  (excerption from math1.pl and set.pl 27 Feb 2003)').
references:-
  wn('% references:'),
  wn('%[1] Shafer,G.(1976). Mathematical Theory of Evidence. Princeton '),
  wn('%  University Press.'),
  wn('%[2] Dempster, A.P.(1967). Upper and lower probabilities induced by a'),
  wn('%  multivalued mapping. Annals of Mathematical Statistics 38: 325-339.'),
  wn('%[3] Moral,S.and De Campos, L.M.(1991). Updating uncertain information.'),  wn('%  In the Proceedings of IPMU90, LNCS 521, Springer, pp.58-67. '),
  wn('%[4] Dow, J. and S. R. da Costa Werlang (1992). Excess volatility of '),
  wn('%  stock prices and Knightian uncertainty. European Economic Review '),
  wn('%   36: 631-638.'),
  wn('%[5] Jaffray J-.Y. and P. Wakker (1994). Decision making with belief '),
  wn('%  functions: compatibility and incompatibility with the sure-thing '),
  wn('%  principle. Journal of Risk and Uncertainty 8: 255-271.'),
  wn('%[6] Gilboa,I. and D. Schmeidler (1993). Updating ambiguous beliefs.'),
  wn('%  Journal of Economic Theory 59: 33-49.'),
  wn('%[7] Mukerji, S. (1997). Understanding the nonadditive probability '),
  wn('%  decision model. Economic Theory 9: 23-46.').

%:- headline.

wn(Z):- write(Z),nl.

/*
Section 1. Definitions.
% (0) Assume a frame of discernment (i.e, a state space with some 
% epistemic interpretation of it).  Then beliefs of agent about uncertain 
% events are represented by belief function stated as follows.
% (1) A belief function is 0-1 normalized totally monotone capacity.
% (2) A belief function is also defined by  the bpa (mass) function. 
% Belief function of an event E summarizes its evidence in that
% it is the sum of all bpas of its focal set F included in E .
% (3) Conversely, a bpa function can be calculated via inversion 
% from belief functions.
% (4) A belief function has its conjugate, a possibility function.
% (5) Updating rule of belief function can be computed by using 
% updated bpa function which is the restriction of prior bpa to 
% the observed event.
*/

/*
Section 2. Informal review of formalization (see bibliography [1-3]).

(1. n-monotone capacity)

  v(union(Ak))

   >=  sum(subset(J,[1,..,n]),

         (-1)^(|J|+1) * v(intersect(for(member(k,J)),Ak))

       ).

(2. definition via basic probability assignment; bpa)

  v(A) =  bel(A)
 
       =  sum(for(subset(B,A),B\=[]),mass(A)).

(3. Mobius inversion formula)

  mass(B) = sum(for(subset(A,B)), (-1)^|B-A| * v(A).

   where 

  mass([]) = 0,
  sum(mass(A))=1.

(4. plausibility function (possibility measure))

  v~(A) = v(All) - v(All - A) = 1 - v(-A).

        = sum(for(intersect(A,B)), mass(B)).

(5. belief updating)

 > Dempster's compostion rule:

  m_12(A) = sum(for(intersection(X,Y,A)), m_1(X) * m_2(Y)).

 > Dempster-Shafer conditioning:

  m_ds(A|B)

   = sum(for(intersection(E,B,A)),  m(E) / (1-Sum) ),

   where 

   Sum = sum(for(intersection(E,B,F),F\=[]), m(E)).

  In other words,

                 v(union(A,-B)) - v(-B)
  v_ds(A|B)  =  ------------------------
                 1 - v(-B)

               v~(B) - v~(intersect(-A,B))
           =  -----------------------------
               v~(B)

  > conditioning by Upper-Lower envelope of additive measures:

                 v(intersect(A,B))
  v_ul(A|B)  =  -----------------------------------------
                 v(intersect(A,B)) + v~(intersect(-A,B))

                 v~(intersect(A,B))
  v~ul(A|B)  =  -----------------------------------------
                 v~(intersect(A,B)) + v(difference(B,A))

*/


/* Section 3. Prolog code for belief functions */

%------------------
%   Examples
%------------------
%
% example 1. Ellsberg 3 color problem
% (Gilboa and Schmeidler, 1993, p.36)
%---------------------------------------------------
/*
states([r,b,y]).

% Basic probability assignment of the example
bpa0([],0).
bpa0([r],1/3).
bpa0([b,y],2/3).
%bpa0([r,b,y],1/3).  % violate to normalization.
*/

/*
bel0([],0).
bel0(E,1/3):- member(E,[[r],[b,r],[r,y]]).
bel0([b,y],2/3).
bel0([b,r,y],1).

% note: 
% see also the results of update_sbel(A/[b,y],_,B) for example 1.

*/


% example 2. Dow and Werlang(1992)'s trader
%---------------------------------------------------
/*
states([1,2,3]).

asset(stock).
asset(cash).
time(T):-member(T,[0,1,2]).
return(cash,0).
return(stock,state(1),1).
return(stock,state(2),1/2).
return(stock,state(3),0).
partition(h1,[1]).
partition(h2,[2,3]).
know(S,H,E):-
   partition(_,H),
   member(S,H),
   sevent(E),
   subset_of(H,E,_).
%
belief(time(0),[],0).
belief(time(0),X,1/4):- member(X,[[1],[2],[3]]).
belief(time(0),X,1/2):- member(X,[[1,2],[2,3],[1,3]]).
belief(time(0),[1,2,3],1).

% belief function for example 3.
bel0(F,Yq,Y):-
  event(F),
  sort(F,F1),
  belief(time(0),F1,Yq),
  Y is Yq.

*/

% note: the core of v(.)=bel(.) in example 2 is 
% the convex hull of additive measures.
%  Core(v(.))=[p | p(.)>=��(.)].
%
%  p*([1])=1/2, p*([2])=1/4, p*([3])=1/4,
%  p*([1])=1/4, p*([2])=1/2, p*([3])=1/4,
%  p*([1])=1/4, p*([2])=1/4, p*([3])=1/2.
%

% example 3. TV set sales
% in Jaffray and Wakker(1994), example 2.1.
%---------------------------------------------------
/*

states([qL,qM,qH]).

bpa0([],0).
bpa0([qL],0).
bpa0([qM],0).
bpa0([qH],0.1).
bpa0([qL,qM],0.6).
bpa0([qL,qH],0).
bpa0([qM,qH],0.3).
bpa0([qL,qM,qH],0).

bel0([],0).
bel0([qL],0).
bel0([qM],0).
bel0([qH],0.1).
bel0([qL,qM],0.6).
bel0([qL,qH],0.1).
bel0([qM,qH],0.4).
bel0([qL,qM,qH],1).
*/

/*
pos0([],0).
pos0([qL],0.6).
pos0([qM],0.9).
pos0([qH],0.4).
pos0([qL,qM],0.9).
pos0([qL,qH],1).
pos0([qM,qH],1).
pos0([qL,qM,qH],1).
*/


/*
Table 1. Beliefs in the TV sales example.

           []    [L]   [M]   [H]  [L,M]  [L,H]  [M,H]   S
------------------------------------------------------------
Bel(=v)     0    0.0   0.0   0.1   0.6    0.1    0.4    1
Pl (=v~)    0	 0.6   0.9   0.4   0.9    1.0    1.0    1
m           0	 0.0   0.0   0.1   0.6    0.0    0.3    0
------------------------------------------------------------
 (Cited from Jaffray and Wakker(1994), Table 3.1).

% mass and belief of example 2, 
% the TV set sales problem (Jaffray and Wakker, 1994)

(a) S=[L,M,H]
 m([])=0.
 m([L])=v([L])=0. 
 m([M])=v([M])=0.
 m([H])=v([H])=0.1.
 m([L,M])=-v([L])-v([M])+v([L,M])=0.6.
 m([L,H])=-v([L])-v([H])+v([L,H]) =-0.1+0.1=0.
 m([M,H])=-v([M])-v([H])+v([M,H])=-0.1+0.4=0.3.
 m(S)=v([L])+v([M])+v([H])-v([L,M])-v([L,H])-v([M,H])+v([S]) 
     =0.1-1.1+1=1-(0.1+0.6+0.3)=0.
(b) bpa->bel
 v([])= m([])=0.
 v([L])= m([])+m([L])=0. 
 v([M])= m([])+m([M])=0. 
 v([H])= m([])+m([H])=0.1.
 v([L,M])
    = m([])+m([L])+m([M])+m([L,M])=0.6.
 v([L,H])
    = m([])+m([L])+m([H])+m([L,H])=0.1.
 v([M,H])
    = m([])+m([M])+m([H])+m([M,H])=0.4.
 v(S)
    = m([])+m([L])+m([M])+m([H])
     +m([L,M])+m([L,H])+m([M,H])
     +m([L,M,H])=1.
(c) bpa->pos
 v~([])=0.
 v~([L])
   = m([L])+m([L,M])+m([L,H])+m(S)=0.6. 
 v~([M])
   = m([M])+m([L,M])+m([M,H])+m(S)=0.9. 
 v~([H])
   = m([H])+m([L,H])+m([M,H])+m(S)=0.4. 
 v~([L,M])
   = m([L])+m([M])+m([L,M])
     +m([L,H])+m([M,H])+m(S)=0.9.
 v~([L,H])
   = m([L])+m([H])+m([L,H])
    +m([L,M])+m([M,H])+m(S)=1.0.
 v~([M,H])
   = m([M])+m([H])+m([M,H])
    +m([L,M])+m([L,H])+m(S)=1.0.
 v~(S)
   = m([])+m([L])+m([M])+m([H])
    +m([L,M])+m([L,H])+m([M,H])
    +m([L,M,H])=1=v(S).
*/

% a brief explanation for TV set example.
% ---------------------------------------------------
/*
A story: Suppose you are the sales person of a TV set.
This product has the possible quality classes, qH, qM, or qL,
each of which brings about earning size in accordance with this 
order of quality. 

You may use the following uncertain evidences of eight types 
listed below in order to predict the purchase behavior of 
customer you now supposed to faced.

% Although it will not be utilized hereafter in this version, ... 
% available evidences and law of movement.

% (E1) share w.r.t. quality: 
current_share_by_quality(qH, 0.1).
current_share_by_quality(qM, 0.3).
current_share_by_quality(qL, 0.6).

% (E2) possible behavior conditionalized on current state.
possible_purchase_of_customer(qH / [qH ]).
possible_purchase_of_customer(qM / [qL, qM]).
possible_purchase_of_customer(qL / [qL]).

% (E3)--(E8) boundary conditions of possible sales.
possible_sales_share([qH], lower(0.1), upper(0.4)).
possible_sales_share([qM], lower(0), upper(0.9)).
possible_sales_share([qL], lower(0),upper(0.6)).
possible_sales_share([qM,qH], lower(0.4),upper(1)).
possible_sales_share([qL,qM], lower(0.6),upper(0.9)).
possible_sales_share([qL,qH], lower(0.1),upper(1)).

possible_sales_share(E, upper(Ub), lower(Lb)):-
   sevent(E),
   findall((Uq,Lq),
     (
      member(Q,E),
      maximum_sales_share_of(Q,Uq),
      minimum_sales_share_of(Q,Lq)
     ),
   D),
   findall(Uq,member((Uq,_),D),Us),
   sum(Us,Ub),
   findall(Lq,member((_,Lq),D),Ls),
   sum(Ls,Lb).

maximum_sales_share_of(Q,Y):-
   state(Q),
   findall(B,
     (
      possible_purchase_of_customer(Q/C),
      member(X,C),
      current_share_by_quality(X,B)
     ),
   A),
   sum(A,Y).

minimum_sales_share_of(qL,0).
minimum_sales_share_of(qM,0).
minimum_sales_share_of(qH,B):-current_share_by_quality(qH,B).
*/


% example 2. contingency trade (Mukerji,1997)
%---------------------------------------------------
states([s1,s2,s3,s4]).
primitive_states([w1,w2,w3,w4,w5]).
prior(w1,0.1).
prior(w2,0.2).
prior(w3,0.3).
prior(w4,0.1).
prior(w5,0.3).
know(at(W),map(H),E):-implicate(W,H),sevent(E),subset_of(H,E,_).
%
bpa0(S,P):-sevent(S),implicate(W,S),prior(W,P).
bpa0(S,0):-sevent(S),\+ implicate(_W,S).
/*
% table 2 of Mukerji(1997, p.29).
bel0([],0).
bel0([s1],0.1).
bel0([s2],0).
bel0([s3],0.1).
bel0([s4],0.3).
bel0([s1,s2],0.1).
bel0([s1,s3],0.2).
bel0([s1,s4],0.4).
bel0([s2,s3],0.3).
bel0([s2,s4],0.3).
bel0([s3,s4],0.4).
bel0([s1,s2,s3],0.4).
bel0([s1,s2,s4],0.4).
bel0([s2,s3,s4],0.6).
bel0([s1,s3,s4],0.5).
bel0([s1,s2,s3,s4],1).
*/

% the implication mapping.
implicate(w1,[s1]).
implicate(w2,[s2,s3]).
implicate(w3,[s4]).
implicate(w4,[s3]).
implicate(w5,[s1,s2,s3,s4]).

% inverse implication.
inverse_implication(X,Y):-
   event(X),
   findall(W,
     (
      implicate(W,S),wn(implicate(W,S)),
      subset(S,X)
     ),
   Ws),
   flatten(Ws,Ws1),
   sort(Ws1,Y).
/*
% sample execution.

?- inverse_implication([s1,s2,s4],B).
implicate(w1, [s1])
implicate(w2, [s2, s3])
implicate(w3, [s4])
implicate(w4, [s3])
implicate(w5, [s1, s2, s3, s4])

B = [w1, w3] ;

No
?- 
*/


%decision part
act(f).
payoff(f,s1,10).
payoff(f,s2,7).
payoff(f,s3,4).
payoff(f,s4,15).


% conservative (and cautious DM's) extension of act 
% and its EU representation (Mukerji, p.41).
%-----------------------------------------------------
payoff(conservative(F),W,X):- 
   act(F),
   implicate(W,H),
   findall(Y,
     (
      member(S,H),
      payoff(F,S,Y)
     ),
   Z),
   min_of(X,Z).


payoff(expected_conservative(F),Es,X):- 
   act(F),
   findall((W,Q,Z),
     (
      prior(W,Q),
      payoff(conservative(F),W,Z),wn((W,Q,Z))
     ),
   D),
   findall(E,
     (
      member((W,Q,Z),D),
      E = Q * Z
     ),
   Es),
   sum(Es,X).

% Choquet EU representation
%-----------------------------------------------------

payoff(ranked(F),SS,[]):-
   act(F),
   states(SS).
payoff(ranked(F),Remains,[(S,X)|Z]):-
   payoff(ranked(F),R1,Z),
   (R1=[]->!,fail;true),
   state(S),
   payoff(F,S,X),
   member(S,R1),
   \+ (
     member(S1,R1),
     payoff(F,S1,X1),
     X1 < X
   ),
   subtract(R1,[S],Remains).

payoff(choquet(F),[SS,[],[0],[0]],0):-
   act(F),
   states(SS).
payoff(choquet(F),[Remains,[S|Y],[X|Z],[V|W]],E):-
   payoff(choquet(F),[R1,Y,Z,W],E1),
   (R1=[]->!,fail;true),
   state(S),
   payoff(F,S,X),
   member(S,R1),
   \+ (
     member(S1,R1),
     payoff(F,S1,X1),
     X1 < X
   ),
   subtract(R1,[S],Remains),
   bel(R1,_,B),
   Z = [Z1|_],
   V = (X-Z1) * B,
   E is E1 + V.

payoff(choquet1(F),[SS,[],[0],[0]],0):-
   act(F),
   states(SS).
payoff(choquet1(F),[Remains,[S|Y],[X|Z],[V|W]],E):-
   payoff(choquet1(F),[R1,Y,Z,W],E1),
   (R1=[]->!,fail;true),
   state(S),
   payoff(F,S,X),
   member(S,R1),
   \+ (
     member(S1,R1),
     payoff(F,S1,X1),
     X1 > X
   ),
   subtract(R1,[S],Remains),
   bel(Y,_,B0),
   bel([S|Y],_,B1),
   V = X * (B1-B0),
   E is E1 + V.

/*

% sample execution
%-----------------------------------------

?- payoff(expected_conservative(F),P,Q).
w1, 0.1, 10
w2, 0.2, 4
w3, 0.3, 15
w4, 0.1, 4
w5, 0.3, 4

F = f
P = [0.1*10, 0.2*4, 0.3*15, 0.1*4, 0.3*4]
Q = 7.9 

Yes
?- payoff(choquet(A),B,C).

A = f
B = [[s1, s2, s3, s4], [], [0], [0]]
C = 0 ;

A = f
B = [[s1, s2, s4], [s3], [4, 0], [ (4-0)*1, 0]]
C = 4 ;

A = f
B = [[s1, s4], [s2, s3], [7, 4, 0], [ (7-4)*0.4, (4-0)*1, 0]]
C = 5.2 ;

A = f
B = [[s4], [s1, s2, s3], [10, 7, 4, 0], [ (10-7)*0.4, (7-4)*0.4, (4-0)*1, 0]]
C = 6.4 ;

A = f
B = [[], [s4, s1, s2, s3], [15, 10, 7, 4, 0], [ (15-10)*0.3, (10-7)*0.4, (7-4)*0.4, (... -...)*1, 0]]
C = 7.9 ;

No
?- payoff(choquet1(F),P,Q).

F = f
P = [[s1, s2, s3, s4], [], [0], [0]]
Q = 0 ;

F = f
P = [[s1, s2, s3], [s4], [15, 0], [15* (0.3-0), 0]]
Q = 4.5 ;

F = f
P = [[s2, s3], [s1, s4], [10, 15, 0], [10* (0.4-0.3), 15* (0.3-0), 0]]
Q = 5.5 ;

F = f
P = [[s3], [s2, s1, s4], [7, 10, 15, 0], [7* (0.4-0.4), 10* (0.4-0.3), 15* (0.3-0), 0]]
Q = 5.5 ;

F = f
P = [[], [s3, s2, s1, s4], [4, 7, 10, 15, 0], [4* (1-0.4), 7* (0.4-0.4), 10* (0.4-0.3), 15* (... -...), 0]]
Q = 7.9 ;

No
?- 
*/



%---------------------------------------------------
%   Part I.   Belief Function
%---------------------------------------------------

% state space, event
%---------------------------------------------------
event(E):-
   states(A),
   bag1(E,A,_N).

sevent(X):-
   event(X),
   sort(X,X).

all_event(S):-setof(E,event(E),S).

all_sevent(S):-setof(E,sevent(E),S).

cevent(E,X):-
   complementary_event(E,X).

complementary_event(E,X):-
   states(A),
   event(X),
   subtract(A,X,C),
   sevent(E),
   seteq(C,E).

state(S):- states(A),member(S,A).


% Basic probability assignment (bpa) 
% computed from database bpa0.
%---------------------------------------------------
bpa(E,B):-
   event(E),
   bpa0(E1,B),
   seteq(E,E1).
bpa(E,0):-
   event(E),
   \+ (
     bpa0(E1,_),
     seteq(E,E1)
   ).


% Belief functions
%---------------------------------------------------
bel(E,Xq,X):-
   bel0(E,Xq,X).

% modified: 29 Jun 2003. bug fix. (setof was used.)
bel(E,Xq,X):-
   (clause(bel0(_,_,_),_)->fail;true),
   (clause(bpa0(_,_),_)->true;fail),
   event(E),
   bagof(B,
     F^(
       sevent(F),
       subset(F,E),
       bpa(F,B)
       %,nl,write(bpa(F,B))
     ),
   G),
   sum_eq(G,Xq,X).

% belief function for sorted event.
sbel(E,Xq,X):-
   sevent(E),
   bel(E,Xq,X).

% Possibility function (conjugate belief function)
%---------------------------------------------------
pos(E,Xq,X):-
   (clause(pos0(_,_,_),_)->fail;true),
   (clause(bpa0(_,_),_)->true;fail),
   event(E),
   findall(B,
     (
      sevent(F),
      intersection(F,E,M),
      M \= [],
      bpa(F,B)
     ),
   G),
   sum_eq(G,Xq,X).

spos(E,Xq,X):-
   sevent(E),
   pos(E,Xq,X).

% a comparison.
pos_1(E,1-Xq,X):-
   event(E),
   cevent(C,E),
   bel(C,Xq,_),
   X is 1 - Xq.

% a verification program as for v(A)=1-v~(-A).
%
% ?- sbel(E,_,B),cevent(F,E),pos_1(F,1-X,_),Y is X.


% b.p.a. via Mevious inversion
%---------------------------------------------------
%  mass(B) = sum(for(subset(A,B)), (-1)^|B-A| * v(A)).

mass([],0,0).
mass(E,Yq,Y):-
   event(E),
   E \= [],
   findall(A,
     (
      sevent(F),
      subset_of(F,_,E),
      bel(F,Bq,_B),
      %nl,write(bel(F,Bq,_B)),
      mevious(F,E,K),
      %nl,write(mevious(F,E,K)),
      A = K * Bq
     ),
   G),
   sum_eq(G,Yq,Y1),
   Y is Y1.

% Coefficients in the Mevious inversion formula.
%---------------------------------------------------
mevious(X,Y,Z):-
   event(X),
   event(Y),
   subtract(Y,X,W),
   length(W,M),
   Z = (-1)^M.


% Checking modularity (i.e., 2-monotonicity).
%---------------------------------------------------
modularity(E,F,Yq1 + Yq2,Yq3 + Yq4,Z):-
   sevent(E),
   sevent(F),
   F \= E,
   union(E,F,G1),
   sort(G1,G),
   intersection(E,F,H1),
   sort(H1,H),
   bel(E,Yq1,_Y1),
   bel(F,Yq2,_Y2),
   bel(G,Yq3,_Y3),
   bel(H,Yq4,_Y4),
   X1 is Yq1 + Yq2,
   X2 is Yq3 + Yq4,
   (num_eq(X1,X2) -> Z = modular; true),
   (X1 > X2 -> Z = submodular; true),
   (X1 < X2 -> Z = supermodular; true).

num_eq(X,Y):-
   Z is (X - Y)^2,
   Z < 10^(-10).

modularity(Z):-
   findall(Y,
     (
      sevent(E),
      sevent(F),
      modularity(E,F,_,_,Y)
     ),
   G),
   (sort(G,[modular])->Z = modular;true),
   (sort(G,[modular,supermodular])->Z = supermodular;true),
   (sort(G,[modular,submodular])->Z = submodular;true),
   (Z = nonlinear; true).

% rephrase into convexity.
convexity(E,F,G,H,I):-
   modularity(E,F,G,H,J),
   A = (concave,submodular),
   B = (convex,supermodular),
   C = (linear,modular),
   C1 = nonlinear,
   D = (I,J),
   member(D,[A,B,C,C1]).

convexity(concave):- modularity(submodular).
convexity(convex):- modularity(supermodular).
convexity(linear):- modularity(modular).

% Checking additivity.
%---------------------------------------------------
additivity(E,F,Yq1 + Yq2,Yq3,Z):-
   sevent(E),
   sevent(F),
   F \= E,
   union(E,F,G1),
   sort(G1,G),
   intersection(E,F,[]),
   bel(E,Yq1,_Y1),
   bel(F,Yq2,_Y2),
   bel(G,Yq3,_Y3),
   X1 is Yq1 + Yq2,
   X2 is Yq3,
   (num_eq(X1,X2) -> Z = additive; true),
   (X1 > X2 -> Z = subadditive; true),
   (X1 < X2 -> Z = superadditive; true).

additivity(Z):-
   findall(Y,
     (
      sevent(E),
      sevent(F),
      additivity(E,F,_,_,Y)
     ),
   G),%nl,write(G),
   (sort(G,[additive])->Z = additive; true),
   (sort(G,[additive,superadditive])->Z = superadditive; true),
   (sort(G,[additive,subadditive])->Z = subadditive; true),
   (Z = nonadditive; true).
   

% Checking monotonicity in event.
%---------------------------------------------------
monotonicity(E,F,Yq1,Yq2,Z):-
   sevent(E),
   sevent(F),
   F \= E,
   subset(E,F),
   bel(E,Yq1,Y1),
   bel(F,Yq2,Y2),
   (Y1 > Y2 -> Z = nonmonotonic; true),
   (Y1 =< Y2 -> Z = monotone; true).

monotonicity(Z):-
   findall(Y,
     (
      sevent(E),
      sevent(F),
      monotonicity(E,F,_,_,Y)
     ),
   G),%nl,write(G),
   (
    sort(G,[monotone])
     -> Z = monotone;
        Z = nonmonotonic
   ).

% definition of capacity.
is_capcity:-  monotonicity(monotone).


/*
% sample executions.

?- sevent(A),mass(A,B,C),bpa(A,D).

A = [r]
B = 1/3* -1^0
C = 0.333333
D = 1/3 ;

A = [b]
B = 0* -1^0
C = 0
D = 0 ;

A = [y]
B = 0* -1^0
C = 0
D = 0 ;

A = [b, r]
B = 1/3* -1^1+ (1/3+0)* -1^0+0* -1^1
C = 0
D = 0 ;

A = [r, y]
B = 1/3* -1^1+ (1/3+0)* -1^0+0* -1^1
C = 0
D = 0 ;

A = [b, y]
B = (2/3+0)* -1^0+0* -1^1
C = 0.666667
D = 2/3 


A = [b, r, y]
B = 1/3* -1^2+ (2/3+0)* -1^1+ (1/3+0)* -1^1+ (2/3+1/3+0)* -1^0+0* -1^2
C = 0.333333
D = 1/3 ;

No
?- sevent(A),cevent(D,A),pos(A,B,C).

A = [r]
D = [b, y]
B = 1/3+0
C = 0.333333 

Yes
?- sevent(A),cevent(D,A),bel(D,E,F),pos(A,B,C).

A = [r]
D = [b, y]
E = 2/3+0
F = 0.666667
B = 1/3+0
C = 0.333333 

Yes
?- 
*/

%---------------------------------------------------
%   Part II.   Updating Belief Function
%---------------------------------------------------


% Updating bpa by Dempster-Shafer rule
%---------------------------------------------------
mass_of_intersection_with(A,B,C,Yq):-
   % mass of A which has intersection C with B.
   event(A),
   event(B),
   intersection(A,B,D),
   sort(D,C),
   mass(A,Yq,_).

update_mass([]/D,0,0):-event(D).
update_mass(E/D,Yq,Y):-
   event(E),
   \+ E = [],
   event(D),
   \+ D = [],
   findall((C,M),
     (
      sevent(B),
      mass_of_intersection_with(B,D,C,M),
      C \= []
     ),
   X),
   %length(X,NX),
   %nl,write(find(NX,X)),
   findall(Q,
     member((_,Q),X),
   BX0),
   sum(BX0,M0),
   sort(E,E1),
   findall(Q,
     member((E1,Q),X),
   BX1),
   sum(BX1,M1),
   Yq = M1 /M0,
   Y is Yq.

% Updating bel by Dempster-Shafer rule
%---------------------------------------------------
update_bel(E/D,Xq,X):-
   event(E),
   event(D),
%   \+ D = [],
   findall(Yq,
     (
      sevent(F),
      subset(F,E),
      update_mass(F/D,Yq,_Y)
     ),
   G),
   sum_of_fractions(G,Xq,X,_).

% sum of fractions switched by case of zero divisor
sum_of_fractions(G,Xq,X,no):-
   \+ (member(_/D,G),0 is D),
   sum_eq(G,Xq,X1),
   X is X1.

sum_of_fractions(G,Xq,X,yes):-
   member(_/D,G),
   0 is D,
   Xq = sum(G),
   X = indefinite.

% updating bel only when an event is of sorted states.

update_sbel(E/D,Xq,X):-
   sevent(E),
   sevent(D),
%   \+ D = [],
   update_bel(E/D,Xq,X).


% a comparison.
%               v(union(A,-B)) - v(-B)
% v_ds(A|B) =  ------------------------
%               1 - v(-B)

update_bel_1(E/D,Xq,X):-
   event(E),
   event(D),
%   \+ D = [],
   cevent(F,D),
   union(E,F,G),
   bel(F,XFq,_),
   bel(G,XGq,_),
   Yq = (XGq - XFq),
   Zq = (1 - XFq),
   sum_of_fractions([Yq/Zq],Xq,X,_).

%  a verification program code like as below may be convenient to you 
%  in order to verify the equality of three versions of update_bel.
%
%  ?- update_sbel(E/D,_,C),(update_bel_2(E/D,_,F)->true;F=non).

% another comparison.
%               v~(B) - v~(intersect(-A,B))
% v_ds(A|B) =  ----------------------------
%               v~(B)

update_bel_2(E/D,Xq,X):-
   event(E),
   event(D),
%   \+ D = [],
   cevent(F,E),
   intersection(D,F,G),
   pos(D,YDq,_),
   pos(G,YGq,_),
   Yq = (YDq - YGq),
   sum_of_fractions([Yq/YDq],Xq,X,_).


% Updating pos by Dempster-Shafer rule
%---------------------------------------------------
update_pos(E/D,Xq,X):-
   event(E),
   event(D),
   \+ D = [],
   findall(Yq,
     (
       sevent(F),
       intersection(F,E,M),
       M \= [],
       update_mass(F/D,Yq,_Y)
     ),
   G),
   sum_of_fractions(G,Xq,X,_).

update_spos(E/D,Xq,X):-
   sevent(E),
   sevent(D),
%   \+ D = [],
   update_pos(E/D,Xq,X).

% a code for test:
%
% ?- update_sbel(E/D,_,C),_,F),
%      cevent(G,E),(update_pos(G/D,_,F)->true;F=non).


% comparison
%               v~(intersection(A,B))
% v~ds(A|B) =  ------------------------
%               v~(B)

update_pos_1(E/D,Xq,X):-
   event(E),
   event(D),
   \+ D = [],
   intersection(D,E,G),
   pos(G,Yed,_),
   pos(D,Yd,_),
   sum_of_fractions([Yed/Yd],Xq,X,_).

% a code for test:
%
% ?- update_spos(A,_,C),_,F),
%      (update_pos_1(A,_,F)->true;F=non).


% Updating bel and pos by upper-lower probabilities conditioning
%----------------------------------------------------------------
/*
                 v(intersect(A,B))
  v_ul(A|B)  =  -----------------------------------------
                 v(intersect(A,B)) + v~(intersect(-A,B))
*/

update_bel_ul(E/D,Xq,X):-
   event(E),
   event(D),
   cevent(F,E),
   intersection(E,D,G),
   intersection(F,D,H),
   bel(G,YGq,_),
   pos(H,YHq,_),
   Yq = YGq,
   Zq = (YGq + YHq),
   sum_of_fractions([Yq/Zq],Xq,X,_).

/*
                 v~(intersect(A,B))
  v~ul(A|B)  =  -----------------------------------------
                 v~(intersect(A,B)) + v(intersect(-A,B))
*/

update_pos_ul(E/D,Xq,X):-
   event(E),
   event(D),
   cevent(F,E),
   intersection(E,D,G),
   intersection(F,D,H),
   pos(G,YGq,_),
   bel(H,YHq,_),
   Yq = YGq,
   Zq = (YGq + YHq),
   sum_of_fractions([Yq/Zq],Xq,X,_).

/*

% sample executions of updating for the example 1:

?- update_sbel(A/[b,y],_,B).

A = []
B = 0 ;

A = [r]
B = 0 ;

A = [b]
B = 0 ;

A = [y]
B = 0 ;

A = [b, r]
B = 0 ;

A = [r, y]
B = 0 ;

A = [b, y]
B = 1 ;

A = [b, r, y]
B = 1 ;

No
?- sevent(A),update_bel_ul(A/[b,y],_,B).

A = []
B = 0 ;

A = [r]
B = 0 ;

A = [b]
B = 0 ;

A = [y]
B = 0 ;

A = [b, r]
B = 0 ;

A = [r, y]
B = 0 ;

A = [b, y]
B = 1 ;

A = [b, r, y]
B = 1 ;

No
?- 

% Dempster-Shafer rule for the example 2 (Dow and Werlang,1992):

case 1)  H=[1]

v([1]|[1])
 = [v([1] U [1]c)-v([1]c)]/[1-v([1]c)] 
 = [v([1] U [2,3])-v([2,3])]/[1-v([2,3])]
 = (1-1/2)/(1-1/2)
 = 1
v([2]|[1])  = [v([2,3])-v([2,3])]/[1-v([2,3])]
 = 0.  
v([3]|[1])  = [v([2,3])-v([2,3])]/[1-v([2,3])]
 = 0.  
v([1,2]|[1]) = [v([1,2,3])-v([2,3])]/[1-v([2,3])]
 = 1.  
v([1,3]|[1]) = [v([1,2,3])-v([2,3])]/[1-v([2,3])]
 = 1.  
v([2,3]|[1]) = [v([2,3])-v([2,3])]/[1-v([2,3])]
 = 0.  
v([1,2,3]|[1]) = [v([1,2,3])-v([2,3])]/[1-v([2,3])]
 = 1.  
case 2)   H=[2,3]
v([1]|[2,3])  =[v([1] U [2,3]c)-v([2,3]c)] / [1-v([2,3]c)]
  = [ v([1] U [1])-v([1])]/[1-v([1])]
  =0.  
v([2]|[2,3]) = [v([1,2])-v([1])]/[1-v([1])]
  = 1/3.
v([3]|[2,3]) = [v([1,3])-v([1])]/[1-v([1])] 
  = 1/3.
v([1,2]|[2,3]) = [v([1,2])-v([1])]/[1-v([1])]
  = 1/3.
v([1,3]|[2,3]) = [v([1,3])-v([1])]/[1-v([1])]
  = 1/3.
v([2,3]|[2,3]) = [v([1,2,3])-v([1])]/[1-v([1])]
  = 1.
v([1,2,3]|[2,3]) = [v([1,2,3])-v([1])]/[1-v([1])]
  = 1.


*/

%
% -----------------------------------------------------------  %
%   Utilities for list operations
% -----------------------------------------------------------  %
%  edited: 14 Feb 2003. (imported from: set1.pl)
%
% equality for pair of set
% -----------------------------------------------------------  %
seteq(X,Y):-
   sort(X,Sort),
   sort(Y,Sort).
%
%
% descending/ascending natural number sequence less than N.
% -----------------------------------------------------------  %
dnum_seq([],N):-N<0,!.
dnum_seq([0],1).
dnum_seq([A|Q],N):-
   A is N - 1,
   length(Q,A),
   dnum_seq(Q,A).
anum_seq(Aseq,N):-dnum_seq(Dseq,N),sort(Dseq,Aseq).

dnum_seq1(Q,N):-
   M is N + 1,
   dnum_seq(Q0,M),
   subtract(Q0,[0],Q).
anum_seq1(Q,N):-
   M is N + 1,
   anum_seq(Q0,M),
   subtract(Q0,[0],Q).
%
% -----------------------------------------------------------  %
bag0([],_A,0).
bag0([C|B],A,N):-
   length([C|B],N),
   bag0(B,A,_N1),
   member(C,A).
zeros(Zero,N):-bag0(Zero,[0],N).
ones(One,N):-bag0(One,[1],N).
%
% bag1/3 : do not allow multiplicity
% -----------------------------------------------------------  %
bag1([],_A,0).
bag1([C|B],A,N1):-
  \+var(A),
  length(A,L),
  anum_seq(Q,L),
  member(N,Q),
  length(B,N),bag1(B,A,N),N1 is N + 1,
  member(C,A),\+member(C,B).

%
% ordering/3
% -----------------------------------------------------------  %
ordering(A,B,C):-bag1(A,B,C).

% a sequence of binary choice for a list:
%--------------------------------------------------
list_projection([],[],[]).
list_projection([X|Y],[_A|B],C):-
   X = 0,
   list_projection(Y,B,C).
list_projection([X|Y],[A|B],[A|C]):-
   X = 1,
   list_projection(Y,B,C).
%
% subset_of/3 : subset-enumeration 
% -----------------------------------------------------------  %
subset_of(A,N,As):-
   length(As,L),
   length(D,L),
   list_projection(D,As,B),
   length(B,N),
   sort(B,A).
%
% complementary list projection
%--------------------------------------------------
% added: 10 Jan 2003.
c_list_projection(X,Y,Z):-
   complement(X,XC,_N),
   list_projection(XC,Y,Z).

complement(X,XC,N):-
   \+ (var(X),var(N)),
   bag0(X,[1,0],N),
   zeros(Zero,N),
   ones(One,N),
   replace(X,Zero,One,XC).

% -----------------------------------------------------------  %
% Arithmetic 
% -----------------------------------------------------------  %
%
% evaluation of a nummerical value
% -----------------------------------------------------------  %
eval_number(X,X1):-
   X1 is X,
   number(X1).

%
% max,min
% -----------------------------------------------------------  %
max_of(X,[X]).
max_of(Z,[X|Y]):-
   max_of(Z1,Y),
   (X > Z1 -> Z=X; Z=Z1).
min_of(X,[X]).
min_of(Z,[X|Y]):-
   min_of(Z1,Y),
   (X < Z1 -> Z=X; Z=Z1).


% count frequency of occurence of the specified value of variable, M.
% -----------------------------------------------------------  %
% note: Both of M and L have to be specified.

counter(N,M,L):-
    length(L,_),
    findall(M,member(M,L),Mx),
    length(Mx,N).

% sum
% -----------------------------------------------------------  %
sum([],0).
sum([X|Members],Sum):-
   sum(Members,Sum1),
   Sum is Sum1 + X.

% added: 27 feb 2003.
sum_eq([],0,0).
sum_eq([X],X,X).
sum_eq([X|Members],Eq,Sum):-
   Members \= [],
   sum_eq(Members,Eq1,Sum1),
   Eq = Eq1 + X,
   Sum is Sum1 + X.
%
% product
% -----------------------------------------------------------  %
product([],1).
product([X|Members],Z):-
   product(Members,Z1),
  %number(X),
   Z is Z1 * X.


% end