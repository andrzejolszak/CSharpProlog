
% -----------------------------------------------------------  %
% Arithmetic and so on including probabilistic operators
% -----------------------------------------------------------  %
%  edited: 12 Jan 2003.
%  edited: 7 Feb 2003.
%  edited: 13 Feb 2003.
%  edited: 19 Feb 2003.
%  edited: 21 Feb 2003.
%  edited: 26,27 Feb 2003.
%  edited: 3 May 2003. selected_sum, project_sum (9 Feb).
%  edited: 7 June 2003. greater_than with the infinite (4 Apr) and eqsum(28 May).
%  edited: 7 July 2003. a solver (i.e., maximization of goal wrt arguments).
%  edited: 15 July 2003. computing basic statistics and entropy (cited from id3.pl).
%  edited: 16 July 2003. another solver (select_maximal/2).
%  edited: 27 July 2003. ranking by sort.
%  edited: 20 Aug 2003. max/2 extended for non-numerical cases.
%  edited: 13 Sep 2003. modify select_minimal/2. multiple-solution case.
%  edited: 15 Sep 2003 bug fix for min/2. 
%  edited: 16-7 Sep 2003 search process (cited from: lagran0.pl) 
%  edited: 21 Sep 2003 sepll (zeroes) 
%  edited: 8 Aug 2004 basic statistics, distribution, combinatorial (from dai0, beleq03) 
%  edited: 2 Mar 2005 inductive_numbers/1, sumof/3 (cited from: kglp01, epcn01) 
%  edited: 21 Sep 2005 rename. sumof/3 --> sumall/3 (reflected: price.pl) 


% descending sequence of integers
%---------------------------------------------------%

inductive_numbers([]).
inductive_numbers([N|H]):-
   length(H,N),
   inductive_numbers(H).

% an aggregator
%---------------------------------------------------%
% modified: 21 Sep 2005
% (sumof/3->sumall/3, and the two variations added.)

sumall(X,Goal,S):-
  findall(X,Goal,Z),
  sum(Z,S).

sum_setof(X,Goal,S):-
  setof(X,Goal,Z),
  sum(Z,S).

sum_bagof(X,Goal,S):-
  bagof(X,Goal,Z),
  sum(Z,S).

% basic statistics 
% -----------------------------------------------------------  %


average(U,G,A):-
   findall(U,G,B),
   length(B,N),
   sum(B,S),
   A is S/N.

stdev(U,G,Y):-
   average(U,G,A),
   findall((U-A)^2,G,B),
   length(B,N),
   sum(B,S),
   Y is S/(N-1).

natural_number_up_to(M,N):-
   (var(M)->max_of_alpha_plus_beta(M);true),
   M1 is integer(M),
   length(L,M1),
   nth1(N,L,_).

factorial_1(0,[]).
factorial_1(N,[N|F]):-
   integer(N),
   N >= 1,
   N1 is N - 1,
   factorial_1(N1,F).

% cf., combination with factorial with commitment.

combination_0(N,K,NCK):-
   integer(N),
   integer(K),
   N >= 0,
   N >= K,
   factorial(N,FN),
   factorial(K,FK),
   M is N - K,
   factorial(M,FM),
   NCK is FN / FK /FM.

factorial(1,1).
factorial(N,F):-
   integer(N),
   N > 1,
   N1 is N - 1,
   factorial(N1,F1),
   F is N * F1.


integer_between(K,[L,U]):-
   integer(L),
   integer(U),
   L =< U,
   M is U - L,
   length(X,M),
   nth1(J,[_|X],_),
   K is L + J - 1.


%%%%%%%% demo %%%%%%%%%

/*

?- N =4, findall(c(N,K)=B,(integer_between(K,[0,N]),
combination(N,K,B)),W).

N = 4
K = _G167
B = _G170
W = [c(4, 0)=1, c(4, 1)=4, c(4, 2)=6, c(4, 3)=4, c(4, 4)=1] 

Yes
?- 

*/



%
% evaluation of a nummerical value
% -----------------------------------------------------------  %
eval_number(X):-
   X1 is X,
   number(X1).


% comparison with the symbol of infinite value
% cited from: traveler.pl (10 Mar 2003)
% -----------------------------------------------------------  %
greater_than(Yw0, Yv0 + Cvw, Z, F):-
   Case1 = (Yv0 = infinite; Cvw = infinite), % RHS=infinite
   Case2 = (Yw0 = infinite; Yw0 > Yvw),
   (
    Case1-> (Z=Yw0, F=no)
     ;
      (
       Yvw is Yv0 + Cvw,
       (
        Case2 -> (Z=Yvw, F=yes)
         ;
          (Z=Yw0, F=no)
       )
      )
   ).


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

% a solver : maximization of goal wrt arguments
% -----------------------------------------------------------  %
% added: 7 July 2003 (cited from: network0.pl) 
% modified: 15 Sep 2003 bug fix for min/2. 

min(X,Goal):-
  max(Z,(Goal,Z is -X)).


max(X,Goal):-
  % X: the objective variable,
  % Goal: the objective function and constraints,
  setof((X,Goal),Goal,Z),
  member((X,Goal),Z),
  \+ (
    member((Y,_),Z),
    Y > X
  ).


/*

% modified: 20 Aug 2003 (cited from: design0.pl)
% mess

max(X,Goal):-
  % X: the objective variable,
  % Goal: the objective function and constraints,
  setof((X,Goal),Goal,Z),
  max_0(X,Z).

max_0(X,Z):-
  \+ ( member((AA,_),Z), \+ number(AA) ),
  member((X,_Goal),Z),
  \+ (
    member((Y,_),Z),
    Y > X
  ).

max_0(X,Z):-
  member((AA,_Goal),Z),
  \+ number(AA),
  sort(Z,Z1),
  rev(Z1,[(X,_)|_]).

*/

% ranking by sort
% -----------------------------------------------------------  %
% added: 27 July 2003 (cited from: dea0.pl) 

ranking(X,Goal,Ranking,ascend):-
  % X: the objective variable,
  % Goal: the objective function and constraints,
  setof((X,Goal),Goal,Z),
  sort(Z,Ranking),
  Ranking=[X|_].

ranking(X,Goal,Ranking,descend):-
  % X: the objective variable,
  % Goal: the objective function and constraints,
  ranking(X,Goal,Ranking0,ascend),
  reverse(Ranking0,Ranking).

forall_write(X,G):-
  forall(G,(nl,write(X))).

% a solver : find the most left maximal element with its index.
%-----------------------------------------
% added: 15 July 2003 (cited from Shoham's code: id3.pl) 
% modified: 13 Sep 2003.

select_minimal( [FirstPair|Remain], Best):-
  select_minimal_0( Remain, FirstPair, Best).
select_minimal_0( [ ], (A, _), A).  % A is the survived. 
select_minimal_0( [ (A, Value) | More ], ( _, Incumbent), Best) :-
  Value < Incumbent, !,  
  select_minimal_0( More, (A, Value), Best).
% to allow local minima
select_minimal_0( [ (A, Value) | More ], ( _, Incumbent), Best) :-
  Value = Incumbent,  
  select_minimal_0( More, (A, Value), Best).
select_minimal_0( [ _P | More ], (A, Value), Best) :-
  select_minimal_0( More, (A, Value), Best ).



% search process to find acceptable solutions
%--------------------------------------------
% added: 16 Sep 2003 (cited from: lagran0.pl) 

set_reservation_rule:-
   abolish(reservation_level/2),
   assert(
     (
      reservation_level(lower_incumbent,_X =< Z)
       :-
        collect_accepted(Bag),
        (
         Bag=[]-> Z is 10^15
          ; min(Z,member(Z,Bag))
        )
     )
   ).

/***************************************/
/*   acceptance and stopping rules     */
/***************************************/


% default
stop_time(100).

update_stop_time(N):-
   integer(N),
   abolish(stop_time/1),
   assert(stop_time(N)).

update_stop_time(_).

reservation_level(upper_incumbent,_X >= 100).
%reservation_level(lower_incumbent,_X =< 50).
acceptable_goal_pattern(goal_pattern).

accept_if([K0,X0,_],ok):-
   reservation_level(Bound,Rsrv),
   Rsrv=.. [_,X0,_],
   Rsrv,
   assert(search_data(accept(K0),reservation_level(Bound,Rsrv))),
   !.

accept_if([K0,_,G0],ok):-
   acceptable_goal_pattern(G0),
   assert(search_data(accept(K0),acceptable_goal_pattern(G0))),
   !.

accept_if(_,no).

stop_if([K0,_,_,_],stop):-
   stop_time(FT),
   K0 >= FT,
   assert(search_data(stop(K0),stop_time(K0))),
   !.

stop_if([K0,_,_,As],stop):-
   limit_of_acceptance(BLA),
   findall(A,search_data(accept(A),_),As),
   length(As,LA),
   LA >= BLA,
   assert(search_data(stop(K0),accept_limit(LA))),
   !.

stop_if(_,go_ahead).

/***************************************/
/*   search with multiple acceptance   */
/***************************************/

% default
limit_of_acceptance(1).

search_multiple(X,Goal,N,Bag):-
   (var(N)->N=1;true),
   abolish(limit_of_acceptance/1),
   assert(limit_of_acceptance(N)),
   search(X,Goal),
   !,
   collect_accepted(Bag0),
   sort(Bag0,Bag).

collect_accepted(Bag):-
   findall(A,
     (
      search_data(accept(T),_),
      search_data(log(T),(A,_))
     ),
   Bag).


/***************************************/
/*      base  model  of  search        */
/***************************************/

search(X,Goal):-
   initialize_search_data,
   search_0(T,X,Goal,_),
   stop_if([T,X,Goal,_],stop). % modified to allow multiple acceptance.

search(_,_):-
   terminate_search.

search_0(T,X,Goal,Accept):-
   Goal,
   update_search_data(T,X,Goal),
   accept_if([T,X,Goal],Accept). % acceptance decision is separated from stopping.

search_0(_,_X,_Goal,stop).

initialize_search_data:-
   abolish(search_data/2),
   assert(search_data(current(0),(0,start))).

update_search_data(K,X,Goal):-
   retract(search_data(current(K0),G0)),
   assert(search_data(log(K0),G0)),
   K is K0 + 1,
   assert(search_data(current(K),(X,Goal))).

terminate_search:-
   retract(search_data(current(K0),G0)),
   assert(search_data(log(K0),G0)),
   assert(search_data(terminate(K0),(_,end))),
   nl, 
   write('End'),
   listing(search_data/2).



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
  %number(X),
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
% symbolic representation of sum. eqsum/2 and reqsum/2
% cited from: eba01.pl (28 May 2003) 
% -----------------------------------------------------------  %
eqsum([],0).
eqsum([X|Members],Sum):-
   eqsum(Members,Sum0),
   (
    X=0 -> Sum = Sum0
     ;
    (
     Sum0=0 -> Sum = X
      ;
     Sum = Sum0 + X
    )
   ).
reqsum(A,B):-
   reverse(A,C),
   eqsum(C,B).

%
% product
% -----------------------------------------------------------  %
product([],1).
product([X|Members],Z):-
   product(Members,Z1),
  %number(X),
   Z is Z1 * X.
%
% weighted sum
% -----------------------------------------------------------  %
product_sum([],[],[],0).
product_sum([P|Q],[A|B],[E|F],V):-
    length(Q,N),
    length(B,N),
    product_sum(Q,B,F,V1),
    E is P * A,
    V is V1 + E.
%
% product sum value with equational
% -----------------------------------------------------------  %
product_sum_eq([],[],[],0,0).
product_sum_eq([P|Q],[A|B],[E|F],V,Vq):-
    length(Q,N),
    length(B,N),
    product_sum_eq(Q,B,F,V1,Vq1),
    Eq = (P) * A,
    E is Eq,
    (Vq1=0 -> Vq = Eq; Vq = Vq1 + Eq),
    V is V1 + E.
%
% selected sum
% -----------------------------------------------------------  %
% added: 3 May 2003. cited from: coop.pl(9 Feb 2003)
selected_sum(Y/N,B/A,RX):-
   findall(AJ,
     (
      member(J,Y),
      nth1(K,N,J),
      nth1(K,A,AJ)
     ),
   B),
   sum(B,RX).
%
% projected sum
% -----------------------------------------------------------  %
projected_sum(M,A,Cols):-
   index_of_tuple(M,B,Cols),
   sum(B,A).
%
% allocation
% -----------------------------------------------------------  %
allocation(N,A,[X|Y]):-
    allocation(N,A,A,[X|Y]).
allocation(0,_,0,[]).
allocation(N,A,B,[X|Y]):-
    integer(A),
    length([X|Y],N),
    allocation(_N1,A,B1,Y),
    % N1 is N - 1,
    % sum(Y,B1),
    K is A - B1 + 1,
    length(L,K),
    nth0(X,L,X),
    B is B1 + X.
%
% probability (percentile) by using allocation
% -----------------------------------------------------------  %
probabilities(0,[]).
probabilities(N,[X|Y]):-
    integer(N),
    length([X|Y],N),
    allocation(N,100,[X|Y]).
% 
% any ratio (weight) can be interpreted into a prob.
% -----------------------------------------------------------  %
scale(W,1/Z,P):-
    findall(Y,(nth1(_K,W,X),Y is X/Z),P).
probabilities(W,N,P):-
    length(W,N),
    sum(W,Z),
    scale(W,1/Z,P).
%
% degenerate probability
%---------------------------------------------
degenerate(Ps):-
   nth1(K,Ps,P),
   characteristic_vector(K,P,Ps,Ps).
% 
% probability over base set with steps of levels.
% -----------------------------------------------------------  %
make_a_prob(P,base(M),steps(L)):-
    var(P),
    length(P,M),
    allocation(M,L,W),
    probabilities(W,M,P).
make_a_prob(P,base(M),_):-
    \+ var(P),
    length(P,M),
    \+ (
     member(P1,P),
     (
      var(P1);
      P1 > 1;
      P1 < 0
     )
    ),
    sum(P,1).
%
% expected value
% -----------------------------------------------------------  %
expected_value(W,A,E/100):-
    length(A,N),
    probabilities(W,N,P),
    product_sum(P,A,_,E).
%
% expected value with equational
% -----------------------------------------------------------  %
expected_value_eq(W,A,E/100,Eq):-
    length(A,N),
    probabilities(W,N,P),
    product_sum_eq(P,A,_,E,Eq).

%
%---------------------------------------------------
%  net present value (NPV) 
%---------------------------------------------------
%
%  time preference: discount factor
%---------------------------------------------

interest_rate(1.1).

discount_factor(R,Y,DF,DFV):-
   DF = R ^ (-Y),
   DFV is DF.

npv(A,Y,Eq,V):-
   interest_rate(R),
   discount_factor(R,Y,DF,_),
   Eq = DF * A,
   V is Eq.

%
% conditional probabilities
% -----------------------------------------------------------  %
probability_of_event(W,E,P):-
    % conditionalization by event specified directly
    event(E),
    (var(E)->E = E1; sort(E,E1)),
    G = member(S,E1),
    findall(A,(probability(W,S,A),G),Ps),
    sum(Ps,P).
probability_of_event(W,E,P,G):-
    \+ var(G), % conditionalization via constraints indirectly
    G=(Goal,M,[W,S,A]),  % constraints with params
    findall([S1,A1],
      (
       (M=do->(W=W1,S=S1,A=A1);true),
       probability(W1,S1,A1),
       Goal
      ),
    Xs),
    findall(S,member([S,A],Xs),E0),
    findall(A,member([S,A],Xs),Ps),
    sort(E0,E),
    sum(Ps,P).
%
% -----------------------------------------------------------  %
%   Utilities for list operations
% -----------------------------------------------------------  %
%
% index for tuples.
% -----------------------------------------------------------  %
% 1) only mention for a direct product of sets.
index_of_tuples(B,A,Index):-
   \+ var(B),
   \+ var(A),
   length(B,LN),  % base sets
   length(A,LN),  
   length(Index,LN),
   findall(L,
     (
      nth1(K,B,BJ), %write(a(K,B,BJ)),
      nth1(L,BJ,AJ),%write(b(L,BJ,AJ)),
      nth1(K,A,AJ)  %,write(c(K,A,AJ)),nl
     ),
   Index).
index_of_tuples(B,A,Index):-
   \+ var(B),
   \+ var(Index),
   var(A),
   length(B,LN),  % base sets
   length(Index,LN),
   length(A,LN),  
   findall(AJ,
     (
      nth1(K,B,BJ),
      nth1(K,Index,L),
      nth1(L,BJ,AJ)
     ),
   A).
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
% inquire the goal multiplicity
% -----------------------------------------------------------  %
sea_multiple(Goal,Cond,N,M):-
  Clause=..Goal,
  findall(Cond,Clause,Z),length(Z,N),sort(Z,Q),length(Q,M).
%
bag0([],_A,0).
bag0([C|B],A,N):-
   length([C|B],N),
   bag0(B,A,_N1),
   member(C,A).
zeroes(Zero,N):-bag0(Zero,[0],N).
ones(One,N):-bag0(One,[1],N).
%
% subset_of/3 : subset-enumeration 
% -----------------------------------------------------------  %
subset_of(A,N,As):-
   length(As,L),
   length(D,L),
   list_projection(D,As,B),
   length(B,N),
   sort(B,A).
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
% characteristic_vector/3
% -----------------------------------------------------------  %
% modified: 8 Feb 2003.  without using nth1.
% modified: 13 Feb 2003.  without using member.
characteristic_vector(X,B,Index):-
   \+ var(B),
   %member(X,B),
   list_projection(Index,B,[X]).
characteristic_vector(1,X,[X|B],[1|DX]):-
   characteristic_vector(X,[X|B],[1|DX]).
characteristic_vector(K,X,[_|B],[0|DX]):-
   characteristic_vector(K1,X,B,DX),
   K is K1 + 1.

%
% -----------------------------------------------------------  %
%   Utilities for outputs
% -----------------------------------------------------------  %
%
% write and new line.
% -----------------------------------------------------------  %
wn(X):-write(X),nl.
%
% output to file.
% -----------------------------------------------------------  %
tell_test(Goal):-
  open('tell.txt',write,S),
  tell('tell.txt'),
  Goal,
  current_stream('tell.txt',write,S),
  tell(user),wn(end),
  close(S).
%



% computing basic statistics and entropy
%---------------------------------------------
% added : 15 July 2003. cited from id3.pl

projected_values(Kth,Attr,Data0, [V1|Col], PosValues ):-
  \+ var(Data0),
  Data0=[D1|Data],
  nth1(Kth,D1,C1),
  C1=(Attr,V1),
  Cell=(Attr,Value),
  findall(Value,
    (
     member(Row,Data),
     member(Cell,Row)
    ),
  Col),
  sort([V1|Col],PosValues).

histogram([], [], [] ).
histogram([X|PosValues], SelectedData, [(X,N)|Dist] ):-
  findall(X, member(X,SelectedData), Xs),
  length(Xs,N),
  subtract(SelectedData,[X],NewSelectedData),
  histogram(PosValues, NewSelectedData, Dist ),!.
  

compute_set_entropy( Data, Entropy ) :- 
  projected_values(1,_TargetAttr,Data, AllValues, PosValues ),
  compute_set_entropy_1( PosValues, AllValues, Entropy ).

compute_set_entropy_1( PosValues, AllValues, Entropy ) :- 
  \+ var(AllValues),
  length( AllValues, Dnum ),
  (var(PosValues)->sort(AllValues,PosValues);true),
  histogram( PosValues, AllValues, Dist ),
  %nl,write(Dist),
  findall(PvLogPv,
    (
     member((_Value,Vnum),Dist),
     Pv is Vnum / Dnum,
     xlogx( Pv, PvLogPv )
    ),
  PLPs),
  sum(PLPs,NegEntropy),
  Entropy is - NegEntropy.

xlogx( X, N) :- X is 0.0E+00, !, N = 0.
xlogx( X, N) :- X \= 0, N is X * log(X).


write_all_histograms(L,D):-
  nl,write(histogram),
  nl,write('---------'),
  forall(nth1(K,L,_X),
    (
     projected_values(K,Attr,D, AllValues, PosValues ),
     histogram( PosValues, AllValues, Dist ),
     nl,tab(2),write(Attr),
     forall_print((V,F),Dist,
       [
       (nl, tab(5),align(left,15,V)), 
       true,
       (write(' |'),nstars(F),write('+'),write([F]))
       ]
     )
    )
  ),
  nl.

% forall print.
%-------------------------------------
% added: 15 July 2003.
forall_print(Y,X,[PrePrint,PostPrint]):-
  forall(member(Y,X),
    (
     PrePrint,
     write(Y),
     PostPrint
    )
  ).
forall_print(Y,X,[PrePrint,Goal,PostPrint]):-
  forall(member(Y,X),
    (
     PrePrint,
     Goal,
     PostPrint
    )
  ).



% prity print.
%-------------------------------------
% added: 13 July 2003.

align(left,N,M):- pp2(N,M).
align(right,N,M):- pp(N,M).

% left align
pp2(0,_,_).
%pp2(0,[],[]).
%pp2(0,Y,_Z):-   Y \= [],   pp2(0,[],[]).
pp2(N,[X|Y],[X|Z]):-
   N > 0,
   N1 is N -1,%(N1=0->trace;true),
   pp2(N1,Y,Z).
pp2(N,[],[' '|Z]):-
   N > 0,
   N1 is N -1,
   pp2(N1,[],Z).
pp2(N,X):-
   list_name(X,_Y,X1),
   pp2(N,X1,Z),
   list_name(Q,_W,Z),
   write(Q).
list_name0([],[]).
list_name0([X|Y],[Z|W]):-
   name(Z,[X]),
   list_name0(Y,W).

list_name(X,W,Y):-
   \+ var(X),
   name(X,W),
   list_name0(W,Y).
list_name(X,W,Y):-
   var(X),
   (\+ var(Y); \+ var(W)),
   list_name0(W,Y),
   name(X,W).

nstars(N):-
  length(L,N),
  forall(member(_X,L),write('*')).
