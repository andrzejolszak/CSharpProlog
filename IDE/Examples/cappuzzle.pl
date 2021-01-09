 
	Solving Crypto-Arithmetic Puzzles with Prolog 
 
			Ray Reeves  
 
 
By crypto-arithmetic puzzles is meant the kind where some arithmetic  
calculation is shown as a pattern of abstract digits, and the puzzle is to  
identify the digits that fit the pattern and represent a valid arithmetic  
tabulation.  Prolog has a positional notation for aggregates that is  
particularly suitable for representing problems of this kind. 
 
The domain of digits is 0-9, and the convention is that different variables 
represent different digits, so in fact the digits can be ``dealt'' to the 
variables as a ``hand'' is dealt from a pack of cards, and this will ensure  
there are no duplications.  The hand can then be tested against the  
constraints implicit in the problem.  If this test fails, Prolog will back  
up and deal another hand.  However, factorial ten is about three and a half 
million, and this process could take a while; what is needed is a tighter  
integration of dealing and testing to prune the possible permutations at  
the earliest possible moment. 
 
Consider the Magic Square of order three.  We ask how could the following 
pattern of non-zero digits be instantiated to add up to the same sum along 
each row, column and diagonal? 
 
        A B C 
        D E F 
        G H I 
 
The constraints are: 
 
Sum is A+B+C. Sum is D+E+F. Sum is G+H+I. 
Sum is A+D+G. Sum is B+E+F. Sum is C+F+I. 
Sum is A+E+F. Sum is G+E+C. 
 
By symmetry, it can be seen that if there any solutions at all then there  
are several, by reflection about four different axes.  There is always an  
element of subjectivity in programmatic puzzles concerning what is ``fair'' 
to add to the constraints to expedite the solution and still claim the  
automaton did it.  In this case we add the constraints that A<C, A<G,  
A<I and G<C to avoid seeing the symmetric variants. 
 
To deal a full hand of digits and only then test the constraints is to  
invite the afore-mentioned combinatorial explosion.  A better strategy is  
to compute the Sum by drawing three digits from the pack, say A,E,I, and  
then, by drawing one more digit, say G, compute the three more digits  
D,H,C that that entails, and then indeed, all the rest of the digits that  
they entail.  This is much faster than waiting to be dealt the correct hand. 
Of course, we still have to thrash through the combinations to get a  
feasible first four digits. 
 
Constructing digits rather than drawing them from the pack does not  
guarantee the integrity of the result, and there are likely to be  
duplicates or values outside the permitted domain.  However, by immediately 
trying to draw the constructed value from the pack it can thereby be  
validated.  The following Prolog code will draw a card from the pack  
(ie: an item from the list), and also return the modified pack: 
 
draw(X,[X|L],L). 
draw(X,[Y|L],[Y|Rest]):- draw(X,L,Rest). 
 
The first clause draws the first card in the pack, and if that turns out to 
be unsatisfactory, then some succeeding goal will fail and back up to here. 
Then the second clause draws some other card.  In the event that the card,  
X, has already been constructed (instantiated), then the pack is searched  
for that card without proceeding further. 
 
It can now be seen that there is an advantage to be obtained by pre-sorting  
the pack, for if we are searching for an instantiated card then that search  
can be abandoned as soon as the proper lexical slot has been passed.   
Indeed, if if the pack is at all large, then a more suitable structure than 
a list should be employed. 
 
Some puzzles are based on a domain of digits with repetitions, and in the  
event that an uninstatiated card is drawn and it leads to failure, there  
is no point in drawing another copy of it.  An ordered pack makes it easy  
to avoid this redundancy. 
 
An improved draw program looks like this: 
 
draw(X,L1,L2):- 
   var(X)->pick(X,L1,L2;delete(X,L1,L2). 
 
pick(X,[Y,Y|L],[Y|Rest]):- !, 
     pick(X,[Y|L],Rest). 
pick(X,[X|L],L). 
pick(X,[Y|L],[Y|Rest]):-  
   pick(X,L,Rest). 
 
delete(X,[X|L],L). 
delete(X,[Y,Z|L],[Y|Rest]):- 
   X>=Z,delete(X,[Z|L],Rest). 
 
 
Uninstantiated draws will use pick, which strips off leading duplicates in 
the first clause.  Instantiated draws will use delete, which tests the pack 
to see if success is still feasible. 
 
The draw program is frequently used as a basis for complete permutations 
as follows: 
 
deal([],[]). 
deal([A|B],[C|D]):-  
    draw(C,[A|B],E),deal(E,D). 
 
In 1987, there were still those (TRILOGY User's Manual, Complete Logic  
Systems, 741 BlueRidge Ave., North Vancouver, B.C. Canada V7R 2J5.) 
who believed that in Prolog it is necessary to perform a complete deal  
before testing constraints such as those in the Magic Square; but drawing  
the minimum number of cards at a timeand  constructing values instead of  
drawing them, together with the above optimisations can make orders of  
magnitude improvement in solution times. 
 
There is a well-known interface to Prolog called the Definite Clause Grammar 
(dcg) interface which provides a way of expressing intentions in terms of 
parameterized grammar rules and semantic actions.  It exploits the special 
data type peculiar to logic languages called the difference list, to  
automatically manage the token stream in the way implied by the rules, and  
thereby take a burden off the programmer. 
 
We have an analagous situation here.  The state of the pack before and  
after drawing a card is represented in the pick clause by the card, the  
before list and the after list.  Certainly we are talking about two lists  
and the difference between them, although by that we mean arbitrary 
differences and not just those that can be described by consing an item on 
to a list.  In the course of applying constraints we shall need to chain 
the before and after states together.  Why not take a leaf from dcg's book  
and define a ``grammar'' for crypto-arithmetic puzzles in order to simplify  
the work? 
 
Instead of writing: 
 
magic:- 
     L1=[1,2,3,4,5,6,7,8,9], 
     draw(A,L1,L2), 
     draw(E,L2,L3), 
     draw(I,L3,L4),A<I, 
     Sum is A+E+I, 
     draw(G,L4,L5),A<G, 
     C is Sum-(G+E),A<C,G<C, 
     draw(C,L5,L6), 
        ... 
 
let us more concisely write: 
 
magic:- 
     solve([1,2,3,4,5,6,7,8,9],[]). 
      
solve <--           
     [A,E,I],A<I, 
     Sum is A+E+I, 
     [G],A<G, 
     C is Sum-(G+E),A<C,G<C,[C], 
     B is Sum-(C+A),[B], 
     D is Sum-(A+G),[D], 
     F is Sum-(D+E),[F],Sum is C+F+I, 
     H is Sum-(B+E),[H],Sum is G+H+I). 
 
and let the special rule interpreter (shown in the Appendix) augment this 
form to Prolog.  Notice that the two arguments to solve which constitute  
the difference list, are not referred to in the rule; but they are, in fact, 
manipulated each time a term, or list of terms, is referenced.  Our  
intention in referencing terminals is not to pop a token from the head of  
the list but to draw a card from the pack in the manner previously  
described, so we cannot actually use the dcg interface. 
 
The interpreter is called ``cap'', for crypto-arithmetic processor, and  
programs written in this condensed language bear the cap extension to their 
names.  Cap programs have the great advantage that if we wish to experiment  
with various order of constraints we do not have to re-thread the difference 
lists; the cap interpreter will do that for us. 
 
The left arrow as the neck operator signifies that this is a grammatical  
sentence, not a regular clause, and is subject to translation.  Unlike dcgs 
our code is mostly actions (constraints) not non-terminal goals, so we  
reverse the dcg convention and use braces to denote non-terminal goals and  
leave actions explicit.  In this and the following examples there are no  
non-terminal goals, but in the event that there were, then they would be  
automatically linked in to the difference list chain. 
 
Consider the following typical crypto-arithmetic sum: 
 
       AND 
        TO 
       ALL 
         A 
      GOOD 
     ----- 
     NIGHT 
 
With this many rows it is a poor choice to evaluate each row as a  
polynomial, because few constraints are encountered until the final row.   
It is better to sum by columns, just as you would by hand.  To this end a  
totaliser is needed which explicitly produces the sum and carry digits,  
and this has been built in to cap as a utility called ``evaluate''. 
 
evaluate(Exp,Carry,Result):- 
     X is Exp,  
     Carry is X//10,  
     Result is X mod 10. 
 
Using this, all four solutions are quickly found. 
 
goodnight:-  
     solve([0,1,2,3,4,5,6,7,8,9],_). 
 
solve <-- 
     [A],A>0,[D,O,L], 
     evaluate(D+L+O+D,Carry1,T), 
     [T],T>0, [N],N>0, 
     evaluate(Carry1+N+T+L+O,Carry2,H), 
     [H], 
     evaluate(Carry2,A+A+O,Carry3,G), 
     [G],G>0, 
     evaluate(Carry3+G,N,I), 
     [I]. 
 
Notice that the second variable in the initial difference list was not the 
empty list this time but a ``don't care'' variable, since not all the 
digits are needed.  This variable is left with the unused ones. 
 
For problems where polynomial evaluation is preferred, the Prolog 
primitive ``name'' is almost what is required.  Name will explode an atom 
to its list of characters or vice-versa. 
 
The following program, called decname, will do this for decimal numbers: 
 
decname(0,[0]).                     %1 
decname(Int,List):-                 %2 
        decname(Int,[],List).           
decname(0,X,X).                     %3 
decname(Int,Acc,List):-             %4 
        nonvar(Int),X is Int//10, 
        Y is Int mod 1, 
        decname(X,[Y|Acc],List), 
decname(Int,Acc,List):-             %5 
        decname(X,Y|Acc],List), 
        Int is 10*X+Y. 
 
Clause 1 is a special case for the value zero.  Clause 2 introduces an 
empty accumulator.  Clause 3 is the base case which terminates recursion  
and unifies the accumulator with the list.  Clause 4 is a straightforward  
integer to list conversion similar to evaluate.  Clause 5 is a somewhat  
unusual method of converting a list to an integer.  It is not tail  
recursive; on the contrary it uses left recursion to push uninstantiated  
variables on to the accumulator until it is a list of the same length as  
the given list, at which point clause 3 will unify them.  Then it builds  
the integer as it returns. 
 
The following cap program illustrates its use: 
 
     EAU*EAU=OCEAN 
 
ocean:-  
     solve([0,1,2,3,4,5,6,7,8,9],X). 
 
solve <--- 
     [E],E>0,[A,U], 
     decname(EAU,[E,A,U]), 
     OCEAN is EAU*EAU, 
     decname(OCEAN,[O,C,E,A,N]),O>0, 
     [O,C,N]. 
 
Here is a multiplication problem attributed to a book of puzzles by  
Prof. Schuh, which shows no pattern of digits but has the constraint 
that every digit appears precisely twice.  Since there are twenty 
placeholders evidently every digit is used. 
 
        *** 
        *** 
        --- 
        *** 
       ***      
      ***      
      -----      
      *****      
 
Here is a cap program to solve it: 
 
                A2  A1  A0 
                B2  B1  B0 
                ---------- 
               R12 R11 R10 
           R23 R22 R21  
       R34 R33 R32 
       ------------------- 
        C4  C3  C2  C1  C0 
 
schuh:-  
     solve( [0,0,1,1,2,2,3,3,4,4, 
             5,5,6,6,7,7,8,8,9,9],[]). 
 
solve <-- 
     [B0],B0>0,[A0],A0>0,           
     evaluate(A0*B0,Cprod10,R10), 
     C0=R10,[C0],[R10], [A1],A1>0, 
     evaluate(A1*B0+Cprod10,Cprod11,R11), 
     [R11], [A2],A2>0, 
     evaluate(A2*B0+Cprod11,0,R12), 
     R12>0,[R12], [B1],B1>0, 
     evaluate(A0*B1,Cprod21,R21), 
     [R21], 
     evaluate(A1*B1+Cprod21,Cprod22,R22), 
     [R22], 
     evaluate(R11+R21,Csum1,C1), 
     [C1], 
     evaluate(A2*B1+Cprod22,0,R23), 
     R23>0,[R23], [B2],B2>0, 
     evaluate(A0*B2,Cprod32,R32), 
     [R32], 
     evaluate(Csum1+R12+R21+R32,Csum2,C2), 
     [C2], 
     evaluate(A1*B2+Cprod32,Cprod33,R33), 
     [R33], 
     evaluate(Csum2+R23+R33,Csum3,C3), 
     [C3], 
     evaluate(A2*B2+Cprod33,0,R34), 
     R34>0,[R34], 
     evaluate(Csum3+R34,0,C4), 
     [C4].      
 
The coding simplification afforded by the cap dialect is quite welcome  
here. 
 
The next problem is similar except that there is no constraint on the 
frequency of occurrence of a particular digit.  For this we need to use 
a pack that does not get diminished by each draw, and we will denote this 
by a Keene closure operator '+' in front of the pack.  In Prolog,  
arithmetic operators do not need to be declared and can be used in other 
contexts.   
 
Only prime digits allowed: 
 
      *** 
       ** 
     ---- 
     **** 
    **** 
    ----- 
    ***** 
 
Solution: 
 
          A2  A1  A0 
              B1  B0 
     --------------- 
     R13 R12 R11 R10 
 R24 R23 R22 R21  
 ------------------- 
  C4  C3  C2  C1  C0 
 
primes:- solve_primes(+[2,3,5,7],_) 
 
solve_primes <-- 
     [B0],[A0], 
     evaluate(A0*B0,Cprod10,R10), 
     R10=C0,[C0], [A1], 
     evaluate(A1*B0+Cprod10,Cprod11,R11), 
     [R11], [A2], 
     evaluate(A2*B0+Cprod11,R13,R12), 
     [R12],[R13], [B1], 
     evaluate(A0*B1,Cprod20,R21), 
     [R21], 
     evaluate(R11+R21,Csum1,C1), 
     [C1], 
     evaluate(A1*B1+Cprod20,Cprod21,R22), 
     [R22], 
     evaluate(R12+R22+Csum1,Csum2,C2), 
     [C2], 
     evaluate(A2*B1+Cprod21,R24,R23),[R23], 
     [R24], 
     evaluate(Csum2+R13+R23,Csum3,C3), 
     [C3], 
     evaluate(Csum3+R24,0,C4), 
     [C4]. 
 
Finally, we consider a more general 'wild card' problem where a star  
denotes a digit in the range 0 to 9 but is not otherwise constrained, but  
at the same time there are labelled digits constrained to be different to  
each other.  This,of course, is easily supported by using separate packs  
for the stars and the labelled digits, but an augmented syntax is needed to 
indicate which pack we are drawing from.   We use prefix operators '+' and  
'*' in front of a list to draw from a 'star' pack, and continue to use an  
unembellished list to draw from the diminishing pack.  The '*' operator  
denotes a draw of any digit including zero, while the '+' operator denotes  
a draw from the range 1 to 9, which is a frequent constraint. 
 
          FUN  
           IN 
         ---- 
          *** 
         *** 
         ---- 
         FACT 
 
Solution:  
           F   U   N 
               I   N 
         ---------- 
         R12 R11 R10 
     R23 R22 R21  
     --------------- 
       F   A   C   T  
 
fun:- solve_fun([0,1,2,3,4,5,6,7,8,9],_). 
 
solve_fun <-- 
     [N],N>0, 
     evaluate(N*N,Cprod10,R10), 
     R10=T,[T], [U], 
     evaluate(U*N+Cprod10,Cprod11,R11), 
     *[R11], [F],F>0, 
     evaluate(F*N+Cprod11,0,R12), 
     +[R12], [I],I>0, 
     evaluate(N*I,Cprod21,R21), 
     +[R21], 
     evaluate(R11+R21,Csum1,C), 
     [C], 
     evaluate(U*I+Cprod21,Cprod22,R22), 
     *[R22], 
     evaluate(R12+R22+Csum1,Csum2,A), 
     [A], 
     evaluate(F*I+Cprod22,0,R23), 
     +[R23], 
     evaluate(Csum2+R23,0,F). 
 
 
The ubiquitous SEND+MORE=MONEY problem will be left as an excercise! 
 
 
Several of these problems were remembered from  Honeywell's in-house 
journal, and others came from the Trilogy User's Manual, as did the  
incentive to refute the canard that the Prolog langauge cannot be efficient. 
 
 
APPENDIX. 
 
% cap code 
 
:- op(1200,xfx,(<--)). 
 
cap(File):-  
     seeing(Old),see(File), 
     consume, 
     seen,see(Old). 
 
consume:- repeat,read(X), 
     (X==end_of_file,!; 
     process(X),fail). 
 
process((L <-- R)):-                               
     explhs(L,S0,S,P),  
     exprhs(R,S0,S,Q), 
     assertz((P:-Q)),!. 
process((:- G)):- !,G.                             
process((P :- Q)):- !, 
     assertz((P :- Q)). 
process(P):- assertz(P). 
 
explhs(L,S0,S,Y):-  
     augment(L,S0,S,Y).                
 
exprhs((X1,X2),S0,S,Y):- !, 
     exprhs(X1,S0,S1,Y1), 
     exprhs(X2,S1,S,Y2), 
     and(Y1,Y2,Y). 
exprhs((X1;X2),S0,S,(Y1;Y2)):- !, 
     exprhs(X1,S0,S,Y1), 
     exprhs(X2,S0,S,Y2). 
exprhs(L,S0,S,Y):- islist(L),!, 
     explist(L,S0,S,Y).    
exprhs({X},S0,S,Y):- !, 
     augment(X,S0,S,Y).     
exprhs(X,S,S,X).                              
 
explist([],S,S,true). 
explist([X],S0,S,draw(X,S0,S)).                 
explist([X|L],S0,S,(draw(X,S0,S1),Y)):-  
     explist(L,S1,S,Y). 
 
augment(P,S0,S,Q):-  
     P=..[F|Args0], 
     append(Args0,[S0,S],Args), 
     Q=..[F|Args]. 
 
* [X]:- draw(X,[0,1,2,3,4,5,6,7,8,9],_). 
 
+ [X]:- draw(X,[1,2,3,4,5,6,7,8,9],_).      
 
draw(X,+Y,+Y):- !,draw(X,Y,_).           
draw(X,Y,Z):- (var(X),!,pick(X,Y,Z); 
               delete(X,Y,Z)). 
 
pick(X, [Y,Y|L], [Y|Rest]):- !, 
     pick(X,[Y|L], Rest). 
pick(X, [X|L], L). 
pick(X, [Y|L], [Y|Rest]) :-  
     pick(X, L, Rest). 
 
delete(X, [X|L], L):- !. 
delete(X, [Y,Z|L], [Y|Rest]) :-       
     X>=Z,                     
     delete(X, [Z|L], Rest). 
 
evaluate(Exp,Carry,Result):-  
     X is Exp,  
     Carry is X//10,  
     Result is X mod 10. 
 
decname(0,[0]).                     
decname(Integer,List):- 
     decname(Integer,[],List). 
 
decname(0,X,X). 
decname(Integer,Acc,List):- 
     nonvar(Integer), 
     X is Integer//10, 
     Y is Integer mod 10, 
     decname(X,[Y|Acc],List). 
decname(Integer,Acc,List):- 
     decname(X,[Y|Acc],List), 
     Integer is 10*X+Y. 
 
islist([]). 
islist([_|_]). 
 
append([],L,L):- !. 
append([X|Y],L,[X|Z]):- append(Y,L,Z). 
 
 
 
 
 
 
 
 
 
 

