
/*  Shelved on the 30th of July 1988                                       */


/*  [JNP...]                                                               */
/*  I've replaced %% comments by star-slash ones. To undo the replacement  */
/*  globally edit out all star-slashes, and replace all slash-stars by     */
/*  percents.                                                              */
/*                                                                         */
/*  Some Prologs may lack the (X->Y;Z) construction which this code        */
/*  uses. You can define it by:                                            */
/*      (X -> Y); Z  :-  X, !, Y.                                          */
/*      (X -> Y); Z  :- !, Z.                                              */
/*                                                                         */
/*  Other problems you may have with portability:                          */
/*                                                                         */
/*  (2)  current_predicate/2. This is not in Clocksin & Mellish Prolog,    */
/*       but several systems seem to provide it as a way to discover the   */
/*       predicates for which there are clauses in the database. Here's    */
/*       the description of the Poplog version, which appears to do what   */
/*       this code needs:                                                  */
/*                                                                         */
/*       ?- current_predicate(Name, Term).                                 */
/*                                                                         */
/*           Unifies Name with a currently defined predicate (for which    */
/*           clauses exist), and Term to be the most general term for that */
/*           predicate.                                                    */
/*                                                                         */
/*           Example:                                                      */
/*               ?- current_predicate(X, Y).                               */
/*               X = foo                                                   */
/*               Y = foo(_1, _2)                                           */
/*               More (y/n)? y                                             */
/*                                                                         */
/*               X = foo                                                   */
/*               Y = foo(_1)                                               */
/*                                                                         */
/*       If your system doesn't implement it, you'll have to find what     */
/*       clauses exist by examining the source file.                       */
/*                                                                         */
/*  (3)  setof/3. Again, this isn't in C&M, but it's widely implemented.   */
/*       You will need a version which meets the description below.        */                                                                  
/*       ?- setof(Pattern, Goal, Set).                                     */
/*                                                                         */
/*           This predicate is for finding the set of all          */
/*           solutions to the Goal.  The Set will be ordered using the     */
/*           standard ordering for terms.                                  */
/*                                                                         */
/*           For example:                                                  */
/*                                                                         */
/*           Given this database:                                          */
/*               parent(jules, jonathan).                                  */
/*               parent(helen, jonathan).                                  */
/*               parent(jules, paul).                                      */
/*               parent(helen, paul).                                      */
/*               parent(muriel, helen).                                    */
/*               parent(kingsley, helen).                                  */
/*                                                                         */
/*           We can ask "Who are the parents of whom?":                    */
/*                                                                         */
/*               ?- setof(Parent, parent(Parent, Child), Parents).         */
/*               Parent = _1                                               */
/*               Child = helen                                             */
/*               Parents = [kingsley, muriel] ?                            */
/*               More (y/n)? y                                             */
/*                                                                         */
/*               Parent = _1                                               */
/*               Child = paul                                              */
/*               Parents = [helen, jules] ?                                */
/*               More (y/n)? y                                             */
/*                                                                         */
/*               Parent = _1                                               */
/*               Child = jonathan                                          */
/*               Parents = [helen, jules] ?                                */
/*               More (y/n)? y                                             */
/*               no                                                        */
/*                                                                         */
/*           Note that we can backtrack on the values of the variables     */
/*           in the goal which are not part of the Pattern.  If there are  */
/*           no solutions, then the whole goal will fail:                  */
/*                                                                         */
/*               ?- setof(Parent, parent(Parent, jules), Parents).         */
/*               no                                                        */
/*                                                                         */
/*           By putting an existential quantifier in the question, we can  */
/*           ask "Who are parents?":                                       */
/*                                                                         */
/*               ?- setof(Parent, Child^parent(Parent, Child), Parents).   */
/*               Parent = _1                                               */
/*               Child = _2                                                */
/*               Parents = [helen, jules, kingsley, muriel] ?              */
/*                                                                         */
/*  (4)  eval_bin and eval_unary, at the end of this file, must be changed */
/*       to include any predicates your system may provide which           */
/*       treat their arguments as goals.                                   */
/*                                                                         */
/*  [...JNP]                                                               */


/*  These predicates can be used to analyze the static calling             */
/*  (invocation) structure of a Prolog program.                            */


/*  total_xref lists every current predicate within the system and,        */
/*  for each, shows which predicates it invokes.                           */

total_xref :-
    setof(CP, Goal^current_predicate(CP, Goal), Func_list),
    call_struct(Func_list, Xref_list),
    print_list(Xref_list).


/*  By setting arg1 of call_struct to a list of functors, you can get      */
/*  back a list of all the functors called by it, within the list.  No     */
/*  functors are reported which aren't in the list.                        */

call_struct(Functor_list, Caller_list) :-
    call_struct_1(Functor_list, Functor_list, Caller_list).


call_struct_1([], Functor_list, []) :- !.
call_struct_1([Func | Rest_func], Functor_list, [Func - Called_list |
 Rest_called]) :-
    (
        setof(Callee, calls(Func, Functor_list, Callee), Called_list)
     ->
        true
     ;
        Called_list = []
     ),
     call_struct_1(Rest_func, Functor_list, Rest_called).


calls(Functor, Functor_list, Callee/Arity) :-
    current_predicate(Functor, Predicate),
    clause(Predicate, Body),
    evaled_clause(Body, Callee/Arity),
    member(Callee, Functor_list).


/*  evaled_clause(Term, Pred) iff Term is a non-variable containing Pred   */
/*  in a position where it's evaluated, ie directly, or immediately below  */
/*  a control functor: ',', ';', '->', call, or 'not', as the second       */
/*  argument to bagof or setof.  That is, these are the sub-structures in  */
/*  Term which Prolog will attempt to prove.                               */

evaled_clause(Term, Func/Arity) :-
    nonvar(Term),
    eval_bin(Term, C1, C2),
    !,
    (evaled_clause(C1, Func/Arity); evaled_clause(C2, Func/Arity)).

evaled_clause(Term, Func/Arity) :-
    nonvar(Term),
    eval_unary(Term, C1),
    !,
    evaled_clause(C1, Func/Arity).

evaled_clause(Term, Func/Arity) :-
    nonvar(Term),
    functor(Term, Func, Arity).


eval_bin( (C1,C2),    C1, C2).
eval_bin( (C1;C2),    C1, C2).
eval_bin( (C1 -> C2), C1, C2).


eval_unary( (\+(C1)),  C1).
eval_unary( (not(C1)), C1).
eval_unary( call(C1),  C1).
eval_unary( setof(_, Pred, _), C1) :- existence_term(Pred, C1).
eval_unary( bagof(_, Pred, _), C1) :- existence_term(Pred, C1).


existence_term(X^P, Q) :- existence_term(P, Q), !.
existence_term(P, P).


%member(Elem, [Elem | _]).
%member(Elem, [_ | Rest_of_list]) :- member(Elem, Rest_of_list).


print_list([])      :- nl.
print_list([E | R]) :-
    nl, print(E), print_list(R).                                                                   
