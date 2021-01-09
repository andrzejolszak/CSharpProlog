% 1 - goal reduced
vanilla(true) :- 
     !.
% 2 - conjunction of goals
vanilla((Goal, Goals)) :-
     !,
     vanilla(Goal),
     vanilla(Goals).


/*  first version 
% 3 - user-defined procedure
vanilla(Goal) :-
    clause(Goal, Subgoals),
    vanilla(Subgoals).
*/
/* stage one addition to include inbuilts  
% 4 - inbuilt predicate
vanilla(Goal) :-
     call(Goal).
*/

% 3 - user-defined procedure
vanilla(Goal) :-
    functor(Goal, Functor, Arity),
    current_predicate(Functor/Arity),
    !,     % red cut
    clause(Goal, Subgoals),
    vanilla(Subgoals).
% 4 - inbuilt predicate
vanilla(Goal) :-
     call(Goal).




a(Arg) :-
     b(Arg).

b(Arg) :-
     c(Arg),
     d(Arg),
     e(Arg).
b(Arg) :-
     f(Arg),
     g(Arg).

c(Arg) :-
     h(Arg),
     i(Arg).

d(Arg) :-
     j(Arg).

e(Arg) :-
     k(Arg).

f(2).
g(2).
h(1).
i(1).
j(1).
k(1).