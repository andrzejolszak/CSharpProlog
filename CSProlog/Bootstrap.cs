/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2015 John Pool -- j.pool@ision.nl

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU Lesser General Public License as published by the Free Software Foundation; either 
  version 3.0 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License (http://www.gnu.org/licenses/lgpl-3.0.html), or 
  enter 'license' at the command prompt.

-------------------------------------------------------------------------------------------*/

namespace Prolog
{
    /// <summary>
    /// Contains the predefined predicates, also required to provide the Prolog engine with basic capabilities
    /// </summary>
    public class Bootstrap
    {
        public static string LicenseUrl = "http://www.gnu.org/licenses/lgpl-3.0.html";

        public static string PredefinedPredicates =
          @"&builtin
       :- op(900,  fy, [\+, not, once, help]).
       :- op(700, xfx, [=, \=, ==, \==, is, :=, =:, =:=, =\=, <, >, =<, >=, @<, @>, @=<, @>=, =.., ?=]).
       :- op(600, xfy, :).
       :- op(500, yfx, [+, -, #, xor, \/]). % '+' also for string concatenation (is-operator)
       :- op(500,  fx, [spy, nospy]).
       :- op(400, yfx, [*, /, //, <<, >>, /\]).
       :- op(300, xfx, [mod, ..]).
       :- op(200,  fy, [+, -, \]).
       :- op(200, xfx, **).
       :- op(200, xfy, ^).
       :- op( 99, xfy, '{}').

%%     license
%
%      This is a comment about license.
%
       license :== license.

%      This is a comment about license.

       license(X, Y) :== license.

       !             :== cut.

%% call(:Goal)
%
% Invoke Goal as a goal. Note that clauses may have variables as subclauses, which is identical to call/1.
%
       call(X)      :== call.
       
%% call(:Goal, +ExtraArg1, ...)
%
% Append ExtraArg1, ExtraArg2, ... to the argument list of Goal and call the result.
% For example, call(plus(1), 2, X) will call plus(1, 2, X), binding X to 3.
%
       call(X, A1) :== call.
       call(X, A1, A2) :== call.
       call(X, A1, A2, A3) :== call.
       call(X, A1, A2, A3, A4) :== call.
       call(X, A1, A2, A3, A4, A5) :== call.
       call(X, A1, A2, A3, A4, A5, A6) :== call.
       call(X, A1, A2, A3, A4, A5, A6, A7) :== call.
       meta$call(X) :== call.

%% fail
%
%Always fail. The predicate fail/0 is translated into a single virtual machine instruction.
%
       fail          :== fail.

       X = X.

%% true
%
% Always succeed. The predicate true/0 is translated into a single virtual machine instruction.
%
       true.

%% \+ :Goal
%
% True if `Goal' cannot be proven (mnemonic: + refers to provable and the backslash (\) 
% is normally used to indicate negation in Prolog).
%
       \+(X) :- X, !, fail.
       \+(X).

%% not(:Goal)
%
% True if Goal cannot be proven. Retained for compatibility only. New code should use \+/1.
%
       not(X) :- X, !, fail.
       not(X).

%%     maplist(:Goal, +List1, ?List2)
%
%      True if Goal can successfully be applied to all
%      successive pairs of elements from List1 and List2.
%
       maplist(Goal, L1, L2) :-
         map$list(L1, L2, Goal).
       map$list([], [], _).
       map$list([H0|T0], [H|T], Goal) :-
         call(Goal, H0, H),
         map$list(T0, T, Goal).

       %make_array(A, [_|_]) :== make_array. % later

/*
       A naive implementation for the ; (or) operator would be:

       X ; Y :- call(X).
       X ; Y :- call(Y).

       This definition is incorrect. If X is compound and ends with a cut, Y will be selected on
       backtracking anyway, since the effect of a cut in a call argument is limited to the call itself
       (and according to ISO does not extend to the ';'. If it did, the second clause would not be tried).

       This has been solved by handling ; directly in ExecuteGoalList()

       The if-then-else below works correctly but could be treated similarly (i.e. in ExecuteGoalList())
*/

       X ; Y  :== or. % just for preventing ';' from redefinition

       C -> X ; Y :- C, !, X.
       C -> X ; Y :- !, Y.
       C -> X :- C, !, X.

%% once(:Goal)
%
% Make a possibly nondet goal semidet, i.e., succeed at most once.
%
       once(X) :- X, !.

%% repeat
%
% Always succeed, provide an infinite number of choice points.
%
       repeat.
       repeat :- repeat.

%% consult(:File)
%
% Read File as a Prolog source file. Calls to consult/1 may be abbreviated 
% by just typing a number of filenames in a list.
%
       consult(X) :== consult.

%% asserta(+Term)
%
% Assert a fact or clause in the database.
% Term is asserted as the first fact or clause of the corresponding predicate. 
%
       asserta(X) :== asserta.

%% assert(+Term)
%
% Equivalent to assertz/1. 
%
       assert(X)  :== assert.

%% assertz(+Term)
%
% Equivalent to asserta/1, but Term is asserted as 
% the last clause or fact of the predicate.
%
       assertz(X) :== assertz.

%% retract(+Term)
%
% When Term is an atom or a term it is unified with the first unifying fact or clause in the database.
% The fact or clause is removed from the database.
%
       retract(X) :== retract.
       retract(X) :- retract(X).

%% retractall(+Head)
%
% All facts or clauses in the database for which the head unifies with Head are removed. 
% If Head refers to a predicate that is not defined, it is implicitly created as a dynamic predicate. 
%
       retractall(X) :== retractall.

%% spy(+Pred)
%
% Put a spy point on all predicates meeting the predicate specification Pred. 
%
       spy(X)          :== spy.
       spy(X, [_|_])   :== spy.

%% nospy(+Pred)
%
% Remove spy point from all predicates meeting the predicate specification Pred.
%
       nospy(X)        :== spy.

%% nospyall
%
% Remove all spy points from the entire program.
%
       nospyall         :== nospyall.
       verbose          :== verbose.
       noverbose        :== noverbose.
       silent           :== silent.

%% trace
%
% Start the tracer. trace/0 itself cannot be seen in the tracer.
%
       trace            :== trace.

%% notrace
%
% Stop the tracer. notrace/0 itself cannot be seen in the tracer.
%
       notrace          :== notrace.

%% debug
%
% Start debugger. In debug mode, Prolog stops at spy and trace points, 
% disables last-call optimisation and aggressive destruction of choice points to make debugging information accessible. 
%
       debug            :== debug.

%% nodebug
%
% Stop debugger. Implemented by the Prolog flag debug. See also debug/0.
%
       nodebug          :== nodebug.

%% profile
%
% switch profiling on: count all calls made during query execution
%
       profile          :== profile.

%% noprofile
%
% switch profiling off
%
       noprofile        :== noprofile.    

%% showprofile
%
% show profiling results (hit count per predicate)
%
       showprofile      :== showprofile.  

%% showprofile(N)
%
% ... top N values only
%
       showprofile(N)  :== showprofile.  

%% cleaprofile
%
% reset profile counters to zero
%
       clearprofile     :== clearprofile. 

%% stacktrace(M)
%
% show C# exception stacktrace (M = 'on' or 'off')
%
       stacktrace(M)   :== stacktrace.  

       collection$(T, X, P, L) :-
         collection$init(T, S),
         (call(P),
           collection$add(T, S, X),
           fail % undoes all var bindings in X
         ;
           true
         ),
         collection$exit(T, S, L).

       collection$init(T, S)    :== collection_init.
       collection$add(T, S, X)  :== collection_add.
       collection$exit(T, S, L) :== collection_exit.

%% bagof(+Template, :Goal, -Bag)
%
% Unify Bag with the alternatives of Template. If Goal has free variables
% besides the one sharing with Template, bagof/3 will backtrack over the alternatives 
% of these free variables, unifying Bag with the corresponding alternatives of Template. 
% The construct +Var^Goal tells bagof/3 not to bind Var in Goal. bagof/3 fails if Goal has no solutions.
%
       bagof(X, P, L) :-
         collection$(bagof, X, P, L).

%% setof(+Template, +Goal, -Set)
%
% Equivalent to bagof/3, but sorts the result using sort/2
% to get a sorted list of alternatives without duplicates.
%
       setof(X, P, L) :-
         collection$(setof, X, P, L).

%% findall(+Template, :Goal, -Bag)
%
% Create a list of the instantiations Template gets successively on backtracking over Goal 
% and unify the result with Bag. Succeeds with an empty list if Goal has no solutions. 
% findall/3 is equivalent to bagof/3 with all free variables bound with the existential operator (^), 
% except that bagof/3 fails when Goal has no solutions.
%
       findall(X, P, L) :-
         collection$(findall, X, P, L).

       version(V, R)    :== version.

%% halt
%
% Terminate Prolog execution. 
%
       halt              :== halt.

%% length(?List, ?Int)
%
% True if Int represents the number of elements in List. This predicate is a true relation 
% and can be used to find the length of a list or produce a list (holding variables) of length Int. 
% The predicate is non-deterministic, producing lists of increasing length if List is a partial 
% list and Int is unbound. It raises errors if
% Int is bound to a non-integer.
% Int is a negative integer.
% List is neither a list nor a partial list. This error condition includes cyclic lists.112
% This predicate fails if the tail of List is equivalent to Int (e.g., length(L,L)).
%
       length(L, N)     :== length.

%% reverse(?List1, ?List2)
%
% Is true when the elements of List2 are in reverse order compared to List1.
%
       reverse(X, R)    :== reverse.

%% sort(+List, -Sorted)
%
% True if Sorted can be unified with a list holding the elements of List, 
% sorted to the standard order of terms (see section 4.7). Duplicates are removed. 
% The implementation is in C, using natural merge sort.114 The sort/2 predicate 
% can sort a cyclic list, returning a non-cyclic version with the same elements.
%
       sort(L, S)       :== sort.

%% functor(?Term, ?Name, ?Arity)
%
% True when Term is a term with functor Name/Arity. If Term is a variable it is unified 
% with a new term whose arguments are all different variables (such a term is called a skeleton). 
% If Term is atomic, Arity will be unified with the integer 0, and Name will be unified with Term.
% Raises instantiation_error() if Term is unbound and Name/Arity is insufficiently instantiated.
%
       functor(T, F, N) :== functor.

%% arg(?Arg, +Term, ?Value)
%
% Term should be instantiated to a term, Arg to an integer between 1 and the arity of Term. 
% Value is unified with the Arg-th argument of Term. Arg may also be unbound. 
% In this case Value will be unified with the successive arguments of the term. 
% On successful unification, Arg is unified with the argument number. Backtracking yields 
% alternative solutions.88 The predicate arg/3 fails silently if Arg = 0 or Arg > arity and raises 
% the exception domain_error(not_less_than_zero, Arg) if Arg < 0.
%
       arg(N, T, A)     :== arg.

%% abolish(+Name, +Arity)
%
% Same as abolish(Name/Arity). The predicate abolish/2 conforms to the Edinburgh standard, 
% while abolish/1 is ISO compliant.
%
       abolish(X/N)     :== abolish.

%% gensym(+Base, -Unique)
%
% Generate a unique atom from base Base and unify it with Unique. 
% Base should be an atom. The first call will return <base>1 , the next <base>2 , etc. 
% Note that this is no guarantee that the atom is unique in the system.
%
       gensym(A, X)     :== gensym.
       gensym(X)        :== gensym.

%% var(@Term)
%
% True if Term currently is a free variable.
%
       var(V)           :== var.

%% var(@Term, +Name)
%
% True if Term currently is a free variable with a given Name.
%
       var(V, N)        :== var.

%% nonvar(@Term)
%
% True if Term currently is not a free variable.
%
       nonvar(V)        :== nonvar.

%% atom(@Term)
%
% True if Term is bound to an atom.
%
       atom(A)          :== atom_.

%% atomic(@Term)
%
% True if Term is bound (i.e., not a variable) and is not compound.
%
       atomic(A)        :== atomic.

%% float(+Expr)
%
% Translate the result to a floating point number. Normally, Prolog will use integers whenever possible. 
% When used around the 2nd argument of is/2, the result will be returned as a floating point number. 
% In other contexts, the operation has no effect.
%
       float(N)         :== float_.

%% number(@Term)
%
% True if Term is bound to an integer or floating point number.
%
       number(N)        :== number.

%% integer(+Expr)
%
% Same as round/1 (backward compatibility).
%
       integer(N)       :== integer.

%% compound(@Term)
%
% True if Term is bound to a compound term. See also functor/3 =../2, compound_name_arity/3 
% and compound_name_arguments/3.
%
       compound(V)      :== compound.

%% list(@Term)
%
% True if Term is bound to a list.
%
       list(L)          :== list.

%% string(@Term)
%
% True if Term is bound to a string.
%
       string(V)        :== string_.

%% bool(@Term)
%
% True if Term is bound to a bool.
%
       bool(V)          :== bool_.

%% datetime(@Term)
%
% True if Term is bound to a datetime.
%
       datetime(DT)                     :== datetime.
       datetime(DT, Y, Mo, D, H, Mi, S) :== datetime.
       datetime(DT, Y, Mo, D)           :== datetime.


       timespan(T)           :== timespan.
       timespan(T, H, Mi, S) :== timespan.
       date_part(DT, D) :== date_part.
       time_part(DT, T) :== time_part.

%% succ(?Int1, ?Int2)
%
% True if Int2 = Int1 + 1 and Int1 >= 0. At least one of the arguments must be instantiated to 
% a natural number. This predicate raises the domain error not_less_than_zero if called with 
% a negative integer. E.g. succ(X, 0) fails silently and succ(X, -1) raises a domain error.
%
       succ(N0, N1)     :== succ.

       string_datetime(X, D) :== string_datetime.
       string_datetime(X, Y, M, D) :== string_datetime.
       string_datetime(X, Y, Mo, D, H, Mi, S) :== string_datetime.

%% -Number is +Expr
%
% True when Number is the value to which Expr evaluates. Typically, is/2 should be used
% with unbound left operand. If equality is to be tested, =:=/2 should be used.
%
       X is Y            :== is_.

%% @Term1 \= @Term2
%
% Equivalent to \+Term1 = Term2.
%
       X \= Y            :== ne_uni.

%% @Term1 == @Term2
%
% True if Term1 is equivalent to Term2. A variable is only identical to a sharing variable.
%
       X == Y            :== eq_str.

%% @Term1 \== @Term2
%
% Equivalent to \+Term1 == Term2.
%
       X \== Y           :== ne_str.

%% +Expr1 =:= +Expr2
%
% True if expression Expr1 evaluates to a number equal to Expr2.
%
       X =:= Y           :== eq_num.

%% +Expr1 =\= +Expr2
%
% True if expression Expr1 evaluates to a number non-equal to Expr2.
%
       X =\= Y           :== ne_num.
       X < Y             :== lt_num.
       X =< Y            :== le_num.
       X > Y             :== gt_num.
       X >= Y            :== ge_num.
       X @< Y            :== lt_ord.
       X @=< Y           :== le_ord.
       X @> Y            :== gt_ord.
       X @>= Y           :== ge_ord.

%% ?Term =.. ?List
%
%List is a list whose head is the functor of Term and the remaining arguments 
% are the arguments of the term. Either side of the predicate may be a variable, but not both. 
%
       X =.. Y           :== univ.

%% unifiable(@X, @Y)
%
% If X and Y can unify, unify Unifier with a list of Var = Value, 
% representing the bindings required to make X and Y equivalent.
%
       unifiable(X, Y)  :== unifiable.

%       X ?= Y :-
%         (A == B ; A \= B), !. % SWI-Prolog

       % I/O

%% fileexists(+FileName) is semidet
%
% True if a file named FileName exists.
%
       fileexists(F)    :== fileexists.

%% see(+SrcDest)
%
% Open SrcDest for reading and make it the current input (see set_input/1). 
% If SrcDest is a stream handle, just make this stream the current input.
%
       see(F)           :== see.

%% seeing(?SrcDest)
%
% Same as current_input/1, except that user is returned if the current input 
% is the stream user_input to improve compatibility with traditional Edinburgh I/O.
%
       seeing(F)        :== seeing.

%% read(-Term)
%
% Read the next Prolog term from the current input stream and unify it with Term. 
% On a syntax error read/1 displays an error message, attempts to skip the erroneous term and fails. 
% On reaching end-of-file Term is unified with the atom end_of_file.
%
       read(X)          :== read.

       readatom(X)      :== readatom.
       readatoms(L)     :== readatoms.

%% readeof(F, T)
%
% unify the entire contents of file F with T
%
       readeof(F, T)    :== readeof.

%% read_line(-Codes) is det
%
% Read a line from the given or current input. The line read does not include 
% the line-termination character. Unifies Codes with end_of_file if the end of the input is reached.
%
       readln(X)        :== readln.

%% get0(-Char)
%
% Edinburgh version of the ISO get_code/1 predicate. Note that Edinburgh Prolog didn't support 
% wide characters and therefore technically speaking get0/1 should have been mapped to get_byte/1.
% The intention of get0/1, however, is to read character codes.
%
       get0(C)          :== get0.

%% get(-Char)
%
% Read the current input stream and unify the next non-blank character with Char. 
% Char is unified with -1 on end of file. The predicate get/1 operates on character codes.
%
       get(C)           :== get.

%% seen
%
% Close the current input stream. The new input stream becomes user_input.
%
       seen              :== seen.

%% tell(+SrcDest)
%
% Open SrcDest for writing and make it the current output (see set_output/1).
% If SrcDest is a stream handle, just make this stream the current output.
%
       tell(F)          :== tell.

%% telling(?SrcDest)
%
% Same as current_output/1, except that user is returned if the current output is the stream
% user_output to improve compatibility with traditional Edinburgh I/O. 
%
       telling(F)       :== telling.
       showfile(F)      :== showfile.

%% write(+Term)
% 
% Write Term to the current output, using brackets and operators where appropriate.
%
       write(X)         :== write.

%% print(+Term)
%
% Print a term for debugging purposes.
%
       print(X)         :== print.
       treeprint(X)     :== treeprint.

%% writeln(+Term)
%
% Equivalent to write(Term), nl..
%
       writeln(X)       :== writeln.

%% writef(+Format, +Arguments)
%
% Formatted write. Format is an atom whose characters will be printed. 
% Format may contain certain special character sequences which specify certain formatting 
% and substitution actions. Arguments provides all the terms required to be output.
%
       writef(S, L)     :== writef.  % formatted write, à la C#. L single arg or list of args.

%% writef(+Atom)
%
% Equivalent to writef(Atom, []).
%
       writef(S)        :== write.

       writelnf(S, L)   :== writelnf.
       writelnf(S)      :== writeln.

       console(S)       :== console.
       console(S, L)    :== console.

%% put(+Char)
%
% Write Char to the current output stream. Char is either an integer expression 
% evaluating to a character code or an atom of one character.
%
       put(C)           :== put.

%% nl
%
% Write a newline character to the current output stream. 
%
       nl                :== nl.

%% tab(+Amount)
%
% Write Amount spaces on the current output stream.
%
       tab(N)           :== tab.

%% display(+Term) is det
%
% Write a term, ignoring operators.
%
       display(X)       :== display.

%% told
%
% Close the current output stream. The new output stream becomes user_output.
%
       told              :== told.
       cls               :== cls. % clear screen
       errorlevel(N)    :== errorlevel. % set DOS ERRORLEVEL (for batch processing)

%% maxwritedepth(N
%
% subterms beyond level N are written as '...'.
% If set to -1 (default): no limit. The effect remains for the duration of the query.
%
       maxwritedepth(N) :== maxwritedepth.

    /* Backtrackable predicates
       ------------------------
       These are all implemented according to the same pattern: a predicate with an
       extra first State argument is introduced. This argument maintains the state
       between two successive backtracking calls. It is initialized at the first call,
       and reset to an unbound variable after the last successful call.
       UserClassTerm (in DerivedTerms.cs) can be used for creating a State term with
       an arbitrary class type content. In my experience, an enumerator is eminently
       suited for this task, since in fact it can be considered a finite state machine
       that yields one value at a time and saves state between calls.
    */

%% clause(:Head, ?Body)
%
% True if Head can be unified with a clause head and Body with the corresponding clause body. 
% Gives alternative clauses on backtracking. For facts, Body is unified with the atom true.
%
       clause(H, B) :-              % returns body B for clause head H
         clause$(State, H, B),
         !,
         clause$(State, H, B).

       clause$(State, H, B) :== clause.
       clause$(State, H, B) :-
         !,
         nonvar(State),             % 'clause' resets State to var upon failure
         clause$(State, H, B).

%% combination(P0, K, P1)
%
% returns next K-combination P1 for list P0.
%
       combination(P0, K, P1) :-         
         combination$(State, P0, K, P1), % First returned value of P1 is P0 sorted
         !,
         combination$(State, P0, K, P1).
 
       combination$(State, P0, K, P1) :== combination.
       combination$(State, P0, K, P1) :-
         !,
         nonvar(State),             % 'combination' resets State to var upon failure
         combination$(State, P0, K, P1).

%% permutation(?Xs, ?Ys)
%
% True when Xs is a permutation of Ys. This can solve for Ys given Xs or Xs given Ys, 
% or even enumerate Xs and Ys together. The predicate permutation/2 is primarily intended 
% to generate permutations. Note that a list of length N has N! permutations, and unbounded 
% permutation generation becomes prohibitively expensive, even for rather short lists (10! = 3,628,800).
%
       permutation(P0, P1) :-         % returns next permutation P1 for list P0.
         permutation$(State, P0, P1), % First returned value of P1 is P0 sorted
         !,
         permutation$(State, P0, P1).
 
       permutation$(State, P0, P1) :== permutation.
       permutation$(State, P0, P1) :-
         !,
         nonvar(State),             % 'permutation' resets State to var upon failure
         permutation$(State, P0, P1).

%% current_op(?Precedence, ?Type, ?:Name)
%
% True if Name is currently defined as an operator of type Type with precedence Precedence. 
%
       current_op(P, F, N) :-       % return operators matching ?P(recedence), ?F(ix), ?N(name)
         current$op(State, P, F, _),
         !,
         current$op(State, P, F, N).

       current$op(State, P, F, N) :== current_op.
       current$op(State, P, F, N) :-
         !,
         nonvar(State),             % 'current_op' resets State to var upon failure
         current$op(State, P, F, N).

%% term_pattern(T, P, Dmin, Dmax)
%
% find pattern P in T, between depths Dmin and Dmax (inclusive)
%
       term_pattern(T, P, Dmin, Dmax) :-
         term_pattern$(State, T, P, Dmin, Dmax, !),
         !,
         term_pattern$(State, T, P, Dmin, Dmax, !).

       term_pattern$(State, T, P, Dmin, Dmax, Loc) :== term_pattern.  % Loc is path to P in T
       term_pattern$(State, T, P, Dmin, Dmax, Loc) :-
         !,
         nonvar(State),
         term_pattern$(State, T, P, Dmin, Dmax, Loc).

%% term_pattern(T, P,)
%
% find pattern P in T.
%
       term_pattern(T, P) :-
         !,
         term_pattern(T, P, _, _).

       % Loc as extra arg
       term_pattern(T, P, Dmin, Dmax, Loc) :-
         term_pattern$(State, T, P, Dmin, Dmax, Loc),
         !,
         term_pattern$(State, T, P, Dmin, Dmax, Loc).

       term_pattern(T, P, Loc) :-
         !,
         term_pattern(T, P, _, _, Loc).

%% between(+Low, +High, ?Value)
%
% Low and High are integers, High >=Low. If Value is an integer, Low =<Value =<High. 
% When Value is a variable it is successively bound to all integers between Low and High. 
% If High is inf or infinite98 between/3 is true iff Value >=Low, a feature that is particularly 
% interesting for generating integers from a certain value.
%
       between(L, H, N) :-          % returns N = L, L+1, ... H upon backtracking,
         between$(State, L, H, _),  % first call only initializes State,
         !,                          % ... which maintains state between subsequent calls
         between$(State, L, H, N).

       between$(State, L, H, N) :== between.
       between$(State, L, H, N) :-
         !,
         nonvar(State),             % 'between' resets State to var upon failure
         between$(State, L, H, N).

%% name(?Atomic, ?CodeList)
%
% CodeList is a list of character codes representing the same text as Atomic.
% Each of the arguments may be a variable, but not both. When CodeList describes an integer 
% or floating point number and Atomic is a variable, Atomic will be unified with the numeric value 
% described by CodeList (e.g., name(N, 300), 400 is N + 100 succeeds). If CodeList is not 
% a representation of a number, Atomic will be unified with the atom with the name given by 
% the character code list. When Atomic is an atom or number, the unquoted print representation of 
% it as a character code list will be unified with CodeList.
%
       name(A, L)               :== name.

%% atom_string(?Atom, ?String)
%
% Bi-directional conversion between an atom and a string. At least one of 
% the two arguments must be instantiated. Atom can also be an integer or floating point number.
%
       atom_string(A, S)        :== atom_string. % conversion between atom and string

%% atom_chars(?Atom, ?List)
%
% TODO
%
       atom_chars(A, L)        :== atom_chars.

       number_chars(A, L)        :== atom_chars.

%% atom_concat(?Atom, ?Atom, ?Atom)
%
% TODO
%
       atom_concat(A1, A2, A3)        :== atom_concat.

%% atom_length(?Atom, ?Len)
%
% TODO
%
       atom_length(Atom, Len)        :== atom_length.

       string_term(S, T)        :== string_term. % convert string S to Prolog term T and v.v.
       string_words(S, L)       :== string_words.

%% expand_term(+Term1, -Term2)
%
% This predicate is normally called by the compiler on terms read from the input 
% to perform preprocessing. It consists of four steps, where each step processes the output
% of the previous step.
%
       expand_term((P-->Q), R)  :== expand_term.

%% numbervars(+Term, +Start, -End)
%
% Unify the free variables in Term with a term $VAR(N), where N is the number of the variable. 
% Counting starts at Start. End is unified with the number that should be given to the next variable.
%
       numbervars(X, B, E)      :== numbervars.

       predicate(P/N)           :== predicatePN.
       predicate(T)             :== predicateX.

       current_predicate(P/N)           :== predicatePN.
       current_predicate(T)             :== predicateX.

%% ground(@Term)
%
% True if Term holds no free variables.
%
       ground(X)                :== ground.

%% throw(+Exception)
%
% Raise an exception. The system looks for the innermost catch/3 ancestor for which 
% Exception unifies with the Catcher argument of the catch/3 call. See catch/3 for details.
%
       throw(S)                 :== throw_. % raise an exception, show text S
       throw(S, L)              :== throw_. % ...; L single arg or list of args.
       throw(C, S, L)           :== throw_. % ...; C exception class name (atom or integer); L single arg or list of args.


%% format(+Output, +Format, :Arguments)
%
% As format/2, but write the output on the given Output. 
% The de-facto standard only allows Output to be a stream. 
%
       format(S, L, X)         :== format.

%% now(-When) is det
%
% Unify when with the current time-stamp
%
       now(H, M, S)            :== now.
       validtime(H, M, S)      :== validtime.
       today(Y, M, D)          :== today.
       validdate(Y, M, D)      :== validdate.
       dayname(Y, M, D, N)     :== dayname.
       dayofweek(Y, M, D, N)   :== dayofweek.
       dayofyear(Y, M, D, N)   :== dayofyear.
       leapyear(Y)             :== leapyear.
       weekno(Y, M, D, N)      :== weekno.
       weekno(N)               :== weekno.

%% appen(?List1, ?List2, ?List1AndList2)
%
% List1AndList2 is the concatenation of List1 and List2
%
       append2(X, Y, Z)        :== append2.      % deterministic append

%% flat(+NestedList, -FlatList)
%
% Is true if FlatList is a non-nested version of NestedList. Note that empty lists are removed. 
% In standard Prolog, this implies that the atom '[]' is removed too
%
       flat(X, Y)              :== flat.         % deterministic flatten
       dcg_flat(X, Y)          :== dcg_flat.     % flatten curly bracket list
       bw_transform(P, E, I)   :== bw_transform. % Burroughs-Wheeler transform

%% listing
%
% List all predicates from the calling module using listing/1. For example, ?- listing. 
% lists clauses in the default user module and ?- lists:listing. lists the clauses in the module lists.
% 
       listing                :== listing.

%% listing(:Pred)
%
% List predicates specified by Pred. Pred may be a predicate name (atom),
% which lists all predicates with this name, regardless of their arity. 
%
       listing(X/N)          :== listingXN.
       listing([X|Rest]) :-
         listing(X),
         !,
         listing(Rest).
       listing([]).
       listing(X)            :== listingX.

       % same, but for predefined predicates (mainly for testing & debugging)

       listing0               :== listing0.
       listing0(X/N)         :== listing0XN.
       listing0([X|Rest]) :-
         listing0(X),
         !,
         listing0(Rest).
       listing0([]).
       listing0(X)           :== listing0X.

       pp_defines(X)         :== pp_defines. % preprocessor symbol definitions

%% copy_term(+In, -Out)
%
% Create a version of In with renamed (fresh) variables and unify it to Out. 
% Attributed variables (see section 7.1) have their attributes copied.
%
       copy_term(X, Y)       :== copy_term.

       clearall               :== clearall.
       spypoints              :== spypoints.
       stringstyle(X)        :== stringstyle.
       /*next version:
       %callstack(S)         :== callstack. % string S contains current call stack
       %callstack(S, L)      :== callstack. % ... L is list representation
        */

%% memberchk(?Elem, +List)
%
% True when Elem is an element of List. This `chk' variant of member/2 is 
% semi deterministic and typically used to test membership of a list. 
%
       memberchk(X, [Y|Rest]) :- nonvar(X), member(X, [Y|Rest]).

%% member(?Elem, ?List)
%
% True if Elem is a member of List. 
% Actually member/2 given below is memberchk/2.
% As member/2, it is not completely correct, e.i. it should backtrack on member(1, [1,1,1]) !!!
%
       member(X, L)           :== member. % fails for unbound X or L. Disables backtracking upon success
       member(X, [X|_]).
       member(X, [_|L]) :- member(X, L).

%% append(?List1, ?List2, ?List1AndList2)
%
% List1AndList2 is the concatenation of List1 and List2
%
       append([], X, X).
       append([X|Y], U, [X|V]):-
         append( Y, U, V).

       regex_match(S, P, L)        :== regex_match. % Find all occurances of match pattern P in string S
       regex_match(S, P, L, [_|_]) :== regex_match. % Optional list of regex options (a la C#)

       regex_replace(S, P, R, T) :== regex_replace. % String T is the result of replacing all ...
                                                     % ... occurances of pattern P in S with R

%% statistics(+Key, -Value)
%
% Unify system statistics determined by Key with Value. 
%
       statistics(T, [MSec,_]) :== statistics.

       get_counter(N, V)       :== get_counter. % Get current integer value V of counter number N
       set_counter(N, V)       :== set_counter. % N is set to integer V, and not unbound in backtracking
       inc_counter(N)          :== inc_counter. % Value of N is increased at each call and not unbound in backtracking
       inc_counter(N, V)       :== inc_counter. % ... return new value in V
       dec_counter(N)          :== dec_counter. % Value of N is decreased at each call and not unbound in backtracking
       dec_counter(N, V)       :== dec_counter. % ... return new value in V
       setvar(N, V)            :== setvar.      % Store a copy of V in a global symbol table under the name N
       getvar(N, V)            :== getvar.      % Get the term stored under the name N from the global symbol table

       query_timeout(MSecs)    :== query_timeout. % must be entered as a separate query *before* the query you want to limit

       catch(X, _, _) :- call(X).

       ";
    }
}
