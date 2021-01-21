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

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;

namespace Prolog
{
    public enum BI // builtins
    {
        none, abolish, arg, append, append2, assert, asserta, assertz, atom_,
        atom_string, atom_chars, atom_concat, atom_length, atomic, between, bool_, cache, call, clause, clearall,
        clearprofile, cls, collection_add, collection_exit, collection_init,
        combination, compound, config_setting, console, consult, copy_term,
        current_op, cut, dayname, dcg_flat, date_part, datetime, dayofweek, dayofyear,
        debug, dec_counter, display, eq_num, eq_str, errorlevel, expand_term,
        fail, fileexists, flat, float_, format, functor, ge_num, ge_ord, gensym, genvar,
        get, get_counter, get0, getvar, ground, gt_num, gt_ord, halt,
        inc_counter, integer, is_, le_num, le_ord,
        leapyear, length, license, list, listing, listing0, listing0X, listing0XN, listingX,
        listingXN, lt_num, lt_ord, member, name, ne_num,
        ne_str, ne_uni, nl, nocache, nodebug, nonvar, noprofile, nospy, nospyall, notrace,
        noverbose, now, number, numbervars, or, permutation, pp_defines,
        predicatePN, predicateX, print, profile, put, query_timeout, read, readatoms,
        readatom, readeof, readln, retract, retractall,
        reverse, set_counter, setvar,
        showfile, showprofile, silent, sort, spy, spypoints, stacktrace, callstack, statistics,
        string_, string_datetime, string_term, string_words, stringstyle, succ, tab,
        term_pattern, throw_, time_part, timespan, today, trace, treeprint,
        unifiable, univ, validdate, validtime, var,
        verbose, version, weekno, write, writef, writeln, writelnf
    }

    public partial class PrologEngine
    {
        public static readonly string PredefinedPredicates =
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

       ';'(A, B) :- A ; B.

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

       cls               :== cls. % clear screen
       errorlevel(N)    :== errorlevel. % set DOS ERRORLEVEL (for batch processing)

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
         clause$(State, H, B),      % executes the ':==' clause which succeeds on the first try and assigns State
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

        private string currentInputName;

        private string currentOutputName;

        private bool DoBuiltin(BI biId, out bool findFirstClause)
        {
            findFirstClause = false;
            BaseTerm term = goalListHead.Term;
            BaseTerm t0, t1, t2, t3, t4, t5;
            int n, y, m, d, h, s;
            int arity;
            string functor;
            bool result;
            bool inFile;
            bool outFile;
            DupMode dupMode = DupMode.DupAccept; // for setof, bagoff, findall
            TermType type;
            string a, x;
            string fileName;
            string cmd = null;
            int cntrValue;
            DateTime dati;
            TimeSpan ti;
            bool mustWait = false;

            switch (biId)
            {
                case BI.license:
                    IO.Message("Opening your browser ...");
                    break;

                case BI.consult: // individual file or list of files
                    t0 = term.Arg(0);

                    if (t0.IsProperList)
                    {
                        int lines = 0;
                        int files = 0;

                        while (t0.Arity == 2)
                        {
                            fileName = Utils.FileNameFromTerm(t0.Arg(0), ".pl");

                            if (fileName == null)
                            {
                                return false;
                            }

                            lines += PredTable.Consult(fileName);
                            files++;
                            t0 = t0.Arg(1);
                        }

                        if (files > 1)
                        {
                            IO.Message($"Grand total is {lines} lines");
                        }

                        PredTable.ResolveIndices();

                        break;
                    }

                    if (t0.IsAtomOrString)
                    {
                        fileName = Utils.FileNameFromTerm(t0, ".pl");

                        if (fileName == null)
                        {
                            return false;
                        }

                        IO.Write($"--- Consulting {fileName} ... ");
                        PredTable.Consult(fileName);
                        IO.WriteLine($"{parser.LineCount} lines read");
                        PredTable.ResolveIndices();

                        break;
                    }

                    return IO.ThrowRuntimeException($"Unable to read file '{t0.Arg(0)}'", CurrVarStack, term);

                case BI.asserta:
                    PredTable.Assert(term.Arg(0), true); // true: at beginning
                    break;

                case BI.assert:
                case BI.assertz:
                    PredTable.Assert(term.Arg(0), false);
                    break;

                case BI.retract:
                    if (PredTable.Retract(term.Arg(0), CurrVarStack, null))
                    {
                        currentCp.NextClause = retractClause;
                    }
                    else
                    {
                        CanBacktrack(null);
                        return false;
                    }

                    break;

                case BI.retractall: // retractall
                    PredTable.RetractAll(term.Arg(0), CurrVarStack);
                    break;

                case BI.verbose:
                    break;

                case BI.noverbose:
                case BI.silent:
                    break;

                case BI.trace:
                case BI.notrace:
                    bool trace = Trace1;
                    SetSwitch("Tracing", ref trace, term.HasFunctor("trace"));
                    trace = Trace1;
                    if (Trace1)
                    {
                        Debugging = true;
                    }

                    Reporting = Debugging || EventDebug;
                    break;

                case BI.debug:
                case BI.nodebug:
                    SetSwitch("Debugging", ref debug, term.HasFunctor("debug"));
                    Reporting = Debugging || EventDebug;
                    break;

                // bagof, setof, findall
                case BI.collection_init:
                    if (term.Arg(0).HasFunctor("setof"))
                    {
                        dupMode = DupMode.DupIgnore;
                    }
                    else
                    {
                        dupMode = DupMode.DupAccept;
                    }

                    term.Arg(1).Unify(new CollectionTerm(term.Symbol, dupMode), CurrVarStack);

                    break;

                case BI.collection_add:
                    if ((t2 = term.Arg(2)).IsVar)
                    {
                        return false;
                    }

                    // t2 must be copied because it is unbound during backtracking
                    if (term.Arg(0).HasFunctor("setof"))
                    {
                        ((CollectionTerm)term.Arg(1)).Insert(t2.Copy(CurrVarStack));
                    }
                    else
                    {
                        ((CollectionTerm)term.Arg(1)).Add(t2.Copy(CurrVarStack));
                    }

                    break;

                case BI.collection_exit:
                    CollectionTerm ct = (CollectionTerm)term.Arg(1);

                    // bagof and setof must Fail if there are no matches; findall will Succeed
                    if (term.Arg(0).FunctorToString != "findall" && ct.Count == 0)
                    {
                        return false;
                    }

                    term.Arg(2).Unify(ct.ToList(), CurrVarStack);

                    break;

                case BI.version: // version(V, R)
                    if (!term.Arg(0).Unify(new AtomTerm(term.Symbol, "1"), CurrVarStack))
                    {
                        return false;
                    }

                    if (!term.Arg(1).Unify(new AtomTerm(term.Symbol, ""), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.halt:
                    Halted = true;
                    break;

                case BI.reverse: // reverse( ?X, ?R) -- proper list X is the reversed version of list R
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsVar)
                    {
                        if (t1.IsVar || !t1.IsProperList)
                        {
                            return false;
                        }

                        t0.Unify(((ListTerm)t1).Reverse(), CurrVarStack);
                    }
                    else // t0 has a value
                    {
                        if (!t0.IsProperList)
                        {
                            return false;
                        }

                        if (!t1.Unify(((ListTerm)t0).Reverse(), CurrVarStack))
                        {
                            return false;
                        }
                    }

                    break;

                case BI.combination: // combination( +P, +K, ?Q) -- list Q is the 'next' K-combination of list P
                    t1 = term.Arg(1);
                    t2 = term.Arg(2); // combination size (k)

                    if (!t1.IsProperList || !t2.IsInteger)
                    {
                        return false;
                    }

                    if (t1.IsEmptyList)
                    {
                        if (term.Arg(3).Unify(BaseTerm.EMPTYLIST, CurrVarStack))
                        {
                            break;
                        }

                        return false;
                    }

                    Combination cmb;
                    IEnumerator<ListTerm> iCombi = null;
                    t0 = term.Arg(0);

                    if (t0.IsVar) // first call only, Arg(0) contains State info
                    {
                        cmb = new Combination((ListTerm)t1, t2.To<int>());
                        iCombi = cmb.Iterator;
                        t0.Unify(new UserClassTerm<IEnumerator<ListTerm>>(term.Symbol, iCombi), CurrVarStack);

                        break;
                    }

                    iCombi = ((UserClassTerm<IEnumerator<ListTerm>>)t0).UserObject;

                    while (true)
                    {
                        if (!iCombi.MoveNext())
                        {
                            term.SetArg(0, BaseTerm.VAR);

                            return false;
                        }

                        if (term.Arg(3).Unify(iCombi.Current, CurrVarStack))
                        {
                            break;
                        }

                        return false;
                    }

                    break;

                case BI.permutation: // permutation( +P, ?Q) -- list Q is the 'next' permutation of list P
                    t1 = term.Arg(1);

                    if (!t1.IsProperList)
                    {
                        return false;
                    }

                    if (t1.IsEmptyList)
                    {
                        if (term.Arg(2).Unify(BaseTerm.EMPTYLIST, CurrVarStack))
                        {
                            break;
                        }

                        return false;
                    }

                    Permutation pmt;
                    IEnumerator<ListTerm> iPermut = null;
                    t0 = term.Arg(0);

                    if (t0.IsVar) // first call only, Arg(0) contains State info
                    {
                        pmt = new Permutation((ListTerm)t1);
                        iPermut = pmt.GetEnumerator();
                        t0.Unify(new UserClassTerm<IEnumerator<ListTerm>>(term.Symbol, iPermut), CurrVarStack);

                        break;
                    }

                    iPermut = ((UserClassTerm<IEnumerator<ListTerm>>)t0).UserObject;

                    while (true)
                    {
                        if (!iPermut.MoveNext())
                        {
                            term.SetArg(0, BaseTerm.VAR);

                            return false;
                        }

                        if (term.Arg(2).Unify(iPermut.Current, CurrVarStack))
                        {
                            break;
                        }

                        return false;
                    }

                    break;

                case BI.length: // properLength( L, N)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsProperOrPartialList)
                    {
                        n = 0;

                        while (t0.IsListNode)
                        {
                            n++;
                            t0 = t0.Arg(1);
                        }

                        if (t0.IsVar && t1.IsNatural) // cope with calls such as properLength( [1,2,3|T], 9)
                        {
                            if ((n = t1.To<int>() - n) < 0)
                            {
                                return false;
                            }

                            t2 = BaseTerm.EMPTYLIST;

                            for (int i = 0; i < n; i++)
                            {
                                t2 = new ListTerm(term.Symbol, new Variable(term.Symbol, CurrVarStack), t2);
                            }

                            t0.Unify(t2, CurrVarStack);

                            break;
                        }

                        if (!term.Arg(1).Unify(new DecimalTerm(term.Symbol, n), CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else if (t0.IsAtomOrString)
                    {
                        if (!term.Arg(1).Unify(new DecimalTerm(term.Symbol, t0.FunctorToString.Length), CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else // create a list with N elements
                    {
                        if (!t1.IsNatural)
                        {
                            return false;
                        }

                        arity = t1.To<int>();
                        t1 = BaseTerm.EMPTYLIST;

                        for (int i = 0; i < arity; i++)
                        {
                            t1 = new ListTerm(term.Symbol, new Variable(term.Symbol, CurrVarStack), t1);
                        }

                        t0.Unify(t1, CurrVarStack);
                    }

                    break;

                case BI.sort: // sort( L, S)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsProperList)
                    {
                        if (!(t1.IsProperList || t1.IsVar))
                        {
                            return false;
                        }

                        BaseTermSet tlist = new BaseTermSet(t0);
                        tlist.Sort();

                        if (!t1.Unify(tlist.ToList(), CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else
                    {
                        return false;
                    }

                    break;

                case BI.succ: // succ(?N0, ?N1) -- succeeds if N1-N0 = 1
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsVar)
                    {
                        if (t1.IsVar || !t1.IsInteger)
                        {
                            return false;
                        }

                        t0.Unify(new DecimalTerm(term.Symbol, t1.To<int>() - 1), CurrVarStack);
                    }
                    else if (t1.IsVar)
                    {
                        t1.Unify(new DecimalTerm(term.Symbol, t0.To<int>() + 1), CurrVarStack);
                    }
                    else if (!t0.IsInteger || !t1.IsInteger || t0.To<int>() != t1.To<int>() - 1)
                    {
                        return false;
                    }

                    break;

                case BI.functor: // functor( T, F, N)
                    t0 = term.Arg(0);

                    if (t0.IsVar)
                    {
                        t1 = term.Arg(1);

                        if (t1.IsVar)
                        {
                            return false;
                        }

                        functor = t1.FunctorToString;
                        t2 = term.Arg(2);

                        if (t2.IsNatural)
                        {
                            arity = t2.To<int>();
                        }
                        else
                        {
                            return false;
                        }

                        BaseTerm[] args = new BaseTerm[arity];

                        for (int i = 0; i < arity; i++)
                        {
                            args[i] = new Variable(term.Symbol, CurrVarStack);
                        }

                        if (!t0.Unify(CreateNewTerm(t2, arity, functor, args), CurrVarStack))
                        {
                            return false;
                        }

                        break;
                    }
                    else
                    {
                        if (!term.Arg(1).Unify(new AtomTerm(term.Symbol, t0.Functor), CurrVarStack))
                        {
                            return false;
                        }

                        if (!term.Arg(2).Unify(new DecimalTerm(term.Symbol, t0.Arity), CurrVarStack))
                        {
                            return false;
                        }

                        break;
                    }

                case BI.arg: // arg( N, BaseTerm, A)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsVar || t1.IsVar)
                    {
                        return false;
                    }

                    n = t0.To<int>(); // N is 1-based

                    if (n <= 0 || n > t1.Arity)
                    {
                        return false;
                    }

                    if (!t1.Arg(n - 1).Unify(term.Arg(2), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.abolish: // abolish( X/N)
                    t0 = term.Arg(0);
                    result = true;
                    if (t0.HasFunctor("/") && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                    {
                        result = PredTable.Abolish(t0.Arg(0).FunctorToString, t0.Arg(1).To<short>());
                    }
                    else
                    {
                        result = false;
                    }

                    if (!result)
                    {
                        return false;
                    }

                    break;

                case BI.gensym: // gensym( X)
                    if (term.Arity == 1)
                    {
                        t0 = new AtomTerm(term.Symbol, "v" + gensymInt++);

                        if (t0.Unify(term.Arg(0), CurrVarStack))
                        {
                            break;
                        }

                        return false;
                    }
                    else
                    {
                        if (!term.Arg(0).IsAtom)
                        {
                            return false;
                        }

                        t0 = new AtomTerm(term.Symbol, term.Arg(0).FunctorToString + gensymInt++);

                        if (t0.Unify(term.Arg(1), CurrVarStack))
                        {
                            break;
                        }

                        return false;
                    }

                case BI.var:
                    if (!term.Arg(0).IsVar ||
                        (term.Arity == 2 &&
                         !term.Arg(1).Unify(new StringTerm(term.Symbol, term.Arg(0).Name), CurrVarStack)))
                    {
                        return false;
                    }

                    break;

                case BI.nonvar:
                    if (!term.Arg(0).IsVar)
                    {
                        break;
                    }

                    return false;

                case BI.atom_:
                    if (term.Arg(0).IsAtom)
                    {
                        break;
                    }

                    return false;

                case BI.atomic:
                    if (term.Arg(0).IsAtomic)
                    {
                        break;
                    }

                    return false;

                case BI.integer:
                    if (term.Arg(0).IsInteger)
                    {
                        break;
                    }

                    return false;

                case BI.float_:
                    if (term.Arg(0).IsFloat)
                    {
                        break;
                    }

                    return false;

                case BI.number:
                    if (term.Arg(0).IsNumber)
                    {
                        break;
                    }

                    return false;

                case BI.compound:
                    if (term.Arg(0).IsCompound)
                    {
                        break;
                    }

                    return false;

                case BI.list:
                    if (term.Arg(0).IsProperList)
                    {
                        break;
                    }

                    return false;

                case BI.string_:
                    if (term.Arg(0).IsString)
                    {
                        break;
                    }

                    return false;

                case BI.bool_:
                    if (term.Arg(0).IsBool)
                    {
                        break;
                    }

                    return false;

                case BI.datetime: // datetime/1/4/7
                    t0 = term.Arg(0);

                    if (term.Arity == 1)
                    {
                        if (!t0.IsDateTime)
                        {
                            return false;
                        }
                    }
                    else if (t0.IsDateTime)
                    {
                        dati = t0.To<DateTime>();

                        if (!term.Arg(1).Unify(new DecimalTerm(term.Symbol, dati.Year), CurrVarStack) ||
                            !term.Arg(2).Unify(new DecimalTerm(term.Symbol, dati.Month), CurrVarStack) ||
                            !term.Arg(3).Unify(new DecimalTerm(term.Symbol, dati.Day), CurrVarStack) ||
                            (term.Arity == 7 &&
                             (!term.Arg(4).Unify(new DecimalTerm(term.Symbol, dati.Hour), CurrVarStack) ||
                              !term.Arg(5).Unify(new DecimalTerm(term.Symbol, dati.Minute), CurrVarStack) ||
                              !term.Arg(6).Unify(new DecimalTerm(term.Symbol, dati.Second), CurrVarStack)
                             )))
                        {
                            return false;
                        }
                    }
                    else if (t0.IsVar)
                    {
                        if (term.Arity == 4)
                        {
                            dati = new DateTime(
                                term.Arg(1).To<int>(),
                                term.Arg(2).To<int>(),
                                term.Arg(3).To<int>());
                        }
                        else
                        {
                            dati = new DateTime(
                                term.Arg(1).To<int>(),
                                term.Arg(2).To<int>(),
                                term.Arg(3).To<int>(),
                                term.Arg(4).To<int>(),
                                term.Arg(5).To<int>(),
                                term.Arg(6).To<int>());
                        }

                        if (!t0.Unify(new DateTimeTerm(term.Symbol, dati), CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else
                    {
                        IO.ThrowRuntimeException("datetime/4/7: first argument must be either a DateTime or a var", CurrVarStack,
                            term);

                        return false;
                    }

                    break;

                case BI.timespan:
                    t0 = term.Arg(0);

                    if (term.Arity == 1)
                    {
                        if (!t0.IsTimeSpan)
                        {
                            return false;
                        }
                    }
                    else if (t0.IsTimeSpan)
                    {
                        ti = t0.To<TimeSpan>();

                        if (!term.Arg(4).Unify(new DecimalTerm(term.Symbol, ti.Hours), CurrVarStack) ||
                            !term.Arg(5).Unify(new DecimalTerm(term.Symbol, ti.Minutes), CurrVarStack) ||
                            !term.Arg(6).Unify(new DecimalTerm(term.Symbol, ti.Seconds), CurrVarStack)
                        )
                        {
                            return false;
                        }
                    }
                    else if (t0.IsVar)
                    {
                        ti = new TimeSpan(
                            term.Arg(1).To<int>(),
                            term.Arg(2).To<int>(),
                            term.Arg(3).To<int>());

                        if (!t0.Unify(new TimeSpanTerm(term.Symbol, ti), CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else
                    {
                        IO.ThrowRuntimeException("timespan/4: first argument must be either a TimeSpan or a var", CurrVarStack,
                            term);

                        return false;
                    }

                    break;

                case BI.is_: // X is Y
                    t0 = term.Arg(1).Eval();
                    if (term.Arg(0).Unify(t0, CurrVarStack))
                    {
                        break;
                    }

                    return false;

                case BI.ne_uni: // X \= Y
                    if (term.Arg(0).Unify(term.Arg(1), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.eq_num: // X =:=
                    if (term.Arg<decimal>(0) == term.Arg<decimal>(1))
                    {
                        break;
                    }

                    return false;

                case BI.ne_num: // X =\= Y
                    if (term.Arg<decimal>(0) != term.Arg<decimal>(1))
                    {
                        break;
                    }

                    return false;

                case BI.lt_num: // X < Y
                    if (term.Arg<decimal>(0) < term.Arg<decimal>(1))
                    {
                        break;
                    }

                    return false;

                case BI.le_num: // X =< Y
                    if (term.Arg<decimal>(0) <= term.Arg<decimal>(1))
                    {
                        break;
                    }

                    return false;

                case BI.gt_num: // X > Y
                    if (term.Arg<decimal>(0) > term.Arg<decimal>(1))
                    {
                        break;
                    }

                    return false;

                case BI.ge_num: // X >= Y
                    if (term.Arg<decimal>(0) >= term.Arg<decimal>(1))
                    {
                        break;
                    }

                    return false;

                case BI.eq_str: // X == Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) == 0)
                    {
                        break;
                    }

                    return false;

                case BI.ne_str: // X \== Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) != 0)
                    {
                        break;
                    }

                    return false;

                case BI.lt_ord: // X @< Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) < 0)
                    {
                        break;
                    }

                    return false;

                case BI.le_ord: // X @=< Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) <= 0)
                    {
                        break;
                    }

                    return false;

                case BI.gt_ord: // X @> Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) > 0)
                    {
                        break;
                    }

                    return false;

                case BI.ge_ord: // X @>= Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) >= 0)
                    {
                        break;
                    }

                    return false;

                case BI.univ: // X =.. Y
                    t0 = term.Arg(0);

                    if (t0.IsVar
                    ) // create a function or operator representation of the term rhs, and bind that to the lhs
                    {
                        t1 = term.Arg(1);

                        if (t1.IsVar || !t1.IsProperList)
                        {
                            return false;
                        }

                        if (t1.Arg(0).IsVar)
                        {
                            return false; // not a valid functor
                        }

                        functor = t1.Arg(0).FunctorToString.ToAtom();
                        // convert rest of term to arguments: calculate arity first
                        t1 = t1.Arg(1);
                        arity = 0;
                        t2 = t1;

                        while (t2.Arity == 2)
                        {
                            arity++;
                            t2 = t2.Arg(1);
                        }

                        // create arguments
                        BaseTerm[] args = new BaseTerm[arity];

                        for (int i = 0; i < arity; i++)
                        {
                            args[i] = t1.Arg(0);
                            t1 = t1.Arg(1);
                        }

                        t0.Unify(CreateNewTerm(t1, arity, functor, args), CurrVarStack);

                        break;
                    }
                    else // create a list representation of the lhs and unify that with the rhs
                    {
                        arity = t0.Arity;
                        t1 = BaseTerm.EMPTYLIST;

                        for (int i = arity; i > 0; i--)
                        {
                            t1 = new ListTerm(term.Symbol, t0.Arg(i - 1), t1); // [arg1, arg2, ...]
                        }

                        t1 = new ListTerm(term.Symbol, new AtomTerm(term.Symbol, t0.FunctorToString),
                            t1); // [functor, arg1, arg2, ...]

                        if (!t1.Unify(term.Arg(1), CurrVarStack))
                        {
                            return false;
                        }

                        break;
                    }

                case BI.unifiable: // X can be unified with Y, but without variable bindings
                    if (!term.Arg(0).IsUnifiableWith(term.Arg(1), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.fileexists:
                    t0 = term.Arg(0);

                    fileName = Utils.FileNameFromTerm(t0, ".pl");

                    if (fileName == null || !File.Exists(fileName))
                    {
                        return false;
                    }

                    break;

                case BI.read: // read( ?Term)
                    t0 = ReadTerm();

                    if (!term.Arg(0).Unify(t0, CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.readatoms: // readatoms( ?List)
                    string line = ReadLine();

                    if (String.IsNullOrEmpty(line = line.Trim()))
                    {
                        t0 = BaseTerm.EMPTYLIST;
                    }
                    else
                    {
                        string[] words = line.Tokens();
                        BaseTerm[] terms = new BaseTerm[words.Length];

                        for (int i = 0; i < words.Length; i++)
                        {
                            terms[i] = TermFromWord(words[i]);
                        }

                        t0 = ListTerm.ListFromArray(terms);
                    }

                    if (!term.Arg(0).Unify(t0, CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.readatom: // readatom( A)
                    t0 = TermFromWord(ReadLine());

                    if (!term.Arg(0).Unify(t0, CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.readln: // readln( L)
                    line = ReadLine();

                    if (line == null || !term.Arg(0).Unify(new StringTerm(term.Symbol, line), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.readeof: // readeof( +F, ?T) -- unify the entire contents of file F with string T
                    if ((t0 = term.Arg(0)).IsVar)
                    {
                        return false;
                    }

                    x = Utils.FileNameFromTerm(t0, ".txt");
                    string fileContents = null;

                    try
                    {
                        fileContents = File.ReadAllText(x);
                    }
                    catch (Exception e)
                    {
                        IO.ThrowRuntimeException($"Error reading file {x}. Message was:\r\n{e.Message}", CurrVarStack, term);
                    }

                    if (!term.Arg(1).Unify(new StringTerm(term.Symbol, fileContents), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.get0: // get0( C): any character
                    n = ReadChar();

                    if (!term.Arg(0).Unify(new DecimalTerm(term.Symbol, n), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.get: // get( C): skip non-printables
                    while (true)
                    {
                        n = ReadChar();

                        if (!Char.IsControl((char)n))
                        {
                            break; // break if printable
                        }
                    }

                    if (!term.Arg(0).Unify(new DecimalTerm(term.Symbol, n), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.write:
                    Write(term.Arg(0), true);
                    break;

                case BI.writeln: // writeln( X)
                    Write(term.Arg(0), true);
                    NewLine();
                    break;

                case BI.writef: // writef( X, L) // formatted write, L last
                    string ln = null;
                    goto case BI.writelnf;
                case BI.writelnf: // writef( X, L) // formatted writeln, L last
                    ln = "ln";
                    if (!(term.Arg(0) is StringTerm))
                    {
                        IO.ThrowRuntimeException(String.Format("First argument of write(0}f/2 must be a string", ln),
                            CurrVarStack,
                            term);
                    }

                    if (!(term.Arg(1) is ListTerm))
                    {
                        IO.ThrowRuntimeException($"Second argument of write{ln}f/2 must be a list", CurrVarStack, term);
                    }

                    string fs = Utils.Format(term.Arg(0), term.Arg(1));

                    if (fs == null)
                    {
                        return false;
                    }

                    Write(fs);

                    if (term.FunctorToString == "writelnf")
                    {
                        NewLine();
                    }

                    break;

                case BI.put: // put( C)
                    n = term.Arg<int>(0);
                    Write(((char)n).ToString());
                    break;

                case BI.nl:
                    NewLine();
                    break;

                case BI.tab: // tab( +N)
                    n = term.Arg<int>(0);

                    if (n > 0)
                    {
                        Write(Spaces(n));
                    }

                    break;

                case BI.errorlevel: // errorlevel( +N) % sets DOS ERRORLEVEL (0..255)
                    break;

                case BI.print: // print( X)
                    Write(term.Arg(0), true);
                    break;

                case BI.treeprint: //
                    term.Arg(0).TreePrint(0, this);
                    break;

                case BI.display:
                    Write(term.Arg(0).ToDisplayString(), false);
                    NewLine();
                    break;

                case BI.console:
                    if (term.Arity == 2 && !(term.Arg(0) is StringTerm))
                    {
                        IO.ThrowRuntimeException("First argument of console/1/2 must be a string", CurrVarStack, term);
                    }

                    if (term.Arity == 2)
                    {
                        if (!(term.Arg(1) is ListTerm))
                        {
                            IO.ThrowRuntimeException("Second argument of console/2 must be a list", CurrVarStack, term);
                        }

                        a = Utils.Format(term.Arg(0), term.Arg(1));
                        IO.WriteLine(a);
                    }
                    else
                    {
                        IO.WriteLine(term.Arg(0).ToString());
                    }

                    break;

                case BI.cls:
                    IO.ClearScreen();
                    break;

                case BI.showfile:
                    t0 = term.Arg(0);
                    fileName = Utils.FileNameFromTerm(t0, ".pl");

                    if (fileName == null || !File.Exists(fileName))
                    {
                        return false;
                    }

                    IO.WriteLine(File.ReadAllText(fileName));
                    break;

                case BI.between:
                    IntRangeTerm irt;

                    if (term.Arg(0).IsVar) // first Call only, Arg(0) contains State info
                    {
                        t1 = term.Arg(1);
                        t2 = term.Arg(2);
                        bool inf = t2.HasFunctor("inf") || t2.HasFunctor("infinity"); // stolen from SWI

                        if (term.OneOfArgsIsVar(1, 2))
                        {
                            return false;
                        }

                        if (!t1.IsInteger || !(t2.IsInteger || inf))
                        {
                            return false;
                        }

                        irt = new IntRangeTerm(term.Symbol, t1, inf ? new DecimalTerm(term.Symbol, int.MaxValue) : t2);
                        term.Arg(0).Unify(irt, CurrVarStack);

                        break;
                    }

                    irt = (IntRangeTerm)term.Arg(0);
                    DecimalTerm dt;

                    if (!irt.GetNextValue(out dt) ||
                        !term.Arg(3).Unify(dt, CurrVarStack)) // done
                    {
                        term.SetArg(0, BaseTerm.VAR);

                        return false;
                    }

                    break;

                case BI.current_op: // current_op( ?Precedence, ?Assoc, ?Functor)
                    IEnumerator<OperatorDescr> iEnum = null;
                    t0 = term.Arg(0);

                    if (t0.IsVar) // first call only, Arg(0) contains State info
                    {
                        iEnum = OpTable.GetEnumerator();
                        term.Arg(0).Unify(new UserClassTerm<IEnumerator<OperatorDescr>>(term.Symbol, iEnum),
                            CurrVarStack);

                        break;
                    }

                    iEnum = ((UserClassTerm<IEnumerator<OperatorDescr>>)t0).UserObject;

                    while (true)
                    {
                        if (!iEnum.MoveNext())
                        {
                            term.SetArg(0, BaseTerm.VAR);

                            return false;
                        }

                        OperatorDescr opDescr = iEnum.Current;

                        DecimalTerm it;
                        AtomTerm at, nt;

                        if (term.Arg(1).IsUnifiableWith(it = new DecimalTerm(term.Symbol, opDescr.Prec),
                                CurrVarStack) &&
                            term.Arg(2).IsUnifiableWith(at = new AtomTerm(term.Symbol, opDescr.Assoc.ToString()),
                                CurrVarStack) &&
                            term.Arg(3).IsUnifiableWith(nt = new AtomTerm(term.Symbol, opDescr.Name), CurrVarStack))
                        {
                            term.Arg(1).Unify(it, CurrVarStack);
                            term.Arg(2).Unify(at, CurrVarStack);
                            term.Arg(3).Unify(nt, CurrVarStack);

                            break;
                        }
                    }

                    break;

                case BI.atom_string: // atom_string( ?A, ?S)
                    t1 = term.Arg(1);

                    if (t1.IsVar) // create a list containing A's characters or character codes
                    {
                        t0 = term.Arg(0);

                        if (!t0.IsAtomic)
                        {
                            return false;
                        }

                        t2 = NewIsoOrCsStringTerm(t0.Symbol, t0.FunctorToString.Dequoted());

                        if (!t1.Unify(t2, CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else // t1 is string
                    {
                        if (t1.IsProperList)
                        {
                            StringBuilder sb = new StringBuilder();

                            while (t1.Arity == 2)
                            {
                                t2 = t1.Arg(0);

                                if (!t2.IsInteger)
                                {
                                    return false;
                                }

                                sb.Append((char)t2.To<int>());
                                t1 = t1.Arg(1);
                            }

                            if (!term.Arg(0).Unify(TermFromWord(sb.ToString()), CurrVarStack))
                            {
                                return false;
                            }
                        }
                        else if (t1.IsString && (a = t1.FunctorToString).Length > 0)
                        {
                            if (!term.Arg(0).Unify(TermFromWord(a.Dequoted()), CurrVarStack))
                            {
                                return false;
                            }
                        }
                        else
                        {
                            return false;
                        }
                    }

                    break;

                case BI.atom_concat:
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);
                    t2 = term.Arg(2);

                    if (t0.IsVar)
                    {
                        if (!t2.IsAtomic || !t1.IsAtomic)
                        {
                            return false;
                        }

                        string both = t2.FunctorToString.Dequoted();
                        string second = t1.FunctorToString.Dequoted();

                        if (!both.EndsWith(second))
                        {
                            return false;
                        }

                        t3 = new AtomTerm(term.Symbol, both.Substring(0, both.Length - second.Length).ToAtom());

                        if (!t0.Unify(t3, CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else if (t1.IsVar)
                    {
                        if (!t2.IsAtomic || !t0.IsAtomic)
                        {
                            return false;
                        }

                        string both = t2.FunctorToString.Dequoted();
                        string first = t0.FunctorToString.Dequoted();

                        if (!both.StartsWith(first))
                        {
                            return false;
                        }

                        t3 = new AtomTerm(term.Symbol, both.Substring(first.Length).ToAtom());

                        if (!t1.Unify(t3, CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else
                    {
                        if (!t0.IsAtomic || !t1.IsAtomic)
                        {
                            return false;
                        }

                        t3 = new AtomTerm(term.Symbol,
                            (t0.FunctorToString.Dequoted() + t1.FunctorToString.Dequoted()).ToAtom());

                        if (!t2.Unify(t3, CurrVarStack))
                        {
                            return false;
                        }
                    }

                    break;

                case BI.atom_length:
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t1.IsVar)
                    {
                        if (!t0.IsAtomic)
                        {
                            return false;
                        }

                        int len = t0.FunctorToString.Dequoted().Length;

                        if (!t1.Unify(new DecimalTerm(term.Symbol, len), CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else
                    {
                        if (t0.IsVar)
                        {
                            return false;
                        }

                        if (!t0.Unify(t1, CurrVarStack))
                        {
                            return false;
                        }
                    }

                    break;

                case BI.atom_chars: // atom_chars( ?A, ?L)
                    t1 = term.Arg(1);
                    t0 = term.Arg(0);

                    if (t0.IsAtom || t0.IsNumber) // create a list containing A's characters or character codes
                    {
                        if (!t0.IsAtomic)
                        {
                            return false;
                        }

                        t2 = NewIsoOrCsStringTerm(term.Symbol, t0.FunctorToString.Dequoted());
                        line = ((StringTerm)t2).Value;
                        ListTerm list;

                        if (String.IsNullOrEmpty(line = line.Trim()))
                        {
                            list = BaseTerm.EMPTYLIST;
                        }
                        else
                        {
                            char[] chars = line.ToCharArray();
                            BaseTerm[] terms = new BaseTerm[chars.Length];
                            for (int i = 0; i < chars.Length; i++)
                            {
                                terms[i] = new AtomTerm(term.Symbol, chars[i].ToString().ToAtom());
                            }

                            list = ListTerm.ListFromArray(terms);
                        }

                        if (!list.Unify(t1, CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else // t1 is string
                    {
                        if (t1.IsProperList)
                        {
                            StringBuilder sb = new StringBuilder();

                            while (t1.Arity == 2)
                            {
                                t2 = t1.Arg(0);

                                if (!t2.IsAtom)
                                {
                                    return false;
                                }

                                sb.Append(t2);
                                t1 = t1.Arg(1);
                            }

                            if (!term.Arg(0).Unify(TermFromWord(sb.ToString()), CurrVarStack))
                            {
                                return false;
                            }
                        }
                        else if (t1.IsString && (a = t1.FunctorToString).Length > 0)
                        {
                            if (!term.Arg(0).Unify(TermFromWord(a.Dequoted()), CurrVarStack))
                            {
                                return false;
                            }
                        }
                        else
                        {
                            return false;
                        }
                    }

                    break;

                case BI.string_term: // string_term( ?S, ?T) -- convert string S to Prolog term T and v.v.
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsString)
                    {
                        PrologParser p = new PrologParser(this)
                        {
                            StreamIn = "&reading\r\n" + t0.FunctorToString.AddEndDot()
                        };

                        if (!t1.Unify(p.ReadTerm, CurrVarStack))
                        {
                            return false;
                        }
                    }
                    else if (!t0.Unify(new StringTerm(term.Symbol, t1.ToString()), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.string_words:
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsString)
                    {
                        ListTerm list;
                        line = ((StringTerm)t0).Value;

                        if (String.IsNullOrEmpty(line = line.Trim()))
                        {
                            list = BaseTerm.EMPTYLIST;
                        }
                        else
                        {
                            string[] words = line.Tokens();
                            BaseTerm[] terms = new BaseTerm[words.Length];

                            for (int i = 0; i < words.Length; i++)
                            {
                                terms[i] = TermFromWord(words[i]);
                            }

                            list = ListTerm.ListFromArray(terms);
                        }

                        if (list.Unify(t1, CurrVarStack))
                        {
                            break;
                        }
                    }
                    else if (t1.IsProperList)
                    {
                        StringBuilder sb = new StringBuilder();
                        bool first = true;

                        foreach (BaseTerm t in (ListTerm)t1)
                        {
                            if (first)
                            {
                                first = false;
                            }
                            else
                            {
                                sb.Append(' ');
                            }

                            sb.Append(t);
                        }

                        if (t0.Unify(new StringTerm(term.Symbol, sb.ToString()), CurrVarStack))
                        {
                            break;
                        }
                    }

                    return false;

                case BI.stringstyle:
                    t0 = term.Arg(0);
                    if (t0.IsVar)
                    {
                        t0.Unify(new AtomTerm(term.Symbol, CsharpStrings ? "csharp" : "iso"), CurrVarStack);
                    }
                    else
                    {
                        SetStringStyle(t0);
                    }

                    break;

                case BI.name: // name( ?A, ?L)
                    t1 = term.Arg(1);

                    if (t1.IsVar) // create a list containing atom A's characters or character codes
                    {
                        t0 = term.Arg(0);

                        if (!t0.IsAtomic)
                        {
                            return false;
                        }

                        char[] chars = t0.FunctorToString.Dequoted("'").ToCharArray();
                        t0 = BaseTerm.EMPTYLIST;

                        for (int i = chars.Length - 1; i >= 0; i--)
                        {
                            t2 = new DecimalTerm(term.Symbol, chars[i]);
                            t0 = new ListTerm(term.Symbol, t2, t0);
                        }

                        t1.Unify(t0, CurrVarStack);
                    }
                    else
                    {
                        if (t1.IsProperList)
                        {
                            StringBuilder sb = new StringBuilder();

                            while (t1.Arity == 2)
                            {
                                t2 = t1.Arg(0);

                                if (!t2.IsInteger)
                                {
                                    return false;
                                }

                                sb.Append((char)t2.To<int>());
                                t1 = t1.Arg(1);
                            }

                            a = sb.ToString().ToAtomic(out type);

                            if (type == TermType.Number)
                            {
                                t2 = new DecimalTerm(term.Symbol, int.Parse(a));
                            }
                            else if (type == TermType.String)
                            {
                                t2 = NewIsoOrCsStringTerm(term.Symbol, a);
                            }
                            else
                            {
                                t2 = new AtomTerm(term.Symbol, a);
                            }

                            if (!term.Arg(0).Unify(t2, CurrVarStack))
                            {
                                return false;
                            }
                        }
                        else
                        {
                            return false;
                        }
                    }

                    break;

                case BI.expand_term: // expand_term( +(P-->Q), -R)
                    t0 = term.Arg(0); // P-->Q
                    t1 = term.Arg(1); // R
                    BaseTerm head = t0.Arg(0);
                    TermNode body = t0.Arg(1).ToDCG(ref head, CurrVarStack);
                    t2 = new ClauseTerm(term.Symbol, new ClauseNode(head, body), CurrVarStack).Copy(CurrVarStack);

                    if (!t1.Unify(t2, CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.numbervars: // numbervars(+X, +B, -E)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);
                    t2 = term.Arg(2);

                    if (!t1.IsInteger || !t2.IsVar)
                    {
                        return false;
                    }

                    int k = t1.To<int>();
                    t0.NumberVars(ref k, CurrVarStack);
                    t2.Unify(new DecimalTerm(term.Symbol, k), CurrVarStack);
                    break;

                case BI.format: // format/3
                    fs = Utils.Format(term.Arg(0), term.Arg(1));

                    if (fs == null)
                    {
                        return false;
                    }

                    if (!term.Arg(2).Unify(new StringTerm(term.Symbol, fs), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.predicatePN: // predicate( +P/N)
                    t0 = term.Arg(0);
                    result = true;

                    if (t0.HasFunctor("/") && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                    {
                        result = PredTable.IsPredicate(t0.Arg(0).FunctorToString, t0.Arg<int>(1));
                    }
                    else
                    {
                        result = false;
                    }

                    if (!result)
                    {
                        return false;
                    }

                    break;

                case BI.predicateX: // predicate( +T)
                    t0 = term.Arg(0);

                    if (t0.IsVar || !PredTable.IsPredicate(t0.FunctorToString, t0.Arity))
                    {
                        return false;
                    }

                    break;

                // term_pattern( T, P, Dmin, Dmax)
                // find pattern P in term T between depths Dmin and Dmax (incl), and unify result with P
                case BI.term_pattern:
                    t0 = term.Arg(0); // State
                    t1 = term.Arg(1); // term

                    if (t1.IsVar)
                    {
                        IO.ThrowRuntimeException("term_pattern: uninstantiated first argument not allowed", CurrVarStack, term);
                    }

                    t2 = term.Arg(2); // pattern
                    t3 = term.Arg(3); // Dmin (0 if var)
                    t4 = term.Arg(4); // Dmax (inf if var)
                    t5 = term.Arg(5); // Path ('!' if not wanted)
                    bool skipVars = true; // i.e. the search pattern will not match a term variable in t

                    NodeIterator iterable = null;

                    if (t0.IsVar) // first call only, Arg(0) contains State info
                    {
                        iterable = new NodeIterator(t1, t2, t3, t4, skipVars, t5, CurrVarStack);
                        term.Arg(0).Unify(new UserClassTerm<NodeIterator>(term.Symbol, iterable), CurrVarStack);

                        break;
                    }

                    iterable = ((UserClassTerm<NodeIterator>)t0).UserObject;

                    if (!iterable.MoveNext()) // if success, pattern gets bound (and its vars get instantiated)
                    {
                        term.SetArg(0, BaseTerm.VAR);

                        return false;
                    }

                    break;

                case BI.ground: // ground( +T)
                    if (!term.Arg(0).IsGround)
                    {
                        return false;
                    }

                    break;

                case BI.throw_: // throw( [+C,] +T [,+L])
                    t0 = term.Arg(0);
                    t1 = null;
                    string exceptionClass = null;
                    string exceptionMessage;

                    if (term.Arity == 2)
                    {
                        if (t0 is AtomTerm || t0 is DecimalTerm) // exception class
                        {
                            exceptionClass = t0.FunctorToString;
                            t0 = term.Arg(1);
                            t1 = term.Arity == 2 ? null : term.Arg(2);
                        }
                        else
                        {
                            t1 = term.Arg(1);
                        }
                    }
                    else if (term.Arity == 3) // something is wrong
                    {
                        IO.ThrowRuntimeException($"First argument of throw/3 ({t0}) is not an atom or an integer", CurrVarStack,
                            term);
                    }

                    if (!(t0 is StringTerm))
                    {
                        IO.ThrowRuntimeException($"Throw/3: string expected instead of '{t0}'", CurrVarStack, term);
                    }

                    exceptionMessage = t1 == null ? t0.FunctorToString : Utils.Format(t0, t1);
                    Throw(exceptionClass, exceptionMessage);
                    break;

                case BI.today: // date( ?Y, ?M, ?D)
                    y = DateTime.Today.Year;
                    m = DateTime.Today.Month;
                    d = DateTime.Today.Day;

                    if (!term.Arg(0).Unify(new DecimalTerm(term.Symbol, y), CurrVarStack))
                    {
                        return false;
                    }

                    if (!term.Arg(1).Unify(new DecimalTerm(term.Symbol, m), CurrVarStack))
                    {
                        return false;
                    }

                    if (!term.Arg(2).Unify(new DecimalTerm(term.Symbol, d), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.now: // time( ?H, ?M, ?S)
                    h = DateTime.Now.Hour;
                    m = DateTime.Now.Minute;
                    s = DateTime.Now.Second;

                    if (!term.Arg(0).Unify(new DecimalTerm(term.Symbol, h), CurrVarStack))
                    {
                        return false;
                    }

                    if (!term.Arg(1).Unify(new DecimalTerm(term.Symbol, m), CurrVarStack))
                    {
                        return false;
                    }

                    if (!term.Arg(2).Unify(new DecimalTerm(term.Symbol, s), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.validdate: // validdate( +Y, +M, +D)
                    t0 = term.Arg(0);

                    if (t0.IsInteger)
                    {
                        y = t0.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t1 = term.Arg(1);

                    if (t1.IsInteger)
                    {
                        m = t1.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t2 = term.Arg(2);

                    if (t2.IsInteger)
                    {
                        d = t2.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    try
                    {
                        new DateTime(y, m, d);
                    }
                    catch
                    {
                        return false;
                    }

                    break;

                case BI.validtime: // validtime( +H, +M, +S)
                    t0 = term.Arg(0);

                    if (t0.IsInteger)
                    {
                        h = t0.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t1 = term.Arg(1);

                    if (t1.IsInteger)
                    {
                        m = t1.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t2 = term.Arg(2);

                    if (t2.IsInteger)
                    {
                        s = t2.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    try
                    {
                        new DateTime(2000, 1, 1, h, m, s);
                    }
                    catch
                    {
                        return false;
                    }

                    break;

                case BI.string_datetime: // convert a string to a DateTime term
                    t0 = term.Arg(0);

                    if (term.Arity > 2)
                    {
                        if (!t0.IsString || !DateTime.TryParse(t0.FunctorToString, out dati))
                        {
                            IO.ThrowRuntimeException($"string_datetime: invalid date format: '{t0}' for first argument",
                                CurrVarStack, term);

                            return false;
                        }

                        if (!term.Arg(1).Unify(new DecimalTerm(term.Symbol, dati.Year), CurrVarStack) ||
                            !term.Arg(2).Unify(new DecimalTerm(term.Symbol, dati.Month), CurrVarStack) ||
                            !term.Arg(3).Unify(new DecimalTerm(term.Symbol, dati.Day), CurrVarStack) ||
                            (term.Arity == 7 &&
                             (!term.Arg(4).Unify(new DecimalTerm(term.Symbol, dati.Hour), CurrVarStack) ||
                              !term.Arg(5).Unify(new DecimalTerm(term.Symbol, dati.Minute), CurrVarStack) ||
                              !term.Arg(6).Unify(new DecimalTerm(term.Symbol, dati.Second), CurrVarStack)
                             )))
                        {
                            return false;
                        }
                    }
                    else
                    {
                        t1 = term.Arg(1);

                        if (t0.IsString)
                        {
                            if (DateTime.TryParse(t0.FunctorToString, out dati))
                            {
                                if (!t1.Unify(new DateTimeTerm(term.Symbol, dati), CurrVarStack))
                                {
                                    return false;
                                }
                            }
                            else
                            {
                                IO.ThrowRuntimeException($"string_datetime: error while parsing first argument: '{t0}'",
                                    CurrVarStack, term);

                                return false;
                            }
                        }
                        else if (t0.IsVar)
                        {
                            if (t1.IsDateTime)
                            {
                                if (!t0.Unify(new StringTerm(term.Symbol, t1.FunctorToString), CurrVarStack))
                                {
                                    return false;
                                }
                            }
                            else
                            {
                                IO.ThrowRuntimeException($"string_datetime: second argument is not a DateTime term: '{t0}'",
                                    CurrVarStack, term);

                                return false;
                            }
                        }
                        else
                        {
                            IO.ThrowRuntimeException($"string_datetime: first argument not a string or var: '{t0}'",
                                CurrVarStack,
                                term);

                            return false;
                        }
                    }

                    break;

                case BI.date_part: // get the date part of a DateTime (time set to 00:00:00)
                    t0 = term.Arg(0);

                    if (!t0.IsDateTime)
                    {
                        IO.ThrowRuntimeException($"date_part: first argument not a DateTime: '{t0}'", CurrVarStack, term);

                        return false;
                    }

                    if (!term.Arg(1).Unify(new DateTimeTerm(term.Symbol, t0.To<DateTime>().Date), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.time_part: // get the time part of a DateTime (time set to 00:00:00)
                    t0 = term.Arg(0);

                    if (!t0.IsDateTime)
                    {
                        IO.ThrowRuntimeException($"date_part: first argument not a DateTime: '{t0}'", CurrVarStack, term);

                        return false;
                    }

                    if (!term.Arg(1).Unify(new TimeSpanTerm(term.Symbol, t0.To<DateTime>().TimeOfDay), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.dayname: // dayname( +Y, +M, +D, ?N)
                    DayOfWeek dow;
                    t0 = term.Arg(0);

                    if (t0.IsInteger)
                    {
                        y = t0.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t1 = term.Arg(1);

                    if (t1.IsInteger)
                    {
                        m = t1.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t2 = term.Arg(2);

                    if (t2.IsInteger)
                    {
                        d = t2.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    try
                    {
                        dow = new DateTime(y, m, d).DayOfWeek;
                    }
                    catch
                    {
                        return false;
                    }

                    if (!term.Arg(3).Unify(new StringTerm(term.Symbol, dow.ToString("G")), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.dayofweek: // dayofweek( +Y, +M, +D, ?N)
                    t0 = term.Arg(0);

                    if (t0.IsInteger)
                    {
                        y = t0.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t1 = term.Arg(1);

                    if (t1.IsInteger)
                    {
                        m = t1.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t2 = term.Arg(2);

                    if (t2.IsInteger)
                    {
                        d = t2.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    try
                    {
                        n = (int)new DateTime(y, m, d).DayOfWeek;
                    }
                    catch
                    {
                        return false;
                    }

                    if (!term.Arg(3).Unify(new DecimalTerm(term.Symbol, n), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.dayofyear: // dayofyear( +Y, +M, +D, ?N)
                    t0 = term.Arg(0);

                    if (t0.IsInteger)
                    {
                        y = t0.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t1 = term.Arg(1);

                    if (t1.IsInteger)
                    {
                        m = t1.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    t2 = term.Arg(2);

                    if (t2.IsInteger)
                    {
                        d = t2.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    try
                    {
                        n = new DateTime(y, m, d).DayOfYear;
                    }
                    catch
                    {
                        IO.ThrowRuntimeException($"dayofyear: Invalid date (Y:{y} M:{m} D:{d})", CurrVarStack, term);

                        return false;
                    }

                    if (!term.Arg(3).Unify(new DecimalTerm(term.Symbol, n), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.leapyear: // leapyear( +Y)
                    t0 = term.Arg(0);

                    if (t0.IsInteger)
                    {
                        y = t0.To<int>();
                    }
                    else
                    {
                        return false;
                    }

                    if (!DateTime.IsLeapYear(y))
                    {
                        return false;
                    }

                    break;

                case BI.weekno: // weekno(+Y, +M, +D, ?N) // week Number of date Y-M-D, or current week Number
                    if (term.Arity == 4)
                    {
                        t0 = term.Arg(0);

                        if (t0.IsInteger)
                        {
                            y = t0.To<int>();
                        }
                        else
                        {
                            return false;
                        }

                        t1 = term.Arg(1);

                        if (t1.IsInteger)
                        {
                            m = t1.To<int>();
                        }
                        else
                        {
                            return false;
                        }

                        t2 = term.Arg(2);

                        if (t2.IsInteger)
                        {
                            d = t2.To<int>();
                        }
                        else
                        {
                            return false;
                        }

                        try
                        {
                            n = Utils.WeekNo(new DateTime(y, m, d));
                        }
                        catch // invalid date
                        {
                            IO.ThrowRuntimeException($"weekno: Invalid date (Y:{y} M:{m} D:{d})", CurrVarStack, term);

                            return false;
                        }
                    }
                    else
                    {
                        n = Utils.WeekNo(DateTime.Today);
                    }

                    if (!term.Arg(term.Arity - 1).Unify(new DecimalTerm(term.Symbol, n), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.flat: // flat( +X, ?Y)
                    t0 = term.Arg(0);

                    if (!t0.IsProperOrPartialList)
                    {
                        return false;
                    }

                    if (!term.Arg(1).Unify(((ListTerm)t0).FlattenList(), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.dcg_flat: // dcg_flat( +X, ?Y)
                    t0 = term.Arg(0);

                    if (!t0.IsDcgList)
                    {
                        return false;
                    }

                    if (!term.Arg(1).Unify(((DcgTerm)t0).FlattenDcgList(), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.append2: // conc2( +X, +Y, ?Z), X proper or partial t, Y anything
                    t0 = term.Arg(0);

                    if (!t0.IsProperOrPartialList)
                    {
                        return false;
                    }

                    if (!term.Arg(2).Unify(((ListTerm)t0).Append(term.Arg(1)), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.listing: // listing
                    if (!PredTable.ListAll(null, -1, false, true))
                    {
                        return false; // i.e. no predefined, all user
                    }

                    break;

                case BI.profile:
                    SetProfiling(true);
                    break;

                case BI.noprofile:
                    SetProfiling(false);
                    break;

                case BI.showprofile:
                    if (!Profiling)
                    {
                        IO.Message("Profiling is not on. Use profile/0 to switch it on");

                        return false;
                    }

                    if (term.Arity == 0)
                    {
                        PredTable.ShowProfileCounts(int.MaxValue);
                    }
                    else
                    {
                        t0 = term.Arg(0);

                        if (t0.IsNatural)
                        {
                            PredTable.ShowProfileCounts(t0.To<int>());
                        }
                        else
                        {
                            return IO.ThrowRuntimeException("Argument for profile/0 must be a positive integer value",
                                CurrVarStack,
                                term);
                        }
                    }

                    break;

                case BI.clearprofile:
                    PredTable.ClearProfileCounts();
                    break;

                case BI.listingXN: // listing( X/N)
                    t0 = term.Arg(0);
                    result = true;

                    if (t0.HasFunctor("/") && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                    {
                        result = PredTable.ListAll(t0.Arg(0).FunctorToString, t0.Arg<int>(1), false, true);
                    }
                    else
                    {
                        result = false;
                    }

                    if (!result)
                    {
                        return false;
                    }

                    break;

                case BI.listingX: // listing( X) -- t all predicates X/N (i.e. for each N)
                    t0 = term.Arg(0);

                    if (!t0.IsAtom)
                    {
                        return false;
                    }

                    if (!PredTable.ListAll(t0.FunctorToString, -1, false, true))
                    {
                        return false;
                    }

                    break;

                case BI.listing0: // listing0
                    if (!PredTable.ListAll(null, -1, true, false))
                    {
                        return false; // i.e. no user, all predefined
                    }

                    break;

                case BI.listing0XN: // listing0( X/N)
                    t0 = term.Arg(0);
                    result = true;

                    if (t0.HasFunctor("/") && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                    {
                        result = PredTable.ListAll(t0.Arg(0).FunctorToString, t0.Arg<int>(1), true, false);
                    }
                    else
                    {
                        result = false;
                    }

                    if (!result)
                    {
                        return false;
                    }

                    break;

                case BI.listing0X: // listing0( X)
                    t0 = term.Arg(0);

                    if (!t0.IsAtom)
                    {
                        return false;
                    }

                    if (!PredTable.ListAll(t0.FunctorToString, -1, true, false))
                    {
                        return false;
                    }

                    break;

                //case BI.pp_defines: // pp_defines( X) -- preprocessor symbol definitions -- mainly useful for debugging in nested calls
                //  t0 = term.Arg (0);
                //  if (!t0.IsVar) return false;
                //  t1 = ListTerm.EMPTYLIST;
                //  //IO.WriteLine ("PrologParser.PpSymbols.count = {0}", PrologParser.PpSymbols.Count);
                //  foreach (DictionaryEntry de in PrologParser.PpSymbols)
                //    t1 = new CompoundTerm ((de.Key as string).ToAtom (), new Variable (), t1);
                //  t0.Unify (t1, varStack);
                //  break;

                case BI.copy_term: // copy_term( X, Y)
                    if (!term.Arg(1).Unify(term.Arg(0).Copy(true, false, CurrVarStack), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.clearall: // clearall
                    Reset();
                    break;

                case BI.clause: // clause (Head,Body)
                    BaseTerm state = term.Arg(0); // State
                    t1 = term.Arg(1); // head
                    t2 = term.Arg(2); // body

                    if (t1.IsVar)
                    {
                        IO.ThrowRuntimeException("First argument of clause/2 is not sufficiently instantiated", CurrVarStack, term);

                        return false;
                    }

                    ClauseIterator iterator = null;

                    if (state.IsVar) // first call only
                    {
                        iterator = new ClauseIterator(PredTable, t1, CurrVarStack);
                        state.Unify(new UserClassTerm<ClauseIterator>(term.Symbol, iterator), CurrVarStack);

                        break;
                    }

                    iterator = ((UserClassTerm<ClauseIterator>)state).UserObject;

                    if (!iterator.MoveNext()) // sets iterator.ClauseBody
                    {
                        // Reset state upon final failure
                        term.SetArg(0, BaseTerm.VAR);

                        return false;
                    }

                    if (!t2.Unify(iterator.ClauseBody, CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.member: // member( X, L)
                    if ((t0 = term.Arg(0)).IsVar || !(t1 = term.Arg(1)).IsListNode)
                    {
                        return false;
                    }

                    result = false;

                    while (t1.Arity == 2)
                    {
                        if (result = t0.Unify(t1.Arg(0), CurrVarStack))
                        {
                            break;
                        }

                        t1 = t1.Arg(1);
                    }

                    currentCp.Kill(); // no backtracking to follow -> remove the choicepoint for the alternative clauses

                    if (!result)
                    {
                        return false;
                    }

                    break;

                // BACKTRACKING VERSION
                //          while (t1.Arity == 2)
                //          {
                //            if (result = t0.Unify (t1.Arg (0), varStack))
                //            {
                //              if ((t0 = t1.Arg (1)).Arity == 0) // empty t
                //                currentCp.Kill (); // no backtracking to follow -> remove the choicepoint for the alternative clauses
                //              else
                //                head.Arg (2).Bind (t0);  // set Rest to remainder of t (for backtracking)
                //
                //              break;
                //            }
                //            t1 = t1.Arg (1);
                //          }

                //case BI.append: // append( [_|_], [_|_], L)
                //  if (!(t0 = head.Arg (0)).IsProperOrPartialList) return false;

                //  t1 = head.Arg (1);

                //  if (!head.Arg (2).Unify (((BaseTermListTerm)t0).Append (t1), varStack))
                //    return false;

                //  break;

                // this version actually only returns the number of ClockTicks
                case BI.statistics: // statistics( X, [MSec,_])
                    t1 = term.Arg(1).Arg(0);
                    long time = ClockTicksMSecs();

                    if (!t1.Unify(new DecimalTerm(term.Symbol, time), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                // not operational yet, some next version
                case BI.callstack: // callstack( S, L) -- S is string, L is list representation of current call stack
                    //string str;
                    //ListTerm lst;
                    ////CallStack (out str, out lst);
                    //if (!term.Arg (0).Unify (new StringTerm (str), varStack) ||
                    //   (term.Arity == 2 && !term.Arg (1).Unify (lst, varStack)))
                    //  return false;
                    break;

                case BI.stacktrace: // stacktrace( Mode). If on: show C# exception stacktrace; if off: don't
                    t0 = term.Arg(0);

                    if (t0.IsVar && !t0.Unify(new AtomTerm(term.Symbol, UserSetShowStackTrace ? "on" : "off"),
                        CurrVarStack))
                    {
                        return false;
                    }

                    string mode = t0.FunctorToString;

                    if (mode == "on")
                    {
                        UserSetShowStackTrace = true;
                    }
                    else if (mode == "off")
                    {
                        UserSetShowStackTrace = false;
                    }
                    else
                    {
                        IO.ThrowRuntimeException($":- stacktrace: illegal argument '{mode}'; use 'on' or 'off' instead",
                            CurrVarStack, term);
                    }

                    break;

                case BI.query_timeout:
                    t0 = term.Arg(0);

                    if (t0.IsVar)
                    {
                        t0.Unify(new DecimalTerm(term.Symbol, queryTimeout), CurrVarStack);

                        break;
                    }

                    if (!t0.IsInteger || (queryTimeout = t0.To<int>()) < 0)
                    {
                        return false;
                    }

                    break;

                case BI.get_counter:
                    globalTermsTable.getctr(term.Arg(0).FunctorToString, out cntrValue);

                    if (!term.Arg(1).Unify(new DecimalTerm(term.Symbol, cntrValue), CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.set_counter:
                    t0 = term.Arg(0);

                    if (!(t0.IsAtom || t0.IsNatural))
                    {
                        IO.ThrowRuntimeException(
                            $"set_counter: first argument ({t0}) must be an atom or a non-negative integer",
                            CurrVarStack,
                            term);
                    }

                    globalTermsTable.setctr(t0.FunctorToString, term.Arg(1).To<int>());
                    break;

                case BI.inc_counter:
                    a = term.Arg(0).FunctorToString;
                    globalTermsTable.getctr(a, out cntrValue);

                    if (term.Arity == 2 &&
                        !term.Arg(1).Unify(new DecimalTerm(term.Symbol, cntrValue + 1), CurrVarStack))
                    {
                        return false;
                    }

                    globalTermsTable.incctr(a);
                    break;

                case BI.dec_counter:
                    a = term.Arg(0).FunctorToString;
                    globalTermsTable.getctr(a, out cntrValue);

                    if (term.Arity == 2 &&
                        !term.Arg(1).Unify(new DecimalTerm(term.Symbol, cntrValue - 1), CurrVarStack))
                    {
                        return false;
                    }

                    globalTermsTable.decctr(a);
                    break;

                case BI.getvar:
                    globalTermsTable.getvar(term.Arg(0).FunctorToString, out t1);
                    if (!term.Arg(1).Unify(t1, CurrVarStack))
                    {
                        return false;
                    }

                    break;

                case BI.setvar:
                    if (!(term.Arg(0) is AtomTerm))
                    {
                        return false;
                    }

                    globalTermsTable.setvar(term.Arg(0).FunctorToString, term.Arg(1).Copy(CurrVarStack));
                    break;
            }

            goalListHead = goalListHead.NextNode;

            findFirstClause = true;

            return true;
        }

        private BaseTerm TermFromWord(string word)
        {
            TermType type;
            word = word.ToAtomic(out type);

            if (type == TermType.Number)
            {
                try
                {
                    return new DecimalTerm(null, decimal.Parse(word, NumberStyles.Any, CIC));
                }
                catch
                {
                    IO.ThrowRuntimeException("*** Unable to convert \"" + word + "\" to a number", null, null);
                }
            }

            return new AtomTerm(null, word);
        }

        private BaseTerm CreateNewTerm(BaseTerm t, int arity, string functor, BaseTerm[] args)
        {
            OperatorDescr od;

            if (arity == 0)
            {
                if (OpTable.HasOpDef(functor))
                {
                    t = new OperatorTerm(t.Symbol, functor);
                }
                else
                {
                    t = new AtomTerm(t.Symbol, functor.ToAtom());
                }
            }
            else if (arity == 1 && OpTable.IsUnaryOperator(functor, out od))
            {
                t = new OperatorTerm(t.Symbol, od, args[0]);
            }
            else if (arity == 2 && OpTable.IsBinaryOperator(functor, out od))
            {
                t = new OperatorTerm(t.Symbol, od, args[0], args[1]);
            }
            else if (arity == 2 && functor == PrologParser.DOT)
            {
                t = new ListTerm(t.Symbol, args[0], args[1]);
            }
            else
            {
                t = new CompoundTerm(t.Symbol, functor, args);
            }

            return t;
        }
    }
}