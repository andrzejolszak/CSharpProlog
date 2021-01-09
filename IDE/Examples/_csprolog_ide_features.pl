/*
Editor key bindings:

Ctrl + N - new file
Shift + Enter - consult source (while in source code editor)
Shift + Enter - query all solutions (while in query text box)
Ctrl + Space - autocompletion + docs for selected items
Ctrl + F - search
Ctrl + Shift + C - toggle line comment
Ctrl + D - duplicate line
Ctrl + E - remove line
Alt + Shift + Up/Down - move line
Right click on consulted term - inspect parsed term
*/

%% foo(++Ground:Type, +Instantiated, -Output, --Unbound, ?BoundToPartial, !Mutable)
%
% This comment will be displayed in the code completion menu.
% The comment header might be used for IDE static analysis in the future.
%
foo(A, B, C, D, E, F) :- A = myFooAtom, fail.

% Unit testing example:
:- begin_tests(arithm).
    testSimpleAdd :- 1 + 2 =:= 3.
    testSub :- 2 - 1 =:= 1.
:- end_tests(arithm).

% Static analysis warning example
bar :- bar(1, 2).

% Visual debugger (work in progress)
% Set the breakpoint on the below line be clicking the left margin
% and query for 'foo1' in debug mode:
foo1 :- bar1, car1, dar1.
bar1 :- true.
car1 :- ear1(1).
ear1(X) :- true.
dar1 :- fail. 
