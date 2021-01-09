	Universal quantification, impredicative terms, 
	       closed/open worlds, and effects

This note (first written on Oct 14, 2004 and updated on Oct 18, 2004)
discusses universal quantification of variables in the body of a
clause, given open- and closed-world assumptions. We also discuss two
ways of expressing impredicative terms like (exists U. [U,U]), and
`common subexpression elimination' in logical programs.  We draw
parallels with effects, of creating logical variables.

We will be discussing Horn-clause--based logical programming systems
such as Prolog, Datalog, and Kanren (kanren.sourceforge.net).


Let us consider the following set of facts (the extensional database,
in Datalog terms):

	?- dynamic reqd/2.
	student(john).
	student(bob).
	reqd(cs,cs311).
	reqd(cs,cs611).
	took(john,cs311).
	took(john,cs611).
	took(bob,cs311).
	took(bob,math101).

and the predicate perhaps_cs/1 defined as

	perhaps_cs(X) :- student(X), reqd(cs,Course), took(X,Course).

The predicate is true for all people who are students and took at
least one required CS course.

	A Horn clause is a disjunction of literals with exactly one
positive literal. All free variables are implicitly universally
quantified. For example, the clause defining perhaps_cs/1 can be
written in the conventional logical notation as follows

	forall X Course. (perhaps_cs(X) or ^(student(X) & reqd(cs,Course)
	                                     & took(X,Course)))
==
	forall X Course. (perhaps_cs(X) or ^student(X) or ^reqd(cs,Course)
	                                or ^took(X,Course))

Using the standard logical transformation rules, we can re-write the
above formula as

	forall X. (forall Course. (perhaps_cs(X) or ^student(X)
				        or ^reqd(cs,Course)
	                                or ^took(X,Course)))

== { use rule: forall A. (a | b) === a | forall A. b if A does not occur
freely in a}

	forall X. (perhaps_cs(X) or ^student(X) or 
	            forall Course. (^reqd(cs,Course) or ^took(X,Course)))
==
	forall X. (perhaps_cs(X) or ^student(X) or 
	            forall Course. (^(reqd(cs,Course) & took(X,Course))))
==
	forall X. (perhaps_cs(X) or ^student(X) or 
	            ^(exists Course. (reqd(cs,Course) & took(X,Course))))

The familiar Prolog rule emerges: variables that occur in the head of
the clause (such as X) are implicitly universally
quantified. Variables that occur only in the body of the clause (such
as Course) are implicitly existentially quantified. In Kanren, the
latter, existential quantification is explicit.

Thus perhaps_cs/1 is indeed true for students who took _some_ required
computer science course. Now we would like to find people who took
_all_ required CS courses. There are two ways of doing this: using
eigenvariables and double negation. There is a subtle difference
between the two methods.

The double negation method (explained, e.g., in Datalog literature):

	cs_complete(X) :- student(X), 
		not((reqd(cs,Course), not(took(X,Course)))).

This clause can be re-written as the following FOL formula

	forall X Course. (student(X) & (reqd(cs,Course) -> took(X,Course)))
                         -> cs_complete(X).

We find that only 'john' has completed all the required courses *that
are listed in the database*. This method takes the closed-world
assumption. The above program is safe in Datalog sense (all logical
variables are range-limited).


A different way of writing cs_complete/1 with the logical variable
Course universally quantified within the body of the clause is to
use the natural deduction [*] rule everyI:
        alpha[A/X] ==> forall X. alpha
where A is an eigenvariable. 

[*] A Prolog program must necessarily operate in some meta-logic; we
just picked NK.

So, if we wish to express the formula
	forall X. (cs_complete_(X) or ^student(X) or 
	            ^(forall Course. (reqd(cs,Course) -> took(X,Course))))

in Prolog or Kanren, we write it in the following form

	forall X. (cs_complete_(X) or ^student(X) or 
	            ^(eigen Course. (reqd(cs,Course) -> took(X,Course))))
Or, in the Prolog notation,

	cs_complete_(X) :- student(X), gensym(ccc,Course), 
			   assert(reqd(cs,Course)), 
			   took(X,Course).

The reason for gensym is to make sure that when the formula gets
duplicated during the resolution (e.g., with the conjunction
"cs_complete_(john), cs_complete_(bill)" that may occur in a program),
each copy has its own value of the eigenvariable. In Kanren,
adding an assumption (like reqd/2) to the database is handled far more
gracefully, see for example, proofs in the Kanren example directory.


In order for cs_complete_ to succeed, we need to add an additional
clause to our program:

	took(john,X) :- reqd(cs,X).

The clause asserts that john took every CS course -- which is
described in the database *or* may be added into the database in the
future. So, the eigen-variable method takes the open-world assumption.


* copy_term and impredicative terms

Suppose we have a logical program

(1)     ok1([U,U]).
        okpair(X,Y) :- ok1(X), ok1(Y).

The predicate ok1/1 determines that its argument is OK if it is a list
of two identical elements. The predicate okpair/2 determines if its
two arguments are OK. We can easily verify that okpair([1,1],[2,2])
holds.

We can say: ok1/1 has only one answer, [U,U]. Why don't we precompute
it and inline it? We can try either


(2)     okpair(X,Y) :- OK = [U,U], X = OK, Y = OK.
or

(3)     okpair(X,Y) :- X = [U,U], Y = [U,U].

From a different perspective: suppose ok1/1 in (1) is a complex
predicate that takes a long time to evaluate. Program (1) invokes this
predicate twice, ok1(X) and ok1(Y). Can we do a `common subexpression
elimination' and re-write (1) into

(4)     ok1([U,U]).
        okpair(X,Y) :- ok1(Z), X = Z, Y = Z.


Obviously, (2)-(4) work differently from (1), because
okpair([1,1],[2,2]) will no longer succeed.

When Prolog resolves both instances of ok1/1 in (1), it makes two copies
of ok1/1 and ensuring that the variable names are renamed correspondingly
(alpha-converted). Logically, program (3) corresponds to

(5)     forall X Y. (okpair(X,Y) or ^(exists U. (= X [U,U]) &
                                                (= Y [U,U])))
whereas program (1), after resolution, is

(6)     forall X Y. (okpair(X,Y) or ^(exists U. (= X [U,U])) or
                                    ^(exists U. (= Y [U,U])))
==
        forall X Y. (okpair(X,Y) or forall U. ^(= X [U,U]) or
                                    forall U. ^(= Y [U,U]))
== {conversion into the Horn clausal form}
        forall X Y U U1. (okpair(X,Y) or ^(= X [U,U]) or
                                         ^(= Y [U1,U1]))

to move the quantifier forward, we have to alpha-convert bound
variables. We can see now how copy_term comes about.

Suppose however we want to re-write (1) into the following form:

(7)     okpair(X,Y) :- okpair1(X,Y,?).
where okpair1/3 is something like
        okpair1(X,Y,Z) :- X = Z, Y = Z.

what should we write in place of '?' in (7)?

A straightforward answer would be: (exists U. [U,U]). That is,
        okpair(X,Y) :- okpair1(X,Y,(exists U. [U,U])).

Here, (exists U. [U,U]) is an impredicative term. Alas, Prolog doesn't
support impredicative logic. So, we need a work-around. One
work-around is this:

(8)     okpair(X,Y) :- okpair1(X,Y,[U,U]).
        okpair1(X,Y,Z) :- copy_term(Z,Z1), X = Z1, copy_term(Z,Z2), Y = Z2.

The other work-around is this:

(9)     ok1([U,U]).
        okpair(X,Y) :- okpair1(X,Y,ok1).
        okpair1(X,Y,Z) :- call(Z,X1), X = X1, call(Z,Y1), Y = Y1.

We're essentially using the second-order logic (variable
predicates). This is precisely the solution in the
kanren/examples/type-inference.scm

One should emphasize the equivalence of (8) and (9). Furthermore, (8) is
an `optimal' form of (9), where the predicate ok1 is in the `solved'
form. Both (8) and (9) are two ways of emulating (exists U. [U,U]).

A different view of looking at this is from the point of view of
effects. As Chung-chieh Shan pointed out, there is one effect in Kanren:
creating of a logical variable. So, if we have an expression "ok1(X),
ok1(Y)" and we know that ok1/1 is effect-free (that is, it creates no
logical variables), then we indeed can do common-sub-expression
elimination and write it as "ok1(X), X = Y".  OTH, if ok1/1 is
effectful, we can factor the call to ok1/1 out only if we perform the
same effects as before. That's what copy_term is for.

