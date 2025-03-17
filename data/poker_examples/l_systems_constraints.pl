:-module(l_systems_constraints, [target/1
                                ,invented/1
                                ,nonterminal/1
                                ,preterminal/1
                                ]).

:-use_module(project_root(configuration)).

/** <module> Grammar constraints for L-Systems.

Defines Lindenmayer Normal Form, a normal form for L-System grammars.

This set of metasubstitution constraints are based on the ones in
grammar_constraints.pl, with similar assumptions about the expansion of
rules to terminals and non-terminals, but tailored to the purpose of
learning L-Systems.

The constraints in grammar_constraints.pl are informed by Chomsky Normal
Form and Greibach Normal Form, two well-known normal forms for Context
Free Grammers. There is no known Normal Form for L-Systems. The
following explains how these constraints were derived.

There are three new metarules defined in this module:

==
ls_constant metarule 'P(x,y,z) :- Q(y,u), Q(x,v), P(v,u,z)'.
ls_variable metarule 'P(x,y,z) :- Q(y,u), R(x,v), P(v,u,z)'.
ls_base metarule 'P(x, y, y):- Q(x,y)'.
==

These were extracted manually from an L-System grammar for the Dragon
Curve fractal, listed here as a set of Definite Clause Grammar (DCG)
rules:

==
dragon_curve([+|Ss])--> plus, dragon_curve(Ss).
dragon_curve([-|Ss])--> minus, dragon_curve(Ss).
dragon_curve([f,+,g|Ss])--> f, dragon_curve(Ss).
dragon_curve([f,-,g|Ss])--> g, dragon_curve(Ss).
dragon_curve([])--> [].
==

Without the DCG syntactic sugar, the above grammar is equivalent to the
following set of definite clauses in ordinary Prolog notation:

==
dragon_curve([+|Ss],X,Y):-  plus(X, Z), dragon_curve(Ss, Z, Y).
dragon_curve([-|Ss],X,Y):-  minus(X, Z), dragon_curve(Ss, Z, Y).
dragon_curve([f,+, g|X],Y,Z):- f(Y, U), dragon_curve(X, U, Z).
dragon_curve([f,-, g|X],Y,Z):- g(Y, U), dragon_curve(X, U, Z).
test_harness:dragon_curve([],X,Y):-  X=Y.
==

Bot forms of the above DCG are equivalent and they correspond to the
following Dragon Curve L-System grammar, used to direct a Turtle
interpreter:

==
f -> f+g
g -> f-g
==

Where + (turn right) and - (turn left) are constants, so they are never
re-written and f and g are variables (both signifying "move forward").

Meta-Interpretive Learning does not allow function symbols like
list-cons, [|], to be used in metarules so it's not possible to
directly instantiate the first variable in the head literal of each
clause in the dragon curve DCG to a list of symbols, using metarules.

Instead, to generalise our grammar as a set of metarules, we must first
transform its clauses with_flattening_ an old ILP technique used to
remove function symbols from logic programs.

If we apply flattening to the five clauses of the un-sugared DCG, we get
the following program:

==
dragon_curve(X,Y,Z) :- plus(Y,U), plus(X,V), dragon_curve(V,U,Z).
dragon_curve(X,Y,Z) :- minus(Y,U), minus(X,V), dragon_curve(V,U,Z).
dragon_curve(X,Y,Z) :- f(Y, U), f(X,V), plus(V,W), g(W,F), dragon_curve(F,U,Z).
dragon_curve(X,Y,Z) :- g(Y, U), f(X,V), minus(V,W), g(W,F), dragon_curve(F,U,Z).
dragon_curve(X, Y, Y):- empty(X,Y).
==

It takes a bit of squinting but it's possible to see how the above
grammar is equivalent to our un-flattened, un-sugared Dragon Curve DCG.
For example, note how lists of symbols are now replaced by two
variables, one for the head and one for the tail.

For instance, in the first clause of the un-sugared DCG, the output list
[+|Ss] in the head literal, dragon_curve([+|Ss],X,Y), is replaced by the
variable X, in the new head literal of the flattened grammar,
dragon_curve(X,Y,Z).

X is now the entire list [+|Ss] but when it is unified with X in the
(second) body literal of the flattened grammar, plus(X,V), because
plus/2 is defined as plus([H|T],T), the tail of the output list,
previously "Ss", is unified to V, and so to the first argument of the
last body literal, dragon_curve(V,U,Z).

Thus the tail of the list is transferred through the body of the clause
by unification, without having to explicitly declare a list anywhere.

The un-flattened grammar above consists of three groups of clauses:

==
% Clauses defining L-System constants (here, + and -).
dragon_curve(X,Y,Z) :- plus(Y,U), plus(X,V), dragon_curve(V,U,Z).
dragon_curve(X,Y,Z) :- minus(Y,U), minus(X,V), dragon_curve(V,U,Z).

% Clauses defining L-System variables (here f and g).
dragon_curve(X,Y,Z) :- f(Y, U), f(X,V), plus(V,W), g(W,F), dragon_curve(F,U,Z).
dragon_curve(X,Y,Z) :- g(Y, U), f(X,V), minus(V,W), g(W,F), dragon_curve(F,U,Z).

% Base-case clause.
dragon_curve(X, Y, Y):- empty(X,Y).
==

Note that the middle pair of clauses, defining the variables of the
Dragon Curve L-System is specific to that L-System. Not only becaues of
the names of its body literals, f/2, g/2, plus/2 and minus/2, but also
because of the _number_ of such body literals, which is four: one for
the input variable (f or g) and three for the output symbols that have
to be composed in the list in the head of the clause.

What we want from a Normal Form is to generalise a class, or at least an
interesting sub-class, of programs. While we could extract a set of
metarules from the clauses of the flattened Dragon Curve DCG above,
those metarules would only be a weak Normal Form, that could only
reperesent L-Systems where each step of the development of the system
adds exactly three symbols to the output, for each variable.

To get a strong-er Normal Form we need a way to push an arbitrary number
of symbols to the output of each variable (i.e. the list in the head
literal of each variale clause).

To do this we need the secret super-power of MIL: predicate invention.

Firs, from the five flattened clauses above extract the three metarules
we saw above:

==
ls_constant metarule 'P(x,y,z) :- Q(y,u), Q(x,v), P(v,u,z)'.
ls_variable metarule 'P(x,y,z) :- Q(y,u), R(x,v), P(v,u,z)'.
ls_base metarule 'P(x, y, y):- Q(x,y)'.
==

This operation could be just as well be done automatically but it's not
hard to do it by hand, at this point. I swear, it just takes a bit of
work. The important thing to note is that this set of three metarules
can represent any L-System grammar that consists of clauses that define
constants, variables, and the empty clause but only if each variable
pushes a single symbol into the output.

Then, we add a fourth metarule, the Tri-chain metarule:

==
tri_chain metarule 'P(x,z):- Q(x,y), R(y,u), S(u,z)'.
==

Now, the second body literal in each instance of the ls_variable
metarule can be instantiated to an invented predicate, defined by the
Tri-Chain metarule.

Indeed, using the full set of four metarules above, ls_constant,
ls_variable, ls_base and tri-chain, and the constraints in this
file, Poker learns the following Dragon Curve grammar:

==
s(A,B,C):-plus(B,D),plus(A,E),s(E,D,C).
s(A,B,C):-minus(B,D),minus(A,E),s(E,D,C).
s(A,B,C):-g(B,D),inv_2_16(A,E),s(E,D,C).
s(A,B,C):-g(B,D),inv_1_15(A,E),s(E,D,C).
s(A,B,C):-f(B,D),inv_2_14(A,E),s(E,D,C).
s(A,B,C):-f(B,D),inv_1_13(A,E),s(E,D,C).
s(A,B,B):-empty(A,B).
inv_2_16(A,B):-f(A,C),minus(C,D),g(D,B).
inv_2_14(A,B):-f(A,C),plus(C,D),g(D,B).
inv_1_15(A,B):-f(A,C),minus(C,D),g(D,B).
inv_1_13(A,B):-f(A,C),plus(C,D),g(D,B).
==

This is admittedly a bit of a mess to look at because there are pairs of
invented predicates that are identtical, although with different names.
This is a bit of a bug in Vanilla. Nevertheless the above is a correct
L-System grammar of the Dragon Curve. For example, if we manually clean
up the above, we can keep just these clauses:

==
s(A,B,C):-plus(B,D),plus(A,E),s(E,D,C).
s(A,B,C):-minus(B,D),minus(A,E),s(E,D,C).
s(A,B,C):-g(B,D),inv_2_16(A,E),s(E,D,C).
s(A,B,C):-f(B,D),inv_2_14(A,E),s(E,D,C).
s(A,B,B):-empty(A,B).
inv_2_16(A,B):-f(A,C),minus(C,D),g(D,B).
inv_2_14(A,B):-f(A,C),plus(C,D),g(D,B).
==

It is now possible (again, bit of squinting required) to see that
inv_2_14 correctly adds the symbols f, + and, g to the output of the
clause that defines the behaviour of the f variable, and that inv_2_16
correctly adds the symbols f, -, and g, to the clause that defines the g
variable.

So that's correct! But note that the above is still a _weak_ normal form
and it will only allow L-Systems to be learned where invented predicates
must have exactly three body literals.

TBC!

*/

:- multifile target/1.
:- multifile invented/1.
:- multifile nonterminal/1.
:- multifile preterminal/1.

target(q0).
invented(inv_1).
preterminal(zero).
preterminal(one).
preterminal(empty).


configuration:ls_constant metarule 'P(x,y,z) :- Q(y,u), Q(x,v), P(v,u,z)'.
configuration:ls_variable metarule 'P(x,y,z) :- Q(y,u), R(x,v), P(v,u,z)'.
configuration:ls_base metarule 'P(x, y, y):- Q(x,y)'.
configuration:tri_chain metarule 'P(x,z):- Q(x,y), R(y,u), S(u,z)'.


% McCarthyite (anti-left recursion) constraint
configuration:metarule_constraints(M,fail):-
	ground(M)
	,M =.. [m,_Id,P,P|_Ps].

% ls_constant defines L-System constants.
% It can't define an invented predicate.
% Note that P is in both the head and the tail and Q is the same in both
% body literals.
configuration:metarule_constraints(m(ls_constant,P,_Q),fail):-
	ground(P)
        ,\+ target(P).
configuration:metarule_constraints(m(ls_constant,_P,Q),fail):-
	ground(Q)
        ,\+ preterminal(Q).

% ls_variable defines L-System variables.
% It can't define an invented predicate.
% Its second body literal only can be an invented predicates.

configuration:metarule_constraints(m(ls_variable,P,_Q,_R),fail):-
	ground(P)
        ,\+ target(P).
configuration:metarule_constraints(m(ls_variable,_P,Q,_R),fail):-
	ground(Q)
        ,\+ preterminal(Q).
configuration:metarule_constraints(m(ls_variable,_P,_Q,R),fail):-
	ground(R)
        ,target(R).

% ls_base is the base case and can only expand the start symbol to the
% empty string. Only ls_base can expand to the empty string.
configuration:metarule_constraints(m(ls_base,P,_Q),fail):-
	ground(P)
        ,\+ target(P).
configuration:metarule_constraints(m(ls_base,_P,Q),fail):-
	ground(Q)
        ,Q \== empty.
configuration:metarule_constraints(M,fail):-
	ground(M)
	,M =.. [m,Id,_P|Ps]
	,Id \== ls_base
        ,memberchk(empty, Ps).

% tri_chain can only define an invented predciate.
% Its body literals can only be L-System constants or variables i.e.
% preterminals. Its last literal can be recursive but not with the
% target predicate.
configuration:metarule_constraints(m(tri_chain,P,_Q,_R,_S),fail):-
	ground(P)
        ,\+ invented(P).
configuration:metarule_constraints(m(tri_chain,_P,Q,_R,_S),fail):-
	ground(Q)
        ,\+ preterminal(Q).
configuration:metarule_constraints(m(tri_chain,_P,_Q,R,_S),fail):-
	ground(R)
        ,\+ preterminal(R).
configuration:metarule_constraints(m(tri_chain,_P,_Q,_R,S),fail):-
	ground(S)
        ,target(S).
% Experiment: don't let an invented predicate recurse with itself.
configuration:metarule_constraints(m(tri_chain,P,_Q,_R,S),fail):-
	ground(P)
	,ground(S)
        ,P == S.
% Experimental: order invented predicates by index
% Forces invented predicates to be constructed in order of their
% indices and avoids the unnecessary combinatorial cost of constructing
% all possible combinations of invented predicates.
configuration:metarule_constraints(m(tri_chain,P,_Q,_R,S),fail):-
	ground(P)
	,ground(S)
	,(   invented(S)
	 ->  S @< P
	 ;   false
	 ).

% Like tri_chain, chain can only define an invented predciate.
% Its body literals can only be L-System constants or variables i.e.
% preterminals. Its last literal can be recursive but not with the
% target predicate.
configuration:metarule_constraints(m(chain,P,_Q,_R),fail):-
	ground(P)
        ,\+ invented(P).
configuration:metarule_constraints(m(chain,_P,Q,_R),fail):-
	ground(Q)
        ,\+ preterminal(Q).
configuration:metarule_constraints(m(chain,_P,_Q,R),fail):-
	ground(R)
        ,target(R).
% Experiment: don't let an invented predicate recurse with itself.
configuration:metarule_constraints(m(chain,P,_Q,R),fail):-
	ground(P)
	,ground(R)
        ,P == R.
% Experimental: order invented predicates by index.
% Forces invented predicates to be constructed in order of their
% indices and avoids the unnecessary combinatorial cost of constructing
% all possible combinations of invented predicates.
configuration:metarule_constraints(m(chain,P,_Q,R),fail):-
	ground(P)
	,ground(R)
	,(   invented(R)
	 ->  R @< P
	 ;   false
	 ).
