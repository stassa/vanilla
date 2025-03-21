:-module(weak_cgnf, [target/1
		    ,invented/1
		    ,nonterminal/1
		    ,preterminal/1
		    ]).

:-use_module(project_root(configuration)).

/** <module> Constraints enforcing a "natural" triadic CFG definition.

This module defines a set of metarule_constraints/2 clauses for the
Identity, Chain and Tri-Chain metarules.

The constraints enforce a "natural" CFG notation where production rules
can be of the form:

==
S --> epsilon
S --> A
A --> a
A --> BC
A --> BCD
==

Where A, B, C and D can be non-terminals or pre-terminals except in the
case of S --> A where A _must_ be a pre-terminal. In practice epsilon,
the empty string, is also defined as a pre-terminal.

This notation allows for the definition of CFGs like a^nb^n or
Palindrome, in Definite Clause Grammars form:

==
% a^nb^n
s --> a,b.
s --> a,s,b.
a --> [a].
a --> [b].

% Palindrome
s --> empty.
s --> one.
s --> zero.
s --> one, s, one.
s --> zero, s, zero.
empty --> []
one --> 1
Zero --> 0
==


*/

configuration:tri_chain metarule 'P(x,y):- Q(x,z), R(z,u), S(u,y)'.

:- multifile target/1.
:- multifile invented/1.
:- multifile nonterminal/1.
:- multifile preterminal/1.

% McCarthyite (anti-left recursion) constraint
configuration:metarule_constraints(M,fail):-
	ground(M)
	,M =.. [m,_Id,P,P|_Ps].

% Identity clauses can only be pre-terminals.
% The left hand side must be the target
configuration:metarule_constraints(m(identity,P0,_P1),fail):-
	ground(P0)
        ,\+ target(P0).
% The right hand side must be a pre-terminal.
configuration:metarule_constraints(m(identity,_P0,P1),fail):-
	ground(P1)
        ,\+ preterminal(P1).

% The empty string can only be a pre-terminal.
configuration:metarule_constraints(M,fail):-
	ground(M)
	,M =.. [m,Id,_P|Ps]
	,Id \== identity
        ,memberchk(empty, Ps).

% The first literal on the right-hand side can't be the target: it can
% only be an invented predicate, or a pre-terminal.
% This enforces Greibach Normal Form but allows for invented predicate
% symbols, necessary for Chomsky Normal Form.
configuration:metarule_constraints(m(chain,_P0,P1,_),fail):-
	ground(P1)
	,target(P1).
configuration:metarule_constraints(m(tri_chain,_P0,P1,_,_),fail):-
	ground(P1)
	,target(P1).

% An invented predicate cannot immediately expand to an invented
% predicate. Eliminates "oblique" left-recursions where an invented
% predicate is mutually recursive with the target predicate without any
% intervening pre-terminal, i.e. stuff like p --> inv_1 | inv_1 --> p.
configuration:metarule_constraints(m(chain,P0,P1,_),fail):-
	ground(P0)
	,ground(P1)
	,invented(P0)
	,invented(P1).
configuration:metarule_constraints(m(tri_chain,P0,P1,_,_),fail):-
	ground(P0)
	,ground(P1)
	,invented(P0)
	,invented(P1).


% Recursion in chain can only be tail-recursion.
configuration:metarule_constraints(m(chain,P0,P1,_P2),fail):-
	ground(P0)
	,ground(P1)
	,P0 == P1.
configuration:metarule_constraints(m(chain,_P0,P1,P2),fail):-
	ground(P1)
	,ground(P2)
	,P1 == P2.


% Recursion in tri-chain can only be tail-recursion or recursion between
% the head and "middle" body literal.
configuration:metarule_constraints(m(tri_chain,P0,P1,_P2,_P3),fail):-
	ground(P0)
	,ground(P1)
	,P0 == P1.
configuration:metarule_constraints(m(tri_chain,_P0,P1,P2,_P3),fail):-
	ground(P1)
	,ground(P2)
	,P1 == P2.
configuration:metarule_constraints(m(tri_chain,_P0,_P1,P2,P3),fail):-
	ground(P2)
	,ground(P3)
	,P2 == P3.

