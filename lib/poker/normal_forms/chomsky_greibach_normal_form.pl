:-module(cgnf, [target/1
	       ,invented/1
	       ,nonterminal/1
	       ,preterminal/1
	       ]).

:-use_module(project_root(configuration)).

/** <module> Constraints enforcing Chomsky-Greibach Normal Form.

This module defines a set of metarule_constraints/2 for the Identity
and Chain metarules.

As a reminder, in a Chomsky Normal Form grammar every production rule is
if the form:

==
A --> BC
A --> a
S --> epsilon
==

Where B and C on the right-hand side of a rule cannot be the start
symbol, S.

While a grammar in Greibach Normal Form every rule is of the form:

==
A --> a, B, ... ,
==

Meaning that every nonterminal expands first to a terminal, thus
eliminating left-recursions.

*/

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

% An invented predicate cannot immediately expand to an invented
% predicate. Eliminates "oblique" left-recursions where an invented
% predicate is mutually recursive with the target predicate without any
% intervening pre-terminal, i.e. stuff like p --> inv_1 | inv_1 --> p.
configuration:metarule_constraints(m(chain,P0,P1,_),fail):-
	ground(P0)
	,ground(P1)
	,invented(P0)
	,invented(P1).
