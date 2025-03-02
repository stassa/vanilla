:-module(l_systems_constraints, [target/1
                                ,invented/1
                                ,nonterminal/1
                                ,preterminal/1
                                ]).

:-use_module(project_root(configuration)).

/** <module> Grammar constraints for L-Systems.

Based on grammar_constraints, with similar assumptions about the
expansion of rules to terminals and non-terminals.

The three metarules, ls_base, ls_rec and ls_rec_2 were extracted
manually from the algae L-System target theory in algage.pl.

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


configuration:ls_base metarule 'P(x,y,y) :- Q(x,y)'.
configuration:ls_rec metarule 'P(x,y,z) :- Q(y,u), R(x,v), S(v,u,z)'.
configuration:ls_rec_2 metarule 'P(x,y,z) :- Q(y,u), R(x,v), S(v,l), T(l,u,z)'.

% McCarthyite (anti-left recursion) constraint
configuration:metarule_constraints(M,fail):-
	ground(M)
	,M =.. [m,_Id,P,P|_Ps].

% FST-Base clauses can only be pre-terminals.

% The left-hand side must be the target
configuration:metarule_constraints(m(ls_base,P,_Q,_R),fail):-
	ground(P)
        ,\+ target(P).
configuration:metarule_constraints(m(ls_base,_P,Q,_R),fail):-
	ground(Q)
        ,\+ preterminal(Q).
configuration:metarule_constraints(m(ls_base,_P,_Q,R),fail):-
	ground(R)
        ,\+ preterminal(R).

% The empty string can only be a pre-terminal.
configuration:metarule_constraints(M,fail):-
	ground(M)
	,M =.. [m,Id,_P|Ps]
	,Id \== ls_base
        ,memberchk(empty, Ps).

% The first literal on the right-hand side can't be the target: it can
% only be an invented predicate, or a pre-terminal.
% This enforces a form similar to Greibach Normal Form but allows for
% invented predicate symbols, necessary for Chomsky Normal Form.
%
configuration:metarule_constraints(m(Id,_P,Q,_R,_S),fail):-
        memberchk(Id,[ls_rec,ls_rec_2])
	,ground(Q)
	,target(Q).

% An invented predicate cannot immediately expand to an invented
% predicate. Eliminates "oblique" left-recursions where an invented
% predicate is mutually recursive with the target predicate without any
% intervening pre-terminal, i.e. stuff like p --> inv_1 | inv_1 --> p.
configuration:metarule_constraints(m(Id,P,Q,_R,_S),fail):-
	memberchk(Id,[ls_rec,ls_rec_2])
	,ground(P)
	,ground(Q)
	,invented(P)
	,invented(Q).
