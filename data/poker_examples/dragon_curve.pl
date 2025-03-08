:-module(dragon_curve, [background_knowledge/2
                       ,metarules/2
                       ,initial_example/2
                       ,f/2
                       ,g/2
                       ,plus/2
                       ,minus/2
                       ,empty/2
                       ,generate_examples/5
                       ]).

:-use_module(l_systems_constraints).
:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_configuration),[]).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(data(poker_examples/test_harness)).

/** <module> Learn an L-System grammar modelling the Dragon Curve fractal.

Learns in the Weak Setting with metarules and metasubstitution
constraints specially designed to represent L-System grammars.

1. Good configs. Remember to uncomment the clauses of
set_poker_configuration_option/2 later in this file to set the correct
options:

==
?- poker_auxiliaries:list_config.
encapsulation_predicate(m)
example_clauses(call)
fetch_clauses(all)
invented_symbol_prefix(inv_)
learner(poker,lib(poker/poker))
metarule_formatting(quantified)
metasubstitution_atoms(existential)
table_meta_interpreter(true)
untable_meta_interpreter(true)
true.

?- poker_auxiliaries:list_poker_config.
clause_limit(7)
experiment_file(data(poker_examples/dragon_curve.pl),dragon_curve)
flatten_prove_all(true)
gestalt(false)
greedy_generalisation(false)
listing_limit(15)
max_invented(2)
multithreading(false)
proof_samples(1.0)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
respecialise(false)
strict_clause_limit(false)
unlabelled_examples(100)
unlabelled_examples_order(random)
true.
==

2. MIL Problem:

==
?- listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(s, A, B, [])) :-
    dragon_curve:
    (   C=8,
        between(0, C, D),
        length(A, D),
        between(0, C, E),
        length(B, E)
    ).

true.

?- poker_auxiliaries:list_mil_problem(s/3).
Initial examples
----------------
s([],[],[]).
s([+],[+],[]).
s([-],[-],[]).
s([+,+],[+,+],[]).
s([+,-],[+,-],[]).
s([-,+],[-,+],[]).
s([-,-],[-,-],[]).
s([+,+,+],[+,+,+],[]).
s([+,+,-],[+,+,-],[]).
s([+,-,+],[+,-,+],[]).
s([+,-,-],[+,-,-],[]).
s([-,+,+],[-,+,+],[]).
s([-,+,-],[-,+,-],[]).
s([-,-,+],[-,-,+],[]).
s([-,-,-],[-,-,-],[]).
% ... 2 more clauses.

Background knowledge (First Order)
----------------------------------
f/2:
f([f|A],A).

g/2:
g([g|A],A).

plus/2:
plus([+|A],A).

minus/2:
minus([-|A],A).

empty/2:
empty(A,B):-A=B.

Background knowledge (Second Order)
-----------------------------------
(Ls-constant) ∃.P,Q ∀.x,y,z,u,v: P(x,y,z)← Q(y,u),Q(x,v),P(v,u,z)
(Ls-variable) ∃.P,Q,R ∀.x,y,z,u,v: P(x,y,z)← Q(y,u),R(x,v),P(v,u,z)
(Ls-base) ∃.P,Q ∀.x,y: P(x,y,y)← Q(x,y)
(Tri-chain) ∃.P,Q,R,S ∀.x,y,z,u: P(x,y)← Q(x,z),R(z,u),S(u,y)

----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(M, fail) :-
    l_systems_constraints:
    (   ground(M),
        M=..[m, _Id, P, P|_Ps]
    ).
metarule_constraints(m(ls_constant, P, _Q), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ target(P)
    ).
metarule_constraints(m(ls_constant, _P, Q), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(ls_variable, P, _Q, _R), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ target(P)
    ).
metarule_constraints(m(ls_variable, _P, Q, _R), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(ls_variable, _P, _Q, R), fail) :-
    l_systems_constraints:
    (   ground(R),
        target(R)
    ).
metarule_constraints(m(ls_base, P, _Q), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ target(P)
    ).
metarule_constraints(m(ls_base, _P, Q), fail) :-
    l_systems_constraints:
    (   ground(Q),
        Q\==empty
    ).
metarule_constraints(M, fail) :-
    l_systems_constraints:
    (   ground(M),
        M=..[m, Id, _P|Ps],
        Id\==ls_base,
        memberchk(empty, Ps)
    ).
metarule_constraints(m(tri_chain, P, _Q, _R, _S), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ invented(P)
    ).
metarule_constraints(m(tri_chain, _P, Q, _R, _S), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(tri_chain, _P, _Q, R, _S), fail) :-
    l_systems_constraints:
    (   ground(R),
        \+ preterminal(R)
    ).
metarule_constraints(m(tri_chain, _P, _Q, _R, S), fail) :-
    l_systems_constraints:
    (   ground(S),
        target(S)
    ).

true.
==

3. Learning query:

==
?- _T = s/3, time( poker:learn(_T,_Pos,_Neg,_Ps) ), auxiliaries:print_clauses('Hypothesis:',_Ps), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% Generalising positive examples
% Derived 129 sub-hypotheses (unsorted)
% Derived 19 sub-hypotheses (sorted)
% 5,613,423 inferences, 0.469 CPU in 0.815 seconds (57% CPU, 11975302 Lips)
Hypothesis:
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
Ps = 11,
Pos = 22,
Neg = 78.
==

There's some redundancy in that hypothesis because the two invented
predicate symbols end up being used twice in different positions of the
hypothesis (you can see that from the different sub-sub-scripts). This
is a bug in the way Vanilla constructs invented predicates.


4. More complete experiment:

==
?- debug(generalise), debug(experiments), debug(experiment_initial), debug(experiment_examples), debug(generate_examples), debug(test_program), debug(test_labelling).
true.

?- test_harness:experiments(dragon_curve,1,all,0,4,[Labelling,Program]).% Experiment 1 of 1
% Generated 41 initial examples:
% s([],[],[])
% s([+],[+],[])
% s([-],[-],[])
% s([+,+],[+,+],[])
% s([+,-],[+,-],[])
% s([-,+],[-,+],[])
% s([-,-],[-,-],[])
% s([+,+,+],[+,+,+],[])
% s([+,+,-],[+,+,-],[])
% s([+,-,+],[+,-,+],[])
% s([+,-,-],[+,-,-],[])
% s([-,+,+],[-,+,+],[])
% s([-,+,-],[-,+,-],[])
% s([-,-,+],[-,-,+],[])
% s([-,-,-],[-,-,-],[])
% s([f,+,g],[f],[])
% s([f,-,g],[g],[])
% s([+,+,+,+],[+,+,+,+],[])
% s([+,+,+,-],[+,+,+,-],[])
% s([+,+,-,+],[+,+,-,+],[])
% s([+,+,-,-],[+,+,-,-],[])
% s([+,-,+,+],[+,-,+,+],[])
% s([+,-,+,-],[+,-,+,-],[])
% s([+,-,-,+],[+,-,-,+],[])
% s([+,-,-,-],[+,-,-,-],[])
% s([+,f,+,g],[+,f],[])
% s([+,f,-,g],[+,g],[])
% s([-,+,+,+],[-,+,+,+],[])
% s([-,+,+,-],[-,+,+,-],[])
% s([-,+,-,+],[-,+,-,+],[])
% s([-,+,-,-],[-,+,-,-],[])
% s([-,-,+,+],[-,-,+,+],[])
% s([-,-,+,-],[-,-,+,-],[])
% s([-,-,-,+],[-,-,-,+],[])
% s([-,-,-,-],[-,-,-,-],[])
% s([-,f,+,g],[-,f],[])
% s([-,f,-,g],[-,g],[])
% s([f,+,g,+],[f,+],[])
% s([f,+,g,-],[f,-],[])
% s([f,-,g,+],[g,+],[])
% s([f,-,g,-],[g,-],[])
% Generalising positive examples
% Derived 701 sub-hypotheses (unsorted)
% Derived 60 sub-hypotheses (sorted)
% 32,899,177 inferences, 1.047 CPU in 5.186 seconds (20% CPU, 31426080 Lips)
% Labelled 41 Positive examples.
% Labelled 87 Negative examples.
% Testing labelling for target: dragon_curve
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Testing learned program for target: dragon_curve
% Generating all dragon_curve examples of length in [0,14].
% Generated 220277 positive testing examples
% Generating all not_dragon_curve examples of length in [0,5].
% Generated 1054280 negative testing examples
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
Labelling = Program, Program = [1.0,1.0,1.0].
==

*/

% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
l_systems_constraints:target(s).
l_systems_constraints:invented(inv_1).
l_systems_constraints:invented(inv_2).
l_systems_constraints:preterminal(f).
l_systems_constraints:preterminal(g).
l_systems_constraints:preterminal(plus).
l_systems_constraints:preterminal(minus).
l_systems_constraints:preterminal(empty).

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[7]).
:-poker_auxiliaries:set_poker_configuration_option(gestalt,[false]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[2]).
:-poker_auxiliaries:set_poker_configuration_option(respecialise,[false]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).
*/



%!	safe_example(-Example) is nondet.
%
%	Generate a safe scaffold for unlabelled examples.
%
%	For examples with list arguments, generating unlabelled examples
%	during learning can "go infinite". This predicate ensures that
%	list arguments in examples are limited in length.
%
%	This argument should not itself be a generator of ground
%	examples. This is left to the user to avoid.
%
poker_configuration:safe_example(m(s,Is,Os,[])):-
        K = 8
	,between(0,K,I)
	,length(Is,I)
	,between(0,K,J)
	,length(Os,J).

background_knowledge(s/3,[f/2
                         ,g/2
                         ,plus/2
                         ,minus/2
                         ,empty/2
                         ]).

metarules(s/3,[ls_constant,ls_variable,ls_base,tri_chain]).

initial_example(s/3,E):-
	generate_initial(dragon_curve,all,0,3,Es)
        ,member(E,Es).

f --> [f].
g --> [g].
plus --> [+].
minus --> [-].
empty --> [].


% Generate examples for evaluation.
% Examples are generated by test harndes predicates.
%
generate_examples(pos,dragon_curve,all,0,14).
generate_examples(neg,not_dragon_curve,all,0,5).
