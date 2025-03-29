:-module(palindrome,[background_knowledge/2
                    ,metarules/2
                    ,labelled_example/2
                    ,unlabelled_example/2
                    ,zero/2
                    ,one/2
                    ,empty/2
		    ,generate_examples/5
                    ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(lib(poker/normal_forms/weak_chomsky_greibach_nf)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn to recognise palindromes with Poker.

To reproduce this experiment make sure you have the same configuration
and learning problem elements, as listed below.

Configuration:

==
1 ?- [load_headless].
Loading poker
Loading experiment file module data(poker_examples/palindrome_weak_setting.pl) from palindrome.
Global stack limit 2,147,483,648
Table space 2,147,483,648
true.

% Remember to make/0 after uncommenting the set_configuration_otpions/2 below.
% It's a drag, I know. Working to fix that. Ish.
% 2 ?- make.

3 ?- poker_auxiliaries:list_poker_config.
clause_limit(5)
experiment_file(data(poker_examples/palindrome_weak_setting.pl),palindrome)
flatten_prove_all(true)
gestalt(false)
greedy_generalisation(false)
listing_limit(15)
max_invented(0)
multithreading(false)
proof_samples(1.0)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
respecialise(false)
strict_clause_limit(false)
unlabelled_examples(100)
unlabelled_examples_order(deterministic)
true.
==


Learning problem. Note all the constraints. This is the "weak" setting
where we assume some knowledge of the target _grammar_ (not just the
language):

==
4 ?- poker_auxiliaries:list_mil_problem(q0/2).
Initial examples
----------------
q0([],[]).
q0([1],[]).
q0([0],[]).
q0([1,1],[]).
q0([0,0],[]).
q0([1,1,1],[]).
q0([1,0,1],[]).
q0([0,1,0],[]).
q0([0,0,0],[]).
q0([1,1,1,1],[]).
q0([1,0,0,1],[]).
q0([0,1,1,0],[]).
q0([0,0,0,0],[]).

Background knowledge (First Order)
----------------------------------
zero/2:
zero([0|A],A).

one/2:
one([1|A],A).

empty/2:
empty(A,B):-A=B.

Background knowledge (Second Order)
-----------------------------------
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Tri-chain) ∃.P,Q,R,S ∀.x,y,z,u: P(x,y)← Q(x,z),R(z,u),S(u,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(M, fail) :-
    grammar_constraints:
    (   ground(M),
        M=..[m, _Id, P, P|_Ps]
    ).
metarule_constraints(m(identity, P0, _P1), fail) :-
    grammar_constraints:
    (   ground(P0),
        \+ target(P0)
    ).
metarule_constraints(m(identity, _P0, P1), fail) :-
    grammar_constraints:
    (   ground(P1),
        \+ preterminal(P1)
    ).
metarule_constraints(M, fail) :-
    grammar_constraints:
    (   ground(M),
        M=..[m, Id, _P|Ps],
        Id\==identity,
        memberchk(empty, Ps)
    ).
metarule_constraints(m(chain, _P0, P1, _), fail) :-
    grammar_constraints:
    (   ground(P1),
        target(P1)
    ).
metarule_constraints(m(tri_chain, _P0, P1, _, _), fail) :-
    grammar_constraints:
    (   ground(P1),
        target(P1)
    ).
metarule_constraints(m(chain, P0, P1, _), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        invented(P0),
        invented(P1)
    ).
metarule_constraints(m(tri_chain, P0, P1, _, _), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        invented(P0),
        invented(P1)
    ).
metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        P0==P1
    ).
metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    grammar_constraints:
    (   ground(P1),
        ground(P2),
        P1==P2
    ).
metarule_constraints(m(tri_chain, P0, P1, _P2, _P3), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        P0==P1
    ).
metarule_constraints(m(tri_chain, _P0, P1, P2, _P3), fail) :-
    grammar_constraints:
    (   ground(P1),
        ground(P2),
        P1==P2
    ).
metarule_constraints(m(tri_chain, _P0, _P1, P2, P3), fail) :-
    grammar_constraints:
    (   ground(P2),
        ground(P3),
        P2==P3
    ).

true.
==


Expected results:

==
5 ?- debug(experiments), debug(experiment_initial), debug(experiment_learned), debug(experiment_examples_full), debug(test_program), debug(test_labelling), debug(generate_examples).
Warning: experiment_initial: no matching debug topic (yet)
Warning: experiment_learned: no matching debug topic (yet)
Warning: experiment_examples_full: no matching debug topic (yet)
true.

6 ?- test_harness:experiments(palindrome,1,all,0,4,[Labelling,Program]).
% Experiment 1 of 1
% Generated 13 initial examples:
% q0([],[])
% q0([1],[])
% q0([0],[])
% q0([1,1],[])
% q0([0,0],[])
% q0([1,1,1],[])
% q0([1,0,1],[])
% q0([0,1,0],[])
% q0([0,0,0],[])
% q0([1,1,1,1],[])
% q0([1,0,0,1],[])
% q0([0,1,1,0],[])
% q0([0,0,0,0],[])
% 30,825,680 inferences, 1.188 CPU in 6.048 seconds (20% CPU, 25958467 Lips)
% Learned hypothesis:
% q0(A,B):-zero(A,B)
% q0(A,B):-one(A,B)
% q0(A,B):-empty(A,B)
% q0(A,B):-zero(A,C),q0(C,D),zero(D,B)
% q0(A,B):-one(A,C),q0(C,D),one(D,B)
% 14 Positive examples:
% q0([1,1,1,1,1,1],[])
% q0([],[])
% q0([1],[])
% q0([0],[])
% q0([1,1],[])
% q0([0,0],[])
% q0([1,1,1],[])
% q0([1,0,1],[])
% q0([0,1,0],[])
% q0([0,0,0],[])
% q0([1,1,1,1],[])
% q0([1,0,0,1],[])
% q0([0,1,1,0],[])
% q0([0,0,0,0],[])
% 86 Negative examples:
% q0([1,1,1,1,1,0],[])
% q0([1,1,1,1,1],[])
% q0([1,1,1,1,0,1],[])
% q0([1,1,1,1,0,0],[])
% q0([1,1,1,1,0],[])
% q0([1,1,1,0,1,1],[])
% q0([1,1,1,0,1,0],[])
% q0([1,1,1,0,1],[])
% q0([1,1,1,0,0,1],[])
% q0([1,1,1,0,0,0],[])
% q0([1,1,1,0,0],[])
% q0([1,1,1,0],[])
% q0([1,1,0,1,1,1],[])
% q0([1,1,0,1,1,0],[])
% q0([1,1,0,1,1],[])
% q0([1,1,0,1,0,1],[])
% q0([1,1,0,1,0,0],[])
% q0([1,1,0,1,0],[])
% q0([1,1,0,1],[])
% q0([1,1,0,0,1,1],[])
% q0([1,1,0,0,1,0],[])
% q0([1,1,0,0,1],[])
% q0([1,1,0,0,0,1],[])
% q0([1,1,0,0,0,0],[])
% q0([1,1,0,0,0],[])
% q0([1,1,0,0],[])
% q0([1,1,0],[])
% q0([1,0,1,1,1,1],[])
% q0([1,0,1,1,1,0],[])
% q0([1,0,1,1,1],[])
% q0([1,0,1,1,0,1],[])
% q0([1,0,1,1,0,0],[])
% q0([1,0,1,1,0],[])
% q0([1,0,1,1],[])
% q0([1,0,1,0,1,1],[])
% q0([1,0,1,0,1,0],[])
% q0([1,0,1,0,1],[])
% q0([1,0,1,0,0,1],[])
% q0([1,0,1,0,0,0],[])
% q0([1,0,1,0,0],[])
% q0([1,0,1,0],[])
% q0([1,0,0,1,1,1],[])
% q0([1,0,0,1,1,0],[])
% q0([1,0,0,1,1],[])
% q0([1,0,0,1,0,1],[])
% q0([1,0,0,1,0,0],[])
% q0([1,0,0,1,0],[])
% q0([1,0,0,0,1,1],[])
% q0([1,0,0,0,1,0],[])
% q0([1,0,0,0,1],[])
% q0([1,0,0,0,0,1],[])
% q0([1,0,0,0,0,0],[])
% q0([1,0,0,0,0],[])
% q0([1,0,0,0],[])
% q0([1,0,0],[])
% q0([1,0],[])
% q0([0,1,1,1,1,1],[])
% q0([0,1,1,1,1,0],[])
% q0([0,1,1,1,1],[])
% q0([0,1,1,1,0,1],[])
% q0([0,1,1,1,0,0],[])
% q0([0,1,1,1,0],[])
% q0([0,1,1,1],[])
% q0([0,1,1,0,1,1],[])
% q0([0,1,1,0,1],[])
% q0([0,1,1,0,0],[])
% q0([0,1,1],[])
% q0([0,1,0,1,1],[])
% q0([0,1,0,1,0],[])
% q0([0,1,0,1],[])
% q0([0,1,0,0,1],[])
% q0([0,1,0,0,0],[])
% q0([0,1,0,0],[])
% q0([0,1],[])
% q0([0,0,1,1,1],[])
% q0([0,0,1,1,0],[])
% q0([0,0,1,1],[])
% q0([0,0,1,0,1],[])
% q0([0,0,1,0,0],[])
% q0([0,0,1,0],[])
% q0([0,0,1],[])
% q0([0,0,0,1,1],[])
% q0([0,0,0,1,0],[])
% q0([0,0,0,1],[])
% q0([0,0,0,0,1],[])
% q0([0,0,0,0,0],[])
% Testing labelling for target: palindrome
% Labelling: Measured Acc: 0.88 TPR: 1.0 TNR: 0.8605
% Testing learned program for target: palindrome
% Generating all palindrome examples of length in [0,8].
% Generated 61 positive testing examples
% Generating all not_palindrome examples of length in [0,8].
% Generated 450 negative testing examples
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
Labelling = [0.88,1.0,0.8605],
Program = [1.0,1.0,1.0].
==

It's difficult to see just by eyeballing but while the learned
hypothesis is exactly the target theory, a grammar for the language of
palindromes, the labelling is slightly off and the negative examples
include some palindromes, e.g.:

==
q0([1,1,1,1,1],[]).
q0([1,1,0,1,1],[]).
q0([1,0,0,0,1],[]).
q0([1,0,0,0,0,1],[]).
q0([0,1,1,1,1,0],[]).
q0([0,1,1,1,0],[]).
q0([0,0,0,0,0],[]).
==

And possibly others I missed. This is because Poker labels examples once
and then continues specialising its hypothesis, so the labelling
performed during learning improves as time goes by, but may well include
errors made in the earlier stages.

Note that this is not the same as a greedy algorithm: rather than commit
to the early errors and return an over-specialised hypothesis that
rejects the false negatives identified at the early stages of learning,
Poker improves its hypothesis as it goes and is capable of returning a
correct hypothesis, including one that covers the false negatives.

On the other hand, we can convince ourselves that, while the labelling
may not be 100% right, the program learned is:

==
% Couple top-level query numbers skipped because I forgot to turn off
% logging.
9 ?- nodebug(_).
true.

10 ?- test_harness:experiments(palindrome,10,all,0,4,[Labelling,Program]).
% 30,825,589 inferences, 1.391 CPU in 5.784 seconds (24% CPU, 22166716 Lips)
% 30,825,589 inferences, 1.703 CPU in 5.546 seconds (31% CPU, 18099428 Lips)
% 30,825,589 inferences, 1.500 CPU in 5.673 seconds (26% CPU, 20550393 Lips)
% 30,825,589 inferences, 1.562 CPU in 5.668 seconds (28% CPU, 19728377 Lips)
% 30,825,589 inferences, 1.172 CPU in 5.791 seconds (20% CPU, 26304503 Lips)
% 30,825,589 inferences, 1.156 CPU in 5.689 seconds (20% CPU, 26659969 Lips)
% 30,825,589 inferences, 1.469 CPU in 5.667 seconds (26% CPU, 20987635 Lips)
% 30,829,386 inferences, 1.547 CPU in 5.559 seconds (28% CPU, 19930108 Lips)
% 30,825,589 inferences, 1.125 CPU in 5.695 seconds (20% CPU, 27400524 Lips)
% 30,825,589 inferences, 1.344 CPU in 5.610 seconds (24% CPU, 22939973 Lips)
Labelling = [0.88,1.0,0.8605],
Program = [1.0,1.0,1.0].
==

*/

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[5]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[250]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).
*/


% Language alphabet for the constraints defeined
% in poker/normal_form/chomsky_greibach_nf.pl
%
weak_cgnf:target(q0).
weak_cgnf:preterminal(one).
weak_cgnf:preterminal(zero).
weak_cgnf:preterminal(empty).

poker_configuration:safe_example(m(q0,Ls,[])):-
	between(0,8,L)
	,length(Ls,L).

background_knowledge(q0/2,[zero/2
			  ,one/2
			  ,empty/2
			  ]).

metarules(q0/2,[identity,chain,tri_chain]).

labelled_example(q0/2,E):-
	generate_initial(palindrome,all,0,4,Es)
        ,member(E,Es).

unlabelled_example(q0/2,_):- fail.

zero --> [0].
one --> [1].
empty --> [].

generate_examples(pos,palindrome,all,0,8).
generate_examples(neg,not_palindrome,all,0,8).
