:-module(balanced_parens, [background_knowledge/2
                          ,metarules/2
                          ,initial_example/2
                          ,lp/2
                          ,rp/2
                          ,empty/2
                          ,generate_examples/5
                          ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(grammar_constraints_weak_setting).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn a grammar of the language of balanced parentheses with Poker.

To reproduce check your configs and MIL problem elements:

==
1 ?- [load_headless].
Loading poker
Loading experiment file module data(poker_examples/balanced_parens_weak_setting.pl) from balanced_parens.
Global stack limit 2,147,483,648
Table space 2,147,483,648
true.

% Remember to make/0 after uncommenting the set_configuration_otpions/2 below.
% It's a drag, I know. Working to fix that. Ish.
% 2 ?- make.

3 ?- poker_auxiliaries:list_poker_config.
clause_limit(3)
experiment_file(data(poker_examples/balanced_parens_weak_setting.pl),balanced_parens)
flatten_prove_all(true)
gestalt(false)
greedy_generalisation(false)
listing_limit(15)
max_invented(1)
multithreading(false)
proof_samples(1.0)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
respecialise(true)
strict_clause_limit(false)
unlabelled_examples(50)
unlabelled_examples_order(random)
true.

4 ?- poker_auxiliaries:list_mil_problem(p/2).
Initial examples
----------------
p([],[]).
p(['(',')'],[]).
p(['(','(',')',')'],[]).
p(['(',')','(',')'],[]).
p(['(','(','(',')',')',')'],[]).
p(['(','(',')','(',')',')'],[]).
p(['(','(',')',')','(',')'],[]).
p(['(',')','(','(',')',')'],[]).
p(['(',')','(',')','(',')'],[]).

Background knowledge (First Order)
----------------------------------
lp/2:
lp(['('|A],A).

rp/2:
rp([')'|A],A).

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

There are too many bloody constraints. Note that those are loaded from
grammar_constraints_weak_setting.pl and they apply to all grammars
learnable with some combination of the Identity, Chain and Tri-Chain
metarules.

This is the "weak setting" because we assume a CFG can be put in that
form, which means we're basically assuming we know something about the
form of the target theory, not just the language as we do in the "strong
setting" encoded in grammar_constraints.pl.

Expected results:

==
% Turn on logging to see what examples are generated and what the learned
% program looks like.

5 ?- debug(experiments), debug(experiment_initial), debug(experiment_learned), debug(test_program), debug(test_labelling), debug(generate_examples).
Warning: experiment_initial: no matching debug topic (yet)
Warning: experiment_learned: no matching debug topic (yet)
true.

6 ?- test_harness:experiments(parens,1,all,0,6,[Labelling,Program]).
% Experiment 1 of 1
% Generated 9 initial examples:
% p([],[])
% p([(,)],[])
% p([(,(,),)],[])
% p([(,),(,)],[])
% p([(,(,(,),),)],[])
% p([(,(,),(,),)],[])
% p([(,(,),),(,)],[])
% p([(,),(,(,),)],[])
% p([(,),(,),(,)],[])
% 23,329,001 inferences, 0.797 CPU in 4.767 seconds (17% CPU, 29275609 Lips)
% Learned hypothesis:
% p(A,B):-empty(A,B)
% p(A,B):-lp(A,C),p(C,D),inv_1_9(D,B)
% p(A,B):-inv_1_4(A,C),rp(C,D),p(D,B)
% p(A,B):-inv_1_10(A,C),p(C,B)
% inv_1_9(A,B):-rp(A,C),p(C,B)
% inv_1_4(A,B):-lp(A,C),p(C,B)
% inv_1_10(A,B):-lp(A,C),p(C,D),rp(D,B)
% Testing labelling for target: parens
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Testing learned program for target: parens
% Generating all parens examples of length in [0,4].
% Generated 4 positive testing examples
% Generating all unbalanced_parens examples of length in [0,4].
% Generated 27 negative testing examples
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
Labelling = Program, Program = [1.0,1.0,1.0].
==

This may look like a mess:
==
% p(A,B):-empty(A,B)
% p(A,B):-lp(A,C),p(C,D),inv_1_9(D,B)
% p(A,B):-inv_1_4(A,C),rp(C,D),p(D,B)
% p(A,B):-inv_1_10(A,C),p(C,B)
% inv_1_9(A,B):-rp(A,C),p(C,B)
% inv_1_4(A,B):-lp(A,C),p(C,B)
% inv_1_10(A,B):-lp(A,C),p(C,D),rp(D,B)
==

But we can unfold it manually as follows:
==
p(A,B):-empty(A,B)
p(A,B):-lp(A,C),p(C,D),rp(D,E),p(E,B)
p(A,B):-lp(A,C),p(C,D),rp(D,E),p(E,B)
p(A,B):-lp(A,C),p(C,D),rp(D,E),p(E,B)
==

Or, as a DCG:

==
p --> empty.
p --> lp, p, rp, p.
p --> lp, p, rp, p.
p --> lp, p, rp, p.

% Where:
empty --> [].
lp --> ['(']
rp --> [')']
==

That'the same clause repeated three times, because of the irreducible
nature of invented predicates. OK, it's just a bit of a bug really. But
despite the redundancy the learned hypothesis is really a gramamr of
balanced parentheses, except it's not the "triadic" version:

==
parens --> empty.
parens --> lp, parens, rp.
parens --> parens, parens.

lp --> ['('].
rp --> [')'].
==

But an equivalent grammar in folded form, with invented predicates. Why?
Well, that's what Poker learns. Shrug.

Why can't we learn that "natural" triadic version? Because the Greibach
Normal Form-ish constraints declared in
grammar_constraints_weak_setting.pl to avoid left-recursions don't allow
this clause to be constructed:

==
parens --> parens, parens.
==

Since it's very obviously left-recursive. So Poker has to find another
way to construct this grammar.

In the end the inclusion of the Tri-Chain metarule is a strong
assumption that makes the setting weak, counter-intuitively, and that
helps Poker avoid over-generalising.

Here's a more complete experiment to assuage your concerns about that
weird hypothesis:
==
8 ?- test_harness:experiments(parens,10,all,0,6,[Labelling,Program]).
% 23,327,525 inferences, 1.156 CPU in 4.594 seconds (25% CPU, 20175157 Lips)
% 23,327,522 inferences, 0.969 CPU in 4.343 seconds (22% CPU, 24080023 Lips)
% 23,327,417 inferences, 1.078 CPU in 4.357 seconds (25% CPU, 21637024 Lips)
% 23,327,559 inferences, 0.531 CPU in 4.573 seconds (12% CPU, 43910699 Lips)
% 23,327,454 inferences, 1.047 CPU in 4.439 seconds (24% CPU, 22282941 Lips)
% 23,327,523 inferences, 1.094 CPU in 4.481 seconds (24% CPU, 21328021 Lips)
% 23,327,524 inferences, 0.281 CPU in 4.452 seconds (6% CPU, 82942308 Lips)
% 23,327,559 inferences, 1.125 CPU in 4.427 seconds (25% CPU, 20735608 Lips)
% 23,327,523 inferences, 0.922 CPU in 4.667 seconds (20% CPU, 25304432 Lips)
% 23,327,559 inferences, 1.062 CPU in 4.294 seconds (25% CPU, 21955350 Lips)
Labelling = Program, Program = [1.0,1.0,1.0].
==

TODO: add a bit of code to display strings of parentheses in a more
readable manner.

*/

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[3]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[1]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[50]).
:-poker_auxiliaries:set_poker_configuration_option(respecialise,[true]).
:-poker_auxiliaries:set_poker_configuration_option(gestalt,[false]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).
*/

% Language alphabet for the constraints defeined
% in grammar_constraints_weak_setting.pl
%
grammar_constraints:target(p).
grammar_constraints:invented(inv_1).
grammar_constraints:invented(inv_2).
grammar_constraints:preterminal(lp).
grammar_constraints:preterminal(rp).
grammar_constraints:preterminal(empty).

configuration:tri_chain metarule 'P(x,y):- Q(x,z), R(z,u), S(u,y)'.


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
poker_configuration:safe_example(m(p,Ls,[])):-
	between(0,10,L)
	,length(Ls,L).

background_knowledge(p/2,[lp/2,rp/2,empty/2]).

metarules(p/2,[identity,chain,tri_chain]).

initial_example(p/2,E):-
	generate_initial(parens,all,0,6,Es)
        ,distinct( member(E,Es) ).

% The background knowledge is the set of pre-terminals in the language.
lp --> ['('].
rp --> [')'].
empty --> [].

generate_examples(pos,parens,all,0,4).
generate_examples(neg,unbalanced_parens,all,0,4).
