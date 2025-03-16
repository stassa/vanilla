:-module(algae, [background_knowledge/2
                 ,metarules/2
                 ,initial_example/2
                 ,a/2
                 ,b/2
                 ,empty/2
		 ,generate_examples/5
                 ]).

:-use_module(l_systems_constraints).
:-use_module(project_root(configuration),[]).
:-use_module(lib(poker/poker_configuration),[]).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(data(poker_examples/test_harness)).

/** <module> Learn a simple L-System grammar.

This experiment file shows how to use Poker to learn the first and
simplest L-system example from Aristide Lindenmayer's textbook
introducing L-Systems, "The Algorithmic Beauty of Plants" (TABoP). The
rules of that L-System's grammar are as follows:

==
a --> ab
b --> a
==

Read "a must be replaced by ab and b by a simultaneously in the current
string". Unlike phrase structure grammars, L-Systems rules are all
applied simultaneously in each step of parsing, called a "generation",
then the resulting string fed back to the next generation, until some
maximum generation set by the user.

Also unlike phrase-structure grammars, L-Systems have two kinds of
symbols: constants and variables, where constants are never replaced (or
more precisely replace only themselves by themselves in a string) and
variables are replaced by other strings. The grammar listed above has
only variables (a and b).

This grammar is illustrated in Figure 1.3 of TABoP and is also given as
the first example of an L-System on the wikipedia page on L-Systems:

https://en.wikipedia.org/wiki/L-system#Example_1:_algae

The Wikipedia page suggests this L-System models "the growth of algae"
but I can't find this information in TABoP. I think the wikipedia
editors confused it with an L-System for the blue-green bacteria
Anabena Catenula, illustrated in Figure 1.4 of TABoP, which uses
similar, but not identical, rules and symbols.

In any case the example here is the simplest example of an L-System so
it should serve as reference for learning L-Systems with Poker, and with
the Lindenmayer Normal Form defined in l_systems_constraints.pl.


1. Configuration:

==
?- make.
Global stack limit 2,147,483,648
Table space 2,147,483,648
% c:/<your_path_to_>/vanilla/load_headless compiled 0.00 sec, 0 clauses
true.

?- auxiliaries:list_config.
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
clause_limit(4)
experiment_file(data(poker_examples/algae.pl),algae)
flatten_prove_all(true)
generalise_conjunction(false)
gestalt(true)
greedy_generalisation(false)
listing_limit(15)
max_invented(1)
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


2. Learning Problem, with all the constraints enforcing Lindenmayer
Normal Form.

==
?- poker_auxiliaries:list_mil_problem(s/3).
Initial examples
----------------
s([],[],[]).
s([a],[b],[]).
s([a,b],[a],[]).
s([a,a],[b,b],[]).
s([a,b,a],[a,b],[]).
s([a,a,b],[b,a],[]).
s([a,a,a],[b,b,b],[]).
s([a,b,a,b],[a,a],[]).
s([a,b,a,a],[a,b,b],[]).
s([a,a,b,a],[b,a,b],[]).
s([a,a,a,b],[b,b,a],[]).
s([a,a,a,a],[b,b,b,b],[]).

Background knowledge (First Order)
----------------------------------
a/2:
a([a|A],A).

b/2:
b([b|A],A).

empty/2:
empty(A,B):-A=B.

Background knowledge (Second Order)
-----------------------------------
(Ls-constant) ∃.P,Q ∀.x,y,z,u,v: P(x,y,z)← Q(y,u),Q(x,v),P(v,u,z)
(Ls-variable) ∃.P,Q,R ∀.x,y,z,u,v: P(x,y,z)← Q(y,u),R(x,v),P(v,u,z)
(Ls-base) ∃.P,Q ∀.x,y: P(x,y,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Tri-chain) ∃.P,Q,R,S ∀.x,y,z,u: P(x,y)← Q(x,z),R(z,u),S(u,y)

Metasubstitution constraints
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
metarule_constraints(m(chain, P, _Q, _R), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ invented(P)
    ).
metarule_constraints(m(chain, _P, Q, _R), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(chain, _P, _Q, R), fail) :-
    l_systems_constraints:
    (   ground(R),
        target(R)
    ).

true.
==


3. Simple experiment:

==
?- debug(experiments), debug(experiment_learned), debug(experiment_examples), debug(generate_examples), debug(test_labelling), debug(test_program).
true.

?- test_harness:experiments(algae,1,all,0,6,[Labelling,Program]).
% Experiment 1 of 1
% 15,996,168 inferences, 0.937 CPU in 3.172 seconds (30% CPU, 17062579 Lips)
% Learned hypothesis:
% s(A,B,C):-b(B,D),a(A,E),s(E,D,C)
% s(A,B,C):-a(B,D),inv_1(A,E),s(E,D,C)
% s(A,B,B):-empty(A,B)
% inv_1(A,B):-a(A,C),b(C,B)
% Labelled 33 Positive examples.
% Labelled 92 Negative examples.
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Testing learned program for target: algae
% Generating all algae examples of length in [0,20].
% Generated 28656 positive testing examples
% Generating all not_algae examples of length in [0,6].
% Generated 4258 negative testing examples
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
l_systems_constraints:preterminal(a).
l_systems_constraints:preterminal(b).
l_systems_constraints:preterminal(empty).

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[4]).
:-poker_auxiliaries:set_poker_configuration_option(gestalt,[true]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[1]).
:-poker_auxiliaries:set_poker_configuration_option(respecialise,[false]).
:-poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned]).
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

background_knowledge(s/3,[a/2
                         ,b/2
                         ,empty/2
                         ]).

metarules(s/3,[ls_constant,ls_variable,ls_base,chain,tri_chain]).

initial_example(s/3,E):-
	generate_initial(algae,all,0,4,Es)
        ,member(E,Es).

a --> [a].
b --> [b].
empty --> [].

% Generate examples for evaluation.
% Examples are generated by test harndes predicates.
%
generate_examples(pos,algae,all,0,20).
generate_examples(neg,not_algae,all,0,6).
