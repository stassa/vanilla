:-module(parity, [background_knowledge/2
                 ,metarules/2
		 ,labelled_example/2
		 ,unlabelled_example/2
		 ,set_configs/0
                 ,zero/2
                 ,one/2
                 ,empty/2
                 ]).

:-use_module(project_root(configuration),[]).
:-use_module(lib(poker/poker_configuration),[]).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(lib(poker/normal_forms/chomsky_greibach_normal_form)).

/** <module> Learn even parity by inventing odd parity with Poker.

Experiment showing how to use Poker to learn a grammar of the language
of even-parity bit-strings, with metarules expressing a combination of
Chomsky and Greibach Normal Forms.

In the following listing of configs and MIL problem elements note in
particular the metarules and their constraints, defined in the module
grammar_constraints.pl. This set of constrained metarules Chain and
Identity, enforces the aforementioned combination of Chomsky Normal Form
and Greibach Normal Form on candidate hypotheses.

Normal Forms are used by Poker to avoid over-specifying metarules to
force a problem-specific form onto learned hypotheses, which defeats the
purpose of Self-Supervised Learning.


1. Check your configus and experiment data.

==
1 ?- [load_headless].
Loading poker
Global stack limit 2,147,483,648
Table space 2,147,483,648
true.

% Remember to set good configuration options:

?- experiment_file:set_configs.
true.

% List current configs:

?- auxiliaries:list_config.
encapsulation_predicate(m)
example_clauses(call)
fetch_clauses([builtins,bk,metarules])
invented_symbol_prefix(inv_)
learner(poker,lib(poker/poker))
metarule_formatting(quantified)
metasubstitution_atoms(existential)
table_meta_interpreter(false)
untable_meta_interpreter(false)
true.

?- poker_auxiliaries:list_poker_config.
clause_limit(5)
experiment_file(data(poker_examples/parity.pl),parity)
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
respecialise(true)
strict_clause_limit(false)
unfold_invented(none)
unfolding_depth_limit(500)
unlabelled_examples(100)
unlabelled_examples_order(random)
true.


% List learning data:

?- poker_auxiliaries:list_mil_problem(q0/2).
Labelled examples
-----------------
q0([],[]).
q0([0],[]).
q0([0,0],[]).
q0([1,1],[]).
q0([0,0,0],[]).
q0([0,1,1],[]).
q0([1,0,1],[]).
q0([1,1,0],[]).
q0([0,0,0,0],[]).
q0([0,0,1,1],[]).
q0([0,1,0,1],[]).
q0([0,1,1,0],[]).
q0([1,0,0,1],[]).
q0([1,0,1,0],[]).
q0([1,1,0,0],[]).
% ... 1 more clauses.

Unlabelled examples
-------------------

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

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(M, fail) :-
    cgnf:
    (   ground(M),
        M=..[m, _Id, P, P|_Ps]
    ).
metarule_constraints(m(identity, P0, _P1), fail) :-
    cgnf:
    (   ground(P0),
        \+ target(P0)
    ).
metarule_constraints(m(identity, _P0, P1), fail) :-
    cgnf:
    (   ground(P1),
        \+ preterminal(P1)
    ).
metarule_constraints(M, fail) :-
    cgnf:
    (   ground(M),
        M=..[m, Id, _P|Ps],
        Id\==identity,
        memberchk(empty, Ps)
    ).
metarule_constraints(m(chain, _P0, P1, _), fail) :-
    cgnf:
    (   ground(P1),
        target(P1)
    ).
metarule_constraints(m(chain, P0, P1, _), fail) :-
    cgnf:
    (   ground(P0),
        ground(P1),
        invented(P0),
        invented(P1)
    ).

true.


% List C-GNF metarule constraints:

4 ?- listing([target,preterminal,invented]).
:- multifile grammar_constraints:target/1.
grammar_constraints:target(q0).

:- multifile grammar_constraints:preterminal/1.

grammar_constraints:preterminal(zero).
grammar_constraints:preterminal(one).
grammar_constraints:preterminal(empty).

:- multifile grammar_constraints:invented/1.

grammar_constraints:invented(inv_1).

true.
==


2. Run a learning query.

Expected result:

==
5 ?- _T = q0/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 11,146,536 inferences, 0.734 CPU in 2.118 seconds (35% CPU, 15178262 Lips)
Hypothesis:
q0(A,B):-empty(A,B).
q0(A,B):-zero(A,C),q0(C,B).
q0(A,B):-inv_1(A,C),inv_1(C,B).
inv_1(A,B):-zero(A,C),inv_1(C,B).
inv_1(A,B):-one(A,C),q0(C,B).
Positive examples:
q0([1,1,1,1],[]).
q0([0,1,1,0],[]).
q0([0,0,0,0],[]).
q0([],[]).
q0([0],[]).
q0([0,0],[]).
q0([1,1],[]).
q0([0,0,0],[]).
q0([0,1,1],[]).
q0([1,0,1],[]).
q0([1,1,0],[]).
q0([1,1,0,0],[]).
q0([1,0,1,0],[]).
q0([1,0,0,1],[]).
q0([0,1,0,1],[]).
q0([0,0,1,1],[]).
q0([0,0,1,1,0,0],[]).
q0([0,0,1,0,0,1],[]).
Negative examples:
q0([1,1,1,0],[]).
q0([1,1,1],[]).
q0([1,1,0,1],[]).
q0([1,0,1,1],[]).
q0([1,0,0,0],[]).
q0([1,0,0],[]).
q0([1,0],[]).
q0([1],[]).
q0([0,1,1,1],[]).
q0([0,1,0,0],[]).
q0([0,1,0],[]).
q0([0,1],[]).
q0([0,0,1,0],[]).
q0([0,0,1],[]).
q0([0,0,0,1],[]).
Ps = 5,
Pos = 18,
Neg = 15.
==

Note how the learned hypothesis is a Context-Free grammar, even though
the language of even-parity bit-strings is Regular:

==
q0(A,B):-empty(A,B).
q0(A,B):-zero(A,C),q0(C,B).
q0(A,B):-inv_1(A,C),inv_1(C,B).
inv_1(A,B):-zero(A,C),inv_1(C,B).
inv_1(A,B):-one(A,C),q0(C,B).
==

This is because of the Chomsky-Greibach Normal Form that forces the
learned grammar to be Context-Free.

*/

% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
cgnf:target(q0).
cgnf:invented(inv_1).
cgnf:preterminal(zero).
cgnf:preterminal(one).
cgnf:preterminal(empty).

set_configs:-
	poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
	,poker_auxiliaries:set_configuration_option(table_meta_interpreter,[false])
	,poker_auxiliaries:set_configuration_option(untable_meta_interpreter,[false])
        ,poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[true])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        % Try setting to "all".
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[none])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).


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
poker_configuration:safe_example(m(q0,Ls,[])):-
	between(0,4,L)
	,length(Ls,L).

background_knowledge(q0/2,[zero/2
			  ,one/2
			  ,empty/2
			  ]).

metarules(q0/2,[identity,chain]).

labelled_example(q0/2,E):-
% Generates all even-parity bit-strings of length between 0 to 4.
	generate_initial(even,all,0,4,Es)
        ,member(E,Es).

unlabelled_example(q0/2,_):- fail.

zero --> [0].
one --> [1].
empty --> [].
