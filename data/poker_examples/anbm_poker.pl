:-module(anbm, [background_knowledge/2
	       ,metarules/2
	       ,labelled_example/2
	       ,unlabelled_example/2
	       ,a/2
	       ,b/2
               ,empty/2
	       ,generate_examples/5
	       ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(lib(poker/normal_forms/chomsky_greibach_normal_form)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn a grammar of the context-free language {a^nb^m|n >= m >= 0}.

Known good configs to reproduce experiments listed here:

==
1 ?- [load_headless].
Loading poker
Loading experiment file module data(poker_examples/anbm_poker.pl) from anbm.
Global stack limit 2,147,483,648
Table space 2,147,483,648
true.

3 ?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), flatten_prove_all(Flatten), gestalt(Gestalt), greedy_generalisation(Greedy), respecialise(Respecialise), strict_clause_limit(Strict), proof_samples(Samples), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)), listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(s, Ls, [])) :-
    anbm:
    (   between(0, 9, L),
        length(Ls, L)
    ).

Fetch = all,
Table = Untable, Untable = Flatten, Flatten = Respecialise, Respecialise = true,
Limit = 4,
Invented = 1,
Gestalt = Greedy, Greedy = Strict, Strict = false,
Samples = 1.0,
Unlabelled = 1500,
Order = random,
Reduction = plotkins.

4 ?- poker_auxiliaries:list_mil_problem(s/2).
Initial examples
----------------
s([],[]).
s([a,a,a,b],[]).
s([a,a,a],[]).

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
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

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
metarule_constraints(m(chain, P0, P1, _), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        invented(P0),
        invented(P1)
    ).

true.
==

Expected results in test harness with randomly generated initial
examples:

==
17 ?- debug(test_program), debug(test_labelling), debug(experiment_learned), debug(experiment_initial), debug(experiment_examples).
true.

18 ?- test_harness:experiments(anbm,1,all,0,4,[Labelling,Program]).
% Generated 12 initial examples:
% s([],[])
% s([a],[])
% s([a,a],[])
% s([a,b],[])
% s([a,a,a],[])
% s([a,a,b],[])
% s([a,a,b],[])
% s([a,a,a,a],[])
% s([a,a,a,b],[])
% s([a,a,a,b],[])
% s([a,a,a,b],[])
% s([a,a,b,b],[])
% 8,027,864 inferences, 0.281 CPU in 1.189 seconds (24% CPU, 28543516 Lips)
% Learned hypothesis:
% s(A,B):-empty(A,B)
% s(A,B):-inv_1_95(A,C),b(C,B)
% s(A,B):-a(A,C),s(C,B)
% inv_1_95(A,B):-a(A,C),s(C,B)
% 33 Positive examples:
% 993 Negative examples:
% Testing labelling for target: anbm
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Testing learned program for target: anbm
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
Labelling = Program, Program = [1.0,1.0,1.0].
==

The learned hypothesis is the target theory in folded form:
==
% Learned hypothesis
s(A,B):-empty(A,B)
s(A,B):-inv_1_95(A,C),b(C,B)
s(A,B):-a(A,C),s(C,B)
inv_1_95(A,B):-a(A,C),s(C,B)

% Unfolded (manually):
s(A,B):-empty(A,B)
s(A,B):-a(A,C),s(C,D),b(D,B)
s(A,B):-a(A,C),s(C,B)

% Target theory (from test_harness.pl):
19 ?- listing(anbm).
test_harness:anbm(A, B) :-
    empty(A, B).
test_harness:anbm(A, B) :-
    a(A, C),
    anbm(C, B).
test_harness:anbm(A, B) :-
    a(A, C),
    anbm(C, D),
    b(D, B).

true.
==

A longer experiment:

==
22 ?- test_harness:experiments(anbm,10,all,0,4,[Labelling,Program]).
% 8,028,723 inferences, 0.578 CPU in 1.220 seconds (47% CPU, 13887521 Lips)
% 8,043,988 inferences, 0.516 CPU in 1.178 seconds (44% CPU, 15600462 Lips)
% 8,001,597 inferences, 0.312 CPU in 1.159 seconds (27% CPU, 25605110 Lips)
% 7,964,406 inferences, 0.266 CPU in 1.206 seconds (22% CPU, 29983646 Lips)
% 7,956,683 inferences, 0.359 CPU in 1.190 seconds (30% CPU, 22140335 Lips)
% 7,981,084 inferences, 0.391 CPU in 1.159 seconds (34% CPU, 20431575 Lips)
% 7,936,272 inferences, 0.547 CPU in 1.169 seconds (47% CPU, 14512040 Lips)
% 7,968,998 inferences, 0.156 CPU in 1.176 seconds (13% CPU, 51001587 Lips)
% 7,946,743 inferences, 0.422 CPU in 1.155 seconds (37% CPU, 18836724 Lips)
% 7,967,390 inferences, 0.266 CPU in 1.157 seconds (23% CPU, 29994880 Lips)
Labelling = Program, Program = [1.0,1.0,1.0].
==

It's nice to see that even with 1500 unlabelled examples learning goes
fast.

This is largely the effect of respecialisation that throws out
over-specialised initial sub-hypotheses. Try turning it off with
respecialise(false) and see what happens:
==
24 ?- test_harness:experiments(anbm,10,all,0,4,[Labelling,Program]).
% 80,966,356 inferences, 4.219 CPU in 12.448 seconds (34% CPU, 19192025 Lips)
% 77,811,689 inferences, 4.047 CPU in 12.276 seconds (33% CPU, 19227599 Lips)
% 67,227,185 inferences, 3.469 CPU in 9.412 seconds (37% CPU, 19380810 Lips)
% 53,281,795 inferences, 1.750 CPU in 7.341 seconds (24% CPU, 30446740 Lips)
% 70,114,755 inferences, 3.031 CPU in 10.068 seconds (30% CPU, 23130641 Lips)
% 56,383,076 inferences, 2.312 CPU in 7.930 seconds (29% CPU, 24381871 Lips)
% 82,199,257 inferences, 3.750 CPU in 12.872 seconds (29% CPU, 21919802 Lips)
% 68,395,025 inferences, 2.297 CPU in 9.950 seconds (23% CPU, 29777426 Lips)
% 91,571,748 inferences, 4.609 CPU in 14.002 seconds (33% CPU, 19866413 Lips)
% 62,018,994 inferences, 3.297 CPU in 9.125 seconds (36% CPU, 18811448 Lips)
Labelling = [0.9807,0.9542,0.9811],
Program = [0.9765,1.0,0.9636].
==

Learning takes longer and there are over-special hypotheses that Poker
can't get rid of (possibly unless it receives many more unlabelled
examples).

Note however that respecialising can only work when it is possible to
learn the entire target theory from a single example. This is not always
the case.

*/

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[4]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[1]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(respecialise,[true]).
:-poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[20]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).
*/


% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
cgnf:target(s).
cgnf:invented(inv_1).
cgnf:preterminal(a).
cgnf:preterminal(b).
cgnf:preterminal(empty).


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
poker_configuration:safe_example(m(s,Ls,[])):-
	between(0,9,L)
	,length(Ls,L).

background_knowledge(s/2,[a/2,b/2,empty/2]).

metarules(s/2,[identity,chain]).

% Hand-picked examples. The empty string is needed.
labelled_example(s/2,s([],[])).
labelled_example(s/2,s([a,a,a,b],[])).
labelled_example(s/2,s([a,a,a],[])).

unlabelled_example(s/2,_):- fail.

% The background knowledge is the set of pre-terminals in the language.
% a^nb^n does not include the empty string.
a --> [a].
b --> [b].
empty --> [].

generate_examples(pos,anbm,all,5,8).
generate_examples(neg,not_anbm,all,0,4).
