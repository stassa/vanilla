:-module(algae, [background_knowledge/2
                 ,metarules/2
                 ,initial_example/2
                 ,a/2
                 ,b/2
                 ,empty/2
		 ,generate_examples/5
                 ]).

:-use_module(project_root(configuration),[]).
:-use_module(lib(poker/poker_configuration),[]).
:-use_module(lib(poker/poker_auxiliaries)).

:-use_module(data(poker_examples/test_harness)).

:-use_module(l_systems_constraints).

/** <module> Learn an L-System grammar modelling the growth of blue algae.

This is kind of a dumb example only useful as a proof of concept. That's
because negative examples aren't really needed to learn the target
theory.

Configs:

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
clause_limit(3)
experiment_file(data(poker_examples/algae.pl),algae)
flatten_prove_all(false)
gestalt(false)
greedy_generalisation(false)
listing_limit(15)
max_invented(0)
multithreading(false)
proof_samples(1.0)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
respecialise(true)
strict_clause_limit(false)
unlabelled_examples(100)
unlabelled_examples_order(random)
true.
==

Learning problem (training on 4 randomly drawn examples):

==
?- poker_auxiliaries:list_mil_problem(s/3).
Initial examples
----------------
s([],[],[]).
s([a,a],[b,b],[]).
s([a,a,a],[b,b,b],[]).
s([a,a,a,b],[b,b,a],[]).

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
(Ls-base) ∃.P,Q ∀.x,y: P(x,y,y)← Q(x,y)
(Ls-rec) ∃.P,Q,R,S ∀.x,y,z,u,v: P(x,y,z)← Q(y,u),R(x,v),S(v,u,z)
(Ls-rec-2) ∃.P,Q,R,S,T ∀.x,y,z,u,v,w: P(x,y,z)← Q(y,u),R(x,v),S(v,w),T(w,u,z)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(M, fail) :-
    l_systems_constraints:
    (   ground(M),
        M=..[m, _Id, P, P|_Ps]
    ).
metarule_constraints(m(ls_base, P, _Q, _R), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ target(P)
    ).
metarule_constraints(m(ls_base, _P, Q, _R), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(ls_base, _P, _Q, R), fail) :-
    l_systems_constraints:
    (   ground(R),
        \+ preterminal(R)
    ).
metarule_constraints(M, fail) :-
    l_systems_constraints:
    (   ground(M),
        M=..[m, Id, _P|Ps],
        Id\==ls_base,
        memberchk(empty, Ps)
    ).
metarule_constraints(m(Id, _P, Q, _R, _S), fail) :-
    l_systems_constraints:
    (   memberchk(Id, [ls_rec, ls_rec_2]),
        ground(Q),
        target(Q)
    ).
metarule_constraints(m(Id, P, Q, _R, _S), fail) :-
    l_systems_constraints:
    (   memberchk(Id, [ls_rec, ls_rec_2]),
        ground(P),
        ground(Q),
        invented(P),
        invented(Q)
    ).

true.
==

Learning:

==
?- _T = s/3, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 379,913 inferences, 0.062 CPU in 0.066 seconds (95% CPU, 6078608 Lips)
Hypothesis:
s(A,B,C):-b(B,D),a(A,E),s(E,D,C).
s(A,B,C):-a(B,D),a(A,E),b(E,F),s(F,D,C).
s(A,B,B):-empty(A,B).
Positive examples:
s([],[],[]).
s([a,a,b],[b,a],[]).
s([a,a,a,a],[b,b,b,b],[]).
s([a,b,a],[a,b],[]).
s([a,a,b,a],[b,a,b],[]).
s([a,a,a],[b,b,b],[]).
s([a],[b],[]).
s([a,b,a,a],[a,b,b],[]).
s([a,a],[b,b],[]).
s([a,a,a,b],[b,b,a],[]).
s([a,b],[a],[]).
s([a,b,a,b],[a,a],[]).
Negative examples:
[]
Ps = 3,
Pos = 12,
Neg = 0.
==


More complete experiment:

==
?- debug(test_program), debug(test_labelling).
true.

?- test_harness:experiments(algae,10,4,0,4,[Labels,Program]).
% 262,403 inferences, 0.047 CPU in 0.110 seconds (43% CPU, 5597931 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 451,743 inferences, 0.062 CPU in 0.142 seconds (44% CPU, 7227888 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 398,456 inferences, 0.062 CPU in 0.136 seconds (46% CPU, 6375296 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 533,759 inferences, 0.109 CPU in 0.164 seconds (67% CPU, 4880082 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 192,412 inferences, 0.062 CPU in 0.094 seconds (66% CPU, 3078592 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 392,194 inferences, 0.078 CPU in 0.132 seconds (59% CPU, 5020083 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 198,025 inferences, 0.016 CPU in 0.099 seconds (16% CPU, 12673600 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 0 TPR: 0 TNR: 0
% 362,279 inferences, 0.000 CPU in 0.062 seconds (0% CPU, Infinite Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 358,044 inferences, 0.062 CPU in 0.129 seconds (48% CPU, 5728704 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% 451,125 inferences, 0.094 CPU in 0.143 seconds (66% CPU, 4812000 Lips)
% Testing labelling for target: algae
% Labelling: Measured Acc: 1.0 TPR: 1.0 TNR: 0
% Testing learned program for target: algae
% Program: Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
Labels = [0.9,0.9,0.0],
Program = [0.95,0.9,1.0].
==


*/

% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
grammar_constraints:target(s).
grammar_constraints:invented(inv_1).
grammar_constraints:invented(inv_2).
grammar_constraints:preterminal(a).
grammar_constraints:preterminal(b).
grammar_constraints:preterminal(empty).

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[3]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[false]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[0]).
:-poker_auxiliaries:set_poker_configuration_option(respecialise,[true]).
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
	between(0,4,I)
	,length(Is,I)
	,between(0,4,J)
	,length(Os,J).


background_knowledge(s/3,[a/2
                         ,b/2
                         ,empty/2
                         ]).

metarules(s/3,[ls_base,ls_rec,ls_rec_2]).

initial_example(s/3,E):-
	generate_initial(algae,4,0,4,Es)
        ,member(E,Es).

a --> [a].
b --> [b].
empty --> [].


% Generate examples for evaluation.
% Examples are generated by test harndes predicates.
%
generate_examples(pos,algae,all,0,20).
generate_examples(neg,not_algae,all,0,6).
