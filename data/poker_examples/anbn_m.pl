:-module(anbn_m, [background_knowledge/2
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

/** <module> Separate anbn from anbm strings.

==
?- _T = s/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 724,490 inferences, 0.078 CPU in 0.093 seconds (84% CPU, 9273472 Lips)
Hypothesis:
s(A,B):-a(A,C),s(C,D),b(D,B).
s(A,B):-a(A,C),b(C,B).
Positive examples:
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,b,b,b],[]).
s([a,a,a,a,b,b,b,b],[]).
s([a,a,a,a,a,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b,b,b,b,b,b],[]).
s([a,a,a,a,a,a,a,b,b,b,b,b,b,b],[]).
s([a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,b],[]).
s([a,a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,b,b],[]).
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,b,b,b],[]).
Negative examples:
s([a,a,b],[]).
s([a,a,a,b,b],[]).
s([a,a,a,b],[]).
s([a,a,a,a,b,b,b],[]).
s([a,a,a,a,b,b],[]).
s([a,a,a,a,b],[]).
s([a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,a],[]).
s([a,a,a,a,a,a,a],[]).
s([a,a,a,a,a,a],[]).
s([a,a,a,a,a],[]).
s([a,a,a,a],[]).
s([a,a,a],[]).
s([a,a],[]).
s([a],[]).
s([],[]).
Ps = 2,
Pos = 12,
Neg = 21.
==

*/

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[3]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[1]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(respecialise,[true]).
:-poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[0]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).
*/


% Language alphabet for the constraints defeined
% in lib/poker/normal_forms/chomsky_greibach_normal_form.pl
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
	between(0,12,L)
	,length(Ls,L).

background_knowledge(s/2,[a/2,b/2,empty/2]).

metarules(s/2,[identity,chain]).

labelled_example(s/2,E):-
        generate_initial(anbn,all,0,6,Es)
        ,member(E,Es).

unlabelled_example(s/2,E):-
        generate_initial(anbm,all,0,8,Es)
        ,member(E,Es).
unlabelled_example(s/2,E):-
        generate_initial(anbn,all,7,18,Es)
        ,member(E,Es).

a --> [a].
b --> [b].
empty --> [].

generate_examples(pos,anbn,all,4,8).
generate_examples(neg,not_anbn,all,4,8).
