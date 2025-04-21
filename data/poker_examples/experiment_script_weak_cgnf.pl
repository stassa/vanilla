:-module(exp_script_wcgnf, [parity/0
                           ,anbn/0
                           ,anbm/0
                           ,parens/0
                           ,palindrome/0
                           ,parity/1
                           ,anbn/1
                           ,anbm/1
                           ,parens/1
                           ,palindrome/1
                           ,anbn_range/2
                           ,anbm_range/2
                           ,parens_range/2
                           ,palindrome_range/2
                           ,set_configs/1
                           ,cleanup_safe_example/0
                           ,setup_safe_example/1
                           ,background_knowledge/2
                           ,metarules/2
                           ,labelled_example/2
                           ,unlabelled_example/2
                           ,a/2
                           ,b/2
                           ,lp/2
                           ,rp/2
                           ,zero/2
                           ,one/2
                           ,empty/2
                          ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(data(poker_examples/experiment_output)).
:-use_module(lib(poker/normal_forms/weak_chomsky_greibach_nf)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Unified script for experiments learning CFGs with Weak C-GNF.

*/

:-debug(generalise).
:-debug(generate).
:-debug(experiments).
:-debug(experiment_initial).
:-debug(experiment_time).
:-debug(experiment_examples).
:-debug(experiment_learned).
:-debug(experiment_learned_full).
:-debug(test_program).
:-debug(test_labelling).
:-debug(generate_examples).
:-debug(generate_initial).
:-debug(filter_by_language).


                /*******************************
                *      SINGLE EXPERIMENTS      *
                *******************************/

% Simple set of single-run experiments. Use for quick eyballing of
% results while working out the right configs.

parity:-
        Lang = even
        ,T = q0/2
        ,Sl = even(all,0,4)
        ,Su = []
        ,TPos = even(all,4,8)
        ,TNeg = odd(all,0,4)
        ,set_configs(Lang)
        ,cleanup_safe_example
        ,setup_safe_example(Lang)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true)).

anbn:-
        Lang = anbn
        ,T = s/2
        ,Sl = anbn(all,0,6)
        ,Su = []
        ,TPos = anbn(all,8,20)
        ,TNeg = not_anbn(all,0,3)
        ,set_configs(Lang)
        ,cleanup_safe_example
        ,setup_safe_example(Lang)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true)).

anbm:-
        Lang = anbm
        ,T = s/2
        ,Sl = anbm(all,0,4)
        ,Su = []
	,TPos = anbm(all,5,8)
        ,TNeg = not_anbm(all,0,4)
        ,set_configs(Lang), cleanup_safe_example
        ,setup_safe_example(Lang)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true)).

parens:-
        Lang = parens
	,T = p/2
	,Sl = parens(all,0,6)
	,Su = []
	,TPos = parens(all,0,20)
	,TNeg = unbalanced_parens(all,0,15)
	,set_configs(Lang)
	,cleanup_safe_example
	,setup_safe_example(Lang)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

palindrome:-
	Lang = palindrome
	,T = q0/2
	,Sl = palindrome(all,0,4)
	,Su = []
	,TPos = palindrome(all,5,8)
	,TNeg = not_palindrome(all,0,8)
	,set_configs(Lang)
	,cleanup_safe_example
	,setup_safe_example(Lang)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).


                /*******************************
                *      MULTI-EXPERIMENTS       *
                *******************************/

% Experiments with repetitions. Use to observe change of results over
% random samples of labelled, unlabelled examples and generated negative
% examples.

%!      parity(+N) is det.
%
%       Run N experiments and print the aggregate evaluation results.
%
parity(N):-
        Lang = even
        ,T = q0/2
        ,Sl = even(all,0,4)
        ,Su = []
        ,TPos = even(all,4,8)
        ,TNeg = odd(all,0,4)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

%!      anbn(+N) is det.
%
%       Run N experiments and print the aggregate evaluation results.
%
anbn(N):-
        Lang = anbn
        ,T = s/2
        ,Sl = anbn(all,0,6)
        ,Su = []
        ,TPos = anbn(all,8,20)
        ,TNeg = not_anbn(all,0,3)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

%!      anbm(+N) is det.
%
%       Run N experiments and print the aggregate evaluation results.
%
anbm(N):-
        Lang = anbm
        ,T = s/2
        ,Sl = anbm(all,0,4)
        ,Su = []
	,TPos = anbm(all,5,8)
        ,TNeg = not_anbm(all,0,4)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

%!      parens(+N) is det.
%
%       Run N experiments and print the aggregate evaluation results.
%
parens(N):-
        Lang = parens
	,T = p/2
	,Sl = parens(all,0,6)
	,Su = []
	,TPos = parens(all,0,20)
	,TNeg = unbalanced_parens(all,0,15)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

%!      palindrome(+N) is det.
%
%       Run N experiments and print the aggregate evaluation results.
%
palindrome(N):-
	Lang = palindrome
	,T = q0/2
	,Sl = palindrome(8,0,4)
	,Su = []
	,TPos = palindrome(all,5,8)
	,TNeg = not_palindrome(all,0,8)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).


                /*******************************
                *      RANGE EXPERIMENTS       *
                *******************************/

% Experiments with repetitions, varying the number of labelled and
% unlabelled and generated negative examples. Use to investigate the
% relation between labelled, unlabelled, and generated examples.


%!      anbn_range(+N,+Stream) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
%       Results are output to the given output Stream.
%
anbn_range(N,S):-
        Lang = anbn
        ,T = s/2
        ,Gs = 0:25/5
        ,Sl = anbn(1:26/5,0,52) % all is 26
        ,Su = []
        ,TPos = anbn(all,46,80)
        ,TNeg = not_anbn(all,0,12)
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg).


%!      anbm_range(+N,+Stream) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
%       Results are output to the given output Stream.
%
anbm_range(N,S):-
        Lang = anbm
        ,T = s/2
        ,Gs = 0:25/5
        ,Sl = anbm(1:81/20,0,7) % all is 88
        ,Su = []
        ,TPos = anbm(all,7,18)
        ,TNeg = not_anbm(all,0,13)
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg).


%!      parens_range(+N,+Stream) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
%       Results are output to the given output Stream.
%
parens_range(N,S):-
        Lang = parens
        ,T = p/2
        ,Gs = 0:100/20
        ,Sl = parens(1:61/10,0,10) % all is 54
        ,Su = []
        ,TPos = parens(all,11,21)
        ,TNeg = unbalanced_parens(all,0,15)
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg).


%!      palindrome_range(+N,+Stream) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
%       Results are output to the given output Stream.
%
palindrome_range(N,S):-
        Lang = palindrome
        ,T = q0/2
        ,Gs = 0:250/50
        ,Sl = palindrome(1:21/4,0,5) % all is 54
        ,Su = []
        ,TPos = palindrome(all,6,16)
        ,TNeg = not_palindrome(all,0,10)
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg).


                /*******************************
                *        CONFIGURATION         *
                *******************************/


%!      set_configs(+Language) is det.
%
%       Set configuration options for a target Language.
%
set_configs(even):-
        poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
        ,poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false])
        ,poker_auxiliaries:set_configuration_option(untable_meta_interpreter, [true])
        ,poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[generalised])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(anbn):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[3])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[0])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[false])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[20])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(anbm):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[3])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[0])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[20])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(parens):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[3])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(palindrome):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[0])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[false])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[250])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).


                /*******************************
                *       EXPERIMENT DATA        *
                *******************************/


% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
weak_cgnf:target(s).
weak_cgnf:target(p).
weak_cgnf:target(q0).
weak_cgnf:invented(inv_1).
weak_cgnf:invented(inv_2).
weak_cgnf:invented(inv_3).
weak_cgnf:invented(inv_4).
weak_cgnf:preterminal(a).
weak_cgnf:preterminal(b).
weak_cgnf:preterminal(lp).
weak_cgnf:preterminal(rp).
weak_cgnf:preterminal(one).
weak_cgnf:preterminal(zero).
weak_cgnf:preterminal(empty).


%!      cleanup_safe_example is det.
%
%       Remove all clauses of safe_example/1 from the dynamic db.
%
cleanup_safe_example:-
        clause(safe_example(_),B)
        ,poker_configuration:retract(safe_example(_):-B)
        ,!.
cleanup_safe_example.


%!      setup_safe_example(+Language) is det.
%
%       Assert a clause of safe_example/1 for a Language.
%
setup_safe_example(even):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(q0,Ls,[])):-
             between(0,4,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(anbn):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Ls,[])):-
             between(0,9,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(anbm):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Ls,[])):-
             between(0,9,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(parens):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(p,Ls,[])):-
             between(0,10,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(palindrome):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(q0,Ls,[])):-
             between(0,8,L)
             ,length(Ls,L))
        ,assert(G).


% s/2: anbn, anbm
% p/2: parens
% q0/2: palindrome
% Probably good idea to unify them?
background_knowledge(s/2,[a/2,b/2,empty/2]).
background_knowledge(p/2,[lp/2,rp/2,empty/2]).
background_knowledge(q0/2,[zero/2,one/2,empty/2]).

metarules(s/2,[identity,chain,tri_chain]).
metarules(p/2,[identity,chain,tri_chain]).
metarules(q0/2,[identity,chain,tri_chain]).

% Dummy, replaced by input to experiment predicate.
labelled_example(s/2,_):- fail.
labelled_example(p/2,_):- fail.
labelled_example(q0/2,_):- fail.

unlabelled_example(s/2,_):- fail.
unlabelled_example(p/2,_):- fail.
unlabelled_example(q0/2,_):- fail.

a --> [a].
b --> [b].
lp --> ['('].
rp --> [')'].
zero --> [0].
one --> [1].
empty --> [].

