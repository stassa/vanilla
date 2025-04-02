:-module(exp_script_lnf, [test_algae/0
                         ,test_dragon_curve/0
                         ,test_hilbert_curve/0
                         ,test_koch_curve/0
                         ,test_sierpinski_triangle/0
                         ,test_algae/1
                         ,test_dragon_curve/1
                         ,test_hilbert_curve/1
                         ,test_koch_curve/1
                         ,test_sierpinski_triangle/1
                         ,set_configs/1
                         ,cleanup_safe_example/0
                         ,setup_safe_example/1
                         ,background_knowledge/2
                         ,metarules/2
                         ,labelled_example/2
                         ,unlabelled_example/2
                         ,a/2
                         ,b/2
                         ,f/2
                         ,g/2
                         ,x/2
                         ,y/2
                         ,plus/2
                         ,minus/2
                         ,empty/2
                         ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(data(poker_examples/experiment_output)).
:-use_module(lib(poker/normal_forms/lindenmayer_normal_form)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Experiment script for learning L-Systems with Lindenmayer Normal Form.

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

test_algae:-
        Lang = algae
        ,T = s/3
        ,Sl = algae(all,0,4)
        ,Su = []
        ,TPos = algae(all,5,20)
        ,TNeg = not_algae(all,0,6)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

test_dragon_curve:-
        Lang = dragon_curve
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        ,Su = []
        ,TPos = dragon_curve(all,5,10)
        ,TNeg = not_dragon_curve(all,0,4)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

test_hilbert_curve:-
% The first Hilbert Curve string that contains variable symbols has
% length 11.
        Lang = hilbert_curve
        ,T = s/3
        ,Sl = [hilbert_curve(all,0,3)
	      ,hilbert_curve_with_vars(all,11,11)
	      ]
        ,Su = []
        ,TPos = hilbert_curve(all,0,12)
        ,TNeg = not_hilbert_curve(all,0,4)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

test_koch_curve:-
% The first Koch Curve string that contains variable symbols has
% length 8.
        Lang = koch_curve
        ,T = s/3
        ,Sl = [koch_curve(all,0,3)
	      ,koch_curve_with_vars(all,8,9)
	      ]
        ,Su = []
        ,TPos = koch_curve(all,0,14)
        ,TNeg = not_koch_curve(all,0,5)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

test_sierpinski_triangle:-
% The first Koch Curve string that contains variable symbols has
% length 9.
        Lang = sierpinski_triangle
        ,T = s/3
        ,Sl = [sierpinski_triangle(30,0,8)
	      ,sierpinski_triangle_with-vars(10,9,15)
	      ]
	,Su = []
        ,TPos = sierpinski_triangle(all,0,14)
        ,TNeg = not_sierpinski_triangle(all,0,5)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).


                /*******************************
                *      MULTI-EXPERIMENTS       *
                *******************************/

% Experiments with repetitions. Use to observe change of results over
% random samples of labelled, unlabelled examples and generated negative
% examples.

test_algae(N):-
        Lang = algae
        ,T = s/3
        ,Sl = algae(all,0,4)
        ,Su = []
        ,TPos = algae(all,5,20)
        ,TNeg = not_algae(all,0,6)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

test_dragon_curve(N):-
        Lang = dragon_curve
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        ,Su = []
        ,TPos = dragon_curve(all,5,10)
        ,TNeg = not_dragon_curve(all,0,4)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

test_hilbert_curve(N):-
% The first Hilbert Curve string that contains variable symbols has
% length 11.
        Lang = hilbert_curve
        ,T = s/3
        ,Sl = [hilbert_curve(all,0,3)
	      ,hilbert_curve_with_vars(all,11,11)
	      ]
        ,Su = []
        ,TPos = hilbert_curve(all,0,12)
        ,TNeg = not_hilbert_curve(all,0,4)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

test_koch_curve(N):-
% The first Koch Curve string that contains variable symbols has
% length 8.
        Lang = koch_curve
        ,T = s/3
        ,Sl = [koch_curve(all,0,3)
	      ,koch_curve_with_vars(all,8,9)
	      ]
        ,Su = []
        ,TPos = koch_curve(all,0,14)
        ,TNeg = not_koch_curve(all,0,5)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

test_sierpinski_triangle(N):-
% The first Koch Curve string that contains variable symbols has
% length 9.
        Lang = sierpinski_triangle
        ,T = s/3
        ,Sl = [sierpinski_triangle(10,0,8)
	      ,sierpinski_triangle_with_vars(5,9,15)
	      ]
	,Su = []
        ,TPos = sierpinski_triangle(all,0,14)
        ,TNeg = not_sierpinski_triangle(all,0,5)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).


                /*******************************
                *    FILTERING EXPERIMENTS     *
                *******************************/

% Experiments into separating unlabelled examples into positive and
% negative with respect to labelling examples and learning a program
% from the labelled examples, and the labelled-negative examples.

test_hilbert_dragon_filtering:-
        Lang = hilbert_dragon
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        ,Su = [hilbert_curve(all,0,3)
              ,hilbert_curve_with_vars(all,11,11)
              ]
        ,TPosL = dragon_curve(all,5,10)
        ,TNegL = not_dragon_curve(all,0,4)
        ,TPosU = hilbert_curve(all,0,12)
        ,TNegU = not_hilbert_curve(all,0,4)
        ,setup_and_run_filter_experiment(Lang,T,Sl,Su,TPosL,TNegL,TPosU,TNegU).


                /*******************************
                *        CONFIGURATION         *
                *******************************/


%!      set_configs(+Language) is det.
%
%       Set configuration options for a target Language.
%
set_configs(algae):-
	poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[true])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(dragon_curve):-
	poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
	,poker_auxiliaries:set_poker_configuration_option(unfolding_depth_limit,[30])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(hilbert_curve):-
	poker_auxiliaries:set_poker_configuration_option(clause_limit,[8])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[6])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(koch_curve):-
	poker_auxiliaries:set_poker_configuration_option(clause_limit,[6])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[4])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(sierpinski_triangle):-
	%poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
	%,poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false])
	%,poker_auxiliaries:set_configuration_option(untable_meta_interpreter, [true])
	poker_auxiliaries:set_poker_configuration_option(clause_limit,[8])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[7])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[deterministic])
	,poker_auxiliaries:set_poker_configuration_option(unfolding_depth_limit,[900]).

set_configs(hilbert_dragon):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[8])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[6])
        ,poker_auxiliaries:set_poker_configuration_option(unfolding_depth_limit,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).

                /*******************************
                *       EXPERIMENT DATA        *
                *******************************/


% Language alphabet for the constraints defeined
% in lib/poker/norma_forms/lindenmayer_normal_form.pl
%
lnf:target(s).
lnf:invented(inv_1).
lnf:invented(inv_2).
lnf:invented(inv_3).
lnf:invented(inv_4).
lnf:invented(inv_5).
lnf:invented(inv_6).
lnf:invented(inv_7).
lnf:preterminal(a).
lnf:preterminal(b).
lnf:preterminal(f).
lnf:preterminal(g).
lnf:preterminal(x).
lnf:preterminal(y).
lnf:preterminal(plus).
lnf:preterminal(minus).
lnf:preterminal(empty).


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
setup_safe_example(algae):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Is,Os,[])):-
	     K = 8
	     ,between(0,K,I)
	     ,length(Is,I)
	     ,between(0,K,J)
	     ,length(Os,J))
	,assert(G).
setup_safe_example(dragon_curve):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Is,Os,[])):-
	     K = 8
	     ,between(0,K,I)
	     ,length(Is,I)
	     ,between(0,K,J)
	     ,length(Os,J))
	,assert(G).
setup_safe_example(hilbert_curve):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Is,Os,[])):-
	     K = 8
	     ,between(0,K,I)
	     ,length(Is,I)
	     ,between(0,K,J)
	     ,length(Os,J))
	,assert(G).
setup_safe_example(koch_curve):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Is,Os,[])):-
	     K = 8
	     ,between(0,K,I)
	     ,length(Is,I)
	     ,between(0,K,J)
	     ,length(Os,J))
	,assert(G).
setup_safe_example(sierpinski_triangle):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Is,Os,[])):-
	     K = 8
	     ,between(0,K,I)
	     ,length(Is,I)
	     ,between(0,K,J)
	     ,length(Os,J))
	,assert(G).
setup_safe_example(hilbert_dragon):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Is,Os,[])):-
	     K = 8
	     ,between(0,K,I)
	     ,length(Is,I)
	     ,between(0,K,J)
	     ,length(Os,J))
	,assert(G).


background_knowledge(s/3,[a/2
			 ,b/2
			 ,f/2
			 ,g/2
			 ,x/2
			 ,y/2
			 ,plus/2
			 ,minus/2
			 ,empty/2]).

metarules(s/3,[ls_constant,ls_variable,ls_base,chain,tri_chain]).

labelled_example(s/3,_):- fail.

unlabelled_example(s/3,_):- fail.

a --> [a].
b --> [b].
f --> [f].
g --> [g].
x --> [x].
y --> [y].
plus --> [+].
minus --> [-].
empty --> [].
