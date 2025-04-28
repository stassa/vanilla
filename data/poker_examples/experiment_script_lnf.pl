:-module(exp_script_lnf, [algae/0
                         ,dragon_curve/0
                         ,hilbert_curve/0
                         ,koch_curve/0
                         ,sierpinski_triangle/0
                         ,algae/1
                         ,dragon_curve/1
                         ,hilbert_curve/1
                         ,koch_curve/1
                         ,sierpinski_triangle/1
                         ,dragon_to_hilbert_curve_range/3
                         ,hilbert_to_dragon_curve_range/3
                         ,koch_to_dragon_curve_range/3
                         ,koch_to_hilbert_curve_range/3
                         ,hilbert_dragon_filtering/0
                         ,koch_dragon_filtering/0
                         ,hilbert_dragon_filtering/1
                         ,koch_dragon_filtering/1
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
:-debug(experiment).
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

%!      algae is det.
%
%       Run a single experiment learning a simple L-System grammar.
%
%       Given are labelled examples of the "algae" L-system and no
%       unlabelled examples.
%
%       Prints the learned hypothesis and labelling of internally
%       generated examples.
%
algae:-
        Lang = algae
        ,T = s/3
        ,Sl = algae(all,0,4)
        ,Su = []
        ,TPos = algae(all,5,20)
        ,TNeg = not_algae(all,0,6)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

%!      dragon_curve is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Dragon Curve L-system and no
%       unlabelled examples.
%
%       Prints the learned hypothesis and labelling of internally
%       generated examples.
%
dragon_curve:-
        Lang = dragon_curve
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        ,Su = []
        ,TPos = dragon_curve(all,5,10)
        ,TNeg = not_dragon_curve(all,0,4)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

dragon_curve_ul:-
        Lang = dragon_curve
        ,T = s/3
        ,Sl = dragon_curve(200,0,6)
        ,Su = [hilbert_curve(all,0,3)
              ,hilbert_curve_with_vars(all,11,11)
              ,dragon_curve(200,0,7)
              %,not_dragon_curve(all,0,3)
              ]
        ,TPos = dragon_curve(2000,7,10)
        ,TNeg = not_dragon_curve(2000,4,5)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

dragon_hilbert_mix:-
        Lang = hilbert_dragon
        ,T = s/3
        ,Sl = [hilbert_curve(all,0,3) % all is 121
              ,hilbert_curve_with_vars(all,11,11) % all is 68
              ,dragon_curve(4,5,7) % all is 1236
              ]
        ,Su = []
        ,TPos = [dragon_curve(1500,8,10)
                ,hilbert_curve(1500,4,10)
                ]
        ,TNeg = [not_dragon_curve(1000,0,4)
                ,not_hilbert_curve(1000,0,4)
                ]
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).


%!      hilbert_curve is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Hilbert Curve L-system and no
%       unlabelled examples.
%
%       Prints the learned hypothesis and labelling of internally
%       generated examples.
%
hilbert_curve:-
% The first Hilbert Curve string that contains variable symbols has
% length 11.
        Lang = hilbert_curve
        ,T = s/3
        ,Sl = [hilbert_curve(all,0,3)
	      ,hilbert_curve_with_vars(all,11,11)
	      ]
        ,Su = []
        ,TPos = hilbert_curve(10000,0,12)
        ,TNeg = not_hilbert_curve(10000,0,4)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).

%!      hilbert_curve is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Koch Curve L-system and no
%       unlabelled examples.
%
%       Prints the learned hypothesis and labelling of internally
%       generated examples.
%
koch_curve:-
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

%!      hilbert_curve is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Sierpinski Triangle L-system
%       and no unlabelled examples.
%
%       Prints the learned hypothesis and labelling of internally
%       generated examples.
%
sierpinski_triangle:-
% The first Koch Curve string that contains variable symbols has
% length 9.
        Lang = sierpinski_triangle
        ,T = s/3
        ,Sl = [sierpinski_triangle(40,0,8)
	      ,sierpinski_triangle_with_vars(all,9,15)
	      ]
	,Su = []
        ,TPos = sierpinski_triangle(1000,0,14)
        ,TNeg = not_sierpinski_triangle(1000,0,5)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).


                /*******************************
                *      MULTI-EXPERIMENTS       *
                *******************************/

% Experiments with repetitions. Use to observe change of results over
% random samples of labelled, unlabelled examples and generated negative
% examples.

%!      algae(+N) is det.
%
%       Run N experiments learning a simple L-System grammar.
%
%       Given are labelled examples of the Sierpinski Triangle L-system
%       and no unlabelled examples.
%
%       Experiments are repeated N times.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the learned hypothesis and labelling of internally generated
%       examples.
%
algae(N):-
        Lang = algae
        ,T = s/3
        ,Sl = algae(all,0,4)
        ,Su = []
        ,TPos = algae(all,5,20)
        ,TNeg = not_algae(all,0,6)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

%!      dragon_curve(+N) is det.
%
%       Run N experiments learning an L-System grammar.
%
%       Given are labelled examples of the Dragon Curve L-system
%       and no unlabelled examples.
%
%       Experiments are repeated N times.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the learned hypothesis and labelling of internally generated
%       examples.
%
dragon_curve(N):-
        Lang = dragon_curve
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        ,Su = []
        ,TPos = dragon_curve(all,5,10)
        ,TNeg = not_dragon_curve(all,0,4)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).

%!      hilbert_curve(+N) is det.
%
%       Run N experiments learning an L-System grammar.
%
%       Given are labelled examples of the Hilbert Curve L-system
%       and no unlabelled examples.
%
%       Experiments are repeated N times.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the learned hypothesis and labelling of internally generated
%       examples.
%
hilbert_curve(N):-
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

%!      koch_curve(+N) is det.
%
%       Run N experiments learning an L-System grammar.
%
%       Given are labelled examples of the Koch Curve L-system
%       and no unlabelled examples.
%
%       Experiments are repeated N times.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the learned hypothesis and labelling of internally generated
%       examples.
%
koch_curve(N):-
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

%!      sierpinski_triangle(+N) is det.
%
%       Run N experiments learning an L-System grammar.
%
%       Given are labelled examples of the Sierpinski Triangle L-system
%       and no unlabelled examples.
%
%       Experiments are repeated N times.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the learned hypothesis and labelling of internally generated
%       examples.
%
sierpinski_triangle(N):-
% The first Koch Curve string that contains variable symbols has
% length 9.
        Lang = sierpinski_triangle
        ,T = s/3
        ,Sl = [sierpinski_triangle(30,0,8)
	      ,sierpinski_triangle_with_vars(all,9,10)
	      ]
	,Su = []
        ,TPos = sierpinski_triangle(1000,0,14)
        ,TNeg = not_sierpinski_triangle(1000,0,5)
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).



                /*******************************
                *      RANGE EXPERIMENTS       *
                *******************************/

% Experiments with repetitions, varying the number of labelled and
% unlabelled and generated negative examples. Use to investigate the
% relation between labelled, unlabelled, and generated examples.

%!      dragon_to_hilbert_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Dragon Curve L-System and
%       unlabelled examples of the Dragon Curve and the Hilbert Curve
%       L-System, all mixed up.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the hypotheses and labellings learned in each experiment set.
%       The number of experimnet sets is determined by the range of
%       internally generated examples.
%
%       Each experiment in an experiment set is repeated N times.
%
%       Results are written to the given Stream. This can be
%       "user_output" to print to terminal.
%
%       Plot is a boolean (true or false) that determines whether to
%       plot experiments results from the given Stream or not. If Plot
%       is "true" then Stream must be the path to a CSV file (not
%       user_output) elser errors will be raised.
%
dragon_to_hilbert_curve_range(N,S,P):-
        Lang = dragon_curve
        ,T = s/3
        ,Gs = 0:1500/250
        ,Sl = dragon_curve(41:41/10,0,4) % all is 41
        ,Su = [hilbert_curve(1:41/10,0,4) % all is 121
              ,hilbert_curve_with_vars(1:41/10,11,13) % all is 68
              ,dragon_curve(1:41/10,5,8) % all is 1236
              ]
        ,TPos = dragon_curve(all,5,10)/hilbert_curve
        ,TNeg = not_dragon_curve(all,0,4)
        ,(   P == true
         ->  Pl = plot('Dragon to Hilbert Curve',@(false),@(true))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      hilbert_to_dragon_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Hilbert Curve L-System and
%       unlabelled examples of the Hilbert Curve and the Dragon Curve
%       L-System, all mixed up.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the hypotheses and labellings learned in each experiment set.
%       The number of experimnet sets is determined by the range of
%       internally generated examples.
%
%       Each experiment in an experiment set is repeated N times.
%
%       Results are written to the given Stream. This can be
%       "user_output" to print to terminal.
%
%       Plot is a boolean (true or false) that determines whether to
%       plot experiments results from the given Stream or not. If Plot
%       is "true" then Stream must be the path to a CSV file (not
%       user_output) elser errors will be raised.
%
hilbert_to_dragon_curve_range(N,S,P):-
        Lang = hilbert_curve
        ,T = s/3
        ,Gs = 0:1500/250 % 5 experiment sets
        ,Sl = [hilbert_curve(20:20/10,0,4) % all is 121
              ,hilbert_curve_with_vars(21:21/10,11,13) % all is 68
              ]
        ,Su = [dragon_curve(1:41/10,0,4) % all is 41
              ,hilbert_curve(1:41/10,5,7) % all is 3159
              ]
        ,TPos = hilbert_curve(1500,0,12)
        ,TNeg = not_hilbert_curve(1500,0,4)
        ,(   P == true
         ->  Pl = plot('Hilbert to Dragon Curve',@(false),@(true))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      koch_to_dragon_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Koch Curve L-System and
%       unlabelled examples of the Koch Curve and the Dragon Curve
%       L-System, all mixed up.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the hypotheses and labellings learned in each experiment set.
%       The number of experimnet sets is determined by the range of
%       internally generated examples.
%
%       Each experiment in an experiment set is repeated N times.
%
%       Results are written to the given Stream. This can be
%       "user_output" to print to terminal.
%
%       Plot is a boolean (true or false) that determines whether to
%       plot experiments results from the given Stream or not. If Plot
%       is "true" then Stream must be the path to a CSV file (not
%       user_output) elser errors will be raised.
%
koch_to_dragon_curve_range(N,S,P):-
        Lang = koch_curve
        ,T = s/3
        ,Gs = 0:1500/250 % 5 experiment sets
        ,Sl = [koch_curve(20:20/10,0,5) % all is 63
              ,koch_curve_with_vars(21:21/10,8,11) % all is 49
	      ]
        ,Su = [dragon_curve(1:41/10,0,4) % all is 41
              ,koch_curve(1:41/10,4,7) % all is 773
              ]
        ,TPos = koch_curve(1500,10,14)
        ,TNeg = not_koch_curve(1500,0,5)
        ,(   P == true
         ->  Pl = plot('Koch to Dragon Curve',@(false),@(true))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      koch_to_hilbert_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Koch Curve L-System and
%       unlabelled examples of the Koch Curve and the Hilbert Curve
%       L-System, all mixed up.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the hypotheses and labellings learned in each experiment set.
%       The number of experimnet sets is determined by the range of
%       internally generated examples.
%
%       Each experiment in an experiment set is repeated N times.
%
%       Results are written to the given Stream. This can be
%       "user_output" to print to terminal.
%
%       Plot is a boolean (true or false) that determines whether to
%       plot experiments results from the given Stream or not. If Plot
%       is "true" then Stream must be the path to a CSV file (not
%       user_output) elser errors will be raised.
%
koch_to_hilbert_curve_range(N,S,P):-
        Lang = koch_curve
        ,T = s/3
        ,Gs = 0:1500/250 % 5 experiment sets
        ,Sl = [koch_curve(20:20/10,0,5) % all is 63
              ,koch_curve_with_vars(21:21/10,8,11) % all is 49
	      ]
        ,Su = [hilbert_curve(1:41/10,0,4) % all is 121
              ,hilbert_curve_with_vars(1:41/10,11,13) % all is 68
              ,koch_curve(1:41/10,4,7) % all is 773
              ]
        ,TPos = koch_curve(1500,10,14)
        ,TNeg = not_koch_curve(1500,0,5)
        ,(   P == true
         ->  Pl = plot('Koch to Hilbert Curve',@(false),@(true))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


                /*******************************
                *    FILTERING EXPERIMENTS     *
                *******************************/

% Experiments into separating unlabelled examples into positive and
% negative with respect to labelling examples and learning a program
% from the labelled examples, and the labelled-negative examples.

%!      hilbert_dragon_filtering is det.
%
%       Run a filtering experiment over two L-Systems.
%
%       Given are labelled examples of the Dragon Curve L-system and
%       unlabelled examples of the Hilbert Curve L-System.
%
%       A hypothesis and labelling of the Dragon Curve L-System is
%       learned, then the negative examples in the labelling are used to
%       learn a hypothesis of the Hilbert Curve.
%
%       Results are the learned hypotheses and labellings for both
%       L-Systems and the Accuracy, TPR and TNR means and standard
%       errors of the hypotheses and labellings.
%
hilbert_dragon_filtering:-
% Trying to draw one L-system after the other raises an error.
        Lang = hilbert_dragon_filter
        ,T = s/3
        ,Sl = dragon_curve(all,0,4) % 41
        ,Su = [hilbert_curve(5,0,4) % all is 121
              ,hilbert_curve_with_vars(5,11,13) % all is 68
              ,dragon_curve(5,5,8) % all is 1236
              ]
        ,TPosL = dragon_curve(1500,5,10)/hilbert_curve
        ,TNegL = not_dragon_curve(1500,0,4)
        ,TPosU = hilbert_curve(1500,0,12)/dragon_curve
        ,TNegU = not_hilbert_curve(1500,0,4)
        ,PL = print_labelled(false)
        ,PU = print_unlabelled(false)
        %,DL = draw_labelled(false)
        ,DL = draw_labelled([T,16,[f],90,90,2,-(-280,50),850,550,'dragon_curve_1.eps'])
        ,DU = draw_unlabelled(false)
        %,DU = draw_unlabelled([T,7,[x],90,90,8,'top_left',850,550,'hilbert_curve.eps'])
        ,Os = [PL,PU,DL,DU]
        ,current_prolog_flag(table_space, V)
        ,set_prolog_flag(table_space, 17_179_869_184)
        ,current_prolog_flag(table_space, C)
        ,format('Table space ~D~n',[C])
        ,setup_run_filter_experiment_draw(Lang,T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Os)
        ,set_prolog_flag(table_space, V).

%!      koch_dragon_filtering is det.
%
%       Run a filtering experiment over two L-Systems.
%
%       Given are labelled examples of the Koch Curve L-system and
%       unlabelled examples of the Dragon Curve L-System.
%
%       A hypothesis and labelling of the Koch Curve L-System is
%       learned, then the negative examples in the labelling are used to
%       learn a hypothesis of the Dragon Curve.
%
%       Results are the learned hypotheses and labellings for both
%       L-Systems and the Accuracy, TPR and TNR means and standard
%       errors of the hypotheses and labellings.
%
koch_dragon_filtering:-
% Trying to draw one L-system after the other raises an error.
        Lang = koch_dragon
        ,T = s/3
        ,Sl = [koch_curve(all,0,3)
	      ,koch_curve_with_vars(all,8,9)
              ]
        ,Su = dragon_curve(all,0,4)
        ,TPosL = koch_curve(all,0,14)
        ,TNegL = not_koch_curve(all,0,5)
        ,TPosU = dragon_curve(1500,5,10)
        ,TNegU = not_dragon_curve(1500,0,4)
        ,PL = print_labelled(false)
        ,PU = print_unlabelled(false)
        %,DL = draw_labelled(false)
        ,DL = draw_labelled([T,6,[f,-,-,f,-,-,f],60,60,1,-(-450,-250),780,880
                            ,'koch_curve.eps'])
        ,DU = draw_unlabelled(false)
        %,DU = draw_unlabelled([T,16,[f],90,90,2,-(-280,50),850,550,'dragon_curve_2.eps'])
        ,Os = [PL,PU,DL,DU]
        ,setup_run_filter_experiment_draw(Lang,T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Os).


                /*******************************
                * FILTERING MULTI_EXPERIMENTS  *
                *******************************/

% Like filtering experiments but with multiple repeates.
% This kind of experiment does not print out the learned hypotheses nor
% does it draw the corresponding L-systems, only prints out the
% evaluation results.

%!      hilbert_dragon_filtering(+N) is det.
%
%       Run N filtering experiments over two L-Systems.
%
%       Given are labelled examples of the Dragon Curve L-system and
%       unlabelled examples of the Hilbert Curve L-System.
%
%       A hypothesis and labelling of the Dragon Curve L-System is
%       learned, then the negative examples in the labelling are used to
%       learn a hypothesis of the Hilbert Curve.
%
%       The experiment is repeated N times.
%
%       Results are the Accuracy, TPR and TNR means and standard errors
%       of the hypotheses and labellings learned for both L-Systems.
%
hilbert_dragon_filtering(N):-
        Lang = hilbert_dragon
        ,T = s/3
        ,Sl = dragon_curve(20,0,4)
        ,Su = [hilbert_curve(20,0,3)
              ,hilbert_curve_with_vars(4,11,11)
              ,dragon_curve(50,5,7) % all is 500
              ]
        ,TPosL = dragon_curve(1500,5,10)
        ,TNegL = not_dragon_curve(1500,0,4)
        ,TPosU = hilbert_curve(1500,0,12)
        ,TNegU = not_hilbert_curve(1500,0,4)
        ,setup_and_run_filter_experiments(Lang,T,N,Sl,Su,TPosL,TNegL,TPosU,TNegU).

%!      koch_dragon_filtering(+N) is det.
%
%       Run N filtering experiments over two L-Systems.
%
%       Given are labelled examples of the Koch Curve L-system and
%       unlabelled examples of the Dragon Curve L-System.
%
%       A hypothesis and labelling of the Koch Curve L-System is
%       learned, then the negative examples in the labelling are used to
%       learn a hypothesis of the Dragon Curve.
%
%       The experiment is repeated N times.
%
%       Results are the Accuracy, TPR and TNR means and standard errors
%       of the hypotheses and labellings learned for both L-Systems.
%
koch_dragon_filtering(N):-
        Lang = koch_dragon
        ,T = s/3
        ,Sl = [koch_curve(7,0,3)
	      ,koch_curve_with_vars(10,8,9)
              ]
        ,Su = dragon_curve(20,0,4)
        ,TPosL = koch_curve(all,0,14)
        ,TNegL = not_koch_curve(all,0,5)
        ,TPosU = dragon_curve(1500,5,10)
        ,TNegU = not_dragon_curve(1500,0,4)
        ,setup_and_run_filter_experiments(Lang,T,N,Sl,Su,TPosL,TNegL,TPosU,TNegU).


                /*******************************
                *        CONFIGURATION         *
                *******************************/


%!      set_configs(+Language) is det.
%
%       Set configuration options for a target Language.
%
set_configs(algae):-
	!
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[true])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(dragon_curve):-
	!
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
	,poker_auxiliaries:set_poker_configuration_option(unfolding_depth_limit,[30])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(hilbert_curve):-
	!
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[8])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[6])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(koch_curve):-
	!
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[6])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[4])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(sierpinski_triangle):-
	!
	%,poker_auxiliaries:set_configuration_option(fetch_clauses,[all])
	%,poker_auxiliaries:set_configuration_option(table_meta_interpreter, [true])
	,poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
	,poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false])
	,poker_auxiliaries:set_configuration_option(untable_meta_interpreter, [true])
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[6])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[5])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[200])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(hilbert_dragon):-
        !
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[8])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[6])
        ,poker_auxiliaries:set_poker_configuration_option(unfolding_depth_limit,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[200])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).

set_configs(koch_dragon):-
        !
        ,poker_auxiliaries:set_poker_configuration_option(clause_limit,[6])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[4])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(hilbert_dragon_filter):-
        !
	,poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
	,poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false])
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
        ,poker_auxiliaries:set_poker_configuration_option(unfolding_depth_limit,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[200])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).


set_configs(Unknown):-
        throw('Unknown language':Unknown).

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
setup_safe_example(Lang):-
        !
        ,must_be(oneof([algae
                       ,dragon_curve
                       ,hilbert_curve
                       ,koch_curve
                       ,sierpinski_triangle
                       ,hilbert_dragon
                       ,koch_dragon
                       ,hilbert_dragon_filter]
                      )
                ,Lang
                )
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
