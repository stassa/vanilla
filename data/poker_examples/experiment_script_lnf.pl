:-module(exp_script_lnf, [algae/0
                         ,dragon_curve/0
                         ,hilbert_curve/0
                         ,koch_curve/0
                         ,sierpinski_triangle/0
                         ,dragon_curve_draw/0
                         ,hilbert_curve_draw/0
                         ,koch_curve_draw/0
                         ,sierpinski_triangle_draw/0
                         ,algae/1
                         ,dragon_curve/1
                         ,hilbert_curve/1
                         ,koch_curve/1
                         ,sierpinski_triangle/1
                         ,dragon_curve_no_generated/1
                         ,dragon_curve_no_generated_draw/0
                         ,hilbert_curve_no_generated/1
                         ,hilbert_curve_no_generated_draw/0
                         ,koch_curve_no_generated/1
                         ,koch_curve_no_generated_draw/0
                         ,sierpinski_triangle_no_generated/1
                         ,dragon_curve_range/3
                         ,hilbert_curve_range/3
                         ,koch_curve_range/3
                         ,sierpinski_triangle_range/3
                         ,dragon_to_hilbert_curve_range/3
                         ,hilbert_to_dragon_curve_range/3
                         ,koch_to_dragon_curve_range/3
                         ,koch_to_hilbert_curve_range/3
                         ,dragon_to_koch_curve_range/3
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
        ,Sl = [hilbert_curve(all,0,4)
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

%!      sierpinski_triangle is det.
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
                *      TURTLE EXPERIMENTS      *
                *******************************/

% Experiments in drawing correct L-Systems with a turtle interpreter.
% Experiments that measure Accuracy, TPR and TNR of learened programs
% evaluate those programs as acceptors, but L-Systems are meant to be
% run as generators of drawing commands for a Turtle languge
% interpreter. This batch of experiments can be used to visualise the
% learned L-systems to check their correctness.

%!      dragon_curve_draw is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Dragon Curve L-system and no
%       unlabelled examples.
%
%       Prints the learned hypothesis and labelling of internally
%       generated examples. Also generates L-System strings from the
%       learned hypothesis and draws the resulting fractal with Turtle
%       graphics.
%
dragon_curve_draw:-
        Lang = dragon_curve
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        ,Su = []
        ,TPos = dragon_curve(all,5,10)
        ,TNeg = not_dragon_curve(all,0,4)
        ,PL = print_labelled(false)
        ,DL = draw_labelled([T,16,[f],90,90,2,-(-280,50),850,550
                            ,'output/dragon_curve_1.eps'])
        ,setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,[PL,DL]).


%!      hilbert_curve_draw is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Hilbert Curve L-system and no
%       unlabelled examples.
%
hilbert_curve_draw:-
        Lang = hilbert_curve
        ,T = s/3
        ,Sl = [hilbert_curve(all,0,4)
	      ,hilbert_curve_with_vars(all,11,11) % all is 2
	      ]
        ,Su = []
        ,TPos = hilbert_curve(10000,0,12)
        ,TNeg = not_hilbert_curve(10000,0,4)
        ,PL = print_labelled(false)
        ,DL = draw_labelled([T,7,[x],90,90,8,'top_left',850,550,'output/hilbert_curve.eps'])
        ,Set = set_table_space(8_589_934_592,TS)
        ,G = setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,[PL,DL])
        ,Cln = set_table_space(TS,_)
        ,setup_call_cleanup(Set,G,Cln).


%!      hilbert_curve is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Koch Curve L-system and no
%       unlabelled examples.
%
koch_curve_draw:-
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
        ,PL = print_labelled(false)
        ,DL = draw_labelled([T,6,[f,-,-,f,-,-,f],60,60,1,-(-450,-250),780,880
                            ,'output/koch_curve.eps'])
        ,setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,[PL,DL]).


%!      sierpinski_triangle_draw is det.
%
%       Run a single experiment learning an L-System grammar.
%
%       Given are labelled examples of the Sierpinski Triangle L-system
%       and no unlabelled examples.
%
sierpinski_triangle_draw:-
        Lang = sierpinski_triangle
        ,T = s/3
        ,Sl = [sierpinski_triangle(40,0,8)
	      ,sierpinski_triangle_with_vars(all,9,15)
	      ]
	,Su = []
        ,TPos = sierpinski_triangle(1000,0,14)
        ,TNeg = not_sierpinski_triangle(1000,0,5)
        ,PL = print_labelled(false)
        ,DL = draw_labelled([T,6,[f,-,g,-,g],120,200,6,-(-480,65),400,350
                            ,'output/sierpinski_triangle.eps'])
        ,setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,[PL,DL]).


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
        ,TPos = hilbert_curve(1500,0,12)
        ,TNeg = not_hilbert_curve(1500,0,4)
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
                *   ZERO GENERATED EXAMPLES    *
                *******************************/

% Experiments with zero automatically generated examples. Used to
% investigate the effect of unlabelled examples on learning and
% labelling accuracy.


%!      dragon_curve_no_generated(+N) is det.
%
%       Run N experiments with zero automatically generated examples.
%
%       If N is 0 a single experiment is carried out and the learned
%       hypothesis is written to the user output. Otherwise N
%       experiments are carried out (including if N is 1) and only the
%       means and standard errors of Accuracy, TPR and TNR are written.
%
%       Labelled examples of Dragon Curve and unlabelled examples of
%       l_star the language of all L-System strings with symbols in
%       {+,-,f,g,x,y} (where each symbol can be both a constant and a
%       variable).
%
dragon_curve_no_generated(N):-
        Lang = dragon_curve_ng
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        ,Su = [] %l_star(800,0,4)
        ,TPos = dragon_curve(all,5,10)
        ,TNeg = not_dragon_curve(all,0,4)
        ,(   N == 0
         ->  setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true))
         ;   setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg)
         ).


%!      dragon_curve_no_generated is det.
%
%       Run N experiments with zero automatically generated examples.
%
%       Like dragon_curve_no_generated/1 but runs a single experiment
%       and draws the resulting L-System.
%
dragon_curve_no_generated_draw:-
        Lang = dragon_curve_ng
        ,T = s/3
        ,Sl = dragon_curve(all,0,4)
        %,Su = l_star(1000,0,4)
        ,Su = [dragon_curve(1500,5,10)
              ,not_dragon_curve(1500,0,4)
              ]
        %,Su = []
        ,TPos = dragon_curve(100,5,10)
        ,TNeg = not_dragon_curve(100,0,4)
        ,PL = print_labelled(false)
        ,DL = draw_labelled([T,16,[f],90,90,2,-(-280,50),850,550
                            ,'output/dragon_curve_1.eps'])
        ,experiment_output:setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,[PL,DL]).


%!      hilbert_curve_no_generated(+N) is det.
%
%       Run N experiments with zero automatically generated examples.
%
%       If N is 0 a single experiment is carried out and the learned
%       hypothesis is written to the user output. Otherwise N
%       experiments are carried out (including if N is 1) and only the
%       means and standard errors of Accuracy, TPR and TNR are written.
%
%       Labelled examples of Hilbert Curve and unlabelled examples of
%       l_star the language of all L-System strings with symbols in
%       {+,-,f,g,x,y} (where each symbol can be both a constant and a
%       variable).
%
hilbert_curve_no_generated(N):-
        Lang = hilbert_curve_ng
        ,T = s/3
        ,Sl = [hilbert_curve(all,0,3)
	      ,hilbert_curve_with_vars(all,11,11)
	      ]
        ,Su = [] % l_star(800,0,4)
        ,TPos = hilbert_curve(1500,4,12)
        ,TNeg = not_hilbert_curve(1500,0,4)
        ,(   N == 0
         ->  setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true))
         ;   setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg)
         ).


%!      hilbert_curve_no_generated is det.
%
%       Run N experiments with zero automatically generated examples.
%
%       Like hilbert_curve_no_generated/1 but runs a single experiment
%       and draws the resulting L-System.
%
hilbert_curve_no_generated_draw:-
        Lang = hilbert_curve_ng
        ,T = s/3
        ,Sl = [hilbert_curve(all,0,3)
	      ,hilbert_curve_with_vars(all,11,11)
	      ]
        ,Su = [] % l_star(800,0,4)
        ,TPos = hilbert_curve(1500,4,12)
        ,TNeg = not_hilbert_curve(1500,0,4)
        ,PL = print_labelled(false)
        ,DL = draw_labelled([T,7,[x],90,90,8,'top_left',850,550,'output/hilbert_curve.eps'])
        ,experiment_output:setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,[PL,DL]).


%!      koch_curve_no_generated(+N) is det.
%
%       Run N experiments with zero automatically generated examples.
%
%       If N is 0 a single experiment is carried out and the learned
%       hypothesis is written to the user output. Otherwise N
%       experiments are carried out (including if N is 1) and only the
%       means and standard errors of Accuracy, TPR and TNR are written.
%
%       Labelled examples of Koch Curve and unlabelled examples of
%       l_star the language of all L-System strings with symbols in
%       {+,-,f,g,x,y} (where each symbol can be both a constant and a
%       variable).
%
koch_curve_no_generated(N):-
        Lang = koch_curve_ng
        ,T = s/3
        ,Sl = [koch_curve(all,0,3)
	      ,koch_curve_with_vars(all,8,9)
	      ]
        ,Su = l_star(800,0,4)
        ,TPos = koch_curve(all,0,14)
        ,TNeg = not_koch_curve(all,0,5)
        ,(   N == 0
         ->  setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true))
         ;   setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg)
         ).


%!      koch_curve_no_generated is det.
%
%       Run N experiments with zero automatically generated examples.
%
%       Like koch_curve_no_generated/1 but runs a single experiment
%       and draws the resulting L-System.
%
koch_curve_no_generated_draw:-
        Lang = koch_curve_ng
        ,T = s/3
        ,Sl = [koch_curve(all,0,3)
	      ,koch_curve_with_vars(all,8,9)
	      ]
        ,Su = [l_star(5000,0,4)
              %,koch_curve(all,4,10)
              ]
        ,TPos = koch_curve(1500,0,14)
        ,TNeg = not_koch_curve(1500,0,5)
        ,PL = print_labelled(false)
        ,DL = draw_labelled([T,6,[f,-,-,f,-,-,f],60,60,1,-(-450,-250),780,880
                            ,'output/koch_curve.eps'])
        ,experiment_output:setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,[PL,DL]).



%!      sierpinski_triangle_no_generated(+N) is det.
%
%       Run N experiments with zero automatically generated examples.
%
%       If N is 0 a single experiment is carried out and the learned
%       hypothesis is written to the user output. Otherwise N
%       experiments are carried out (including if N is 1) and only the
%       means and standard errors of Accuracy, TPR and TNR are written.
%
%       Labelled examples of Sierpinski Triangle and unlabelled examples
%       of l_star the language of all L-System strings with symbols in
%       {+,-,f,g,x,y} (where each symbol can be both a constant and a
%       variable).
%
sierpinski_triangle_no_generated(N):-
        Lang = sierpinski_triangle_ng
        ,T = s/3
        ,Sl = [sierpinski_triangle(40,0,8)
	      ,sierpinski_triangle_with_vars(all,9,15)
	      ]
        ,Su = l_star(800,0,4)
        ,TPos = sierpinski_triangle(1000,0,14)
        ,TNeg = not_sierpinski_triangle(1000,0,5)
        ,(   N == 0
         ->  setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true))
         ;   setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg)
         ).



                /*******************************
                *      RANGE EXPERIMENTS       *
                *******************************/

% Experiments with repetitions, varying the number of labelled and
% unlabelled and generated negative examples. Use to investigate the
% relation between labelled, unlabelled, and generated examples.


%!      dragon_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying automatically generated examples.
%
%       Given are labelled examples of the Dragon Curve L-System. No
%       unlabelled examples are given.
%
%       Prints the Accuracy, TPR, and TNR means and standard errors of
%       the hypotheses and labellings learned in each experiment set.
%       The number of experimnet sets is determined by the range of
%       automatically generated examples.
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
dragon_curve_range(N,S,P):-
        Lang = dragon_curve
        ,T = s/3
        ,Gs = 0:1500/250 % 5 experiment sets
        ,Sl = dragon_curve(1:41/10,0,4) % all is 41
        ,Su = []
        ,TPos = dragon_curve(all,5,10)/hilbert_curve
        ,TNeg = not_dragon_curve(all,0,4)
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Dragon Curve',@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      hilbert_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying automatically generated examples.
%
%       Given are labelled examples of the Hilbert Curve L-System. No
%       unlabelled examples are given.
%
hilbert_curve_range(N,S,P):-
        % Don't know why but this experiment sucks up all the table RAM
        Lang = hilbert_curve_no_tabling
        ,T = s/3
        ,Gs = 0:1500/250 % 5 experiment sets
        ,Sl = [hilbert_curve(1:21/5,0,4) % all is 121
              ,hilbert_curve_with_vars(1:21/5,11,13) % all is 68
              ]
        ,Su = []
        ,TPos = hilbert_curve(1500,0,12)
        ,TNeg = not_hilbert_curve(1500,0,4)
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Hilbert Curve',@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      koch_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying automatically generated examples.
%
%       Given are labelled examples of the Koch Curve L-System. No
%       unlabelled examples are given.
%
koch_curve_range(N,S,P):-
        Lang = koch_curve
        ,T = s/3
        ,Gs = 0:1500/250 % 5 experiment sets
        ,Sl = [%koch_curve(1:21/5,0,4) % all is 63
              %,koch_curve_with_vars(1:21/5,8,11) % all is 49
               koch_curve(0:20/5,0,4) % all is 20
              ,koch_curve_with_vars(1:5/1,8,9) % all is 5
	      ]
        ,Su = []
        ,TPos = koch_curve(1500,10,14)
        ,TNeg = not_koch_curve(1500,0,5)
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Koch Curve',@(false))
         ;   Pl = false
         )
        % Increased number of examples needs more table RAM
        ,Sup = set_table_space(33_554_432_000,TS)
        ,G = setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl)
        ,Cup = set_table_space(TS,_)
        ,setup_call_cleanup(Sup,G,Cup).


%!      sierpinski_triangle_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying automatically generated examples.
%
%       Given are labelled examples of the Sierpinski Triangle L-System.
%       No unlabelled examples are given.
%
sierpinski_triangle_range(N,S,P):-
        Lang = sierpinski_triangle
        ,T = s/3
        ,Gs = 0:1500/250 % 5 experiment sets
        ,Sl = [%sierpinski_triangle(1:21/5,0,8) % all is 1681
              %,sierpinski_triangle_with_vars(1:21/5,9,15) % all is 207
               sierpinski_triangle(1:31/6,0,8) % all is 1681
              ,sierpinski_triangle_with_vars(1:126/25,9,10) % all is 128
              ]
        ,Su = []
        ,TPos = sierpinski_triangle(1000,0,14)
        ,TNeg = not_sierpinski_triangle(1000,0,5)
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Sierpinski Triangle',@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


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
        %,Sl = dragon_curve(41:41/10,0,4) % all is 41
        ,Sl = dragon_curve(1:41/10,0,4) % all is 41
        ,Su = [hilbert_curve(1:41/10,0,4) % all is 121
              ,hilbert_curve_with_vars(1:41/10,11,13) % all is 68
              ,dragon_curve(1:41/10,5,8) % all is 1236
              ]
        ,TPos = dragon_curve(all,5,10)/hilbert_curve
        ,TNeg = not_dragon_curve(all,0,4)
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Dragon to Hilbert Curve',@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      hilbert_to_dragon_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Hilbert Curve L-System and
%       unlabelled examples of the Hilbert Curve and the Dragon Curve
%       L-System, all mixed up.
%
hilbert_to_dragon_curve_range(N,S,P):-
        Lang = hilbert_curve
        ,T = s/3
        ,Gs = 0:1500/250
        ,Sl = [hilbert_curve(20:20/10,0,4) % all is 121
              ,hilbert_curve_with_vars(21:21/10,11,13) % all is 68
              ]
        ,Su = [dragon_curve(1:41/10,0,4) % all is 41
              ,hilbert_curve(1:41/10,5,7) % all is 3159
              ]
        ,TPos = hilbert_curve(1500,0,12)
        ,TNeg = not_hilbert_curve(1500,0,4)
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Hilbert to Dragon Curve',@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      koch_to_dragon_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Koch Curve L-System and
%       unlabelled examples of the Koch Curve and the Dragon Curve
%       L-System, all mixed up.
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
        ,What = 'generated'
        ,TPos = koch_curve(1500,10,14)
        ,TNeg = not_koch_curve(1500,0,5)
        ,(   P == true
         ->  Pl = plot('Koch to Dragon Curve',@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      koch_to_hilbert_curve_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Koch Curve L-System and
%       unlabelled examples of the Koch Curve and the Hilbert Curve
%       L-System, all mixed up.
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
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Koch to Hilbert Curve',@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      dragon_to_koch_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying given and generated examples.
%
%       Given are labelled examples of the Dragon Curve L-System and
%       unlabelled examples of the Koch Curve and the Dragon Curve
%       L-System, all mixed up.
%
dragon_to_koch_curve_range(N,S,P):-
        Lang = dragon_curve
        ,T = s/3
        ,Gs = 0:1500/250
        %,Sl = dragon_curve(41:41/10,0,4) % all is 41
        ,Sl = dragon_curve(1:41/10,0,4) % all is 41
        ,Su = [koch_curve(1:21/5,0,5) % all is 63
              ,koch_curve_with_vars(1:21/5,8,11) % all is 49
	      ,dragon_curve(1:41/10,5,8) % all is 1236
              ]
        ,TPos = dragon_curve(all,5,10)/hilbert_curve
        ,TNeg = not_dragon_curve(all,0,4)
        ,What = 'generated'
        ,(   P == true
         ->  Pl = plot('Dragon to Koch Curve',@(true))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


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
                *      EXPERIMENT HELPERS      *
                *******************************/


%!      set_table_space(+New,-Current) is det.
%
%       Set the RAM limit for tabling.
%
%       New is the number of bytes, in base-two, in which to set the
%       tabling RAM limit. This is done by modifying the value of
%       the Prolog flag table_space.
%
%       Current is the current value of the table_space flag. Used to
%       reset the table RAM to its previous setting later.
%
set_table_space(S,TS):-
        current_prolog_flag(table_space, TS)
        ,format('Current table space ~D~n',[TS])
        ,set_prolog_flag(table_space,S)
        ,current_prolog_flag(table_space, NS)
        ,format('New table space ~D~n',[NS]).


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
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
	,poker_auxiliaries:set_poker_configuration_option(unfolding_depth_limit,[30])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(dragon_curve_ng):-
	!
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[0])
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

set_configs(hilbert_curve_no_tabling):-
	!
	,poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
	,poker_auxiliaries:set_configuration_option(table_meta_interpreter,[false])
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[8])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[6])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
							 ,[random]).

set_configs(hilbert_curve_ng):-
	!
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[8])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[6])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[0])
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

set_configs(koch_curve_ng):-
	!
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[6])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[4])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[0])
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

set_configs(sierpinski_triangle_ng):-
	!
	,poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
	,poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false])
	,poker_auxiliaries:set_configuration_option(untable_meta_interpreter, [true])
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[6])
	,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
	,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[5])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
	,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[0])
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
                       ,dragon_curve_ng
                       ,hilbert_curve
                       ,hilbert_curve_no_tabling
                       ,hilbert_curve_ng
                       ,koch_curve
                       ,koch_curve_ng
                       ,sierpinski_triangle
                       ,sierpinski_triangle_ng
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
