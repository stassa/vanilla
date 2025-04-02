:-module(experiment_output,[setup_and_run_experiment/7
                           ,setup_and_run_experiments/7
                           ,setup_and_run_range_experiments/9
                           ,setup_and_run_filter_experiment/9
                           ,setup_run_filter_experiment_draw/9
                           ]).

:-use_module(data(poker_examples/test_harness)).

:- poker_configuration:experiment_file(P,_M)
  ,use_module(P).

/** <module> Output results of experiments in an experiment script.

*/

%!      setup_and_run_experiment(+Lang,+Tgt,+Lab,+Unlb,+Tpos,+TNeg,+Print)
%!      is det.
%
%       Configure Poker and run an experiment.
%
%       Lang is the symbol of a DCG in test_harness. Lang is used to
%       generate labelled training exampls and select clauses of
%       set_config/1 and setup_safe_example/1 in this file.
%
%       Tgt is the predicate symbol and arity of the examples of Lang,
%       used to select clauses of background_knowledge/2 and metarules/2
%       declared in this file. It is possible for multiple target
%       languages (defined by Lang) to share the same Tgt, in which
%       case they will share the same background_knowledge/2 and
%       metarules/2 declarations.
%
%       Lab, Unlb are the language specification terms, or lists of
%       terms, for the labelled and unlabelled examples, respectively.
%
%       TPos, TNeg are the language specification terms, or lists of
%       terms, for the positive and negative testing examples.
%
%       Print is an atom print_examples(Bool) where Bool is either
%       "true" or "false", denoting whether the labelling of positive
%       and negative examples derived during learning will be printed,
%       or not, respectively. Printing out a large number of examples
%       may be undesirable. If so, set this to "false" in the
%       experiment query calling this predicate.
%
setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,P):-
        experiment_file:set_configs(Lang)
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiment(T,Sl,Su,TPos,TNeg
                                ,[Ps,Pos,Neg,Labelling,Program])
        ,print_results(Ps,Pos,Neg,Labelling,Program,P).


%!      print_results(+Clauses,+Pos,+Neg,+Labelling,+Progam) is det.
%
%       Pretty-print experiments results.
%
print_results(Ps,Pos,Neg,[LAcc,LTPR,LTNR],[PAcc,PTPR,PTNR],print_examples(P)):-
        auxiliaries:print_clauses('Learned Hypothesis:',Ps)
	,maplist(length,[Pos,Neg],[Pos_n,Neg_n])
        ,format('Labelled Positive: ~w~n',[Pos_n])
        ,format('Labelled Negative: ~w~n',[Neg_n])
        ,format('Labelling Acc:  ~w TPR: ~w TNR: ~w~n',[LAcc,LTPR,LTNR])
        ,format('Hypothesis Acc: ~w TPR: ~w TNR: ~w~n',[PAcc,PTPR,PTNR])
        ,(   P == true
         ->   maplist(auxiliaries:print_clauses,['Positives:','Negatives:'],[Pos,Neg])
         ;   true
         ).


%!      setup_and_run_experiment(+Lang,+N,+Tgt,+Lab,+Unlb,+Tpos,+TNeg)
%!      is det.
%
%       Configure Poker and run an experiment.
%
%       Like setup_and_run_experiment/7 but runs N experiments and
%       prints out the aggregate results.
%
%       Also unlike setup_and_run_experiment/7 this predicate does not
%       print out a hypothesis and labelling. This predicate calls
%       experiments/7 (in test_harness.pl) which does not return a
%       program and labelling.
%
%       N is the number of experiments to run.
%
%       Remaining rguments are as in setup_and_run_experiment/7 except
%       for the last argument in setup_and_run_experiment/7, missing
%       from this predicate.
%
setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg):-
        experiment_file:set_configs(Lang)
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiments(T,N,Sl,Su,TPos,TNeg,Results)
        ,print_experiments_results(Results).


%!      print_results(+Results) is det.
%
%       Pretty-print experiments results.
%
%       Like print_results/6 but prints out aggregate evaluation results
%       returned by experiments/7.
%
print_experiments_results([[LAccMs,LTPRMs,LTNRMs]
                          ,[LAccSEs,LTPRSEs,LTNRSEs]
                          ,[PAccMs,PTPRMs,PTNRMs]
                          ,[PAccSEs,PTPRSEs,PTNRSEs]
                          ]):-
        format('Labelling means:            ~w TPR: ~w TNR: ~w~n',[LAccMs,LTPRMs,LTNRMs])
        ,format('Labelling Starndard Errors: ~w TPR: ~w TNR: ~w~n'
               ,[LAccSEs,LTPRSEs,LTNRSEs])
        ,format('Hypothesis Means:           ~w TPR: ~w TNR: ~w~n',[PAccMs,PTPRMs,PTNRMs])
        ,format('Hypothesis Standard Erors:  ~w TPR: ~w TNR: ~w~n'
               ,[PAccSEs,PTPRSEs,PTNRSEs]).


%!      setup_and_run_range_experiments(+Stream,+Lang,+Tgt,+N,+Gs,+Lab,+Unl,+TPos,+TNeg)
%!      is det.
%
%       Setup Poker and run an experiment varying inputs over a range.
%
%       Stream is the output to which the results of the experiment are
%       to be written. That can be the path of an output file, or the
%       user output stream, user_output.
%
%       Arguments after Stream are passed to experiments_ranges/8.
%       Consult that predicate's documnetation for the meaning of its
%       arguments.
%
setup_and_run_range_experiments(Strm,Lang,T,N,Gs,Sl,Su,TPos,TNeg):-
        experiment_file:set_configs(Lang)
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiments_ranges(T,N,Gs,Sl,Su,TPos,TNeg,Results)
        ,print_range_experiment_results(Strm,Results).


%!      print_range_experiment_results(+Stream,+Results) is det.
%
%       Pretty-print results of experiments over a range of inputs.
%
%       Stream is the output stream.
%
%       Results are printed out as the rows of a CSV.
%
print_range_experiment_results(Stm,Res):-
        csv_write_stream(user_output,[row('Iteration'
                                          ,'Generated'
                                          ,'Labelled'
                                          ,'Unlabelled'
                                          ,'LabAccM'
                                          ,'LabTPRM'
                                          ,'LabTNRM'
                                          ,'LabAccSE'
                                          ,'LabTPRSE'
                                          ,'LabTNRSE'
                                          ,'ProgAccM'
                                          ,'ProgTPRM'
                                          ,'ProgTNRM'
                                          ,'ProgAccSE'
                                          ,'ProgTPRSE'
                                          ,'ProgTNRSE')],[])
        ,forall(member(I/G/L/U-Rs_i,Res)
              ,print_range_result(Stm,I,G,L,U,Rs_i)
              ).


%!      print_range_result(+Stream,+I,+G,+L,+U,+Results) is det.
%
%       Pretty-print one row of results of experiments over a range.
%
%       Stream is the output stream.
%
%       Results is printed out as one row of a CSV.
%
%       Subsequent arguments are the output of experiments_ranges/8. See
%       that predicate for more details.
%
print_range_result(Stm,I,G,L,U,[[LAccM,LTPRM,LTNRM]
                               ,[LAccSE,LTPRSE,LTNRSE]
                               ,[PAccM,PTPRM,PTNRM]
                               ,[PAccSE,PTPRSE,PTNRSE]
                               ]):-
        csv_write_stream(Stm
                         ,[row(I,G,L,U
                              ,LAccM,LTPRM,LTNRM
                              ,LAccSE,LTPRSE,LTNRSE
                              ,PAccM,PTPRM,PTNRM
                              ,PAccSE,PTPRSE,PTNRSE
                              )
                          ]
                         ,[]).



%!      setup_and_run_filter_experiment(+Lang,+Tgt,+Sl,+Su,+TPosL,+TNegL,+TPosU,+TNegU,+Pr)
%!      is det.
%
%       Configure Poker and run a filtering experiment.
%
%       A "filtering experiment" begins with labelled examples of a
%       target program A, and unlabelled examples of program A and B,
%       and must learn a hypothesis and labelling from both programs, by
%       filtering the unlabelled examples to separate those of program A
%       from those of program B. The filtered examples of program B are
%       used as labelled examples to learn B. The "filtering" is done by
%       Poker's labelling procedure.
%
%       Lang is the languege of the filtering experiment that must be
%       matched in the argument of one clause of set_confgis/1 and
%       safe_example/1 to setup correct configuration options.
%
%       Remaining arguments up to Pr are passed to the test_harness.pl
%       predicate experiment_filtering/9. See that predicate for a
%       detailed description of arguments.
%
%       Pr is a list: [ print_labelled(BoolP), print_unlabelled(BoolN) ]
%       where the two booleans denote whether to print the labelling
%       learned from the labelled, and unlabelled, examples,
%       respectively.
%
setup_and_run_filter_experiment(Lang,T,Sl,Su,TPosL,TNegL,TPosU,TNegU
                               ,[print_labelled(Pl)
                                ,print_unlabelled(Pu)
                                ]):-
        experiment_file:set_configs(Lang)
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiment_filtering(T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,Res_l = [PsL,PosL,NegL,LabL,ProgL]
        ,experiment_output:print_results(PsL,PosL,NegL,LabL,ProgL,print_examples(Pl))
        ,writeln('Results for unlabelled:')
        ,Res_u = [PsU,PosU,NegU,LabU,ProgU]
        ,experiment_output:print_results(PsU,PosU,NegU,LabU,ProgU,print_examples(Pu)).


%!      setup_run_filter_experiment_draw(r+Lang,+T,+Sl,+Su,+TPL,+TNgL,+TPU,+TNgU,+Os)
%!      is det.
%
%       Run a filtering experiment on L-Systems and draw the results.
%
%       Like setup_and_run_filter_experiment/9 but also draws the
%       learned L-Systems with turtle graphics for er visual
%       inspections.
%
%       Os is a list: [print_examples(Bool), draw_labelled(ParamsL),
%       draw_unlabelled(ParamsU] where Bool is a boolean denoting
%       whether to print the learned programs and evaluation results and
%       ParamsL, ParamsU are lists [T,I,Ax,RA,LA,D,St], passed to
%       test_draw/8, along with the clauses of each learned program.
%
%       In particular, if the clauses of the learned prorgam are in a
%       list Cs, test_draw/9 is called like this:
%       ==
%       test_draw(T,Cs,I,Ax,RA,LA,D,St)
%       ==
%
%       Cs is not passed in as an argument from the calling script, but
%       returned in the output of experiment_filtering/9 called by this
%       predicate.
%
setup_run_filter_experiment_draw(Lang,T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Os):-
        Os = [print_labelled(Pl)
             ,print_unlabelled(Pu)
             ,draw_labelled(DsL)
             ,draw_unlabelled(DsU)
             ]
        ,experiment_file:set_configs(Lang)
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiment_filtering(T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,Res_l = [PsL,PosL,NegL,LabL,ProgL]
        ,experiment_output:print_results(PsL,PosL,NegL,LabL,ProgL,print_examples(Pl))
        ,writeln('Results for unlabelled:')
        ,Res_u = [PsU,PosU,NegU,LabU,ProgU]
        ,experiment_output:print_results(PsU,PosU,NegU,LabU,ProgU,print_examples(Pu))
        ,draw_results(PsL,DsL)
        ,draw_results(PsU,DsU).


%!      draw_results(+Program,+Params) is det.
%
%       Draw the results of a filtering experiments.
%
%       Only for L-Systems.
%
%       Program is a program learned from a filtering experiment.
%
%       Params are the parameters passed to test_draw/8 together with
%       Program to draw the output of Program with turtle graphics.
%
draw_results(_,false):-
        !.
draw_results(Ps,[T,I,Ax,RA,LA,D,St]):-
        test_draw(T,Ps,I,Ax,RA,LA,D,St).
