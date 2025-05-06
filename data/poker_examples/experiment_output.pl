:-module(experiment_output,[setup_and_run_experiment/7
                           ,setup_and_run_experiments/7
                           ,setup_and_run_range_experiments/10
                           ,setup_and_run_range_experiments/11
                           ,setup_and_run_filter_experiment/9
                           ,setup_run_filter_experiment_draw/9
                           ,setup_and_run_filter_experiments/9
                           ,setup_run_experiment_draw/7
                           ,run_experiment_protocol/3
                           ]).

:-use_module(data(poker_examples/test_harness)).
:-use_module(data(poker_examples/l_systems)).

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
        ,experiment_file:cleanup_safe_example
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
        ,experiment_file:cleanup_safe_example
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
         format('Labelling means:           ~w TPR: ~w TNR: ~w~n',[LAccMs,LTPRMs,LTNRMs])
        ,format('Labelling Standard Errors: ~w TPR: ~w TNR: ~w~n'
               ,[LAccSEs,LTPRSEs,LTNRSEs])
        ,format('Hypothesis Means:           ~w TPR: ~w TNR: ~w~n',[PAccMs,PTPRMs,PTNRMs])
        ,format('Hypothesis Standard Errors: ~w TPR: ~w TNR: ~w~n'
               ,[PAccSEs,PTPRSEs,PTNRSEs]).



%!      setup_and_run_range_experiments(+Stream,+W,+Lang,+Tgt,+N,+Gs,+Lab,+Unl,+TPos,+TNeg)
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
setup_and_run_range_experiments(Strm,Lang,W,T,N,Gs,Sl,Su,TPos,TNeg):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiments_ranges(W,T,N,Gs,Sl,Su,TPos,TNeg,Results)
        ,print_range_experiment_results(Strm,Results).


%!      print_range_experiment_results(+Stream,+Results) is det.
%
%       Pretty-print results of experiments over a range of inputs.
%
%       Stream is the output stream.
%
%       Results are printed out as the rows of a CSV.
%
%       Stream can be the user output stream, user_output. If Stream is
%       not user_output, results CSV rows are echoed to user_output
%       anyway. This is so that a complete record of an experiment can
%       be kept with run_experiment_protocol/3 (which only records what
%       is printed in the user_output stream).
%
print_range_experiment_results(user_output,Res):-
        !
        ,write_csv_header(user_output)
        ,write_csv_rows(user_output,Res).
print_range_experiment_results(Stm,Res):-
        Set = open(Stm,write,S)
        ,Cal = (write_csv_header(S)
               ,forall(member(I/G/L/U-Rs_i,Res)
                      ,print_range_result(S,I,G,L,U,Rs_i)
                      )
               )
        ,Cln = close(S)
        ,setup_call_cleanup(Set,Cal,Cln)
        ,!
        % Mirror output to user_output
        ,print_range_experiment_results(user_output,Res).


%!      write_csv_header(+Stream) is det
%
%       Write out a CSV header row for a range experiment's results.
%
write_csv_header(S):-
        csv_write_stream(S,[row('Iteration'
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
                               ,'ProgTNRSE')],[]).


%!      write_csv_rows(+Stream,+Results) is det.
%
%       Write out rows of a range experiment's results CSV.
%
%       Stream is the stream where the CSV rows are to be written.
%
%       Results is a list of results returned by experiments_ranges/8.
%       See that predicate for more details.
%
write_csv_rows(S,Res):-
        forall(member(I/G/L/U-Rs_i,Res)
              ,print_range_result(S,I,G,L,U,Rs_i)
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



%!      setup_and_run_range_experiments(+Strm,+L,+Wt,+Tgt,+N,+Gs,+Lab,+Ul,+TPos,+TNeg,+Plot)
%!      is det.
%
%       Setup Poker and run an experiment varying inputs over a range.
%
%       Like setup_and_run_range_experiments/9 but can also plot
%       results.
%
setup_and_run_range_experiments(Strm,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiments_ranges(What,T,N,Gs,Sl,Su,TPos,TNeg,Results)
        ,print_range_experiment_results(Strm,Results)
        ,(   Pl == false
         ->  true
         ;   Pl = plot(Exp,D)
            ,plot_range_experiment_results(Exp,Strm,What,D)
         ).


%!      plot_range_experiment_results(+Experiment,+File,+What,+Debug)
%!      is det.
%
%       Plot experiments results with Matplotplib.
%
%       Thin shell calling Python plotting module.
%
plot_range_experiment_results(Exp,Fn,W,D):-
        py_call(plot_experiment_results:plot_data(Exp,Fn,experiment_sets=W,debug=D)).



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
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiment_filtering(T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,Res_l = [PsL,PosL,NegL,LabL,ProgL]
        ,print_results(PsL,PosL,NegL,LabL,ProgL,print_examples(Pl))
        ,writeln('Results for unlabelled:')
        ,Res_u = [PsU,PosU,NegU,LabU,ProgU]
        ,print_results(PsU,PosU,NegU,LabU,ProgU,print_examples(Pu)).



%!      setup_and_run_filter_experiments(+Lang,+Tgt,+N,+Sl,+Su,+TPosL,+TNegL,+TPosU,+TNegU)
%!      is det.
%
%       Configure Poker and run N filtering experiments.
%
%       Like setup_and_run_filter_experiment/9 but repeates experiments
%       N times.
%
%       Arguments other than N are pased to experiments_filtering/10.
%
%       Unlike setup_and_run_filter_experiment/9, this predicate only
%       prints out evaluation result means and standard errors for the
%       labelled and unlabelled examples' programs. It does not print
%       the learned hypotheses nor does it draw any L-systems like
%       setup_run_filter_experiment_draw/9.
%
setup_and_run_filter_experiments(Lang,T,N,Sl,Su,TPosL,TNegL,TPosU,TNegU):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiments_filtering(T,N,Sl,Su,TPosL,TNegL,TPosU,TNegU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,print_experiments_results(Res_l)
        ,writeln('Results for unlabelled:')
        ,print_experiments_results(Res_u).



%!      setup_run_filter_experiment_draw(+Lang,+T,+Sl,+Su,+TPL,+TNgL,+TPU,+TNgU,+Os)
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
%       test_draw(T,Cs,I,Ax,RA,LA,D,St,W,H,F)
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
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiment_filtering(T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,Res_l = [PsL,PosL,NegL,LabL,ProgL]
        ,print_results(PsL,PosL,NegL,LabL,ProgL,print_examples(Pl))
        ,writeln('Results for unlabelled:')
        ,Res_u = [PsU,PosU,NegU,LabU,ProgU]
        ,print_results(PsU,PosU,NegU,LabU,ProgU,print_examples(Pu))
        ,draw_results(PsL,DsL)
        ,draw_results(PsU,DsU).


%!      setup_run_experiment_draw(+Lang,+Tgt,+Lab,+Ulab,+TPos,+TNeg,+Opts)
%!      is det.
%
%       Setup & run an L-System experiment and draw the resulting image.
%
setup_run_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,Os):-
        Os = [print_labelled(Pl)
             ,draw_labelled(DsL)
             ]
        ,experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,experiment(T,Sl,Su,TPos,TNeg,Res)
        ,Res = [Ps,Pos,Neg,Lab,Prog]
        ,print_results(Ps,Pos,Neg,Lab,Prog,print_examples(Pl))
        ,draw_results(Ps,DsL).


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
% Default width, height and filename.
        test_draw(T,Ps,I,Ax,RA,LA,D,St,nil,nil,nil)
        ,!.
draw_results(Ps,[T,I,Ax,RA,LA,D,St,W,H]):-
% Default filename.
        test_draw(T,Ps,I,Ax,RA,LA,D,St,W,H,nil)
        ,!.
draw_results(Ps,[T,I,Ax,RA,LA,D,St,W,H,F]):-
        test_draw(T,Ps,I,Ax,RA,LA,D,St,W,H,F)
        ,!.



%!      run_experiment_protocol(+Target,+Experiment,+Protocol) is det.
%
%       Run an experiment and output a record of the generated output.
%
%       Target is the name of a learning target in the current
%       experiment file.
%
%       Experiment is an experiment script head literal. The script will
%       be called in the body of this predicate after starting a record
%       of the seession with protocol/1. When the script finishes
%       running this predicate will also list configuration options and
%       other relevant information and then stop recording the session
%       with noprotocol/0.
%
%       Protocol is the name of the file where the session output will
%       be saved.
%
run_experiment_protocol(T,G,F):-
        Set = protocol(F)
        ,C = (format('Running experiment script ~w~n:',[G])
             ,listing(G)
             ,time_stamp('Starting time: ')
             ,call(G)
             ,current_prolog_flag(stack_limit, X)
             ,format('Global stack limit ~D~n',[X])
             ,current_prolog_flag(table_space, V)
             ,format('Table space ~D~n',[V])
             ,auxiliaries:list_config
             ,poker_auxiliaries:list_poker_config
             ,listing(user:[target/1,invented/1,preterminal/1])
             ,listing(experiment_file:safe_example/1)
             ,poker_auxiliaries:list_mil_problem(T)
             ,experiment_file:background_knowledge(T,B)
             ,listing(experiment_file:B)
             ,time_stamp('End time: ')
             )
        ,Cln = noprotocol
        ,setup_call_cleanup(Set,C,Cln).


%!      time_stamp(+Message) is det.
%
%       Print a time stamp with a message.
%
%       Example:
%       ==
%       ?- experiment_output:time_stamp('Time is: ').
%       Time is: Thu, 24 Apr 2025 20:00:33
%       true.
%       ==
%
time_stamp(M):-
        get_time(S)
        ,format_time(atom(TS),'%a, %d %b %Y %T',S)
        ,format('~w~w~n',[M,TS]).
