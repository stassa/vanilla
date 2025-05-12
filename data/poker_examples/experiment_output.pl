:-module(experiment_output,[setup_experiment/7
                           ,setup_experiment/8
                           ,setup_experiments/7
                           ,setup_experiments/8
                           ,setup_range_experiments/11
                           ,setup_range_experiments/12
                           ,setup_filter_experiment/9
                           ,setup_filter_experiment/11
                           ,setup_filter_experiments/9
                           ,setup_filter_experiments/11
                           ,setup_filter_experiment_draw/9
                           ,setup_filter_experiment_draw/11
                           ,setup_experiment_draw/7
                           ,setup_experiment_draw/8
                           ,run_experiment_protocol/3
                           ]).

:-use_module(data(poker_examples/test_harness)).
:-use_module(data(poker_examples/l_systems)).

:- poker_configuration:experiment_file(P,_M)
  ,use_module(P).

/** <module> Output results of experiments in an experiment script.

*/

%!      setup_experiment(+Lang,+Tgt,+Lab,+Unlb,+Tpos,+TNeg,+Print)
%!      is det.
%!      setup_experiment(+Lang,+Tgt,+Lab,+Unlb,+Tpos,+TNeg,+TGen,+Print)
%!      is det.
%
%       Setup configuration and run an experiment.
%
%       Lang is an atomic identifier for one or more experiment scripts
%       in the current experiment file. Lang is used to generate
%       labelled training examples and select clauses of set_config/1
%       and setup_safe_example/1 in the experiment file.
%
%       Tgt is the predicate symbol and arity of the examples of Lang,
%       used to select clauses of background_knowledge/2 and metarules/2
%       declared in the experiment file. It is possible for multiple
%       experiments (defined by Lang) to share the same Tgt, in which
%       case they will share the same background_knowledge/2 and
%       metarules/2 declarations.
%
%       Lab, Unlb are the language specification terms, or lists of
%       terms, for the labelled and unlabelled examples, respectively.
%
%       TPos, TNeg are the language specification terms, or lists of
%       terms, for the positive and negative testing examples.
%
%       TGen is the language specification term for experiments running
%       a learned hypothesis as a generator.
%
%       This predicate calls experiment/7 and passes it all arguments.
%       If TGen is missing as in the 7-arity version, then 'nil' is
%       passed to experiment/7 in its place.
%
%       Print is an atom print_examples(Bool) where Bool is either
%       "true" or "false", denoting whether the labelling of positive
%       and negative examples derived during learning will be printed,
%       or not, respectively. Printing out a large number of examples
%       may be undesirable. If so, set this to "false" in the
%       experiment query calling this predicate.
%
setup_experiment(Lang,T,Sl,Su,TPos,TNeg,P):-
        setup_experiment(Lang,T,Sl,Su,TPos,TNeg,nil,P).

setup_experiment(Lang,T,Sl,Su,TPos,TNeg,TGen,P):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiment(T,Sl,Su,TPos,TNeg,TGen
                                ,[Ps,N,Pos,Neg,Labelling,Program,Generator])
        ,print_results(Ps,N,Pos,Neg,Labelling,Program,Generator,P).


%!      print_results(+Clauses,+Pos,+Neg,+Labelling,+Progam) is det.
%
%       Pretty-print experiments results.
%
print_results(Ps,N,Pos,Neg,[LAcc,LTPR,LTNR],[PAcc,PTPR,PTNR],GenAcc,print_examples(P)):-
        auxiliaries:print_clauses('Learned Hypothesis:',Ps)
	,maplist(length,[Pos,Neg],[Pos_n,Neg_n])
        ,format('Program Length: ~w~n',[N])
        ,format('Labelled Positive: ~w~n',[Pos_n])
        ,format('Labelled Negative: ~w~n',[Neg_n])
        ,format('Labelling Acc:  ~w TPR: ~w TNR: ~w~n',[LAcc,LTPR,LTNR])
        ,format('Hypothesis Acc: ~w TPR: ~w TNR: ~w~n',[PAcc,PTPR,PTNR])
        ,format('Generative Acc: ~w~n',[GenAcc])
        ,(   P == true
         ->   maplist(auxiliaries:print_clauses,['Positives:','Negatives:'],[Pos,Neg])
         ;   true
         ).



%!      setup_experiments(+Lang,+N,+Tgt,+Lab,+Unlb,+Tpos,+TNeg,+TGen)
%!      is det.
%!      setup_experiments(+Lang,+N,+Tgt,+Lab,+Unlb,+Tpos,+TNeg)
%!      is det.
%
%       Configure Poker and run an experiment.
%
%       Like setup_experiment/[7,8] but runs N experiments and
%       prints out the aggregate results.
%
%       Unlike setup_experiment/[7,8] this predicate does not print out
%       a hypothesis and labelling. This predicate calls experiments/7
%       (in test_harness.pl) which does not return a program and
%       labelling but only the results of evaluation thereof.
%
%       Lang is an atomic identifier for one or more experiment scripts
%       in the current experiment file. Lang is used to generate
%       labelled training examples and select clauses of set_config/1
%       and setup_safe_example/1 in the experiment file.
%
%       N is the number of experiments to run.
%
%       Tgt is the predicate symbol and arity of the examples of Lang,
%       used to select clauses of background_knowledge/2 and metarules/2
%       declared in the experiment file. It is possible for multiple
%       experiments (defined by Lang) to share the same Tgt, in which
%       case they will share the same background_knowledge/2 and
%       metarules/2 declarations.
%
%       Lab, Unlb are the language specification terms, or lists of
%       terms, for the labelled and unlabelled examples, respectively.
%
%       TPos, TNeg are the language specification terms, or lists of
%       terms, for the positive and negative testing examples.
%
%       TGen is the language specification term for experiments running
%       a learned hypothesis as a generator.
%
%       This predicate calls experiment/7 and passes it all arguments.
%       If TGen is missing as in the 7-arity version, then 'nil' is
%       passed to experiment/7 in its place.
%
setup_experiments(Lang,T,N,Sl,Su,TPos,TNeg):-
        setup_experiments(Lang,T,N,Sl,Su,TPos,TNeg,nil).

setup_experiments(Lang,T,N,Sl,Su,TPos,TNeg,TGen):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,test_harness:experiments(T,N,Sl,Su,TPos,TNeg,TGen,Results)
        ,print_experiments_results_(Results).


%!      print_experiemnts_results_(+Results) is det.
%
%       Pretty-print experiments results.
%
%       Like print_results/6 but prints out aggregate evaluation results
%       returned by experiments/7.
%
print_experiments_results_([HM
                           ,HSE
                           ,[LAccMs,LTPRMs,LTNRMs]
                           ,[LAccSEs,LTPRSEs,LTNRSEs]
                           ,[PAccMs,PTPRMs,PTNRMs]
                           ,[PAccSEs,PTPRSEs,PTNRSEs]
                           ,GenM
                           ,GenSE
                           ]):-
         format('Hypothesis mean length:     ~w~n',[HM])
        ,format('Length Standard Error:      ~w~n',[HSE])
        ,format('Labelling means:            ~w TPR: ~w TNR: ~w~n',[LAccMs,LTPRMs,LTNRMs])
        ,format('Labelling Standard Errors:  ~w TPR: ~w TNR: ~w~n'
               ,[LAccSEs,LTPRSEs,LTNRSEs])
        ,format('Hypothesis Means:           ~w TPR: ~w TNR: ~w~n',[PAccMs,PTPRMs,PTNRMs])
        ,format('Hypothesis Standard Errors: ~w TPR: ~w TNR: ~w~n'
               ,[PAccSEs,PTPRSEs,PTNRSEs])
        ,format('Generation Mean:            ~w~n',[GenM])
        ,format('Generation Standard Error:  ~w~n',[GenSE]).
% Vestigial version without generator evaluation for filtering
% experiments which currently don't evaluate generation.
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



%!      setup_range_experiments(+Strm,+L,+Wt,+Tgt,+N,+Gs,+Lab,+Ul,+TPos,+TNeg,+TGen,+Plot)
%!      is det.
%!      setup_range_experiments(+Strm,+L,+Wt,+Tgt,+N,+Gs,+Lab,+Ul,+TPos,+TNeg,+Plot)
%!      is det.
%
%       Setup Poker and run an experiment varying inputs over a range.
%
%       Stream is the output to which the results of the experiment are
%       to be written. That can be the path of an output file, or the
%       user output stream, user_output.
%
%       Plot is either the atom 'false' or a compound plot(Experiment,
%       Debug), where Experiment is an atomic identifier for the
%       experiment whose results are to be plotted, and Debug is a
%       compound @(false) or @(true) that determines whether the
%       plotting script will debug itself (if Debug is @(true)).
%
%       If Plot is 'false' then nothing is plotted. If Plot is a
%       compound plot(Experiment,Debug) the Python plotting script
%       plot_experiment_results.py is called and passed Experiment as
%       the title of the generated plot.
%
%       Arguments other than Stream and Plot are passed to
%       experiments_ranges/8. Consult that predicate's documnetation for
%       a full explanation of the arguments. A brief listing follows.
%
%       What is one of [generated,unlabelled], determining what
%       parameter will be varied during experiments.
%
%       T is a predicate indicator, Functor/Arity of a learning target.
%
%       N is the numeber of iterations per experiment set.
%
%       Gen, Lab, Unlab are language generation specification terms for
%       automatically generated, labelled and unlabelled examples,
%       respectively.
%
%       TPos, TNeg are language generation specification terms for the
%       testing positive and negative examples, respectively, used to
%       evaluate learned hypotheses as acceptors.
%
%       TGen is the specification term used to evaluate learned
%       hypotheses as generators.
%
setup_range_experiments(Strm,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,Pl):-
        setup_range_experiments(Strm,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,nil,Pl).

setup_range_experiments(Strm,Lang,What,T,N,Gs,Sl,Su,TPos,TNeg,TGen,Pl):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,experiments_ranges(What,T,N,Gs,Sl,Su,TPos,TNeg,TGen,Results)
        ,print_range_experiment_results(Strm,Results)
        ,(   Pl == false
         ->  true
         ;   Pl = plot(Exp,D)
            ,plot_range_experiment_results(Exp,Strm,What,D)
         ).


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
                               ,'LengthM'
                               ,'LengthSE'
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
                               ,'ProgTNRSE'
                               ,'GenM'
                               ,'GenSE'
                               )],[]).


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
print_range_result(Stm,I,G,L,U,[HM
                               ,HSE
                               ,[LAccM,LTPRM,LTNRM]
                               ,[LAccSE,LTPRSE,LTNRSE]
                               ,[PAccM,PTPRM,PTNRM]
                               ,[PAccSE,PTPRSE,PTNRSE]
                               ,GenM
                               ,GenSE
                               ]):-
        csv_write_stream(Stm
                         ,[row(I,G,L,U
                              ,HM
                              ,HSE
                              ,LAccM,LTPRM,LTNRM
                              ,LAccSE,LTPRSE,LTNRSE
                              ,PAccM,PTPRM,PTNRM
                              ,PAccSE,PTPRSE,PTNRSE
                              ,GenM
                              ,GenSE
                              )
                          ]
                         ,[]).


%!      plot_range_experiment_results(+Experiment,+File,+What,+Debug)
%!      is det.
%
%       Plot experiments results with Matplotplib.
%
%       Thin shell calling Python plotting module.
%
plot_range_experiment_results(Exp,Fn,W,D):-
        py_call(plot_experiment_results:plot_data(Exp,Fn,experiment_sets=W,debug=D)).



%!      setup_filter_experiment(+Lang,+Tgt,+Sl,+Su,+TPosL,+TNegL,+TPosU,+TNegU,+Pr)
%!      is det.
%!      setup_filter_experiment(+L,+Tgt,+Sl,+Su,+TPosL,+TNegL,+TGnL,+TPosU,+TNegU,+TGnU,+Pr)
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
setup_filter_experiment(Lang,T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Ps):-
        setup_filter_experiment(Lang,T,Sl,Su,TPosL,TNegL,nil,TPosU,TNegU,nil,Ps).

setup_filter_experiment(Lang,T,Sl,Su,TPosL,TNegL,TGenL,TPosU,TNegU,TGenU
                               ,[print_labelled(Pl)
                                ,print_unlabelled(Pu)
                                ]):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,experiment_filtering(T,Sl,Su,TPosL,TNegL,TGenL,TPosU,TNegU,TGenU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,Res_l = [PsL,NL,PosL,NegL,LabL,ProgL,GsL]
        ,print_results(PsL,NL,PosL,NegL,LabL,ProgL,GsL,print_examples(Pl))
        ,writeln('Results for unlabelled:')
        ,Res_u = [PsU,NU,PosU,NegU,LabU,ProgU,GsU]
        ,print_results(PsU,NU,PosU,NegU,LabU,ProgU,GsU,print_examples(Pu)).


%!      setup_filter_experiments(+Lang,+Tgt,+N,+Sl,+Su,+TPosL,+TNegL,+TPosU,+TNegU)
%!      is det.
%!      setup_filter_experiments(+Lng,+T,+N,+Sl,+Su,+TPosL,+TNegL,+GenL,+TPosU,+TNegU,+GenU)
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
setup_filter_experiments(Lang,T,N,Sl,Su,TPosL,TNegL,TPosU,TNegU):-
        setup_filter_experiments(Lang,T,N,Sl,Su,TPosL,TNegL,nil,TPosU,TNegU,nil).

setup_filter_experiments(Lang,T,N,Sl,Su,TPosL,TNegL,TGenL,TPosU,TNegU,TGenU):-
        experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,experiments_filtering(T,N,Sl,Su,TPosL,TNegL,TGenL,TPosU,TNegU,TGenU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,print_experiments_results(Res_l)
        ,writeln('Results for unlabelled:')
        ,print_experiments_results(Res_u).


%!      setup_run_filter_experiment_draw(+Lang,+T,+Sl,+Su,+TPL,+TNgL,+TPU,+TNgU,+Os)
%!      is det.
%
%!      setup_run_filter_experiment_draw(+Lang,+T,+Sl,+Su,+TPL,+TNgL,GnL,+TPU,+TNgU,GnU,+Os)
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
setup_filter_experiment_draw(Lang,T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Os):-
        setup_filter_experiment_draw(Lang,T,Sl,Su,TPosL,TNegL,nil,TPosU,TNegU,nil,Os).

setup_filter_experiment_draw(Lang,T,Sl,Su,TPosL,TNegL,TGenL,TPosU,TNegU,TGenU,Os):-
        Os = [print_labelled(Pl)
             ,print_unlabelled(Pu)
             ,draw_labelled(DsL)
             ,draw_unlabelled(DsU)
             ]
        ,experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,experiment_filtering(T,Sl,Su,TPosL,TNegL,TGenL,TPosU,TNegU,TGenU,Res_l,Res_u)
        ,writeln('Results for labelled:')
        ,Res_l = [PsL,NL,PosL,NegL,LabL,ProgL,GenL]
        ,print_results(PsL,NL,PosL,NegL,LabL,ProgL,GenL,print_examples(Pl))
        ,writeln('Results for unlabelled:')
        ,Res_u = [PsU,NU,PosU,NegU,LabU,ProgU,GenU]
        ,print_results(PsU,NU,PosU,NegU,LabU,ProgU,GenU,print_examples(Pu))
        ,draw_results(PsL,DsL)
        ,draw_results(PsU,DsU).


%!      setup_experiment_draw(+Lang,+Tgt,+Lab,+Ulab,+TPos,+TNeg,+Opts)
%!      is det.
%!      setup_experiment_draw(+Lang,+Tgt,+Lab,+Ulab,+TPos,+TNeg,+TGen,+Opts)
%!      is det.
%
%       Setup & run an L-System experiment and draw the resulting image.
%
setup_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,Os):-
        setup_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,nil,Os).

setup_experiment_draw(Lang,T,Sl,Su,TPos,TNeg,TGen,Os):-
        Os = [print_labelled(Pl)
             ,draw_labelled(DsL)
             ]
        ,experiment_file:set_configs(Lang)
        ,experiment_file:cleanup_safe_example
        ,experiment_file:setup_safe_example(Lang)
        ,experiment(T,Sl,Su,TPos,TNeg,TGen,Res)
        ,Res = [Ps,N,Pos,Neg,Lab,Prog,Gen]
        ,print_results(Ps,N,Pos,Neg,Lab,Prog,Gen,print_examples(Pl))
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
