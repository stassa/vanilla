:-module(test_harness,[experiments_filtering/10
                      ,experiments_filtering/12
                      ,experiment_filtering/9
                      ,experiment_filtering/11
                      ,experiments_ranges/10
                      ,experiments_ranges/9
                      ,range_helper/2
                      ,experiments/8
                      ,experiments/7
                      ,experiment/7
                      ,experiment/6
                      ,test_labelling/4
                      ,test_program/6
                      ,print_confusion_matrix/5
                      ,debug_confusion_matrix/6
                      ,confusion_matrix_totals/4
                      ,format_confusion_matrix/3
                      ,debug_confusion_matrix/4
                      ]).

:-use_module(lib(poker/poker)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(src(auxiliaries)).
:-use_module(lib(mathemancy/mathemancy)).
:-use_module(lib(poker/sampling/sampling)).
:-use_module(data(poker_examples/language_generation)).
:-use_module(library(clpfd),[transpose/2]).

/** <module> A test harness for Poker.

Run experiments evaluating the labelling of initial and new atoms
labelled by Poker, or the programs it learns to label them.

*/

%!      experiments_filtering(+T,+N,+Ls,+Us,+LPos,+LNeg,+UPos,+UNeg,+LRes,+URes)
%!      is det.
%!      experiments_filtering(+T,+N,+Ls,+Us,+LPos,+LNeg,+LGen,+UPos,+UNeg,+UGen,+LRes,+URes)
%!      is det.
%
%       Run N experiments separating unlabelled examples of two targets.
%
%       Like experiment_filtering/[9,11] but repeats each experiment N
%       times.
%
experiments_filtering(T,N,Sl,Su,TPosL,TNegL,TPosU,TNegU,RsLab,RsUlb):-
        experiments_filtering(T,N,Sl,Su,TPosL,TNegL,nil,TPosU,TNegU,nil,RsLab,RsUlb).

experiments_filtering(T,N,Sl,Su,TPosL,TNegL,TGnL,TPosU,TNegU,TGnU,RsLab,RsUlb):-
        findall([LRes_l,LRes_p]-[URes_l,URes_p]
               ,(between(1,N,I)
                ,debug(experiments,'Experiment ~w of ~w',[I,N])
                ,experiment_filtering(T,Sl,Su,TPosL,TNegL,TGnL,TPosU,TNegU,TGnU,Res_l,Res_u)
                ,Res_l = [_PsL,_NL,_PosL,_NegL,LRes_l,LRes_p,_LRes_g]
                ,Res_u = [_PsU,_NU,_PosU,_NegU,URes_l,URes_p,_URes_g]
                )
               ,Rs)
        ,pairs_keys_values(Rs,Rs_l,Rs_u)
        ,filtering_results('Labelled',Rs_l,RsLab)
        ,filtering_results('Unlabelled',Rs_u,RsUlb).



%!      filtering_results(+What,+Results,-Expanded) is det.
%
%       Calculate means and standard errors of filtering Results.
%
%       Helper to untangle the complicated Results list and calculate
%       means and standard errors.
%
%       What is one of: 'Labelled' or 'Unlabelled', used only for
%       debugging.
%
%       Results is a list of lists of key-value pairs L-U, where L is a
%       list of Accuracy, TPR and TNR results for a hypothesis learned
%       from a set of laballed examples and U is the same for a
%       hypothesis learned from a list of unlabelled examples.
%
%       Expanded is a list [LabellingMeans ,LabelingSEs, ProgramMeans,
%       ProgramSEs] with the means and standard errors of the
%       measurements in Results.
%
filtering_results(LU,Rs,[Ms_l,SEs_l,Ms_p,SEs_p]):-
        % I know. Simplest thing though really.
        findall(L-U
               ,member([L,U],Rs)
               ,Rs_)
        ,pairs_keys_values(Rs_,Rs_l,Rs_p)
        ,result_means(Rs_l,Ms_l)
        ,result_means(Rs_p,Ms_p)
        ,result_SEs(Rs_l,Ms_l,SEs_l)
        ,result_SEs(Rs_p,Ms_p,SEs_p)
        ,debug(experiments,'~w: Labelling means: ~w, standard errors: ~w'
              ,[LU,Ms_l,SEs_l])
        ,debug(experiments,'~w: Program means: ~w:, standard errors: ~w'
              ,[LU,Ms_p,SEs_p]).



%!      experiment_filtering(+T,+Ls,+Us,+LPos,+LNeg,+UPos,+UNeg,+LRes,+URes)
%!      is det.
%!      experiment_filtering(+T,+Ls,+Us,+LPos,+LNeg,+LGen,+UPos,+UNeg,+UGen,+LRes,+URes)
%!      is det.
%
%       Run an experiment separating unlabelled examples of two targets.
%
%       This predicate runs a "filtering" experiment: assuming labelled
%       examples of program A and unlabelled examples of progam A U B,
%       laern a program P and labelling L = {L+, L-} consistent with A
%       and learn a program P' from L-, the unlabelled examples labelled
%       negative during learning. If everything goes well then L- should
%       be the examples of program B in the set of unlabelled examples,
%       with L+ being the examples of program A in the set of
%       unlabelled examples. In other words: filter the unlabelled
%       examples for examples of A to keep those of B and learn from
%       them.
%
%       Argument names are shorthand of the following, for brevity:
%       T: Target
%       Ls: Labelled
%       Us: Unlabelled
%       LPos: TestPosL (Positive testing examples for Labelled)
%       LNeg: TestNegL (Negative testing examples for Lablled)
%       UPos: TestPosU (Negative testing examples for Unlabelled)
%       UNeg: TestNegU (Negative testing examples for Unlabelled)
%       LRes: LabelledResults
%       URes: UnlabelledResults.
%
%       The following sections use the expanded names.
%
%       Target (T) is a predicate indicator, S/A, of a learning target
%       defined in the current experiment file. T is used to collect
%       background knowledge and metarules for the experiment, but _not_
%       to generate examples. Examples are generated according to
%       Labelled and Unlabelled (Ls and Us). The programs represented in
%       both Labelled and Unlabelled must use the same backgound
%       knowledge and metarules.
%
%       Labelled and Unlabelled are language generation specification
%       terms, or lists thereof, as in experiment/6, used to generate
%       labelled and unlabelled training examples, respectively.
%
%       TestPosL, TestNegL, TestNegU and TestPosU are language
%       specification terms, or lists thereof, for the positive and
%       negative testing examples for the program learned from the
%       labelled and (filtered) unlabelled examples, respectively.
%
%       LRes and URes are the evaluation results for the labeling and
%       program learned from the Labelled and Unlabelled training
%       examples, respectively. LRes and URes are as in experiment/6.
%       Refer to that predicate for details.
%
%       LGen and UGen are language generation specification terms
%       Lang(N,Min,Max) used to evaluate the learned hypothesis as a
%       generator, not just an acceptor. Lang should be the
%       language of Labelled or Unlabelled (or the first one, if
%       Labelled is a list) for LGen and UGen, respectively. N, Min, Max
%       are the numbers, and min and max string lengths of generated
%       atoms. Each generated atom is passed to Lang to count the number
%       of atoms it accepts.
%
%       Alternatively, either of LGen, UGen can be the atom 'nil', in
%       which case generation accuracy will not be tested.
%
experiment_filtering(T,Sl,Su,TPosL,TNegL,TPosU,TNegU,Res_l,Res_u):-
        experiment_filtering(T,Sl,Su,TPosL,TNegL,nil,TPosU,TNegU,nil,Res_l,Res_u).

experiment_filtering(T,Sl,Su,TPosL,TNegL,TGenL,TPosU,TNegU,TGenU,Res_l,Res_u):-
        debug(experiment,'Learning from labelled examples.',[])
        ,experiment_data(T,_,_,BK,MS)
        ,generate_initial(Sl,T,Ls)
        ,generate_initial(Su,T,Us)
        ,test_target(Sl,Sl_T)
        ,experiment(Sl_T,T,Ls,Us,BK,MS,TPosL,TNegL,TGenL,Res_l)
        ,debug(experiment,'Learning from examples labelled negative.',[])
        ,test_target(Su,Su_)
        ,debug(experiment,'Filtering out internally generated example.',[])
        ,Res_l = [_Ps,_N,_Pos,Neg,_Rs_l,_Rs_p,_Rs_g]
        ,maplist(list_to_ord_set,[Us,Neg],[Us_s,Neg_s])
        ,ord_intersect(Us_s,Neg_s,Ss)
        ,debug_length(experiment_filtered,'Left with ~w negative examples.',Ss)
        ,debug_clauses_length(experiment_filtered_full,'Left with ~w negative examples:',Ss)
        ,experiment(Su_,T,Ss,[],BK,MS,TPosU,TNegU,TGenU,Res_u).



%!      experiments_ranges(+Target,+What,+N,+Gen,+Lab,+Unlab,+TPos,+TNeg,-Results)
%!      is det.
%!      experiments_ranges(+What,+Tgt,+N,+Gen,+Lab,+Unlab,+TPos,+TNeg,+TGen,-Results)
%!      is det.
%
%       Run N experiments with example numbers increasing over a range.
%
%       This predicate runs a set of experiments determined by What,
%       each with K iteration determined by Init, and repeating N times.
%
%       What is one of [generated, unlabelled], that determines whether
%       the experiment is based on the variation of automatically
%       generated or given unlabelled examples.
%
%       If What is 'generated', the experiment is executed as follows:
%
%       For each setting G in |Gen| experiment sets
%           Set unlabelled_examples(G)
%           For each iteration I in |Lab| iterations
%                   expand Lab, Unlab -> Li, Ui
%                   Call: experiments(Target,N,Li,Ui,TPos,TNeg,TGen,Res)
%
%       If What is 'unlabelled', the experiment is executed as follows:
%
%       For each language spec term U in |Unlab| experiment sets
%           Expand U generating unlabelled exampls Ui
%           For each iteration I in |Lab| iterations
%                   expand Lab, Gen -> Li, G
%                   Set unlabelled_examples(G)
%                   Call: experiments(Target,N,Li,Ui,TPos,TNeg,TGen,Res)
%
%       Arguments are as follows.
%
%       Target is a predicate indicator, S/A, of a learning target
%       defined in the current experiment file. T is used to collect
%       background knowledge and metarules for the experiment, but _not_
%       to generate examples. Examples are generated according to
%       Labelled and Unlabelled.
%
%       N is the number of experiments to run.
%
%       Lab is a language generartion specification term S(M,J,K), or a
%       list thereof, denoting the languages of the labelled examples
%       and the quantity (M, a number or "all") and minimum (J) and
%       maximum (K) length of strings of S in those examples. S must be
%       defined in test_harness as a DCG.
%
%       Unlab is as in Labelled, a term or list of terms S(M,J,K), used
%       to generate unlabelled examples. If Su is a list of terms, the
%       unlabelled examples used for training are a mix of atoms of all
%       the languages in the list.
%
%       TestPos and TestNeg are language generation specification terms,
%       or lists thereof, as in Labelled and Unlabelled where, used to
%       generate positive and negative testing examples, respectively.
%
%       TGen is a language generation specification term Lang(N,Min,Max)
%       used to evaluate the learned hypothesis as a generator, not
%       just an acceptor. Lang should be the language of Lab (or the
%       first one, if Lab is a list) and N, Min, Max are the numbers,
%       and min and max string lengths of generated atoms. Each
%       generated atom is passed to Lang to count the number of atoms it
%       accepts.
%
%       Alternatively, TGen can be the atom 'nil', in which case
%       generation accuracy will not be tested.
%
%       Results is a list of lists of terms I/G/L/U-Rs, where I is the
%       iteration, G is the number of internally generated examples, L
%       and U are the numbers of labelled and unlabelled examples
%       respectively and Rs is a list-of-lists of results lists as
%       returned by experiments/[7,8]. See that predicate for details of
%       the composition of Results.
%
%       experiment_ranges/[9,10] queries can get pretty complicated. See
%       experiment scripts in data/poker_experiments for examples of
%       such queries.
%
%       @tbd: this predicate really needs some error checking to make
%       sure we're not giving as arguments totally bogus ranges, or ones
%       that will raise an error.
%
experiments_ranges(W,T,N,Gs,Ls,Us,TestPos,TestNeg,Rs):-
        experiments_ranges(W,T,N,Gs,Ls,Us,TestPos,TestNeg,nil,Rs).

experiments_ranges(generated,T,N,Gs,Ls,Us,TestPos,TestNeg,TestGen,Rs):-
        !
        ,range_interval(Gs,Gs_e)
        ,maplist(expanded_range,[Ls,Us],[Ls_e,Us_e])
        ,maplist(length,[Gs_e,Ls_e],[Gn,Ln])
        ,Set =  poker_configuration:unlabelled_examples(C)
        ,Call = findall(I/G/L/U-Rs_i
                       ,(nth1(Gi,Gs_e,G)
                        ,set_poker_configuration_option(unlabelled_examples,[G])
                        ,debug(experiments,'Experiment set ~w of ~w.',[Gi,Gn])
                        % Iteration: incrments of initial examples
                        ,debug(experiments,'Iterations per set: ~w',[Ln])
                        % Experiments: repetitions over each iteration
                        ,nth1_labelled_unlabelled(I,Ls_e,Us_e,Ls_i,Us_i)
                        ,debug(experiments,'Iteration ~w of ~w with:',[I,Ln])
                        ,debug(experiments,'Generated negative examples: ~w',[G])
                        ,maplist(spec_value,[Ls_i,Us_i],[L,U])
                        ,debug(experiments,'Labelled examples: ~w',[L])
                        ,debug(experiments,'Unlabelled examples: ~w',[U])
                        ,experiments(T,N,Ls_i,Us_i,TestPos,TestNeg,TestGen,Rs_i)
                        )
                       ,Rs)
        ,Clean = set_poker_configuration_option(unlabelled_examples,[C])
        ,setup_call_cleanup(Set,Call,Clean).
experiments_ranges(unlabelled,T,N,Gs,Ls,Us,TestPos,TestNeg,TestGen,Rs):-
        range_interval(Gs,Gs_e)
        ,maplist(expanded_range,[Ls,Us],[Ls_e,Us_e])
        ,maplist(length,[Gs_e,Ls_e,Us_e],[_Gn,Ln,Un])
        ,Set =  poker_configuration:unlabelled_examples(C)
        ,Call = findall(I/Gi/L/U-Rs_i
                       ,(nth1(Ui,Us_e,Us_i)
                        ,debug(experiments,'Experiment set ~w of ~w.',[Ui,Un])
                        % Iteration: incrments of initial examples
                        ,debug(experiments,'Iterations per set: ~w',[Ln])
                        % Experiments: repetitions over each iteration
                        ,nth1_labelled_unlabelled(I,Ls_e,Gs_e,Ls_i,Gi)
                        ,set_poker_configuration_option(unlabelled_examples,[Gi])
                        ,debug(experiments,'Iteration ~w of ~w with:',[I,Ln])
                        ,debug(experiments,'Generated negative examples: ~w',[Gi])
                        ,maplist(spec_value,[Ls_i,Us_i],[L,U])
                        ,debug(experiments,'Labelled examples: ~w',[L])
                        ,debug(experiments,'Unlabelled examples: ~w',[U])
                        ,experiments(T,N,Ls_i,Us_i,TestPos,TestNeg,TestGen,Rs_i)
                        )
                       ,Rs)
        ,Clean = set_poker_configuration_option(unlabelled_examples,[C])
        ,setup_call_cleanup(Set,Call,Clean).


%!      nth1_labelled_unlabelled(?I,?Labelled,?Unlabelled,?IthLab,?IthUnlab)
%!      is nondet.
%
%       Iterate over sets of Labelled and Unlabelled examples.
%
%       Wrapper around nth1/3 to allow for an empty list of Unlabelled
%       examples. experiment_ranges/8 allows experiments with 0
%       unlabelled examples but when Labelled is non-empty and
%       Unlabelled is empty we can't iterate over both in tandem with
%       nth1/3. This predicate ensures correct iteration over both
%       lists by binding the empty list to IthUnlab every time IthLab
%       is bound to an element in labelled, if Unlabelled is empty;
%       otherwise this iterates over the elements in Labelled and
%       Unlabelled in tandem.
%
nth1_labelled_unlabelled(I,Ls,[],Ls_i,[]):-
        !
        ,nth1(I,Ls,Ls_i).
nth1_labelled_unlabelled(I,Ls,Us,Ls_i,Us_i):-
        maplist(nth1(I),[Ls,Us],[Ls_i,Us_i]).


%!      spec_value(+Specification,-Value) is det.
%
%       Unpack a quantity value from a Specification term.
%
%       Specification is a language generation specification term, or a
%       list thereof.
%
%       Value is the first argument of the term in Specification, or the
%       sume thereof, denoting a quantity of examples to be generated.
%
%       Helper to simplify unpacking experiment set, iteration, examples
%       numbers etc values for outputting at the end of a set
%       ofexperiments with experiments_ranges/8.
%
spec_value([],0):-
        !.
spec_value(S,V):-
        \+ is_list(S)
        ,!
        ,spec_value([S],V).
spec_value(Ss,V):-
        findall(Vi
               ,(member(S,Ss)
                ,S =.. [_,Vi|_]
                )
               ,Vs)
        ,sumlist(Vs,V).


%!      expanded_range(+Spec,-Expanded) is det.
%
%       Expand a group of range definitions to language terms.
%
%       Helper for to expand range specifications for ranged
%       experiments to language terms S(M,J,K) as expected by
%       experiments/7.
%
%       Spec is a single range specification term of the form
%       Language(Init,Min,Max), or a list of such terms. Language, Init,
%       Min and Max are as follows.
%
%       Language is the symbol, but not arity, of a target language
%       defined as a DCG in this file.
%
%       Init, Min and Max are ranges of the form I:J/K, where I, J are
%       integers that define a closed interval [I,J] and K is the stride
%       by which to increment I until it reaches J.
%
%       Additionally, Min and Max (but not Init) can be singe integers
%       in whihh case they are interpredted as the range I:I/0.
%
%       Init is the range of numbers of initial, labelled or unlabelled,
%       training examples or positive or negative testing examples of
%       Language that are to be genereated for an experiment.
%
%       Min and Max are the ranges of the lengths of strings of Language
%       in the initial examples of Language generated according to Init.
%
%       Expanded is a list of language generation specification terms of
%       the form Language(N,Min,Max) where N is in the range I0:J0
%       increasing by K0, and Min and Max are in the ranges I1:J1
%       increasing by K1 and I2:J2 increasing by K2. Each term in
%       Expanded can be passed to experiments/7 to iterate over a
%       language specification for an iterated experiment.
%
%       Example query:
%       ==
%       % Min and Max can both be single integers:
%
%       ?- test_harness:expanded_range(anbn(1:5/1,0,3),_Es), maplist(writeln,_Es).
%       anbn(1,0,3)
%       anbn(2,0,3)
%       anbn(3,0,3)
%       anbn(4,0,3)
%       anbn(5,0,3)
%       true.
%
%       % Min and Max can both be ranges:
%       ?- test_harness:expanded_range(anbn(1:5/1,0:0/5,4:4/5),_Es), maplist(writeln,_Es).
%       anbn(1,0,4)
%       anbn(2,0,4)
%       anbn(3,0,4)
%       anbn(4,0,4)
%       anbn(5,0,4)
%       true.
%
%       % Either of Min or Max can be a single integer or a range:
%       ?- test_harness:expanded_range(anbn(1:5/1,0,4:4/5),_Es), maplist(writeln,_Es).
%       anbn(1,0,4)
%       anbn(2,0,4)
%       anbn(3,0,4)
%       anbn(4,0,4)
%       anbn(5,0,4)
%       true.
%
%       ?- test_harness:expanded_range(anbn(1:5/1,0:0/5,4),_Es), maplist(writeln,_Es).
%       anbn(1,0,4)
%       anbn(2,0,4)
%       anbn(3,0,4)
%       anbn(4,0,4)
%       anbn(5,0,4)
%       true.
%       ==
%
%       If Spec is a list of range specification terms Expanded is a
%       list-of-lists, where each sub-list is of length equal to the
%       list of Spec and contains language specification terms that are
%       passed together to experiments/7 and combined to generate data.
%
%       This is really best explained with an example:
%       ==
%       ?- _Spec = [koch_curve(1:41/10,0,5),koch_curve_with_vars(1:41/10,8,10)]
%       ,test_harness:expanded_range(_Spec,_Rs)
%       ,maplist(writeln,_Rs).
%
%       [koch_curve(1,0,5),koch_curve_with_vars(1,8,10)]
%       [koch_curve(11,0,5),koch_curve_with_vars(11,8,10)]
%       [koch_curve(21,0,5),koch_curve_with_vars(21,8,10)]
%       [koch_curve(31,0,5),koch_curve_with_vars(31,8,10)]
%       [koch_curve(41,0,5),koch_curve_with_vars(41,8,10)]
%       true.
%       ==
%
%       When specifying Min and Max as ranges, the predicate
%       range_helper/2 can be used to calculate the number of
%       language specification terms that will be generated for the
%       range in Init, so that the right upper limit can be set:
%       ==
%       ?- test_harness:(_Init = 1:5/1
%       , range_helper(_Init,N)
%       , expanded_range(anbn(_Init,0:0/N,4:4/N),_Es)
%       , maplist(writeln,_Es)
%       )
%       , length(_Es,M).
%
%       anbn(1,0,4)
%       anbn(2,0,4)
%       anbn(3,0,4)
%       anbn(4,0,4)
%       anbn(5,0,4)
%       N = M, M = 5.
%       ==
%
%       However this is not absolutely necessary and is only given for
%       more clear debugging of experiment options, as the number of
%       language specification terms in Expanded is determined by the
%       range in Init anyway.
%
%       @tbd If Spec is a list and the number of sub-lists of terms in
%       the expanded sub-lists, as in the example above is not equal
%       then errors will be raised, and they'll be quite cryptic too -
%       they come from the division in the third clause of
%       expanded_range/2 that expects the number of spec sub-lists to be
%       equally divisible to the number of sub-lists of expanded terms.
%       so range_helper/2 is actually quite useful here. It might be
%       useful to automatically truncate a list of expanded terms or
%       otherwise warn the user more specifically about the problem.
%
expanded_range([],[]):-
% Is there any point trying to expand an empty range?
% Answer: neupe.
        !.
expanded_range(S,Es):-
        \+ is_list(S)
        ,!
        ,expanded_range([S],Es).
expanded_range(Ss,Es):-
        expanded_range(Ss,Es_,[])
        ,flatten(Es_,Es_f)
        ,maplist(length,[Es_f,Ss],[N,M])
        ,M_ is N / M
        ,every_nth1(N,M_,Es_f,Es).


%!      expanded_range(+Spec,+Acc,-Range) is det.
%
%       Expand a list of range definitions to a list of language terms.
%
%       Business end of expanded_range/2.
%
expanded_range([],Es,Es):-
        !.
expanded_range([Spec|Ss],[Es|Acc],Bind):-
        Spec =.. [L,RIs,RMin,RMax]
        ,range_interval(RIs,Is)
        ,string_lengths(RIs,RMin,RMax,Mins,Maxs)
        ,findall(T
                ,(maplist(nth1(_I),[Is,Mins,Maxs],[In_i,Min_i,Max_i])
                 ,T =.. [L,In_i,Min_i,Max_i]
                 )
                ,Es)
        ,expanded_range(Ss,Acc,Bind).


%!      range_interval(+Range,-Interval) is det.
%
%       Expand a Range specification to a list of numbers.
%
%       Helper to allow ranges with the same minimum and maximum value,
%       which still have to be expanded to the same length as other
%       ranges.
%
range_interval(I:I/K,Ss):-
        !
        ,length(Ss,K)
        ,findall(I
                ,member(I,Ss)
                ,Ss).
range_interval(I:J/K,Ss):-
        interval(I,J,K,Ss).


%!      every_nth1(+M,+N,+Es,-Ns) is det.
%
%       Select every M'th of N elements in Es.
%
%       Example:
%       ==
%       ?- N = 2, _Ls = [a,b,c,d,e,f], length(_Ls,M), test_harness:every_nth1(M,N,_Ls,Ns).
%       N = 2,
%       M = 6,
%       Ns = [[a,c,e],[b,d,f]].
%
%       ?- N = 3, _Ls = [a,b,c,d,e,f], length(_Ls,M), test_harness:every_nth1(M,N,_Ls,Ns).
%       N = 3,
%       M = 6,
%       Ns = [[a,d],[b,e],[c,f]].
%       ==
%
%       @tbd This is probably veeery inefficient but it also probably
%       doesn't matter. In case it does, the efficient implementation
%       treates Es as an N x M array in row-major order and iterates
%       over it with the usual formula for indexing over row-marjor
%       order arrays. I just can't be arsed right now.
%
%       @tbd Also I don't like the order of arguments. N should be
%       first. It's first in the code but only as the name of a
%       variable. That's confusing. Correct this.
%
every_nth1(N,M,Es,Ns):-
        findall(Ns_
               ,(between(1,M,I)
                ,interval(I,N,M,Is)
                ,findall(Es_j
                        ,(member(J,Is)
                         ,nth1(J,Es,Es_j)
                         )
                        ,Ns_)
                )
               ,Ns).


%!      string_lengths(+Init,+Min,+Max,-Mins,-Maxs) is det.
%
%       Expanded Min and Max ranges of string lengths.
%
%       Helper to allow ranges of string lenghts in expanded_range/2 to
%       be given as any mix of one or two integers or ranges.
%
string_lengths(_R,MinI:MinJ/MinK,MaxI:MaxJ/MaxK,Ms,Ns):-
        !
        ,maplist(range_interval,[MinI:MinJ/MinK,MaxI:MaxJ/MaxK],[Ms,Ns]).
string_lengths(R,Min,I:J/K,Ms,Ns):-
        number(Min)
        ,!
        ,range_helper(R,N)
        ,maplist(range_interval,[Min:Min/N,I:J/K],[Ms,Ns]).
string_lengths(R,I:J/K,Max,Ms,Ns):-
        number(Max)
        ,!
        ,range_helper(R,N)
        ,maplist(range_interval,[I:J/K,Max:Max/N],[Ms,Ns]).
string_lengths(R,Min,Max,Ms,Ns):-
        number(Min)
        ,number(Max)
        ,range_helper(R,N)
        ,maplist(range_interval,[Min:Min/N,Max:Max/N],[Ms,Ns]).


%!      range_helper(+Range,-Max) is det.
%
%       Calculate the Max value in a Range.
%
%       Helper to faciliate setting the values of intervals in the input
%       of experiments like experiments_low_uncertainty/7.
%
%       TODO: give example.
%
range_helper(I:I/K,K):-
        !.
range_helper(I:J/K,N):-
        M is J - I
        ,N is M / K + 1.



%!      experiments(+Target,+N,+Labelled,+Unlabelled,+TestPos,+TestNeg,-Results)
%!      is det.
%!      experiments(+Trgt,+N,+Labelled,+Unlabelled,+TestPos,+TestNeg,+TestGen,-Results)
%!      is det.
%
%       Run N experiments with both Labelled and Unlabelled examples.
%
%       As experiments/7 but also measures the average and standard
%       error of a) the length of learned hypotheses and b) the accuracy
%       of the learned hypotheses as generators, compared to the true
%       definition of a Target.
%
%       Trgt is a predicate indicator, S/A, of a learning target defined
%       in the current experiment file. T is used to collect background
%       knowledge and metarules for the experiment, but _not_ to
%       generate examples. Examples are generated according to Labelled
%       and Unlabelled.
%
%       N is the number of experiments to run.
%
%       Labelled is a language generartion specification term S(M,J,K),
%       denoting the language of the labelled examples and the quantity
%       (M, a number or "all") and minimum (J) and maximum (K) length of
%       strings of S in those examples. S must be defined in
%       test_harness as a DCG.
%
%       Su is as in Labelled, or a list of terms S(M,J,K), used to
%       generate unlabelled examples. If Su is a list of terms, the
%       unlabelled examples used for training are a mix of atoms of all
%       the languages in the list.
%
%       TestPos and TestNeg are language generation specification terms,
%       or lists thereof, as in Labelled and Unlabelled where, used to
%       generate positive and negative testing examples, respectively.
%
%       TestGen is a language generation specification term Lang(N,Min,Max)
%       used to evaluate the learned hypothesis as a generator, not
%       just an acceptor. Lang should be the language of Lab (or the
%       first one, if Lab is a list) and N, Min, Max are the numbers,
%       and min and max string lengths of generated atoms. Each
%       generated atom is passed to Lang to count the number of atoms it
%       accepts.
%
%       Alternatively, TestGen can be the atom 'nil', in which case
%       generation accuracy will not be tested.
%
%       Results is a list [Ps,N,Pos,Neg,Rs_L,Rs_R,Rs_G], where:
%
%       * Ps is the learned hypothesis
%       * N is the length of Ps
%       * Pos is the list of positive examples identified
%       * Neg is the list of negative examples identified
%       * Rs_L is the list of labelling results
%       * Rs_P is the list of program results.
%       * Rs_G is the accuracy of Ps executed as a geneator.
%
%       Each of Rs_L and Rs_R [Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC] with
%       elements as follows:
%
%       Acc: Accuracy
%       Err: Error
%       TPR: True Positive Rate
%       TNR: True Negative Rate
%       FPR: False Positive Rate
%       FNR: Fale Negative Rate
%       PRE: Precision
%       REC: Recall (TPR)
%       FSC: F1 Score
%
%       Acc, Err, etc are floating point numbers.
%
experiments(T,N,Sl,Su,TPos,TNeg,Res):-
        experiments(T,N,Sl,Su,TPos,TNeg,nil,Res).

experiments(T,N,Sl,Su,TPos,TNeg,TestGen,[Ms_h,SE_h,Ms_l,SEs_l,Ms_p,SEs_p,Ms_g,SE_g]):-
        findall([H,Res_l,Res_p,Res_g]
               ,(between(1,N,I)
                ,debug(experiments,'Experiment ~w of ~w',[I,N])
                ,experiment(T,Sl,Su,TPos,TNeg,TestGen
                           ,[_Ps,H,_Pos,_Neg,_Counts_l-Res_l,_Counts_p-Res_p,Res_g])
                )
               ,Rs)
        ,results_parts(Rs,Rs_h,Rs_l,Rs_p,Rs_g)
        ,result_means(Rs_l,Ms_l)
        ,result_means(Rs_p,Ms_p)
        ,result_SEs(Rs_l,Ms_l,SEs_l)
        ,result_SEs(Rs_p,Ms_p,SEs_p)
        ,maplist(average,[Rs_h,Rs_g],[Ms_h_,Ms_g_])
        ,maplist(standard_deviation,[Rs_h,Rs_g],[Ms_h_,Ms_g_],SDs)
        ,maplist(standard_error,[Rs_h,Rs_g],SDs,[SE_h_,SE_g_])
        ,maplist(atomize,[Ms_h_,Ms_g_,SE_h_,SE_g_],[Ms_h,Ms_g,SE_h,SE_g])
        ,debug(experiments,'Labelling means: ~w~n% Standard Errors: ~w',[Ms_l,SEs_l])
        ,debug(experiments,'Program means:   ~w~n% Standard Errors: ~w',[Ms_p,SEs_p])
        ,debug(experiments,'Program length mean: ~w, Standard Error: ~w',[Ms_h,SE_h])
        ,debug(experiments,'Generation mean:     ~w, Standard Error: ~w',[Ms_g,SE_g]).


%!      results_parts(+Res,-Length,-Labelling,-Proggram,-Generated) is
%!      det.
%
%       Split a list of lists of Results to its component parts.
%
results_parts(Rs,Hs,Rs_l,Rs_p,Rs_g):-
        results_parts(Rs,Hs,[],Rs_l,[],Rs_p,[],Rs_g,[]).

%!      results_parts(+Rs,-Hs,+HsAcc,-RsL,+RsLAcc,-RsP,+RsPAcc,-RsG,+RsGAcc)
%!      is det.
%
%       Business end of results_parts/5.
%
results_parts([],Hs,Hs,Rs_l,Rs_l,Rs_p,Rs_p,Rs_g,Rs_g):-
        !.
results_parts([[H,L,P,G]|Rs],[H|AccHs],Hs,[L|AccL],Rs_l,[P|AccP],Rs_p,[G|AccG],Rs_g):-
        results_parts(Rs,AccHs,Hs,AccL,Rs_l,AccP,Rs_p,AccG,Rs_g).


%!      result_SEs(+Results,+Means,-StandardErrors) is det.
%
%       Calculate the standard error of metrics in Results.
%
%       Results is a list of lists [Acc,TPR,TNR,FPR,FNR,PRE,REC,FSC],
%       of numbers, the labelling or hypothesis evaluation metrics
%       calculated for each of a number of experiments, in
%       experiments/6.
%
%       Means is a list of numbers, the means of each evaluation result
%       in the list Results.
%
%       StandardErrors is a list of numbers, the standard error of each
%       sublist in Results, calculated with respect to Means.
%
result_SEs(Rs,Ms,SEs):-
        % Gulp.
        transpose(Rs,Rs_T)
        ,maplist(standard_deviation,Rs_T,Ms,SDs)
        ,maplist(standard_error,Rs_T,SDs,SEs_)
        ,maplist(atomize,SEs_,SEs).



%!      result_means(+Results,-Means) is det.
%
%       Calculate the means of a list of metrics in Results.
%
%       Results is a list of lists [Acc,TPR,TNR,FPR,FNR,PRE,REC,FSC],
%       of labelling or learned program evaluation results calculated of
%       each of a number of experiments, in experiments/6.
%
%       Means is a list of numbers, the means of each evaluation result
%       in the list Results.
%
result_means(Res,Ms):-
        %writeln(Res)
        % 9 metrics in Res.
        findall(0
               ,between(1,9,_)
               ,Zs)
        ,length(Res,L)
        ,result_means(Res,Zs,Ss)
        ,maplist(mean(L),Ss,Ms_)
        ,maplist(atomize,Ms_,Ms).

%!      result_means(+Results,+Acc,-Means) is det.
%
%       Business end of result_means/2.
%
%       Results is a list of lists [Acc,TPR,TNR,FPR,FNR,PRE,REC,FSC],
%       received from result_means/2.
%
%       Acc is the accumulator of means, initially a list of 0's of
%       length 9 (one for each metric in Results).
%
%       Means is the list in Acc with means of the three metrics updated
%       according to the contents of Results.
%
result_means([],Ms,Ms):-
        !.
% Is there no better way to pass around an ordered list like this one?
result_means([[Acc1,Err1,TPR1,TNR1,FPR1,FNR1,PRE1,REC1,FSC1]|Rs]
            ,[Acc0,Err0,TPR0,TNR0,FPR0,FNR0,PRE0,REC0,FSC0]
            ,Ms):-
        maplist(sum
               ,[Acc0,Err0,TPR0,TNR0,FPR0,FNR0,PRE0,REC0,FSC0]
               ,[Acc1,Err1,TPR1,TNR1,FPR1,FNR1,PRE1,REC1,FSC1]
               ,[Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC])
        ,result_means(Rs,[Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC],Ms).


%!      mean(+Length,+Sum,-Mean) is det.
%
%       Calculate the mean of a Sum of a Length number of values.
%
%       Helper predicate passed to maplist/2.
%
mean(N,X,M):-
        M is X / N.


%!      sum(+X,+Y,-Z) is det.
%
%       Z is the sum of X and Y.
%
%       Helper predicate passed to maplist/2.
%
sum(A,B,C):-
        C is A + B.



%!      experiment(+Trgt,+Labelled,+Unlabelled,+TestPos,+TestNeg,-Results)
%!      is det.
%!      experiment(+Trgt,+Labelled,+Unlabelled,+TestPos,+TestNeg,TestGen,-Results)
%!      is det.
%
%       Run an experiment with both Labelled and Unlabelled examples.
%
%       Trgt is a predicate indicator, S/A, of a learning target defined
%       in the current experiment file. Trgt is used to collect
%       background knowledge and metarules for the experiment, but _not_
%       to generate examples. Examples are generated according to
%       Labelled and Unlabelled.
%
%       Labelled is a language generation specification term
%       Sl(Nl,Jl,Kl), or list thereof, where Sl is the symbol of a
%       grammar defined in test_harness and used to a) generate labelled
%       examples and b) evaluate the labelling and c) the program
%       learned by Poker from those examples, and Nl, Jl, and Kl, are
%       the number (or atom "all" for ... all) of strings of the
%       language Sl to generate as Definite Clause Grammars atoms, and
%       minimum and maximum length of strings in those atoms.
%
%       Unlabelled is a languge generation specification term
%       Su(Nu,Ju,Ku), or list thereof, with the same meaning as
%       Labelled, except that Su is the symbol of a grammar used to
%       generate unlabelled examples. Unlabelled can alterantively be a
%       list of terms Su(Nu,Ju,Ku), in which case the generated
%       unlabelled examples are a combination of atoms from all the
%       listed languages and with the corresponding numbers.
%
%       TestPos and TestNeg are language generation specification terms,
%       or lists thereof, as in Labelled and Unlabelled where, used to
%       generate positive and negative testing examples, respectively.
%
%       TestGen is a language specification term Lang(I,J,K) used to
%       test the learned program as a generator. First, I atoms of
%       Target with strings of length between J and K are generated by
%       executing the learned hypothesis as a genereator. Then the
%       generated atoms are passed to Lang. The number of those atoms
%       that are accepted by Lang is added to Results.
%
%       Alternatively, TestGen can be the atom 'nil', in which case
%       generation accuracy will not be tested.
%
%       Generation of labelled and unlabelled training examples, and
%       positive and negative testing examples is handled by
%       generate_initial/3 and Labelled and Unlabelled are passed to
%       that predicate. Refer to that predicate's comments for more
%       details on generation. Note that while generate_initial/3
%       accepts a list in the first argument, i.e. the argument passed
%       from Labelled, experiment/4 doesn't currently allow Labelled to
%       be a list, i.e. it is not currently possible to compose two
%       grammars to create a set of labelled examples.
%
%       Results is a list [Ps,N,Pos,Neg,Rs_L,Rs_R,Rs_G], where:
%
%       * Ps is the learned hypothesis
%       * N is the length of Ps
%       * Pos is the list of positive examples identified
%       * Neg is the list of negative examples identified
%       * Rs_L is the list of labelling results
%       * Rs_R is the list of program results.
%       * Rs_G is the accuracy of Ps executed as a geneator.
%
%       Each of Rs_L and Rs_R is a pair Counts-Results, where:
%
%       Counts is a list: [TP,TN,FP,FN], with elements as follows:
%       TP: number of true positives.
%       TN: number of true negatives.
%       FP: number of false positives.
%       FN: number of false negatives.
%
%       Results is a list [Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC] with
%       elements as follows:
%       Acc: Accuracy
%       Err: Error
%       TPR: True Positive Rate
%       TNR: True Negative Rate
%       FPR: False Positive Rate
%       FNR: Fale Negative Rate
%       PRE: Precision
%       REC: Recall (TPR)
%       FSC: F1 Score
%
%       Results in Rs_L are the results of evaluation of Poker's
%       labelling and results in Ps_R are the results of evaluation of
%       the program learned by Poker. See test_labelling/4 and
%       test_program/6 for details of these evaluations.
%
experiment(T,Sl,Su,TPos,TNeg,Res):-
        experiment(T,Sl,Su,TPos,TNeg,nil,Res).

experiment(T,Sl,Su,TPos,TNeg,TGen,Res):-
        debug(experiment,'Generating labelled examples...',[])
        ,generate_initial(Sl,T,Ls)
        ,debug(experiment,'Generating unlabelled examples...',[])
        ,generate_initial(Su,T,Us)
        ,experiment_data(T,_,_,BK,MS)
        ,test_target(Sl,Sl_)
        ,experiment(Sl_,T,Ls,Us,BK,MS,TPos,TNeg,TGen,Res).


%!      experiment(+Tgt,+Sym,+Lab,+Unlab,+BK,+MS,+TestPos,+TestNeg,TestGen,-Results)
%!      is det.
%
%       Business end of experiment/6
%
%       Learns from examples of Lab and Unlab, expanded from language
%       generation specification terms passed to its parent
%       (experiment/7).
%
%       Tgt is the symbol, but not arity, of a target theory, used to
%       evaluate the labelling and program learned from Lab and Unlab.
%
%       Sym is a predicate indicator, Functor/Arity of the program
%       learned from Lab and Unlab. Sym is also the predicate indicator
%       of test examples in TestPos and TestNeg, and in examples
%       generated according to Lab and Unlab. Sym should correspond to a
%       learning target defined in the current experiment file (i.e. the
%       first argument of experiment file interface predicates like
%       background_knowledge/2, metarules/2, labelled_examples/2 etc).
%
%       Lab and Unlab are sets of ground atoms, labelled examples of
%       Tgt and unlabelled examples of unknown programs, respectively.
%
%       BK and MS are the predicate indicators of predicates in the
%       background knowledge given for Tgt, and MS are the atomic IDs of
%       metarules.
%
%       TestPos and TestNeg are sets of ground atoms, used as positive
%       and negative testing examples of the program learned from Lab
%       and Unlab. Note that TestPos and TestNeg are not used to test
%       the learned _labelling_, only the program. The learned labelling
%       is evaluated according to the known definition of Tgt in this
%       file.
%
%       TestGen is a language generation specification term
%       Lang(N,Min,Max) used to evaluate the learned hypothesis as a
%       generator, not just an acceptor. Lang should be the language of
%       Lab (or the first one, if Lab is a list) and N, Min, Max are the
%       numbers, and min and max string lengths of generated atoms. Each
%       generated atom is passed to Lang to count the number of atoms it
%       accepts.
%
%       Alternatively, TestGen can be the atom 'nil', in which case
%       generation accuracy will not be tested.
%
%       Results is the list of evaluation results for the labelling and
%       program learned from Lab and Unlab. Refer to the parent
%       predicate, experiment/7, for a full description.
%
%       The motivation to have this as a separate predicate is that we
%       reuse it e.g. in experiment_filtering/9 where it is not
%       convenient to pass around language generation specification
%       terms and it's more convenient to pass sets of examples.
%
experiment(Sl,S,Ls,Us,BK,MS,TPos,TNeg,TGen,[Ps,N,Pos,Neg,Rs_l,Rs_p,Rs_g]):-
        debug_length(experiment_initial,'Got ~w labelled examples.',Ls)
        ,debug_clauses_length(experiment_initial_full,'Got ~w labelled examples:',Ls)
        ,debug_length(experiment_initial,'Got ~w unlabelled examples.',Us)
        ,debug_clauses_length(experiment_initial_full,'Got ~w unlabelled examples:',Us)
        ,debug_time(experiment_time, learn(Ls,Us,BK,MS,Pos,Neg,Ps) )
        ,debug_length(experiment_learned,'Learned ~w clause hypothesis.',Ps)
        ,debug_clauses(experiment_learned_full,'Learned hypothesis:',Ps)
        ,debug_length(experiment_examples,'Labelled ~w Positive examples.',Pos)
        ,debug_length(experiment_examples,'Labelled ~w Negative examples.',Neg)
        ,debug_clauses_length(experiment_examples_full,'~w Positive examples:',Pos)
        ,debug_clauses_length(experiment_examples_full,'~w Negative examples:',Neg)
        ,test_labelling(Sl,Pos,Neg,Rs_l)
        % Evaluate program as acceptor
        ,test_program(Sl,S,Ps,TPos,TNeg,Rs_p)
        % Evaluate program as generator
        ,test_generated(Sl,S,TGen,Ps,Rs_g)
        % May be a better way to count program length: count literals.
        ,length(Ps,N).


%!      debug_time(+Subject,+Goal) is det.
%
%       Debug helper to log time taken to run a goal.
%
debug_time(S,G):-
        call_time(G,T)
        ,debug(S,'Wall Clock time: ~4f CPU time: ~4f Inferences: ~D'
              ,[T.wall,T.cpu,T.inferences]).


%!      test_target(+Specification,-Target) is det.
%
%       Extract the symbol of a Target from a generator Specification.
%
%       Specification is a single language generation specification
%       term, or a list thereof.
%
%       Target is the symbol of the language in the term in
%       Specification, if Specification is only one term, or the first
%       term in the list if Specification is a list.
%
test_target([T|_Ts],T_):-
        !
        ,T =.. [T_|_].
test_target(T,F):-
        \+ is_list(T)
        ,compound(T)
        ,functor(T,F,_).



%!      test_labelling(+Language,+Pos,+Neg,-Results) is det.
%
%       Test a labelling of sets of Positive and Negative examples.
%
%       Language is the symbol, but not arity, of the target theory used
%       to verify examples learned by Poker.
%
%       Pos and Neg are positive and negative examples of Language
%       identified by a program learned by Poker from some initial
%       observations.
%
%       Results is a pair Counts-Results, where:
%
%       Counts is a list: [TP,TN,FP,FN], with elements as follows:
%       TP: number of true positives.
%       TN: number of true negatives.
%       FP: number of false positives.
%       FN: number of false negatives.
%
%       Results is a list [Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC] with
%       elements as follows:
%       Acc: Accuracy
%       Err: Error
%       TPR: True Positive Rate
%       TNR: True Negative Rate
%       FPR: False Positive Rate
%       FNR: Fale Negative Rate
%       PRE: Precision
%       REC: Recall (TPR)
%       FSC: F1 Score
%
%       Counts and Results are calculated over the labelling of Pos and
%       Neg, by Poker and with respect to the ground theory provided by
%       Language.
%
test_labelling(S,Pos,Neg,Cs-Res):-
        debug(test_labelling,'Testing labelling for target: ~w',[S])
        ,Program_module = language_generation
        ,evaluation(labelling,Program_module,S,Pos,Neg,Cs,Res)
        ,Res = [Acc,_Err,TPR,TNR,_FPR,_FNR,_PRE,_REC,_FSC]
        ,debug(test_labelling,'Labelling: Measured Acc: ~w TPR: ~w TNR: ~w',[Acc,TPR,TNR])
        ,debug_confusion_matrix(test_labelling_full,labelling,Program_module,S,Pos,Neg).


%!      test_program(+Language,+Symbol,+Program,+TestPos,+TestNeg,-Results)
%!      is det.
%
%       Test a Program on positive and negative examples.
%
%       Language is a predicate symbol, but not arity, of the grammar to
%       use to generate positive and negative example strings.
%
%       Symbol is a predicate indicator, Functor/Arity of Program and
%       of the examples in TestPos and TestNeg. Sym should correspond to
%       a learning target defined in the current experiment file (i.e.
%       the first argument of experiment file interface predicates like
%       background_knowledge/2, metarules/2, labelled_examples/2 etc).
%
%       Program is the learned program to test against the examples
%       generated by Language.
%
%       TestPos, TestNeg are language generation specification terms for
%       the generation of the positive and negative testing examples,
%       respectively.
%
%       Results is a pair Counts-Results, where:
%
%       Counts is a list: [TP,TN,FP,FN], with elements as follows:
%       TP: number of true positives.
%       TN: number of true negatives.
%       FP: number of false positives.
%       FN: number of false negatives.
%
%       Results is a list [Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC] with
%       elements as follows:
%       Acc: Accuracy
%       Err: Error
%       TPR: True Positive Rate
%       TNR: True Negative Rate
%       FPR: False Positive Rate
%       FNR: Fale Negative Rate
%       PRE: Precision
%       REC: Recall (TPR)
%       FSC: F1 Score
%
%       Counts and Results are calculated over the labelling of the
%       ground truth examples in Pos and Neg by Program.
%
test_program(_T,S,[],Test_Pos,Test_Neg,[0,FN,0,TN]-[0.5,0.5,0.0,1.0,0.0,1.0,0.0,0.0,0.0]):-
% The empty hypothesis rejects all.  % TP,FN,FP,TN Acc Err TPR TNR FPR FNR PRE REC FSC
        !
        ,count_initial(Test_Pos,S,FN)
        ,count_initial(Test_Neg,S,TN).
test_program(T/_,S,Cs,TPs,TNs,Res):-
% Allow the target to be a predicate indicator.
% TODO: wait, why?
        test_program(T,S,Cs,TPs,TNs,Res)
        ,!.
test_program(T,S/A,Cs,Test_Pos,Test_Neg,Counts-Res):-
        debug(test_program,'Testing learned program for target: ~w',[T])
        ,debug_clauses_length(test_program_full,'Testing ~w-clause learned program:',Cs)
        ,Program_module = experiment_file
        ,Set = (assert_program(Program_module,Cs,Rs)
               ,poker:table_untable_predicates(table,Program_module,Cs)
               )
        ,G = (debug(test_program_full,'Generating positive testing examples.',[])
             ,generate_examples(pos,Test_Pos,S/A,Pos)
             ,debug(test_program_full,'Generating negative testing examples.',[])
             ,generate_examples(neg,Test_Neg,S/A,Neg)
             ,evaluation(hypothesis,Program_module,S,Pos,Neg,Counts,Res)
             )
        ,C = (erase_program_clauses(Rs)
             ,poker:table_untable_predicates(table,Program_module,Cs)
             )
        ,setup_call_cleanup(Set,G,C)
        ,Res = [Acc,_Err,TPR,TNR,_FPR,_FNR,_PRE,_REC,_FSC]
        ,debug(test_program,'Program: Measured Acc: ~w TPR: ~w TNR: ~w',[Acc,TPR,TNR])
        ,debug_confusion_matrix(test_program_full,hypothesis,Program_module,S,Pos,Neg).


%!      generate_positives(+Sign,+Spec,+Symbol,-Examples) is det.
%
%       Generate Examples of the given Sign to test a learned program.
%
%       Sign is one of [pos,neg], for positive and negative examples,
%       respectively.
%
%       Spec is a language generation specification term.
%
%       Symbol is a predicate indicator, Functor/Arity of the Examples
%       to be generated.
%
%       Examples is a list of examples generated according to Spec with
%       a call generate_initial(Spec,Symbol,Example_I).
%
generate_examples(Sign,Ts,S,Es):-
        findall(Es_i
               ,generate_initial(Ts,S,Es_i)
               ,Es_)
        ,flatten(Es_,Es)
        ,(   Sign == pos
         ->  Sign_ = positive
         ;   Sign == neg
         ->  Sign_ = negative
         )
        ,format(atom(M),'Generated ~~w ~w testing examples',[Sign_])
        ,debug_length(generate_examples,M,Es)
        ,debug_clauses_length(generate_examples_full,M,Es).



%!      test_generated(+Language,+Symbol,+Spec,+Program,-Acc) is det.
%
%       Measure the accuracy of a learned Program as a generator.
%
%       Language is the symbol, but not arity of a DCG used to test
%       generated atoms.
%
%       Symbol is the predicate indicator, Symbol/Arity of a learning
%       target.
%
%       Spec is a language generation term used to generate examples of
%       Program for testing against Language.
%
%       Program is a learned hypothesis.
%
%       Acc is the accuracy of Program as a generator with respect to
%       Language, calculated by executing Program as a generator
%       according to Spec, then passing the generator atoms to Language
%       to test if they are accepted.
%
test_generated(_,_S,_,[],0.0):-
% The empty hypothesis generates nothing.
        !.
test_generated(_,_S,nil,_Cs,0.0):-
        !.
test_generated(Lang,S/A,Spec,Cs,TPR):-
        debug(test_generator,'Testing learned program as generator for target: ~w',[S/A])
        ,debug_clauses_length(test_generator_full,'Testing ~w-clause learned program:',Cs)
        ,Spec =.. [Lang,N,J,K]
        ,M = experiment_file
        ,Set = (assert_program(M,Cs,Rs)
               ,poker:table_untable_predicates(table,M,Cs)
               )
        ,G = (debug(test_generator_full,'Generating examples of learned program.',[])
             ,generate_test(M,S/A,N,J,K,Es)
             ,debug(test_generator_full,'Testing generated examples against target.',[])
             ,accuracy(language_generation,Lang,Es,[],TPR)
             )
        ,C = (erase_program_clauses(Rs)
             ,poker:table_untable_predicates(table,M,Cs)
             )
        ,setup_call_cleanup(Set,G,C).


%!      generate_test(+Module,+Sym,+N,+J,+K,-Examples) is det.
%
%       Genereate examples of a learned hypothesis.
%
%       Filthy copy/pasta of generate_test/5 with a Module term to
%       locate the learned program to use as a generator.
%
generate_test(M,S,N,J,K,Es):-
        debug(generate_test,'Generating ~w ~w test examples of length in [~w,~w].'
             ,[N,S,J,K])
        ,findall(E
               ,(between(J,K,I)
                ,generate_example_test(M,S,I,E)
                )
               ,Es_)
        ,debug(generate_test_full,'Sampling ~w generated examples',[N])
        ,(   number(N)
         ->  k_list_samples(N,Es_,Es)
         ;   N == all
         ->  Es = Es_
         ).


%!      generate_example_test(+Module,+Symbol,+N,-Example) is nondet.
%
%       Generator of examples for generate_test/6.
%
%       Filthy copy/pasta of generate_example_test/3 with extra term to
%       locate the learned program to use as a generator.
%
generate_example_test(M,S/2,N,E):-
        !
        ,length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(M:E).
generate_example_test(M,S/3,N,E):-
        length(Is,N)
        ,E =.. [S,Is,_Os,[]]
        ,call(M:E).



                /*******************************
                *          EVALUATION          *
                *******************************/


%!	print_confusion_matrix(+Evaluation,+Module,+Language,+Pos,+Neg)
%!      is det.
%
%	Print a confusion matrix of current evaluation results.
%
%       Evaluation is one of: [labelling, hypothesis], indicating how
%       positive and negative examples are treated when counting false
%       positives and false negatives. See false_positives/6 and
%       false_negatives/6 for details.
%
%       Module is the name of the module where Language is defined as a
%       DCG.
%
%       Language is the symbol, but not arity of a language used to test
%       the given positive and negative examples.
%
%       Pos and Neg are lists of examples used in evaluation.
%
%       Note: A meaningful confusion matrix can be printed for labelling
%       and hypothesis results, but not when evaluating a hypothesis as
%       a generator, with test_generated/5, because in the latter
%       evaluation, no negative examples are given, or generated. Hence
%       the absence of an Evaluation option for generative accuracy.
%
print_confusion_matrix(E,M,S,Pos,Neg):-
        evaluation(E,M,S,Pos,Neg,[TP,FN,FP,TN],[ACC,ERR,_TPR,_TNR,FPR,FNR,PRE,REC,FSC])
        ,confusion_matrix_totals(Pos,Neg,[TP,FN,FP,TN],[TPFN,FPTN,TPFP,FNTN,T])
        ,format_confusion_matrix([TP,FN,FP,TN]
                                ,[TPFN,FPTN,TPFP,FNTN,T]
                                ,[ACC,ERR,FPR,FNR,PRE,REC,FSC]
                                ).



%!	debug_confusion_matrix(+Subject,+Evaluation,+Module,+Language,+Pos,+Neg)
%!      is det.
%
%	Log current evaluation results as a confusion matrix.
%
%       As print_confusion_matrix/5 but prints the confusion matrix to
%       the debug output rather than main output.
%
%       Subject is the debug subject given as a first argument to
%       logging predicates such as debug/3.
%
debug_confusion_matrix(Sub,E,M,S,Pos,Neg):-
        evaluation(E,M,S,Pos,Neg,[TP,FN,FP,TN],[ACC,ERR,TPR,TNR,FPR,FNR,PRE,REC,FSC])
        ,confusion_matrix_totals(Pos,Neg,[TP,FN,FP,TN],[TPFN,FPTN,TPFP,FNTN,T])
        ,debug_confusion_matrix(Sub
                               ,[TP,FN,FP,TN]
                               ,[TPFN,FPTN,TPFP,FNTN,T]
                               ,[ACC,ERR,TPR,TNR,FPR,FNR,PRE,REC,FSC]
                               ).


%!      evaluation(+Eval,+Mod,+Lang,+Pos,+Neg,-Counts,-Results) is det.
%
%       Calculate evaluation metrics for a confusion matrix.
%
%       Eval is one of: [labelling, hypothesis], indicating how
%       positive and negative examples are treated when counting false
%       positives and false negatives. See false_positives/6 and
%       false_negatives/6 for details.
%
%       Module is the name of the module where Language is defined as a
%       DCG.
%
%       Language is the symbol, but not arity of a language used to test
%       the given positive and negative examples.
%
%       Pos and Neg are lists of examples used in evaluation.
%
%       Counts is a list: [TP,TN,FP,FN], with elements as follows:
%       TP: number of true positives.
%       TN: number of true negatives.
%       FP: number of false positives.
%       FN: number of false negatives.
%
%       Results is a list [Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC] with
%       elements as follows:
%       Acc: Accuracy
%       Err: Error
%       TPR: True Positive Rate
%       TNR: True Negative Rate
%       FPR: False Positive Rate
%       FNR: Fale Negative Rate
%       PRE: Precision
%       REC: Recall (TPR)
%       FSC: F1 Score
%
%       Counts and Results are passed to format_confusion_matrix/3 and
%       debug_confusion_matrix/4 for pretty-printing.
%
evaluation(E,M,S,Pos,Neg,[TP,FN,FP,TN],[Acc,Err,TPR,TNR,FPR,FNR,PRE,REC,FSC]):-
        true_positives(M,S,Pos,TP_)
        ,debug_clauses(evaluation_full,'True Positives:',TP_)
        ,true_negatives(M,S,Neg,TN_)
        ,debug_clauses(evaluation_full,'True Negatives:',TN_)
        ,false_positives(E,M,S,Pos,Neg,FP_)
        ,debug_clauses(evaluation_full,'False Positives:',FP_)
        ,false_negatives(E,M,S,Pos,Neg,FN_)
        ,debug_clauses(evaluation_full,'False Negatives:',FN_)
        ,maplist(length,[TP_,TN_,FP_,FN_],[TP,TN,FP,FN])
        ,accuracy(M,S,Pos,Neg,Acc)
        ,error(M,S,Pos,Neg,Err)
        ,tpr(M,S,Pos,TPR)
        ,tnr(M,S,Neg,TNR)
        ,fpr(E,M,S,Pos,Neg,FPR)
        ,fnr(E,M,S,Pos,Neg,FNR)
        ,pre(E,M,S,Pos,Neg,PRE)
        ,rec(M,S,Pos,REC)
        ,Num is PRE * REC
        ,Den is PRE + REC
        ,safe_division(Num,Den,Pil)
        ,FSC is 2 * Pil
        ,debug(evaluation,'Acc: ~w',[Acc])
        ,debug(evaluation,'Err: ~w',[Err])
        ,debug(evaluation,'TPR: ~w',[TPR])
        ,debug(evaluation,'TNR: ~w',[TNR])
        ,debug(evaluation,'FPR: ~w',[FPR])
        ,debug(evaluation,'FNR: ~w',[FNR])
        ,debug(evaluation,'PRE: ~w',[PRE])
        ,debug(evaluation,'REC: ~w',[REC])
        ,debug(evaluation,'FSC: ~w',[FSC]).



%!      confusion_matrix_totals(+Pos,+Neg,+Counts,-Totals) is det.
%
%       Calculate Totals for a confusion matrix.
%
%       Pos, Neg are sets of positive examples.
%
%       Counts is a list [TP,FN,FP,TN] with:
%       TP: count of true positives.
%       FN: count of false negatives.
%       FP: count of false positives.
%       TN: duke of true negatives.
%
%       Totals is a list [TPFN,FPTN,TPFP,FNTN,T] with:
%       TPFN: TP + FN
%       FPTN: FP + TN
%       TPFP: TP + FP
%       FNTN: FN + TN
%       T: |Pos| + |Neg|
%
%       Totals can be passed to format_confusion_matrix/3 and
%       debug_confusion_matrix/4 along with Counts and evaluation
%       results to pretty-print a confusion matrix.
%
confusion_matrix_totals(Pos,Neg,[TP,FN,FP,TN],[TPFN,FPTN,TPFP,FNTN,T]):-
        maplist(length,[Pos,Neg],[P,N])
        ,TPFP is TP + FP
        ,FNTN is FN + TN
        ,TPFN is TP + FN
        ,FPTN is FP + TN
        ,T is P + N.



%!	format_confusion_matrix(+Counts,+Totals,+Metrics) is det.
%
%	Pretty-prints a confusion matrix to standard output.
%
%	Counts is a list of numbers [PP,PN,NP,NN,T], where:
%	* PP: positive instances predicted as positive
%	* PN: positive instances predicted as negative
%	* NP: negative instances predicted as positive
%	* NN: negative instances predicted as negative
%	* T:  total positive and negatives predicted
%
%	Totals is a list of numbers [TP,TN,PPNP,PNNN,T], where:
%	* TP: the total number of positive instances
%	* TN: the total number of negative instances
%	* PPNP: the sum of PP + NP
%	* PNNN: the sum of PN + NN
%
%	Metrics is a list of numbers [Acr,Err,FPR,FNR,PRE,REC,FSC],
%	where:
%	* Acr: Accuracy, calculated as  PP + NN / T
%	* Err: Error, calculated as NP + PN / T
%	* FPR: False Positive Rate, NP / TN
%	* FNR: False Negative Rate, PN / TP
%	* PRE: Precision, calculated as PP / PPNP
%	* REC: Recall, calculated as PP / TP (i.e. TPR)
%	* FSC: F-Score, PRE * REC / PRE + REC
%
%	Given the above lists of numbers, format_confusion_matrix/3 will
%	print approximately the following table (with some differences
%	in formatting):
%
%			Predicted +	Predicted -	Total
%	Actual +	PP		PN		TP
%	Actual -	NP		NN		TN
%	-----------------------------------------------------
%	Total		PPNP		PNNN		T
%
%	Accuracy:		PP + NN / T
%	Error:			NP + PN / T
%	False Positive Rate:	NP / TN
%	False Negative Rate:	PN / TP
%	Precision:		PP / PPNP
%	Recall(TPR):		PP / TP
%	F-Score:                Precision * Recall / Precision + Recall
%
format_confusion_matrix([PP,PN,NP,NN]
		       ,[TP,TN,PPNP,PNNN,T]
		       ,[ACC,ERR,TPR,TNR,FPR,FNR,PRE,REC,FSC]):-
	%decimal_places(D)
        D = 4
	,atom_chars('Actual + Predicted + Predicted - Total', Hs)
	% Length of an entire header line
	,length(Hs, L1)
	,atom_chars('Actual + ', Act)
	% Length of second line's first column
	,length(Act, L21)
	,atom_chars('Predicted + ', Pred_p)
	,length(Pred_p, L22)
	,atom_chars('Predicted - ', Pred_n)
	,length(Pred_n, L23)
	% Printing header line
	,format('~*+~w ~*+~w ~*+~w~*+~n'
	       ,[L21,'Predicted +',L22,'Predicted -',L23,'Total',L1])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Actual +',L21,PP,L22,PN,L23,TP])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Actual -',L21,NP,L22,NN,L23,TN])
	,format('-------------------------------------~n',[])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Total',L21,PPNP,L22,PNNN,L23,T])
	% Longest left column
	,atom_chars('False Positive Rate: ',TPR_cs)
	,length(TPR_cs, TPR_cs_L)
	,format('Accuracy: ~*+~*f~n', [TPR_cs_L,D,ACC])
	,format('Error: ~*+~*f~n', [TPR_cs_L,D,ERR])
        ,format('True Positive Rate: ~*+~*f~n', [TPR_cs_L,D,TPR])
	,format('True Negative Rate: ~*+~*f~n', [TPR_cs_L,D,TNR])
	,format('False Positive Rate: ~*+~*f~n', [TPR_cs_L,D,FPR])
	,format('False Negative Rate: ~*+~*f~n', [TPR_cs_L,D,FNR])
	,format('Precision: ~*+~*f~n', [TPR_cs_L,D,PRE])
	,format('Recall (TPR): ~*+~*f~n', [TPR_cs_L,D,REC])
	,format('F-Score: ~*+~*f~n', [TPR_cs_L,D,FSC]).



%!      debug_confusion_matrix(+Subject,+Counts,+Totals,+Metrics) is
%!      det.
%
%       Pretty-print a confusion matrix to the current debug stream.
%
%       As format_confusion_matrix/3 but prints the confusion matrix to
%       the debug output.
%
%       @tbd Copy/pasta from format_confusion_matrix/3 with slight
%       changes. Can this be abstracted a bit?
%
debug_confusion_matrix(Sub
                      ,[PP,PN,NP,NN]
                      ,[TP,TN,PPNP,PNNN,T]
                      ,[ACC,ERR,TPR,TNR,FPR,FNR,PRE,REC,FSC]):-
	%decimal_places(D)
        D = 4
	,atom_chars('Actual + Predicted + Predicted - Total', Hs)
	% Length of an entire header line
	,length(Hs, L1_)
	,atom_chars('Actual + ', Act)
	% Length of second line's first column
	,length(Act, L21_)
	,atom_chars('Predicted + ', Pred_p)
	,length(Pred_p, L22_)
	,atom_chars('Predicted - ', Pred_n)
	,length(Pred_n, L23_)
	,atom_chars('False Positive Rate: ',TPR_cs)
	,length(TPR_cs, TPR_cs_L_)
        ,findall(L_
                ,(member(L,[L1_,L21_,L22_,L23_,TPR_cs_L_])
                 % One for the mon... for the "%" and one for the space
                 % added by debug/3 at the start of a line.
                 ,L_ is L + 2
                 )
                ,[L1,L21,L22,L23,TPR_cs_L])
	% Printing header line
	,debug(Sub,'',[])
	,debug(Sub,'~*+~w ~*+~w ~*+~w~*+'
	       ,[L21,'Predicted +',L22,'Predicted -',L23,'Total',L1])
	,debug(Sub,'~w ~*+~w ~*+~w ~*+~w'
	       ,['Actual +',L21,PP,L22,PN,L23,TP])
	,debug(Sub,'~w ~*+~w ~*+~w ~*+~w'
	       ,['Actual -',L21,NP,L22,NN,L23,TN])
	,debug(Sub,'----------------------------------------',[])
	,debug(Sub,'~w ~*+~w ~*+~w ~*+~w'
	       ,['Total',L21,PPNP,L22,PNNN,L23,T])
	% Longest left column
	,debug(Sub,'',[])
	,debug(Sub,'Accuracy: ~*+~*f', [TPR_cs_L,D,ACC])
	,debug(Sub,'Error: ~*+~*f', [TPR_cs_L,D,ERR])
        ,debug(Sub,'True Positive Rate: ~*+~*f', [TPR_cs_L,D,TPR])
	,debug(Sub,'True Negative Rate: ~*+~*f', [TPR_cs_L,D,TNR])
	,debug(Sub,'False Positive Rate: ~*+~*f', [TPR_cs_L,D,FPR])
	,debug(Sub,'False Negative Rate: ~*+~*f', [TPR_cs_L,D,FNR])
	,debug(Sub,'Precision: ~*+~*f', [TPR_cs_L,D,PRE])
	,debug(Sub,'Recall (TPR): ~*+~*f', [TPR_cs_L,D,REC])
	,debug(Sub,'F-Score: ~*+~*f', [TPR_cs_L,D,FSC])
        ,debug(Sub,'',[]).



%!      accuracy(+Module,+Language,+Pos,+Neg,-Accuracy) is det.
%
%       Calculate the Accuracy of labelling a set of atoms.
%
%       Module is the name of the module where Language is defined as a
%       DCG.
%
%       Language is the symbol, but not the arity, of a language used to
%       test the given positive and negative examples in Pos, Neg.
%
%       Pos, Neg, are lists of positive and negative examples used to
%       calculate Accuracy.
%
%       Accuracy is a float, the Accuracy of Pos and Neg with respect to
%       Language.
%
accuracy(PM,S,Pos,Neg,Acc):-
        true_positives(PM,S,Pos,TP)
        ,true_negatives(PM,S,Neg,TN)
        ,maplist(total,[Pos,TP],[Neg,TN],[L,T])
        ,safe_division(T,L,Acc_)
        ,atomize(Acc_,Acc).


%!      error(+Module,+Language,+Pos,+Neg,-Error) is det.
%
%       Calculate the Error of labelling a set of atoms.
%
error(M,S,Pos,Neg,Err):-
        accuracy(M,S,Pos,Neg,Acc)
        ,Err is 1 - Acc.


%!      tpr(+Module,+Target,+Pos,-TPR) is det.
%
%       Calculate the True Positive Rate of labelling a set of atoms.
%
tpr(M,S,Pos,TPR):-
        true_positives(M,S,Pos,TP)
        ,ratio(TP,Pos,TPR_)
        ,atomize(TPR_,TPR).


%!      tnr(+Module,+Target,+Neg,-TNR) is det.
%
%       Calculate the True Negative Rate of labelling a set of atoms.
%
tnr(M,S,Neg,TNR):-
        true_negatives(M,S,Neg,TN)
        ,ratio(TN,Neg,TNR_)
        ,atomize(TNR_,TNR).


%!      fpr(+Module,+Language,+Pos,+Neg,-FPR) is det.
%
%       Calculate the False Positive Rate of labelling a set of atoms.
%
fpr(_,M,S,_Pos,Neg,FPR):-
        !
        ,tnr(M,S,Neg,TNR)
        ,FPR is 1 - TNR.


%!      fnr(+Module,+Language,+Pos,+Neg,-FNR) is det.
%
%       Calculate the False Negative Rate of labelling a set of atoms.
%
fnr(_,M,S,Pos,_Neg,FNR):-
        !
        ,tpr(M,S,Pos,TPR)
        ,FNR is 1 - TPR.


%!      pre(+Module,+Lang,+Pos,+Neg,-Precision) is det.
%
%       Calculate the Precision of labelling a set of atoms.
%
pre(E,M,S,Pos,Neg,PRE):-
        true_positives(M,S,Pos,TP)
        ,false_positives(E,M,S,Pos,Neg,FP)
        ,append(TP,FP,Ps)
        ,ratio(TP,Ps,PRE_)
        ,atomize(PRE_,PRE).


%!      rec(+Module,+Language,+Pos,-Recall) is det.
%
%       Calculate the Recall of labelling a set of atoms.
%
rec(M,S,Pos,REC):-
        tpr(M,S,Pos,REC).


%!      true_positives(+Module,+Target,+Pos,-True) is det.
%
%       Collect all True positives in a set of atoms labelled positive.
%
true_positives(M,S,Pos,TP):-
        test_examples(succeed,M,S,Pos,TP).


%!      true_negatives(+Module,+Target,+Neg,-True) is det.
%
%       Collect all True negatives in a set of atoms labelled negative.
%
true_negatives(M,S,Neg,TN):-
        test_examples(fail,M,S,Neg,TN).


%!      false_positives(+Eval,+Module,+Language,+Pos,+Neg,-False) is
%!      det.
%
%       Collect all False positives in a a labelling or by a hypothesis.
%
%       Eval is one of: [labelling, hypothesis], denoting what is
%       evaluating, and changing the meaning of Pos and Neg.
%
%       If Eval is "labelling", then Pos and Neg are atoms labelled true
%       and false, respectively, by Poker. Accordingly, Pos and Neg must
%       be evaluated according to the true definition of the target
%       Language.
%
%       If Eval is "hypothesis", then Pos and Neg are ground truth atoms
%       that we know are true and false, respectively. Then Pos and Neg
%       must be evaluated according to a learned hypothesis of the
%       definition of Language.
%
%       Eval is used to select clause sof false_positives/6 according to
%       how Pos and Neg are interpreted.
%
%       False is the set of atoms that are false positives according to
%       the interpretation of Pos and Neg.
%
false_positives(labelling,M,S,Pos,_Neg,FP):-
	!
	,test_examples(fail,M,S,Pos,FP).
false_positives(hypothesis,M,S,_Pos,Neg,FP):-
	test_examples(succeed,M,S,Neg,FP).


%!      false_negatives(+Eval,+Module,+Language,+Pos,+Neg,-False) is
%!      det.
%
%       Collect all False Negatives in a labelling or by a hypothesis.
%
%       Eval is one of: [labelling, hypothesis], denoting what is
%       evaluating, and changing the meaning of Pos and Neg.
%
%       If Eval is "labelling", then Pos and Neg are atoms labelled true
%       and false, respectively, by Poker. Accordingly, Pos and Neg must
%       be evaluated according to the true definition of the target
%       Language.
%
%       If Eval is "hypothesis", then Pos and Neg are ground truth atoms
%       that we know are true and false, respectively. Then Pos and Neg
%       must be evaluated according to a learned hypothesis of the
%       definition of Language.
%
%       Eval is used to select clause sof false_positives/6 according to
%       how Pos and Neg are interpreted.
%
%       False is the set of atoms that are false negatives according to
%       the interpretation of Pos and Neg.
%
false_negatives(labelling,M,S,_Pos,Neg,FN):-
	!
	,test_examples(succeed,M,S,Neg,FN).
false_negatives(hypothesis,M,S,Pos,_Neg,FN):-
	test_examples(fail,M,S,Pos,FN).


%!      test_examples(+How,+Module,+Language,+Exampls,-Passed) is det.
%
%       Test a set of Examples and report those that Passed the test.
%
%       How is one of: [succeed,fail], denoting whether an atom in
%       Examples should be accepted, or rejected, by agrammar denoted by
%       the given Language.
%
%       Module is the module where Language is defined as a DCG.
%
%       Language is the symbol, but not arity, of a language used to
%       test the given Examples.
%
%       Examples is a list of atoms to be tested for membership in (in
%       the sense of acceptance by a grammar of) Language.
%
%       Passed is the list of atoms accepted, or rejected, by Language,
%       depending on the value of How.
%
%       @tbd Consider replacing succeed --> accepts, fail -- rejects
%       for a bit more nuance.
%
test_examples(succeed,M,S,Es,Ts):-
        !
        ,findall(E_
               ,(member(E,Es)
                ,E =.. [_S|As]
                ,E_ =.. [S|As]
                ,once( M:call(E_) )
                )
               ,Ts).
test_examples(fail,M,S,Es,Ts):-
        findall(E_
               ,(member(E,Es)
                ,E =.. [_S|As]
                ,E_ =.. [S|As]
                ,\+ once( M:call(E_) )
                )
               ,Ts).


%!	total(+Xs,+Ys,-Sum) is det.
%
%	Sum of the lengths of two lists.
%
total(Xs,Ys,S):-
	length(Xs,N)
	,length(Ys,M)
	,S is N + M.


%!	ratio(+Xs,+Ys,-Ratio) is det.
%
%	Ratio of the lengths of two lists.
%
ratio(Xs,Ys,R):-
	length(Xs,N)
	,length(Ys,M)
	,safe_division(N,M,R).


%!	safe_division(+A,+B,-C) is det.
%
%	Avoid dividing by zero.
%
%	If the denominator of a division is 0, return A, else divide
%	A/B and return the result in C.
%
safe_division(A,0,A):-
	!.
safe_division(A,0.0,A):-
	!.
safe_division(A,B,C):-
	C is A / B.


%!      atomize(+Number,-Formatted) is det.
%
%       Format a Number into a float with 4 decimal digits.
%
atomize(N,An):-
        format(atom(A),'~4f',[N])
        ,atom_number(A,An).
