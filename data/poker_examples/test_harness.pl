:-module(test_harness,[experiments_ranges/8
                      ,range_helper/2
                      ,experiments/7
                      ,experiment/6
                      ,generate_initial/2
                      ,generate_initial/5
                      ,test_labelling/4
                      ,test_program/5
                      ,test_draw/8
                      ]).

:-use_module(lib(poker/poker)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(src(auxiliaries)).
:-use_module(lib(poker/sampling/sampling)).
:-use_module(lib(mathemancy/mathemancy)).

/** <module> A test harness for Poker.

Run experiments evaluating the labelling of initial and new atoms
labelled by Poker, or the programs it learns to label them.

*/

% To draw L-Systems with Python's turtle library, via Janus.
:- py_add_lib_dir(data(poker_examples)).


%!      experiments_ranges(+Target,+N,+Gen,+Lab,+Unlab,+TPos,+TNeg,-Results)
%!      is det.
%
%       Run N experiments with example numbers increasing over a range.
%
%       This predicate runs a set of experiments determined by Gend,
%       each with K iteration determined by Init, and repeating N times:
%
%       For each setting G in |Gen| experiment sets
%           Set unlabelled_examples(G)
%           For each itereation I in |Lab| iterations
%                   expand Lab, Unlab -> Li, Ui
%                   Call: experiments(Target,N,Li,Ui,TPos,TNeg,Res)
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
%       Results is a list of lists of terms I/G/L/U-Rs, where I is the
%       iteration, G is the number of internally generated examples, L
%       and U are the numbers of labelled and unlabelled examples
%       respectively and Rs is a list-of-lists of results lists as
%       returned by experiments/7.
%
%       Example query:
%       ==
%       test_harness:experiments_ranges(
%       s/2
%       ,10
%       ,0:5/1
%       ,anbn(1:5/1,0,12)
%       ,[]
%       ,anbn(all,13,18)
%       ,not_anbn(all,0,3)
%       ,_Rs
%       )
%       , maplist(writeln,_Rs).
%       ==
%
%       The query above runs an experiment with anbn as the target
%       language, used to generate labelled examples, and no unlabelled
%       examples, iterating over the number of internally generated,
%       and labelled examples. Parameters are as follows:
%
%       * s/2: The symbol and arity of a learning target defined in the
%       current experiment file.
%
%       * 10: The number of experiments to run in each iteration.
%
%       * 0:5/1: The range of values for internally generated examples.
%       Defines the range of integers in [0,5] increasing by 1.
%
%       * anbn(1:5/1,0,12): Language generation specification for
%       labelled examples. 1:5/1 defines the numbers of examples of anbn
%       that will be generated in successive iterations: from 1 to 5,
%       increasing by 1, therefore for 5 total iterations. 0 and 12 are
%       the maximum and minimum lengths of strings in examples.
%
%       * []: No unlabelled examples will be generated.
%
%       * anbn(all,13,18): language generation specification for
%       positive testing examples. All examples of anbn of length
%       between 13 and 18 will be generated and used to test the
%       hypothesis learned at each iteration.
%
%       * not_anbn(all,0,3): language generation specification term for
%       negative testing examples. All exampls of anbm of length between
%       0 and 3 will be generated and used to test the hypothesis
%       learned at each iteration.
%
%       * _Rs: List of results.
%
%       @tbd: this predicate really needs some error checking to make
%       sure we're not giving as arguments totally bogus ranges, or ones
%       that will raise an error.
%
experiments_ranges(T,N,Gs,Ls,Us,TestPos,TestNeg,Rs):-
        range_interval(Gs,Gs_e)
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
                        %,maplist(nth1(I),[Ls_e,Us_e],[Ls_i,Us_i])
                        ,nth1_labelled_unlabelled(I,Ls_e,Us_e,Ls_i,Us_i)
                        ,debug(experiments,'Iteration ~w of ~w with:',[I,Ln])
                        ,debug(experiments,'Generated negative examples: ~w',[G])
                        ,maplist(spec_value,[Ls_i,Us_i],[L,U])
                        ,debug(experiments,'Labelled examples: ~w',[L])
                        ,debug(experiments,'Unlabelled examples: ~w',[U])
                        ,experiments(T,N,Ls_i,Us_i,TestPos,TestNeg,Rs_i)
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
%       Specification is a language generation specification term.
%
%       Value is the first argument of Specification, denoting a
%       quantity of examples to be generated.
%
%       Helper to simplify unpacking experiment set, iteration, examples
%       numbers etc values for outputting at the end of a set
%       ofexperiments with experiments_ranges/8.
%
spec_value([],0):-
        !.
spec_value(S,V):-
        S =.. [_,V|_].


%!      expanded_range(+Spec,-Expanded) is det.
%
%       Expand a group of range definitions to language terms.
%
%       Helper for to expand range specifications for ranged
%       experiments to language terms S(M,J,K) as expected by
%       experiments/7.
%
%       Spec is a range specification term of the form
%       Language(Init,Min,Max). Language, Init, Min and Max are as
%       follows.
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
expanded_range([],[]):-
        !.
expanded_range(Spec,Es):-
        Spec =.. [L,RIs,RMin,RMax]
        ,range_interval(RIs,Is)
        ,string_lengths(RIs,RMin,RMax,Mins,Maxs)
        ,findall(T
                ,(maplist(nth1(_I),[Is,Mins,Maxs],[In_i,Min_i,Max_i])
                 ,T =.. [L,In_i,Min_i,Max_i]
                 )
                ,Es).


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
range_helper(I:J/K,N):-
        M is J - I
        ,N is M / K + 1.



%!      experiments(+Target,+N,+Labelled,+Unlabelled,+TestPos,+TestNeg,-Results)
%!      is det.
%
%       Run N experiments with both Labelld and Unlabelled examples.
%
%       Target is a predicate indicator, S/A, of a learning target
%       defined in the current experiment file. T is used to collect
%       background knowledge and metarules for the experiment, but _not_
%       to generate examples. Examples are generated according to
%       Labelled and Unlabelled.
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
%       Results is a list Labelling, Program, with the same meaning as
%       in experiments/6. Copying from that predicate's documentation:
%
%       Results is a list of four sub-lists [Ms_L, SEs_L, Ms_P, SEs_P],
%       each of which is a list [Acc,TPR,TNR], where Acc is the
%       accuracy, TPR true positive rate and TNR true negative rate of
%       the N labellings returned, or hypotheses learned from the sets
%       of N examples generated in the N steps of the experiment. The
%       values of the three metrics in the four sub-lists are as
%       follows:
%
%       * Ms_L: [Acc,TPR,TNR] are the means of accuracy, TPR and TNR
%         over all N labellings.
%       * SEs_L: [Acc,TPR,TNR] are the standard errors of accuracy, TPR,
%         and TNR over all N labellings, according to the means in MS_L.
%       * Ms_P: [Acc,TPR,TNR] are the means of accuracy, TPR, and TNR,
%         over all N programs learned.
%       * SEs_P: [Acc,TPR,TNR] are the standard errors of accuracy, TPR,
%         and TNR over all N programs learned.
%
%       Example query:
%       ==
%       _T = s/2
%       , _Sl = anbn(all,0,6)
%       , _Su = [anbm(all,0,8),anbn(all,7,18)]
%       , _TPos = anbn(all,19,40)
%       , _TNeg = [anbm(all,0,4),not_anbn(all,4,8)]
%       , experiment(_T,_Sl,_Su,_TPos,_TNeg,[_Ps,_Pos,_Neg,LabellingMeans,ProgramMeans])
%       , maplist(print_clauses,['Hypothesis:','Positives:','Negatives:'],[_Ps,_Pos,_Neg]).
%       ==
%
experiments(T,N,Sl,Su,TPos,TNeg,[Ms_l,SEs_l,Ms_p,SEs_p]):-
        findall(Res_l-Res_p
               ,(between(1,N,I)
                ,debug(experiments,'Experiment ~w of ~w',[I,N])
                ,experiment(T,Sl,Su,TPos,TNeg,[_Ps,_Pos,_Neg,Res_l,Res_p])
                )
               ,Rs)
        ,pairs_keys_values(Rs,Rs_l,Rs_p)
        ,result_means(Rs_l,Ms_l)
        ,result_means(Rs_p,Ms_p)
        ,result_SEs(Rs_l,Ms_l,SEs_l)
        ,result_SEs(Rs_p,Ms_p,SEs_p)
        ,debug(experiments,'Labelling means ~w, standard errors: ~w: ',[Ms_l,SEs_l])
        ,debug(experiments,'Program means ~w, standard errors: ~w: ',[Ms_p,SEs_p]).


%!      result_SEs(+Results,+Means,-StandardErrors) is det.
%
%       Calculate the standard error of metrics in Results.
%
%       Results is a list of lists [Acc,TPR,TNR], where Acc, TPR and TNR
%       are numbers representing the Accuracy, True Positive Rate and
%       True Negative Rate calculated of each of a number of
%       experiments, in experiments/6.
%
%       Means is a list of numbers: [M_Acc,M_TPR,M_TNR], the means of
%       Acc, TPR, and TNR, respectively, in the list Results, as
%       returned by result_means/2.
%
%       StandardErrors is a list of numbers [Acc_SE,TPR_SE,TNR_SE], the
%       standard error of Acc, TPR and TNR in Results, calculated with
%       respect to Means.
%
result_SEs(Rs,Ms,SEs):-
        findall(Acc-TPR-TNR
               ,member([Acc,TPR,TNR],Rs)
               ,Vs)
        ,pairs_keys_values(Vs,Accs_Tprs,TNRs)
        ,pairs_keys_values(Accs_Tprs,Accs,TPRs)
        ,maplist(standard_deviation,[Accs,TPRs,TNRs],Ms,SDs)
        ,maplist(standard_error,[Accs,TPRs,TNRs],SDs,SEs_)
        ,maplist(atomize,SEs_,SEs).


%!      result_means(+Results,-Means) is det.
%
%       Calculate the means of a list of metrics in Results.
%
%       Results is a list of lists [Acc,TPR,TNR], where Acc, TPR and TNR
%       are numbers representing the Accuracy, True Positive Rate and
%       True Negative Rate calculated of each of a number of
%       experiments, in experiments/6.
%
%       Means is a list of numbers: [M_Acc,M_TPR,M_TNR], the means of
%       Acc, TPR, and TNR, respectively, in the list Results.
%
result_means(Rs,Ms):-
        result_means(Rs,[0,0,0],Ss)
        ,length(Rs,L)
        ,maplist(mean(L),Ss,Ms_)
        ,maplist(atomize,Ms_,Ms).


%!      result_means(+Results,+Acc,-Means) is det.
%
%       Business end of result_means/2.
%
%       Results is a list of lists [Acc,TPR,TNR], received from
%       result_means/2.
%
%       Acc is the accumulator of means, initially a list [0,0,0].
%
%       Means is the list in Acc with means of the three metrics updated
%       according to the contents of Results.
%
result_means([],Ms,Ms):-
        !.
result_means([[Acc1,TPR1,TNR1]|Rs],[Acc0,TPR0,TNR0],Ms):-
        maplist(sum,[Acc0,TPR0,TNR0],[Acc1,TPR1,TNR1],[Acc,TPR,TNR])
        ,result_means(Rs,[Acc,TPR,TNR],Ms).


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



%!      experiment(+Target,+Labelled,+Unlabelled,+TestPos,+TestNeg,-Results)
%!      is det.
%
%       Run an experiment with both Labelled and Unlabelled examples.
%
%       Similar to experiment/5, but allows training and testing with
%       both labelled and unlabelled examples. Additionally the
%       Unlabelled examples (but not the labelled examples) can be
%       generated from a composition of grammars rather than a single
%       grammar.
%
%       T is a predicate indicator, S/A, of a learning target defined in
%       the current experiment file. T is used to collect background
%       knowledge and metarules for the experiment, but _not_ to
%       generate examples. Examples are generated according to Labelled
%       and Unlabelled.
%
%       Labelled is a language generation specification term
%       Sl(Nl,Jl,Kl), where Sl is the symbol of a grammar defined in
%       test_harness and used to a) generate labelled examples and b)
%       evaluate the labelling and c) the program learned by Poker from
%       those examples, and Nl, Jl, and Kl, are the number (or atom
%       "all" for ... all) of strings of the language Sl to generate as
%       Definite Clause Grammars atoms, and minimum and maximum length
%       of strings in those atoms.
%
%       Unlabelled is a languge generation specification term
%       Su(Nu,Ju,Ku), with the same meaning as Labelled, except that Su
%       is the symbol of a grammar used to generate unlabelled examples.
%       Unlabelled can alterantively be a list of terms Su(Nu,Ju,Ku), in
%       which case the generated unlabelled examples are a combination
%       of atoms from all the listed languages and with the
%       corresponding numbers.
%
%       TestPos and TestNeg are language generation specification terms,
%       or lists thereof, as in Labelled and Unlabelled where, used to
%       generate positive and negative testing examples, respectively.
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
%       Results is as in experiment/5, a list [Ps,Pos,Neg,Ms_L,Ms_R],
%       where:
%
%       * Ps is the learned hypothesis
%       * Pos is the list of positive examples identified
%       * Neg is the list of negative examples identified
%       * Ms_L is the list of labelling results
%       * Ms_R is the list of program results.
%
%       Each of Ms_L and Ms_R is a list [Acc,TPR,TNR], where:
%
%       In Ms_L
%       * Acc is the accuracy of the labelling of atoms in Pos and Neg.
%       * TPR is the True Positive Rate of the labelling in Pos and Neg.
%       * TNR is the True Negative Rate of the labelling in Pos and Neg.
%
%       In Ms_P
%       * Acc is the accuracy of the program learned from N examples
%         measured against the ground truth of Language.
%       * TPR is the True Positive Rate of the program.
%       * TNR is the True Negative Rate of the program.
%
experiment(T,Sl,Su,TPos,TNeg,[Ps,Pos,Neg,Rs_l,Rs_p]):-
        generate_initial(Sl,Ls)
        ,debug_length(experiment_initial,'Generated ~w labelled examples.',Ls)
        ,debug_clauses_length(experiment_initial_full,'Generated ~w labelled examples:',Ls)
        ,generate_initial(Su,Us)
        ,debug_length(experiment_initial,'Generated ~w unlabelled examples.',Us)
        ,debug_clauses_length(experiment_initial_full,
                              'Generated ~w unlabelled examples:',Us)
        ,experiment_data(T,_,_,BK,MS)
        ,debug_time(experiment_time, learn(Ls,Us,BK,MS,Pos,Neg,Ps) )
        ,debug_length(experiment_learned,'Learned ~w clause hypothesis.',Ps)
        ,debug_clauses(experiment_learned_full,'Learned hypothesis:',Ps)
        ,debug_length(experiment_examples,'Labelled ~w Positive examples.',Pos)
        ,debug_length(experiment_examples,'Labelled ~w Negative examples.',Neg)
        ,debug_clauses_length(experiment_examples_full,'~w Positive examples:',Pos)
        ,debug_clauses_length(experiment_examples_full,'~w Negative examples:',Neg)
        ,test_target(Sl,Sl_)
        ,test_labelling(Sl_,Pos,Neg,Rs_l)
        ,test_program(Sl_,Ps,TPos,TNeg,Rs_p).


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
%       Results is a list [Acc,TPR,TNR], the accuracy, True Positive
%       Rate and True Negative Rate of the labelling of the positive and
%       negative examples in Pos and Neg, compared to the true labelling
%       by Language.
%
test_labelling(S,Pos,Neg,[Acc,TPR,TNR]):-
        debug(test_labelling,'Testing labelling for target: ~w',[S])
        ,accuracy(test_harness,S,Pos,Neg,Acc)
        ,tpr(test_harness,S,Pos,TPR)
        ,tnr(test_harness,S,Neg,TNR)
        ,debug(test_labelling,'Labelling: Measured Acc: ~w TPR: ~w TNR: ~w',[Acc,TPR,TNR]).


%!      test_program(+Language,+Program,+TestPos,+TestNeg,-Results) is
%!      det.
%
%       Test a Program on positive and negative examples.
%
%       Language is a predicate symbol, but not arity, of the grammar to
%       use to generate positive and negative example strings.
%
%       Program is the learned program to test against the examples
%       generated by Language.
%
%       TestPos, TestNeg are language generation specification terms for
%       the generation of the positive and negative testing examples,
%       respectively.
%
%       Results is a list [Acc, TPR, TNR], the accuracy, True Positive
%       Rate and True Negative Rate, respectively, of the Program's
%       labelling of examples generated by Language.
%
test_program(_,[],_,_,[0.5,0.0,1.0]):-
% The empty hypothesis rejects all.
        !.
test_program(T/_,Cs,TPs,TNs,[Acc,TPR,TNR]):-
% Allow the target to be a predicate indicator.
        test_program(T,Cs,TPs,TNs,[Acc,TPR,TNR])
        ,!.
test_program(T,Cs,Test_Pos,Test_Neg,[Acc,TPR,TNR]):-
        debug(test_program,'Testing learned program for target: ~w',[T])
        ,debug_clauses_length(test_program_full,'Testing ~w-clause learned program:',Cs)
        ,Program_module = experiment_file
        ,once( internal_symbol(T,T_) )
        ,S = (assert_program(Program_module,Cs,Rs)
             ,poker:table_untable_predicates(table,Program_module,Cs)
             )
        ,G = (debug(test_program_full,'Generating positive testing examples.',[])
             ,generate_examples(pos,Test_Pos,Pos)
             ,debug(test_program_full,'Generating negative testing examples.',[])
             ,generate_examples(neg,Test_Neg,Neg)
             ,accuracy(Program_module,T_,Pos,Neg,Acc)
             ,tpr(Program_module,T_,Pos,TPR)
             ,tnr(Program_module,T_,Neg,TNR)
             )
        ,C = (erase_program_clauses(Rs)
             ,poker:table_untable_predicates(table,Program_module,Cs)
             )
        ,setup_call_cleanup(S,G,C)
        ,debug(test_program,'Program: Measured Acc: ~w TPR: ~w TNR: ~w',[Acc,TPR,TNR]).


%!      generate_positives(+Sign,+Spec,-Examples) is det.
%
%       Generate Examples of the given Sign to test a learned program.
%
%       Spec is a language generation specification term.
%
%       Sign is one of [pos,neg], for positive and negative examples,
%       respectively.
%
%       Examples is a list of examples generated according to
%       geneate_examples/5, which should be loaded to memory from the
%       current experiment file; the symbols of grammars to use to
%       generate examples are defined in that predicate.
%
generate_examples(Sign,Ts,Es):-
        findall(Es_i
               ,generate_initial(Ts,Es_i)
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



                /*******************************
                *     LANGUAGE GENERATION      *
                *******************************/


%!      generate_initial(+Targets,-Examples) is det.
%
%       Generate atoms used as examples for a set of targets.
%
%       Similar to generate_initial/5 but allows atoms to be generated
%       from a compisition of two or more target languages.
%
%       Targets is either a list of terms S(N,J,K), or a single such
%       term. Targets may also be a pair Targets/Filter.
%
%       In each term S(N,J,K), S is the symbol of a target language
%       that must be defined as a Definite Clause Grammar in
%       test_harness.
%
%       N, J, K, are the numbers of atoms to generate and the minimum
%       (J) and maximum (K) length of strings represented by those
%       atoms. N can be the atom "all", in which case, you guessed it,
%       _all_ examples of strings of S of length between J and K will be
%       generated.
%
%       If Targets is a term Targets/Filter, Filter is an atom, the
%       symbol, but not arity, of a language by which to filter
%       Examples.
%
%       Examples is the list of atoms generated that way. Those are
%       atoms of each S, therefore they are atoms in Definite Clause
%       Grammars notation i.e. they are of the form S(Xs,Ys,...) where
%       Xs, Ys, etc. are lists of characters representing strings in
%       a language.
%
%       If a Filter option is given all examples of the filtering
%       language are removed from Examples before returning.
%
%       Example:
%       ==
%       ?- test_harness:generate_initial(anbn(all,0,12),_Es), maplist(writeln,_Es).
%       s([a,b],[])
%       s([a,a,b,b],[])
%       s([a,a,a,b,b,b],[])
%       s([a,a,a,a,b,b,b,b],[])
%       s([a,a,a,a,a,b,b,b,b,b],[])
%       s([a,a,a,a,a,a,b,b,b,b,b,b],[])
%       true.
%
%       ?- test_harness:generate_initial([anbn(all,0,6),anbm(all,0,3)],_Es)
%       ,maplist(writeln,_Es).
%
%       s([a,b],[])
%       s([a,a,b,b],[])
%       s([a,a,a,b,b,b],[])
%       s([],[])
%       s([a],[])
%       s([a,a],[])
%       s([a,b],[])
%       s([a,a,a],[])
%       s([a,a,b],[])
%       s([a,a,b],[])
%       true.
%       ==
%
%       This predicate calls generate_initial/5 and passes it the name
%       of each tagret language, and the corresponding number, and min
%       and max length of strings to generate.
%
%       Also see filter_by_language/3 for examples of calling
%       generate_initial/2 with a filtering term.
%
generate_initial(T/F,Es_f):-
        !
        ,generate_initial(T,Es)
        ,filter_by_language(F,Es,Es_f).
generate_initial(T,Es):-
        \+ is_list(T)
        ,compound(T)
        ,T =.. [S,N,J,K]
        ,generate_initial(S,N,J,K,Es)
        ,!.
generate_initial(Ts,Es):-
        is_list(Ts)
        ,findall(Es_s
               ,(member(T,Ts)
                ,generate_initial(T,Es_s)
                )
               ,Es_)
        ,flatten(Es_,Es).


%!      filter_by_language(+Filter,+Unlabelled,-Filtered) is det.
%
%       Filter, or not, Unlabelled examples by a target predicate.
%
%       Filter is the name of a DCG defined in test_harness. This
%       predicate will remove from Unlabelled all examples that are also
%       examples of Filter.
%
%       Unlabelled is a list of negative examples of the current target
%       lanuage.
%
%       Filtered is the list Unlabelled with examples of the grammar in
%       Filter removed.
%
%       Example use:
%       ==
%       % No filtering
%       ?- test_harness:generate_initial(anbm(all,0,4),_Es)
%       , maplist(writeln,_Es), length(_Es,N).s([],[])
%       s([a],[])
%       s([a,a],[])
%       s([a,b],[])
%       s([a,a,a],[])
%       s([a,a,b],[])
%       s([a,a,b],[])
%       s([a,a,a,a],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       s([a,a,b,b],[])
%       N = 12.
%
%       % Filtering by anbn; note aabb is gone:
%
%       ?- test_harness:generate_initial(anbm(all,0,4)/anbn,_Es)
%       , maplist(writeln,_Es), length(_Es,N).
%       s([],[])
%       s([a],[])
%       s([a,a],[])
%       s([a,a,a],[])
%       s([a,a,b],[])
%       s([a,a,b],[])
%       s([a,a,a,a],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       N = 10.
%
%       %Filter by anbm itself: everything's gone:
%
%       ?- test_harness:generate_initial(anbm(all,0,4)/anbm,_Es)
%       , maplist(writeln,_Es), length(_Es,N).
%       N = 0.
%       ==
%
filter_by_language(S,Us,Us_f):-
        debug(filter_by_language,'Filtering examples by ~w',[S])
        ,once( internal_symbol(S,S_) )
        ,current_predicate(S,E)
        % S may be S//0 or S//1. Or...?
        ,functor(E,S,A)
        ,length(Args,A)
        % Atom with internal symbol name
        ,E =.. [S|Args]
        ,findall(U
                ,(member(U,Us)
                 % Atom with external symbol name
                 ,U =.. [S_|Args]
                 % Call the internal one
                 ,\+ call(E)
                 )
                ,Us_f)
        % Cuts backtracking over more call(E)results.
        ,!.



%!      generate_initial(+Language,+N,+Min,+Max,-Atoms) is det.
%
%       Generate a set of atoms to use as initial examples in Poker.
%
%       Language is the symbol, but not arity, of the target theory used
%       to generate atoms.
%
%       N is either a number or the atom all. If N is a number, it's the
%       number of atoms to generate. If it is "all", then all atoms with
%       strings of length between Min and Max will be generated.
%
%       Min and Max are the upper and lower bounds on the length of
%       strings, represented as definite clause grammars input lists, in
%       the generated atoms.
%
%       Atoms is the list of generated atoms.
%
%       When N is a number this predicate first generates _all_ atoms
%       with input lists of length between Min and Max, and then samples
%       N of those atoms, with a call to k_list_samples/3. The sampled
%       atoms are returned in Atoms.
%
generate_initial(S,N,J,K,Es):-
        debug(generate_initial,'Generating ~w ~w examples of length in [~w,~w].'
             ,[N,S,J,K])
        ,findall(E
               ,(between(J,K,I)
                ,generate_example(S,I,E)
                )
               ,Es_)
        ,(   number(N)
         ->  k_list_samples(N,Es_,Es)
         ;   N == all
         ->  Es = Es_
         ).


%!      generate_example(+Target,+Length,-Atom) is nondet.
%
%       Generate an Atom with a list of characters of the given Length.
%
%       Variant of generate_example_all/3 used by generate_initial/5.
%
%       @tbd This predicate initially filtered out atoms with strings
%       consisting of a single character, e.g. only 1 or only 0. This is
%       probably no longer a good idea so for now the filtering is
%       commented out, which means this predicate behaves exactly like
%       generate_example_all/3. If filtering is really not needed, the
%       two should be merged.
%
generate_example(S,N,E_):-
        once( internal_symbol(S,S_) )
        ,length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,current_predicate(_,E)
        ,!
        ,call(E)
        ,E_ =.. [S_,Xs,[]].
generate_example(S,N,E_):-
        once( internal_symbol(S,S_) )
        ,length(Is,N)
        ,E =.. [S,Is,Os,[]]
        ,current_predicate(_,E)
        ,call(E)
        ,E_ =.. [S_,Is,Os,[]].


%!      internal_symbol(?Internal,?External) is semidet.
%
%       Mapping between names of External and Internal predicate defs.
%
%       Needed because the grammars used to generate ground-truth
%       strings are given more descriptive names than the ones in
%       experiment files. Experiment files are putting it on to
%       underline the self-supervised capabilities of Poker.
%
%       Raises type error if Internal is not a language defined in this
%       module.
%
internal_symbol(L,S):-
        internal_symbol_(L,S)
        ,!.
internal_symbol(L,_S):-
        findall(Li
               ,internal_symbol_(Li,_)
               ,Ls)
        ,writeln(Ls)
        ,must_be(oneof(Ls),L).

%!      internal_symbol_(?Language,?Symbol) is semidet.
%
%       Business end of internal_symbol/2.
%
internal_symbol_(even,q0).
internal_symbol_(odd,q0).
internal_symbol_(palindrome,q0).
internal_symbol_(not_palindrome,q0).
internal_symbol_(anbn,s).
internal_symbol_(not_anbn,s).
internal_symbol_(anbm,s).
internal_symbol_(not_anbm,s).
internal_symbol_(anbn_uo,s).
internal_symbol_(not_anbn_uo,s).
internal_symbol_(parens,p).
internal_symbol_(unbalanced_parens,p).
internal_symbol_(not,q0).
internal_symbol_(not_not,q0).
internal_symbol_(algae,s).
internal_symbol_(not_algae,s).
internal_symbol_(fractal_plant,s).
internal_symbol_(not_fractal_plant,s).
internal_symbol_(dragon_curve,s).
internal_symbol_(not_dragon_curve,s).
internal_symbol_(koch_curve,s).
internal_symbol_(koch_curve_with_vars,s).
internal_symbol_(not_koch_curve,s).
internal_symbol_(hilbert_curve,s).
internal_symbol_(hilbert_curve_with_vars,s).
internal_symbol_(not_hilbert_curve,s).
internal_symbol_(sierpinski_triangle,s).
internal_symbol_(not_sierpinski_triangle,s).
internal_symbol_(sierpinski_arrowhead,s).
internal_symbol_(not_sierpinski_arrowhead,s).



                /*******************************
                *          EVALUATION          *
                *******************************/


%!      accuracy(+Module,+Target,+Pos,+Neg,-Accuracy) is det.
%
%       Calculate the Accuracy of labelling a set of atoms.
%
accuracy(M,S,[],Neg,Acc):-
        !
        ,tnr(M,S,Neg,Acc).
accuracy(M,S,Pos,[],Acc):-
        !
        ,tpr(M,S,Pos,Acc).
accuracy(PM,S,Pos,Neg,Acc):-
        true_positives(PM,S,Pos,TP)
        ,true_negatives(PM,S,Neg,TN)
        ,maplist(length,[Pos,Neg],[N,M])
        ,Acc_ is (TP + TN) / (N + M)
        ,atomize(Acc_,Acc).


%!      tpr(+Module,+Target,+Pos,-TPR) is det.
%
%       Calculate the True Positive Rate of labelling a set of atoms.
%
tpr(_M,_S,[],0):-
        !.
tpr(M,S,Pos,TPR):-
        true_positives(M,S,Pos,TP)
        ,length(Pos,N)
        ,TPR_ is TP / N
        ,atomize(TPR_,TPR).


%!      tnr(+Module,+Target,+Neg,-TNR) is det.
%
%       Calculate the True Negative Rate of labelling a set of atoms.
%
tnr(_M,_S,[],0):-
        !.
tnr(M,S,Neg,TNR):-
        true_negatives(M,S,Neg,TN)
        ,length(Neg,N)
        ,TNR_ is TN / N
        ,atomize(TNR_,TNR).


%!      true_positives(+Module,+Target,+Pos,-True) is det.
%
%       Collect all True positives in a set of atoms labelled positive.
%
true_positives(_M,_S,[],0):-
        !.
true_positives(M,S,Pos,TP):-
        aggregate_all(count
                     ,(member(Ep,Pos)
                      ,Ep =.. [_S|As]
                      ,Ep_ =.. [S|As]
                      ,once( call(M:Ep_) )
                      )
                     ,TP).


%!      true_negatives(+Module,+Target,+Neg,-True) is det.
%
%       Collect all True positives in a set of atoms labelled negative.
%
true_negatives(_M,_,[],0):-
        !.
true_negatives(M,S,Neg,TN):-
        aggregate_all(count
                     ,(member(En,Neg)
                      ,En =.. [_S|As]
                      ,En_ =.. [S|As]
                      ,\+ once( M:call(En_) )
                      )
                     ,TN).


%!      atomize(+Number,-Formatted) is det.
%
%       Format a Number into a float with 4 decimal digits.
%
atomize(N,An):-
        format(atom(A),'~4f',[N])
        ,atom_number(A,An).



                /*******************************
                *       TARGET LANGUAGES       *
                *******************************/


%!      even(+Input,-Output) is nondet.
%
%       A grammar accepting bit strings with an even number of 1s.
%
%       Equivalent to the following grammar in DCG notation:
%       ==
%       q0 --> [].
%       q0 --> zero, q0.
%       q0 --> one, q1.
%       q1 --> zero --> q1.
%       q1 --> one --> q0.
%       ==
%
%       Input is a list of 1s and 0s, with an even number of 1s and any
%       number of 0s, including none, representing even parity. The
%       empty string is even.
%
%       Output is the remainder of the string in Input once all
%       characters are consumed by the grammar. Output should be [] when
%       the Input string has an even number of 1s.
%
even(X,Y):- empty(X,Y).
even(X,Y):- zero(X,Z), even(Z,Y).
even(X,Y):- one(X,Z), odd(Z,Y).
odd(X,Y):- zero(X,Z), odd(Z,Y).
odd(X,Y):- one(X,Z), even(Z,Y).

zero --> [0].
one --> [1].
empty --> [].


%!      palindrome is nondet.
%
%       A grammar for the language of palindromic bit-strings.
%
palindrome --> empty.
palindrome --> one.
palindrome --> zero.
palindrome --> one, palindrome, one.
palindrome --> zero, palindrome, zero.


%!      not_palindrome is nondet.
%
%       A grammar for the language of non-palindromic bit-strings.
%
not_palindrome --> bit_string(Ss), { \+ phrase(palindrome, Ss) }.

bit_string([B]) --> bit(B).
bit_string([B|Bs]) --> bit(B), bit_string(Bs).

bit(1) --> [1].
bit(0) --> [0].


%!      s0 is nondet.
%
%       Grammar for the a^nb^n context-free language.
%
anbn --> a,b.
anbn --> a,anbn,b.

a --> [a].
b --> [b].


%!      not_anbn is nondet.
%
%       A grammar for the language of non-a^nb^n a-b strings.
%
not_anbn --> ab_string(Ss), { \+ phrase(anbn,Ss) }.

ab_string([S]) --> ab(S).
ab_string([S|Ss]) --> ab(S), ab_string(Ss).

ab(a) --> a.
ab(b) --> b.


% Grammar for the language {a^nb^m|n >= m >= 0}
anbm --> empty.
anbm --> a,anbm.
anbm --> a,anbm,b.

%!      not_anbn is nondet.
%
%       A grammar for the language of non-a^nb^n a-b strings.
%
not_anbm --> ab_string(Ss), { \+ phrase(anbm,Ss) }.


% "anbn_uo" stands for "anbn unordered"
% Grammar for the language of equal numbers of as and bs in any order.
% L = {w in {a,b}* | n_a(w) = n_b(w)} (n_a is n with underscore a, so
% "the n for a").
:- table anbn_uo/2. % Because left-recursive.

anbn_uo --> empty.
anbn_uo --> a,anbn_uo,b.
anbn_uo --> b,anbn_uo,a.
anbn_uo --> anbn_uo, anbn_uo.

not_anbn_uo --> ab_string(Ss), { \+ phrase(anbn_uo,Ss) }.


% Language of balanced parentheses.

:- table parens/2.

parens --> empty.
parens --> lp, parens, rp.
parens --> parens, parens.

lp --> ['('].
rp --> [')'].


unbalanced_parens --> paren_string(Ss), { \+ phrase(parens,Ss) }.

paren_string([S]) --> paren(S).
paren_string([S|Ss]) --> paren(S), paren_string(Ss).

paren('(') --> ['('].
paren(')') --> [')'].


%!      not is nondet.
%
%       Bit string inverter grammar
%
not([1]) --> zero.
not([0]) --> one.
not([1|Bs]) --> zero, not(Bs).
not([0|Bs]) --> one, not(Bs).


%!      not_not is nondet.
%
%       Bit string inverter inverter grammar
%
not_not([0]) --> zero.
not_not([1]) --> one.
not_not([0|Bs]) --> zero, not_not(Bs).
not_not([1|Bs]) --> one, not_not(Bs).



                /*******************************
                *          L-SYSTEMS           *
                *******************************/


%!      algae// is nondet.
%
%       Algae L-System as a context-free grammar.
%
%       Lindemayer's book ("The algorithmic beauty of plans") claims
%       that L-Systems are not context-free grammars, because at each
%       iteration all new tokens placed on the grammar stack are
%       processed "simultaneously".
%
%       That seems to be right, after all. Treating the algae grammar as
%       a Context-Free Grammar, here implemented in Definite Clause
%       Grammars form, results in a grammar that can only perform one
%       iteration of generation. On the other hand, this grammar can
%       start at any point in the generation, taking as input the algae
%       string at iteration k, and outputing the string at iteration
%       k+1.
%
%       Example query:
%       ==
%       ?- phrase(test_harness:algae(Ss),[a,b,a]).
%       Ss = [a,b,a,a,b] ;
%       false.
%
%       % You can also run the grammar backwards to go one iteration back:
%       ?- phrase(test_harness:algae([a,b,a]),Ss).
%       Ss = [a,b] ;
%       false.
%       ==
%
%       __Implementation Notes__
%
%       The DCG below is indeed a direct translation of the rules:
%       ==
%       A -> AB
%       B -> A
%       ==
%
%       And so it's missing the iteration counter that controls the
%       number of iterations. Parsing ends when the entire input string
%       (in the second variable of phrase/2) is consumed. It is possible
%       to add a counter if desired, but that's left as an exercise to
%       the reader.
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_1:_algae
%
algae([a,b|Ss]) --> a, algae(Ss).
algae([a|Ss]) --> b, algae(Ss).
algae([]) --> [].


%!      not_algae(Ss) is nondet.
%
%       Not an algage grammar.
%
%       Not great though.
%
not_algae(Ss) -->
        ab_string(Ss)
        ,{  \+ phrase(algae(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(ab_string(Xs),_)
         }
        ,Xs.


%!      fractal_tree(?String) is semidet.
%
%       Calculate a fractal tree String.
%
%       Mnyeah, dunno. This one's not very clear.
%
%       Axiom: [0]
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_2:_fractal_(binary)_tree
%
fractal_tree(['['|Ss]) --> lsb,  fractal_tree(Ss).
fractal_tree([']'|Ss]) --> rsb,  fractal_tree(Ss).
fractal_tree([1,1|Ss]) --> one,  fractal_tree(Ss).
fractal_tree([1,'[',0,']',0|Ss]) --> zero,  fractal_tree(Ss).
fractal_tree([]) --> [].



                /*******************************
                *      L-SYSTEM FRACTALS       *
                *******************************/


% Vocabulary, used to command a turtle.
% Note: b//0 is not declared here because it is already declared earlier
% but when used in an L-System grammar it is interpreted as "move
% backwards"


%!      lsb// is semidet.
%
%       Push current position and angle on the stack.
%
lsb --> ['['].

%!      rsb// is semidet.
%
%       Pop position and angle from the stack.
%
rsb --> [']'].

%!      f// is semidet.
%
%       Move the turtle forward in the current heading.
%
%       Synonymous to g//0.
%
f --> [f].

%!      g// is semidet.
%
%       Move the turtle forward in the current heading.
%
%       Synonymous to f//0.
%
g --> [g].


%!      plus// is semidet.
%
%       Turn left by the current angle setting.
%
plus --> [+].


%!      minus// is semidet.
%
%       Turn right by the current angle setting.
%
minus --> [-].

%!      x// is semidet.
%
%       Ignored, used to control evolution of shape.
%
x --> [x].


%!      y// is semidet.
%
%       Ignored, used to control evolution of shape.
%
y --> [y].


%!      dragon_curve(?String) is semidet.
%
%       Calculate a Dragon Curve string.
%
%       Axiom: [f]
%       Angle: 90
%
%       Draw with:
%       _S = dragon_curve, _K = 15, _A = [f], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ),atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,90,90,2,'center').
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_6:_dragon_curve
%
dragon_curve([+|Ss])--> plus, dragon_curve(Ss).
dragon_curve([-|Ss])--> minus, dragon_curve(Ss).
dragon_curve([f,+,g|Ss])--> f, dragon_curve(Ss).
dragon_curve([f,-,g|Ss])--> g, dragon_curve(Ss).
dragon_curve([])--> [].


%!      not_dragon_curve(?String) is semidet.
%
%       Not a dragon curve. Honest.
%
not_dragon_curve(Ss) -->
        dragon_string(Ss)
        ,{  \+ phrase(dragon_curve(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(dragon_string(Xs),_)
         }
        ,Xs.

dragon_string([C]) --> dragon_char(C).
dragon_string([C|Ss]) --> dragon_char(C), dragon_string(Ss).

dragon_char(+) --> plus.
dragon_char(-) --> minus.
dragon_char(f) --> f.
dragon_char(g) --> g.



%!      koch_curve(?String) is semidet.
%
%       Calculate a Koch Curve String.
%
%       Axiom: [f,-,-,f,-,-,f]
%       Alt:   [f,+,+,f,+,+,f]
%       Angle: 90
%
%       Draw with:
%       _S = koch_curve, _K = 6, _A = [f,-,-,f,-,-,f], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,60,60,1,'bottom_center').
%
%       Sources:
%       https://en.wikipedia.org/wiki/L-system#Example_4:_Koch_curve
%       https://en.wikipedia.org/wiki/Koch_snowflake#Representation_as_Lindenmayer_system
%
koch_curve([+|Ss])--> plus, koch_curve(Ss).
koch_curve([-|Ss])--> minus, koch_curve(Ss).
koch_curve([f,+,f,-,-,f,+,f|Ss]) --> f, koch_curve(Ss).
% Variants - which ones?
% Square:
%koch_curve([f,+,f,-,f,-,f,+,f|Ss]) --> f, koch_curve(Ss).
%koch_curve([f,-,f,+,+,f,-,f|Ss]) --> f, koch_curve(Ss).
koch_curve([]) --> [].


koch_curve_with_vars(Is,Os,[]):-
% The first Koch Curve string that contains variable symbols has
% length 8.
        generate_initial(koch_curve,all,8,9,Es)
        ,findall(s(Is,Os,[])
                ,(member(s(Is,Os,[]),Es)
                 ,member(f,Is)
                 )
                ,Vs)
        ,member(s(Is,Os,[]),Vs).



%!      hilbert_curve(?String) is semidet.
%
%       Calculate a Hilbert Curve String.
%
%       Axiom: [x]
%       Angle: 90
%
%       Draw with:
%       _S = hilbert_curve, _K = 7, _A = [x], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,90,90,8,'bottom_left').
%
%       Source:
%       https://en.wikipedia.org/wiki/Hilbert_curve#Representation_as_Lindenmayer_system
%
hilbert_curve([+|Ss])--> plus, hilbert_curve(Ss).
hilbert_curve([-|Ss])--> minus, hilbert_curve(Ss).
hilbert_curve([f|Ss])--> f, hilbert_curve(Ss).
hilbert_curve([+,y,f,-,x,f,x,-,f,y,+|Ss]) --> x, hilbert_curve(Ss).
hilbert_curve([-,x,f,+,y,f,y,+,f,x,-|Ss]) --> y, hilbert_curve(Ss).
hilbert_curve([]) --> [].

not_hilbert_curve(Ss) -->
        hilbert_string(Ss)
        ,{  \+ phrase(hilbert_curve(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(hilbert_string(Xs),_)
         }
        ,Xs.

hilbert_string([C]) --> hilbert_char(C).
hilbert_string([C|Ss]) --> hilbert_char(C), hilbert_string(Ss).

hilbert_char(+) --> plus.
hilbert_char(-) --> minus.
hilbert_char(f) --> f.
hilbert_char(x) --> x.
hilbert_char(y) --> y.


%!      hilbert_curve_with_vars(?Is,?Os,?Rs) is nondet.
%
%       Generator for Hilbert Curve strings with variable symbols.
%
%       Hilbert Curve strings begin including variable symbols at length
%       11. This predicate ensures that examples can be generated that
%       include variables.
%
hilbert_curve_with_vars(Is,Os,[]):-
% The first Hilbert Curve string that contains variable symbols has
% length 11.
        generate_initial(hilbert_curve,all,11,14,Es)
        ,findall(s(Is,Os,[])
                ,(member(s(Is,Os,[]),Es)
                 ,( member(x,Is)
                  ; member(y,Is)
                  )
                 )
                ,Vs)
        ,member(s(Is,Os,[]),Vs).



%!      sierpinski_triangle(?String) is semidet.
%
%       Calculate a Sierpinski Triangle string.
%
%       Axiom: [f,-,g,-,g]
%       Angle: 120
%
%       Draw with:
%       _S = sierpinski_triangle, _K = 6, _A = [f,-,g,-,g], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,120,120,6,'center').
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_5:_Sierpinski_triangle
%
%       Also see:
%       https://en.wikipedia.org/wiki/Sierpi%C5%84ski_curve#Representation_as_Lindenmayer_system
%
sierpinski_triangle([+|Ss])--> plus, sierpinski_triangle(Ss).
sierpinski_triangle([-|Ss])--> minus, sierpinski_triangle(Ss).
sierpinski_triangle([f,-,g,+,f,+,g,-,f|Ss]) --> f, sierpinski_triangle(Ss).
sierpinski_triangle([g,g|Ss]) --> g, sierpinski_triangle(Ss).
sierpinski_triangle([]) --> [].

not_sierpinski_triangle(Ss) -->
        sierpinski_string(Ss)
        ,{  \+ phrase(sierpinski_triangle(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(sierpinski_string(Xs),_)
         }
        ,Xs.

sierpinski_string([C]) --> sierpinski_char(C).
sierpinski_string([C|Ss]) --> sierpinski_char(C), sierpinski_string(Ss).

sierpinski_char(+) --> plus.
sierpinski_char(-) --> minus.
sierpinski_char(f) --> f.
sierpinski_char(g) --> g.



%!      sierpinski_arrowhead(?String) is semidet.
%
%       Calculate a Sierpinski Arrowhead String.
%
%       Axiom: [x,f]
%       Angle: 60
%
%       Draw with:
%       _S = sierpinski_arrowhead, _K = 8, _A = [x,f], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,60,60,4,'bottom_center').
%
%       Source:
%       https://en.wikipedia.org/wiki/Sierpi%C5%84ski_curve#Representation_as_Lindenmayer_system_2
%
sierpinski_arrowhead([+|Ss])--> plus, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([-|Ss])--> minus, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([f|Ss])--> f, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([y,f,+,x,f,+,y|Ss])--> x, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([x,f,-,y,f,-,x|Ss])--> y, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([]) --> [].



                /*******************************
                *       L-SYSTEMS PLANTS       *
                *******************************/


%!      fractal_plant(?String) is semidet.
%
%       Calculate a fractal plant String.
%
%       This is the classic L-System that draws a grass-like shape.
%
%       Axiom: [-,x]
%       Angle: 25
%
%       Draw with:
%       _S = fractal_plant, _K = 8, _A = [-,x], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,25,25,1.5,'bottom_left').
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_7:_fractal_plant
%
fractal_plant([+|Ss]) --> plus, fractal_plant(Ss).
fractal_plant([-|Ss]) --> minus, fractal_plant(Ss).
fractal_plant(['['|Ss]) --> lsb,  fractal_plant(Ss).
fractal_plant([']'|Ss]) --> rsb,  fractal_plant(Ss).
fractal_plant([f,f|Ss]) --> f,  fractal_plant(Ss).
fractal_plant([f,+,'[','[',x,']',-,x,']',-,f,'[',-,f,x,']',+,x|Ss]) -->
        x,
        fractal_plant(Ss).
fractal_plant([]) --> [].



                /*******************************
                *L-SYSTEMS PARSING AND DRAWING *
                *******************************/


%!      draw(+String,+LeftAngle,+RightAngle,+Distance,+Start) is det.
%
%       Draw an L-System String to screen with Turtle graphics.
%
%       String is an atom, an L-system string in atomic form.
%
%       LeftAngle is the angle for left turns when changing the
%       turtle's heading.
%
%       RightAngle is the angle for right turns.
%
%       Distance is the amount (of pixels, I guess) the turtle moves
%       across the screen in its current heading.
%
%       Start is an atom denoting the initial position of the turtle:
%       * 'center': start at dead center on screen
%       * 'bottom_right': duh
%       * 'bottom_left': also
%
%       Tweaking LeftAngle and RightAngle can rotate a shape in space,
%       e.g. the fractal plant controls how slanted the main stem of the
%       plant is.
%
draw(Is,S,A,D,P):-
        py_call(turtle_mapping:draw(Is,S,A,D,P)).


%!      test_draw(+Tgt,+H,+I,+Axiom,+Right,+Left,+Dist,+Start) is
%!      det.
%
%       Draw an L-System string generated by a learned hypothesis.
%
%       Tgt is a predicate indicator, F/A, of a learning target.
%
%       H is the hypothesis, a learned logic program definition of Tgt.
%       H should be a learned grammar of an L-System, in DCG form
%       (unsugared).
%
%       I is an integer, the maximum iteration for L-System iteration.
%
%       Axiom is a list of characters, the input string of the L-System
%       grammar defined in H.
%
%       Right and Left are the angles for left and right turns of the
%       cursor drawing the L-System (a turtle-graphics cursor).
%
%       Distance is the distance covered by the cursor when moving in
%       a straight line (in pixels).
%
%       Start is an atom denoting the initial position of the cursor.
%       See draw/5 for options.
%
test_draw(S/Ar,Ps,I,Ax,RA,LA,D,St):-
        PM = l_systems
        ,experiment_data(S/Ar,_Ls,_Us,B,_MS)
        ,closure(B,experiment_file,Bs)
        ,flatten(Bs,Bs_f)
        ,Sup = maplist(assert_program(PM),[Ps,Bs_f],[Rs_1,Rs_2])
        ,G = once( l_system(S,PM,Ax,I,Ss) )
        ,C = maplist(erase_program_clauses,[Rs_1,Rs_2])
        ,setup_call_cleanup(Sup,G,C)
        ,atomic_list_concat(Ss,'',Ss_)
        ,draw(Ss_,LA,RA,D,St).


%!      l_system(+Symbol,+Module,+Input,+Iteration,-Output) is det.
%
%       L-system interpreter.
%
%       Symbol is the starting symbol of an L-System grammar.
%
%       Module is the module where the L-System grammar is defined.
%
%       Input is a list of terminals accepted by an L-system grammar,
%       given as an initial state.
%
%       Iteration is an integer, the final iteration of generation
%       following from the initial state given as Input.
%
%       Output is the output string of the L-System at the given
%       Iteration.
%
%       An L-System grammar must be loaded in memory as a set of DCG
%       rules with the start symbol Symbol//1 and accessible from the
%       named Module.
%
%       Examples:
%       ==
%       ?- test_harness:l_system(algae,test_harness,[a],3,Ss).
%       Ss = [a,b,a,a,b] ;
%       false.
%
%       ?- test_harness:l_system(algae,test_harness,[a,b,a,a,b],1,Ss).
%       Ss = [a,b,a,a,b,a,b,a] ;
%       false.
%
%       ?- test_harness:l_system(algae,test_harness,[a],4,Ss).
%       Ss = [a,b,a,a,b,a,b,a] ;
%       false.
%       ==
%
l_system(_S,_M,Os,0,Os):-
        !.
l_system(S,M,Is,N,Os):-
        S_ =.. [S,Ss]
	,phrase(M:S_, Is)
        ,N_ is N - 1
        ,l_system(S,M,Ss,N_,Os).


                /*******************************
                *  I JUST LIKE CTRL+C-CTRL+H   *
                *******************************/

