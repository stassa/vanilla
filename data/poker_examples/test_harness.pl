:-module(test_harness,[experiments/6
                      ,experiment/5
                      ,generate_initial/5
                      ,generate_all_initial/4
                      ]).

:-use_module(lib(poker/poker)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(src(auxiliaries)).
:-use_module(lib(poker/sampling/sampling)).

/** <module> A test harness for Poker.

Run experiments evaluating the labelling of initial and new atoms
labelled by Poker.

*/

%!      experiments(+Target,+N,+M,+J,+K,-Means) is det.
%
%       Run N experiments learning a Target predicate.
%
%       Target is the symbol, but not arity, of the target theory used
%       to verify examples learned by Poker.
%
%       N is the number of experiments to run.
%
%       M is the number of initial examples to generate, with
%       generate_initial/5.
%
%       J and K are the upper and lower bounds of the length of input
%       lists in generated initial example strings of Target. Each
%       initial example will have an input list of length between J and
%       K.
%
%       Means is a list [Acc,TPR,TNR], where each is the mean accuracy,
%       true positive rate and true negative rate of the N hypotheses
%       learned from the sets of N examples generated in the N steps of
%       the experiment.
%
%       This predicate runs N experiments calling experiment/5 with
%       Target, M, J, and K as input.
%
experiments(S,N,M,J,K,Ms):-
        findall([Acc,TPR,TNR]
               ,(between(1,N,I)
                ,debug(experiments,'Experiment ~w of ~w',[I,N])
                ,experiment(S,M,J,K,[_Ps,_Pos,_Neg,Acc,TPR,TNR])
                )
               ,Rs)
        ,result_means(Rs,Ms).


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


%!      experiment(+Target,+N,+J,+K,-Results) is det.
%
%       Run an experiment learning a Target predicate.
%
%       Target is the symbol, but not arity, of the target theory used
%       to verify examples learned by Poker.
%
%       M is the number of initial examples to generate, with a call to
%       generate_initial/5.
%
%       J and K are the upper and lower bounds of the length of input
%       lists in in generated initial examples of strings of the Target
%       language. Each initial example will have an input list of length
%       between J and K.
%
%       Results is a list [Ps,Pos,Neg,Acc,TPR,TNR], where:
%       * Ps is the learned hypothesis
%       * Pos is the list of positive examples identified
%       * Neg is the list of negative examples identified
%       * Acc is the accuracy of the labelling of atoms in Pos and Neg.
%       * TPR is the True Positive Rate of the labelling in Pos and Neg.
%       * TNR is the True Negative Rate of the labelling in Pos and Neg.
%
experiment(S,N,J,K,[Ps,Pos,Neg,Acc,TPR,TNR]):-
        generate_initial(S,N,J,K,Es)
        %,debug_clauses(experiment_initial,'Generated initial examples.',Es)
        ,debug_clauses_length(experiment_initial,'Generated ~w initial examples:',Es)
        ,time( learn(Es,Pos,Neg,Ps) )
        ,debug_clauses(experiment_learned,'Learned hypothesis:',Ps)
        ,debug_clauses_length(experiment_examples,'~w Positive examples:',Pos)
        ,debug_clauses_length(experiment_examples,'~w Negative examples:',Neg)
        ,accuracy(S,Pos,Neg,Acc)
        ,tpr(S,Pos,TPR)
        ,tnr(S,Neg,TNR)
        ,debug(experiment_result,'Measured Acc: ~w TPR: ~w TNR: ~w',[Acc,TPR,TNR]).


%!      accuracy(Target,+Pos,+Neg,-Accuracy) is det.
%
%       Calculate the Accuracy of labelling a set of atoms.
%
accuracy(S,[],Neg,Acc):-
        !
        ,tnr(S,Neg,Acc).
accuracy(S,Pos,[],Acc):-
        !
        ,tpr(S,Pos,Acc).
accuracy(S,Pos,Neg,Acc):-
        true_positives(S,Pos,TP)
        ,true_negatives(S,Neg,TN)
        ,maplist(length,[Pos,Neg],[N,M])
        ,Acc_ is (TP + TN) / (N + M)
        ,atomize(Acc_,Acc).


%!      tpr(Target,+Pos,-TPR) is det.
%
%       Calculate the True Positive Rate of labelling a set of atoms.
%
tpr(_S,[],0):-
        !.
tpr(S,Pos,TPR):-
        true_positives(S,Pos,TP)
        ,length(Pos,N)
        ,TPR_ is TP / N
        ,atomize(TPR_,TPR).


%!      tnr(Target,+Neg,-TNR) is det.
%
%       Calculate the True Negative Rate of labelling a set of atoms.
%
tnr(_S,[],0):-
        !.
tnr(S,Neg,TNR):-
        true_negatives(S,Neg,TN)
        ,length(Neg,N)
        ,TNR_ is TN / N
        ,atomize(TNR_,TNR).


%!      true_positives(+Target,+Pos,-True) is det.
%
%       Collect all True positives in a set of atoms labelled positive.
%
true_positives(_S,[],0):-
        !.
true_positives(S,Pos,TP):-
        aggregate_all(count
                     ,(member(Ep,Pos)
                      ,Ep =.. [_S|As]
                      ,Ep_ =.. [S|As]
                      ,call(Ep_)
                      )
                     ,TP).


%!      true_negatives(+Target,+Neg,-True) is det.
%
%       Collect all True positives in a set of atoms labelled negative.
%
true_negatives(_,[],0):-
        !.
true_negatives(S,Neg,TN):-
        aggregate_all(count
                     ,(member(En,Neg)
                      ,En =.. [_S|As]
                      ,En_ =.. [S|As]
                      ,\+ call(En_)
                      )
                     ,TN).


%!      atomize(+Number,-Formatted) is det.
%
%       Format a Number into a float with 4 decimal digits.
%
atomize(N,An):-
        format(atom(A),'~4f',[N])
        ,atom_number(A,An).


%!      generate_initial(+Target,+N,+Min,+Max,-Atoms) is det.
%
%       Generate a set of atoms to use as initial examples in Poker.
%
%       Target is the symbol, but not arity, of the target theory used
%       to generate atoms.
%
%       N is the number of atoms to generate.
%
%       Min and Max are the upper and lower bounds on the length of
%       strings, represented as definite clause grammars input lists, in
%       the generated atoms.
%
%       Atoms is the list of generated atoms.
%
%       This predicate first generates _all_ atoms with input lists of
%       length between Min and Max, and then samples N of those atoms,
%       with a call to k_list_samples/3. The sampled atoms are returned
%       in Atoms.
%
generate_initial(S,N,J,K,Es):-
        findall(E
               ,(between(J,K,I)
                ,generate_example(S,I,E)
                )
               ,Es_)
        ,k_list_samples(N,Es_,Es).


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
generate_example(S,N,q0(Xs,[])):-
        length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(E)
        % Avoid all-0', all-1 strings.
        %,\+ sort(Xs,[_])
        .


%!      generate_all_initial(+Target,+Min,+Max,-Atoms) is det.
%
%       Generate all strings of Target with length between Min, Max.
%
%       Target is the symbol, but not arity, of the target theory used
%       to generate atoms.
%
%       Min and Max are the upper and lower bounds on the length of
%       strings, represented as definite clause grammars input lists, in
%       the generated atoms.
%
%       Atoms is the list of generated atoms.
%
%       This predicate is similar to generate_initial/4 but generates
%       _all_ atoms with input lists of length between Min and Max,
%       without sampling.
%
generate_all_initial(S,J,K,Es):-
        findall(E
               ,(between(J,K,I)
                ,generate_example_all(S,I,E)
                )
               ,Es).


%!      generate_example_all(+Target,+Length,-Atom) is nondet.
%
%       Generate an Atom with a list of characters of the given Length.
%
generate_example_all(S,N,q0(Xs,[])):-
        length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(E).



:-table even/2, odd/2.

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


%!      palindrome//0 is nondet.
%
%       A grammar for the language of palindromic bit-strings.
%
palindrome --> empty.
palindrome --> one.
palindrome --> zero.
palindrome --> one, palindrome, one.
palindrome --> zero, palindrome, zero.
