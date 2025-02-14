:-module(test_harness,[experiments/6
                      ,experiment/5
                      ,generate_initial/5
                      ,generate_all_initial/4
                      ,test_program/3
                      ]).

:-use_module(lib(poker/poker)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(src(auxiliaries)).
:-use_module(lib(poker/sampling/sampling)).

/** <module> A test harness for Poker.

Run experiments evaluating the labelling of initial and new atoms
labelled by Poker, or the programs it learns to label them.

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
        (   number(N)
        ->  generate_initial(S,N,J,K,Es)
        ;   N == all
        ->  generate_all_initial(S,J,K,Es)
        )
        ,debug_clauses_length(experiment_initial,'Generated ~w initial examples:',Es)
        ,time( learn(Es,Pos,Neg,Ps) )
        ,debug_clauses(experiment_learned,'Learned hypothesis:',Ps)
        ,debug_clauses_length(experiment_examples,'~w Positive examples:',Pos)
        ,debug_clauses_length(experiment_examples,'~w Negative examples:',Neg)
        ,accuracy(test_harness,S,Pos,Neg,Acc)
        ,tpr(test_harness,S,Pos,TPR)
        ,tnr(test_harness,S,Neg,TNR)
        ,debug(experiment_result,'Measured Acc: ~w TPR: ~w TNR: ~w',[Acc,TPR,TNR]).


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
                      ,\+ M:call(En_)
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
%       @tbd This and generate_all_initial can be merged with N
%       accepting the value "all". Just saying.
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
generate_example(S,N,E_):-
        internal_symbol(S,S_)
        ,length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(E)
        ,E_ =.. [S_,Xs,[]].


%!      internal_symbol(?Internal,?External) is semidet.
%
%       Mapping between names of External and Internal predicate defs.
%
%       Needed because the grammars used to generate ground-truth
%       strings are given more descriptive names than the ones in
%       experiment files. Experiment files are putting it on to
%       underline the self-supervised capabilities of Poker.
%
internal_symbol(even,q0).
internal_symbol(palindrome,q0).
internal_symbol(anbn,s).



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
generate_example_all(S,N,E_):-
        internal_symbol(S,S_)
        ,length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(E)
        ,E_ =.. [S_,Xs,[]].



%!      test_program(+Language,+Program,-Results) is det.
%
%       Test a Program on positive and negative examples.
%
%       Language is a predicate symbol, but not arity, of the grammar to
%       use to generate positive and negative example strings.
%
%       Program is the learned program to test against the examples
%       generated by Language.
%
%       Results is a list [Acc, TPR, TNR], the accuracy, True Positive
%       Rate and True Negative Rate, respectively, of the Program's
%       labelling of examples generated by Language.
%
test_program(T,Cs,[Acc,TPR,TNR]):-
        Program_module = experiment_file
        ,S = assert_program(Program_module,Cs,Rs)
        ,G = (generate_positives(Pos)
             ,generate_negatives(Neg)
             ,accuracy(Program_module,T,Pos,Neg,Acc)
             ,tpr(Program_module,T,Pos,TPR)
             ,tnr(Program_module,T,Neg,TNR)
             )
        ,C = erase_program_clauses(Rs)
        ,setup_call_cleanup(S,G,C).


%!      generate_positives(-Examples) is det.
%
%       Generate positive Examples to test a learned program.
%
%       @tbd You don't need two predicates to generate positives and
%       negatives- you can use the first argument of
%       generate_examples/6.
%
generate_positives(Pos):-
        experiment_file:generate_examples(pos,N,S,_N,J,K)
        ,(   number(N)
         ->  generate_initial(S,N,J,K,Pos)
         ;   N == all
         ->   generate_all_initial(S,J,K,Pos)
         ).


%!      generate_negatives(-Examples) is det.
%
%       Generate negative Examples to test a learned program.
%
generate_negatives(Neg):-
        experiment_file:generate_examples(neg,N,S,_N,J,K)
        ,(   number(N)
         ->  generate_initial(S,N,J,K,Neg)
         ;   N == all
         ->   generate_all_initial(S,J,K,Neg)
         ).



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


%!      not_s0 is nondet.
%
%       A grammar for the language of non-a^nb^n a-b strings.
%
not_anbn --> ab_string(Ss), { \+ phrase(anbn,Ss) }.

ab_string([S]) --> ab(S).
ab_string([S|Ss]) --> ab(S), ab_string(Ss).

ab(a) --> a.
ab(b) --> b.


% Grammar for the langauge {a^nb^n|n >= m >= 0}
s1 --> a,s1,b.
s1 --> a,s1.
s1 --> empty.

a --> [a].
b --> [b].

% Grammar for the language of equal numbers of as and bs in any order.
% L = {w in {a,b}* | n_a(w) = n_b(w)} (n_a is n with underscore a, so
% "the n for a".
s2 --> a,s2,b.
s2 --> b,s2,a.
s2 --> s2, s2.
s2 --> empty.

% Language of balanced parentheses.
parens --> lp, parens, rp.
parens --> parens, parens.
parens --> empty.

lp --> ['('].
rp --> [')'].
