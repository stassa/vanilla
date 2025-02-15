:-module(test_harness,[experiments/6
                      ,experiment/5
                      ,generate_initial/5
                      ,test_labelling/4
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
%       Run N experiments learning a program and labelling with Poker.
%
%       Language is the symbol, but not arity, of the target theory used
%       to verify examples learned by Poker.
%
%       N is the number of experiments to run.
%
%       M is the number of initial (training) examples to generate, with
%       generate_initial/5.
%
%       J and K are the upper and lower bounds of the length of input
%       lists in generated initial example strings of Language. Examples
%       are in atomic Definite Clause Grammars form, with two lists:
%       an input list and output list; like this- p(Xs,[]), where Xs is
%       the input list and [] the output list. Each initial example will
%       have an input list of length between J and K.
%
%       Means is a list of two lists [Ms_L, Ms_P], each of which is a
%       list [Acc,TPR,TNR], where Acc is the mean accuracy, TPR the
%       mean true positive rate and TNR the mean true negative rate of
%       the N hypotheses learned from the sets of N examples generated
%       in the N steps of the experiment. The first sub-list, Ms_L holds
%       the means of the labelling results, and Ms_P holds the means of
%       the program testing results.
%
%       This predicate runs N experiments calling experiment/5 with
%       Target, M, J, and K as input.
%
experiments(S,N,M,J,K,[Ms_l,Ms_p]):-
        findall(Res_l-Res_p
               ,(between(1,N,I)
                ,debug(experiments,'Experiment ~w of ~w',[I,N])
                ,experiment(S,M,J,K,[_Ps,_Pos,_Neg,Res_l,Res_p])
                )
               ,Rs)
        ,pairs_keys_values(Rs,Rs_l,Rs_p)
        ,result_means(Rs_l,Ms_l)
        ,result_means(Rs_p,Ms_p).


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


%!      experiment(+Language,+N,+J,+K,-Results) is det.
%
%       Run an experiment learning a program and labelling examples.
%
%       Language is the symbol, but not arity, of the target theory used
%       to verify examples learned by Poker.
%
%       M is the number of initial examples to generate, with a call
%       to generate_initial/5. N can be the atom all in which case all
%       the input lists of length between J and K are generated.
%
%       J and K are the upper and lower bounds of the length of input
%       lists in in generated initial examples of strings of the Target
%       language. Each initial example will have an input list of length
%       between J and K.
%
%       Results is a list [Ps,Pos,Neg,Ms_L,Ms_R], where:
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
experiment(S,N,J,K,[Ps,Pos,Neg,Rs_l,Rs_p]):-
        generate_initial(S,N,J,K,Es)
        ,debug_clauses_length(experiment_initial,'Generated ~w initial examples:',Es)
        ,time( learn(Es,Pos,Neg,Ps) )
        ,debug_clauses(experiment_learned,'Learned hypothesis:',Ps)
        ,debug_clauses_length(experiment_examples,'~w Positive examples:',Pos)
        ,debug_clauses_length(experiment_examples,'~w Negative examples:',Neg)
        ,test_labelling(S,Pos,Neg,Rs_l)
        ,test_program(S,Ps,Rs_p).



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
test_program(_,[],[0.5,0.0,1.0]):-
% The empty hypothesis rejects all.
        !.
test_program(T/_,Cs,[Acc,TPR,TNR]):-
% Allow the target to be a predicate indicator.
        test_program(T,Cs,[Acc,TPR,TNR])
        ,!.
test_program(T,Cs,[Acc,TPR,TNR]):-
        debug(test_program,'Testing learned program for target: ~w',[T])
        ,debug_clauses(test_program_full,'Testing learned program:',Cs)
        ,Program_module = experiment_file
        ,once( internal_symbol(T,T_) )
        ,S = assert_program(Program_module,Cs,Rs)
        ,G = (debug(test_program,'Generating positive testing examples.',[])
             ,generate_examples(pos,Pos)
             ,debug(test_program,'Generating negative testing examples.',[])
             ,generate_examples(neg,Neg)
             ,accuracy(Program_module,T_,Pos,Neg,Acc)
             ,tpr(Program_module,T_,Pos,TPR)
             ,tnr(Program_module,T_,Neg,TNR)
             )
        ,C = erase_program_clauses(Rs)
        ,setup_call_cleanup(S,G,C)
        ,debug(test_program,'Program: Measured Acc: ~w TPR: ~w TNR: ~w',[Acc,TPR,TNR]).


%!      generate_positives(+Sign,-Examples) is det.
%
%       Generate Examples of the given Sign to test a learned program.
%
%       Sign is one of [pos,neg], for positive and negative examples,
%       respectively.
%
%       Examples is a list of examples generated according to
%       geneate_examples/5, which should be loaded to memory from the
%       current experiment file; the symbols of grammars to use to
%       generate examples are defined in that predicate.
%
generate_examples(Sign,Es):-
        experiment_file:generate_examples(Sign,S,N,J,K)
        ,generate_initial(S,N,J,K,Es)
        ,debug_clauses_length(generate_examples,'Generated ~w testing examples',Es).



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
        findall(E
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
