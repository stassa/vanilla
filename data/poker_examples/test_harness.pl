:-module(test_harness,[experiments/6
                      ,experiment/5
                      ,accuracy/4
                      ,tpr/3
                      ,tnr/3
                      ,generate_initial/5
                      ]).

:-use_module(lib(poker/poker)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(src(auxiliaries)).
:-use_module(lib(poker/sampling/sampling)).

/** <module> A test harness for Poker.

test_harness:experiments(even,1,25,0,10,[Acc,TPR,TNR]).

*/

%:-debug(experiments).
%:-debug(experiment).

%!      experiments(+Target,+N,+M,+J,+K,-Means) is det.
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
%       lists in examples representing automata in generated initial
%       examples. Each initial example will have an input list of length
%       between J and K.
%
%       Means is a list [Acc,TPR,TNR], where each is the mean accuracy,
%       true positive rate and true negative rate of the N hypotheses
%       learned from the sets of N examples generated in the N steps of
%       the experiment.
%
experiments(S,N,M,J,K,Ms):-
        findall([Acc,TPR,TNR]
               ,(between(1,N,I)
                ,debug(experiments,'Experiment ~w of ~w',[I,N])
                ,experiment(S,M,J,K,[_Ps,_Pos,_Neg,Acc,TPR,TNR])
                )
               ,Rs)
        ,result_means(Rs,Ms).

result_means(Rs,Ms):-
        result_means(Rs,[0,0,0],Ss)
        ,length(Rs,L)
        ,maplist(mean(L),Ss,Ms_)
        ,maplist(atomize,Ms_,Ms).


result_means([],Ms,Ms):-
        !.
result_means([[Acc1,TPR1,TNR1]|Rs],[Acc0,TPR0,TNR0],Ms):-
        maplist(sum,[Acc0,TPR0,TNR0],[Acc1,TPR1,TNR1],[Acc,TPR,TNR])
        ,result_means(Rs,[Acc,TPR,TNR],Ms).


mean(N,X,M):-
        M is X / N.

sum(A,B,C):-
        C is A + B.


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

tpr(_S,[],0):-
        !.
tpr(S,Pos,TPR):-
        true_positives(S,Pos,TP)
        ,length(Pos,N)
        ,TPR_ is TP / N
        ,atomize(TPR_,TPR).

tnr(_S,[],0):-
        !.
tnr(S,Neg,TNR):-
        true_negatives(S,Neg,TN)
        ,length(Neg,N)
        ,TNR_ is TN / N
        ,atomize(TNR_,TNR).

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

atomize(N,An):-
        format(atom(A),'~4f',[N])
        ,atom_number(A,An).

/*
generate_initial(S,N,J,K,Es):-
        generate_initial(S,0,N,J,K,[],Es).

generate_initial(_S,N,N,_J,_K,Es,Es):-
        !.
generate_initial(S,M,N,J,K,Acc,Bind):-
        random_between(J,K,R)
        ,generate_example(S,R,E)
        ,\+ memberchk(E,Acc)
        ,!
        ,succ(M,M_)
        ,generate_initial(S,M_,N,J,K,[E|Acc],Bind).
generate_initial(S,M,N,J,K,Acc,Bind):-
        generate_initial(S,M,N,J,K,Acc,Bind).

generate_example(S,N,q0(Xs,[])):-
        length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(E).
*/
/*
generate_initial(S,N,J,K,Es):-
        findall(E
               ,(between(J,K,I)
                ,generate_example(S,I,E)
                )
               ,Es_)
        ,k_list_samples(N,Es_,Es).

generate_example(S,N,q0(Xs,[])):-
        length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(E).
*/
generate_initial(S,N,J,K,Es):-
        findall(E
               ,(between(J,K,I)
                ,generate_example(S,I,E)
                )
               ,Es_)
        ,k_list_samples(N,Es_,Es).

generate_example(S,N,q0(Xs,[])):-
        length(Xs,N)
        ,E =.. [S,Xs,[]]
        ,call(E)
        % Avoid all-0', all-1 strings.
        ,\+ sort(Xs,[_]).



:-table even/2, odd/2.

even(X,Y):- empty(X,Y).
even(X,Y):- zero(X,Z), even(Z,Y).
even(X,Y):- one(X,Z), odd(Z,Y).
odd(X,Y):- zero(X,Z), odd(Z,Y).
odd(X,Y):- one(X,Z), even(Z,Y).

zero --> [0].
one --> [1].
empty --> [].

%zero([0|T],T).
%one([1|T],T).
%empty([],[]).
%empty --> [].
%empty(T,T).

%:-table palindrome/2.
%/*
palindrome --> empty.
palindrome --> one.
palindrome --> zero.
%palindrome --> one,one.
%palindrome --> zero,zero.
palindrome --> one, palindrome, one.
palindrome --> zero, palindrome, zero.
%*/
/*
palindrome(X,Y):- empty(X,Y).
palindrome(X,Y):- one(X,Y).
palindrome(X,Y):- zero(X,Y).
palindrome(X,Y):- one(X,Z),palindrome(Z,U), one(U,Y).
palindrome(X,Y):- zero(X,Z),palindrome(Z,U), zero(U,Y).

*/
