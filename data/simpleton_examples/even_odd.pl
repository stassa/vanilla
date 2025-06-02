:-module(even_odd, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ,zero/1
                   ,prev/2
                   ,set_configs/0
                   ]).

:-use_module(project_root(configuration)).

% Identify thine self.
:- simpleton_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).


/** <module> Learn "even" by inventing "odd".

This experiment file shows how to learn "even" by inventing "odd" from a
single positive and a single negative example of "even".

==
?- experiment_file:set_configs.
true.

?- simpleton:learn(even/1).
even(A):-zero(A).
even(A):-prev(A,B),inv_1(B).
inv_1(A):-prev(A,B),even(B).
true ;
even(A):-prev(A,B),inv_1(B).
inv_1(A):-prev(A,B),even(B).
inv_1(A):-prev(A,B),zero(B).
true ;
[]
true.
==

set_simpleton_configuration_option/02 can't be called as a directive
because it raises an error, unknown why.

*/

set_configs:-
        simpleton_auxiliaries:set_simpleton_configuration_option(clause_limit,[3])
        ,simpleton_auxiliaries:set_simpleton_configuration_option(max_invented,[1]).

configuration: m1 metarule 'P(x):- Q(x)'.
configuration: m2 metarule 'P(x):- Q(x,y), R(y)'.

background_knowledge(even/1, [zero/1, prev/2]).

metarules(even/1,[m2,m1]).

positive_example(even/1,even(4)).

negative_example(even/1,even(3)).

zero(0).

prev(1,0).
prev(2,1).
prev(3,2).
prev(4,3).
