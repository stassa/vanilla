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
:- louise_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).


/** <module> Learn "even" by inventing "odd".

This experiment file shows how to learn "even" by inventing "odd" from a
single positive and a single negative example of "even".

Remember to call set_config/0 before running this experiment to set
necessary configuration options:

==
?- experiment_file:set_configs.
true.

?- louise:learn(even/1).
inv_1(A):-prev(A,B),even(B).
even(A):-zero(A).
even(A):-prev(A,B),inv_1(B).
true.
==

set_louise_configuration_option/2 can't be called in a directive because
it raises an error, unknown why.

*/

set_configs:-
        louise_auxiliaries:set_louise_configuration_option(clause_limit,[3])
        ,louise_auxiliaries:set_louise_configuration_option(max_invented,[1])
        ,louise_auxiliaries:set_louise_configuration_option(gestalt,[true]).

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
