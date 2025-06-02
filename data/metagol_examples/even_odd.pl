:-module(even_odd, [program_signature/3
                   ,background_knowledge/2
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

?- metagol:learn(even/1).
even(A):-prev(A,B),inv_1(B).
inv_1(A):-prev(A,B),even(B).
inv_1(A):-prev(A,B),zero(B).
true ;
even(A):-zero(A).
even(A):-prev(A,B),inv_1(B).
inv_1(A):-prev(A,B),even(B).
true ;
[]
true.
==

*/

set_configs:-
        metagol_auxiliaries:set_metagol_configuration_option(depth_limits,[0,3])
        ,metagol_auxiliaries:set_metagol_configuration_option(max_invented,[1]).


configuration: m1 metarule 'P(x):- Q(x)'.
configuration: m2 metarule 'P(x):- Q(x,y), R(y)'.

metagol_configuration:order_constraints(m1,[P,Q],[_X],[P>Q],[]).
metagol_configuration:order_constraints(m2,[_P,_Q,_R],[X,Y],[],[X>Y]).

% Applies to both the target and the invented predicate. This allows
% mutual recursion between the two.
program_signature(_,[even,zero,prev,inv_1],[4,3,2,1,0]).

background_knowledge(even/1, [zero/1, prev/2]).

metarules(even/1,[m2,m1]).

positive_example(even/1,even(4)).

negative_example(even/1,even(3)).

zero(0).

prev(1,0).
prev(2,1).
prev(3,2).
prev(4,3).
