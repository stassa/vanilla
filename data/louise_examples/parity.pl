:-module(parity, [background_knowledge/2
                 ,metarules/2
		 ,positive_example/2
		 ,negative_example/2
                 ,zero/2
                 ,one/2
                 ,empty/2
                 ,set_configs/0
                 ]).

:-use_module(project_root(configuration),[]).

% Identify thine self.
:- louise_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn even parity by inventing odd parity with Louise.

Remember to call set_config/0 before running this experiment to set
necessary configuration options:

==
?- experiment_file:set_configs.
true.

?- time( louise:learn(q0/2) ).
q0(A,B):-empty(A,B).
q0(A,B):-zero(A,C),q0(C,B).
q0(A,B):-one(A,C),inv_1(C,B).
inv_1(A,B):-zero(A,C),inv_1(C,B).
inv_1(A,B):-one(A,C),q0(C,B).
% 687,582,097 inferences, 22.750 CPU in 72.989 seconds (31% CPU, 30223389 Lips)
true.
==

This can't be done in a directive because it raises an error, unknown
why.

Warning: listening to bog deep dark slow sludge doom metal while running
this experiment will make it take even longer to learn.

*/

:- auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
:- auxiliaries:set_configuration_option(table_meta_interpreter,[false]).
:- auxiliaries:set_configuration_option(untable_meta_interpreter,[true]).

set_configs:-
        louise_auxiliaries:set_louise_configuration_option(clause_limit,[5])
        ,louise_auxiliaries:set_louise_configuration_option(max_invented,[1])
        ,louise_auxiliaries:set_louise_configuration_option(gestalt,[true])
        ,louise_auxiliaries:set_louise_configuration_option(respecialise,[true]).


configuration:metarule_constraints(m(identity, P0, P1), fail) :-
    louise_configuration:(P0==P1).
configuration:metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    louise_configuration:(P0==P1).
configuration:metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    louise_configuration:(P1==P2).

background_knowledge(q0/2,[zero/2
			  ,one/2
			  ,empty/2
			  ]).

metarules(q0/2,[identity,chain]).

positive_example(q0/2,E):-
% All even-parity bit-strings of length between 0 to 4.
	member(E,[q0([],[])
		  ,q0([0],[])
		  ,q0([0,0],[])
		  ,q0([1,1],[])
		  ,q0([0,0,0],[])
		  ,q0([0,1,1],[])
		  ,q0([1,0,1],[])
		  ,q0([1,1,0],[])
		  ,q0([0,0,0,0],[])
		  ,q0([0,0,1,1],[])
		  ,q0([0,1,0,1],[])
		  ,q0([0,1,1,0],[])
		  ,q0([1,0,0,1],[])
		  ,q0([1,0,1,0],[])
		  ,q0([1,1,0,0],[])
		  ,q0([1,1,1,1],[])
		 ]).

%negative_example(q0/2,_):- fail.
negative_example(q0/2,E):-
% All odd-parity bit-strings of length between 0 to 3.
	member(E,[q0([1],[])
		  ,q0([0,1],[])
		  ,q0([1,0],[])
		  ,q0([0,0,1],[])
		  ,q0([0,1,0],[])
		  ,q0([1,0,0],[])
		  ,q0([1,1,1],[])
		 ]).

zero --> [0].
one --> [1].
empty --> [].
