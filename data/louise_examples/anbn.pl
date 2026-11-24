:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,a/2
	       ,b/2
               ,set_configs/0
	       ]).

:-use_module(project_root(configuration)).

% Identify thine self.
:- louise_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn an a^nb^n CFG with MIL.

Remember to call set_config/0 before running this experiment to set
necessary configuration options:

==
?- experiment_file:set_configs.
true.

?- louise:learn(s/2).
s(A,B):-a(A,C),s(C,D),b(D,B).
s(A,B):-a(A,C),b(C,B).
true.
==

set_louise_configuration_option/2 can't be called in a directive because
it raises an error, unknown why.

*/

set_configs:-
        louise_auxiliaries:set_louise_configuration_option(clause_limit,[3])
        ,louise_auxiliaries:set_louise_configuration_option(max_invented,[1])
	,louise_auxiliaries:set_louise_configuration_option(unfold_invented,[all]).

% Constraints to avoid unnecessary left-recursions.
configuration:metarule_constraints(m(identity,P0,P1),fail):-
        P0 == P1.
configuration:metarule_constraints(m(chain,P0,P1,_P2),fail):-
        P0 == P1.
configuration:metarule_constraints(m(chain,_P0,P1,P2),fail):-
        P1 == P2.

background_knowledge(s/2,[a/2,b/2]).

metarules(s/2,[chain]).

% For Metagol, Simpleton and Louise.
positive_example(s/2,E):-
% Uncomment extra examples to experiment with more or less general ones.
	member(E, [%s([a,b],[])
		  s([a,a,b,b],[])
		  %,s([a,a,a,b,b,b],[])
		  %,s([a,a,a,a,b,b,b,b],[])
		  %,s([a,a,a,a,a,b,b,b,b,b],[])
		  %,s([a,a,a,a,a,a,a,b,b,b,b,b,b,b],[])
		  ]).

negative_example(s/2,E):-
	member(E,[s([a,a],[])
		 ,s([b,b],[])
		 ,s([a,a,b],[])
		 ,s([a,b,b],[])
		 ,s([a,a,a,a],[])
		 ,s([b,b,b,b],[])
		 ]).

% The background knowledge is the set of pre-terminals in the language.
a --> [a].
b --> [b].
