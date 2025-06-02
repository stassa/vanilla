:-module(anbn, [program_signature/3
	       ,background_knowledge/2
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

/** <module> Learn an a^nb^n CFG with Metagol.

Remember to call set_config/0 before running this experiment to set
necessary configuration options:

==
?- experiment_file:set_configs.
true.

?- metagol:learn(s/2).
s(A,B):-a(A,C),a(C,B).
s(A,B):-s(A,C),b(C,B).
true ;
s(A,B):-a(A,C),s(C,B).
s(A,B):-b(A,C),b(C,B).
true ;
s(A,B):-a(A,C),a(C,B).
s(A,B):-s(A,C),b(C,B).
true ;
s(A,B):-a(A,C),a(C,B).
s(A,B):-s(A,C),b(C,B).
s(A,B):-s(A,C),b(C,B).
true ;
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-s(A,C),b(C,B).
true ;
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-s(A,C),b(C,B).
true ;
inv_1(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1(C,B).
true ;
s(A,B):-a(A,C),s(C,B).
s(A,B):-b(A,C),b(C,B).
true ;
s(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-b(A,C),b(C,B).
true ;
s(A,B):-a(A,C),a(C,B).
s(A,B):-b(A,C),b(C,B).
s(A,B):-s(A,C),s(C,B).
true ;
inv_1(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),inv_1(C,B).
s(A,B):-b(A,C),b(C,B).
true ;
inv_1(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1(C,B).
s(A,B):-s(A,C),b(C,B).
true ;
inv_1(A,B):-a(A,C),b(C,B).
inv_1(A,B):-inv_1(A,C),b(C,B).
s(A,B):-a(A,C),inv_1(C,B).
true ;
inv_1(A,B):-b(A,C),b(C,B).
s(A,B):-a(A,C),a(C,B).
s(A,B):-s(A,C),inv_1(C,B).
true ;
inv_1(A,B):-b(A,C),b(C,B).
s(A,B):-a(A,C),inv_1(C,B).
s(A,B):-a(A,C),s(C,B).
true ;
inv_1(A,B):-a(A,C),inv_1(C,B).
inv_1(A,B):-b(A,C),b(C,B).
s(A,B):-a(A,C),inv_1(C,B).
true ;
[]
true.
==

The correct hypothesis is in there somewhere.

*/

set_configs:-
        metagol_auxiliaries:set_metagol_configuration_option(depth_limits,[0,3])
        ,metagol_auxiliaries:set_metagol_configuration_option(max_invented,[1]).


% Constraints to avoid unnecessary left-recursions.
configuration:metarule_constraints(m(identity,P0,P1),fail):-
        P0 == P1.
configuration:metarule_constraints(m(chain,P0,P1,_P2),fail):-
        P0 == P1.
configuration:metarule_constraints(m(chain,_P0,P1,P2),fail):-
        P1 == P2.
% This is a Metagol-specific constraint excluding metasubstitutions of
% the form m(chain,s,inv_1,_) which cause infinite recursion with
% m(chain,inv_1,s,_). The latter form is necessary to construct a clause
% of inv_1/2 that is mutually recursive with s/2 and complete the proof,
% but cannot be excluded only with order constraints because both
% clauses are instances of Chain and we can only apply one set of order
% constraints to Chain.
%
% That's not a problem with Simpleton, Poker or Louise thanks to tabling
% and in fact may stop them from constructing some correct hypotheses,
% so this is only applied when learner/2 is set to load Metagol.
%
configuration:metarule_constraints(m(_ID,P,Q,_R)/_U,fail):-
	P == s
	,Q == inv_1.

program_signature(s/2,[s,a,'inv_1',b],[[a,a,b,b],[a,b,b],[b,b],[b],[]]).
program_signature(inv_1/2,['inv_1',s,b],[[a,a,b,b],[a,b,b],[b,b],[b],[]]).

metagol_configuration:order_constraints(chain_s,[P,Q,R],[_X,_Y,_Z],[P>Q,P>R],[]).

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

% Metagol needs no negative examples to learn a correct hypothesis.
% This is thanks to the stronger inductive bias provided by order
% constraints.
negative_example(s/2,_E):- fail.

% The background knowledge is the set of pre-terminals in the language.
a --> [a].
b --> [b].
