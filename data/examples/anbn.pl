:-module(anbn, [program_signature/3
	       ,background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,a/2
	       ,b/2
	       ]).

:-use_module(project_root(configuration)).

/** <module> Learn an a^nb^n CFG with MIL.


*/

% Constraints to avoid unnecessary left-recursions.
configuration:metarule_constraints(m(identity,P0,P1),fail):-
        P0 == P1.
configuration:metarule_constraints(m(chain,P0,P1,_P2),fail):-
        P0 == P1.
configuration:metarule_constraints(m(chain,_P0,P1,P2),fail):-
        P1 == P2.

% Used by Metagol
program_signature(s/2,[s,a,'inv_1',b],[[a,a,b,b],[a,b,b],[b,b],[b],[]]).
program_signature(inv_1/2,['inv_1',s,b],[[a,a,b,b],[a,b,b],[b,b],[b],[]]).

metagol_configuration:order_constraints(chain_s,[P,Q,R],[_X,_Y,_Z],[P>Q,P>R],[]).

:-if( configuration:learner(metagol,_) ).
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
:-endif.

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

:- if(configuration:learner(metagol,_)).
% Metagol needs no negative examples to learn a correct hypothesis.
% This is thanks to the stronger inductive bias provided by order
% constraints.
negative_example(s/2,_E):- fail.
:- else.
% Simpleton and Louise both need negative examples otherwise they
% construct over-general hypotheses.
negative_example(s/2,E):-
	member(E,[s([a,a],[])
		 ,s([b,b],[])
		 ,s([a,a,b],[])
		 ,s([a,b,b],[])
		 ,s([a,a,a,a],[])
		 ,s([b,b,b,b],[])
		 ]).
:- endif.

% The background knowledge is the set of pre-terminals in the language.
a --> [a].
b --> [b].
