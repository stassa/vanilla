:-module(anbn, [program_signature/3
	       ,background_knowledge/2
	       ,metarules/2
	       ,initial_example/2
	       ,positive_example/2
	       ,negative_example/2
	       ,a/2
	       ,b/2
	       ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_configuration),[]).

/** <module> Learn an a^nb^n CFG with Poker.

*/

configuration:metarule_constraints(m(identity,P0,P1),fail):-
        P0 == P1.

configuration:metarule_constraints(m(chain,P0,P1,_P2),fail):-
        P0 == P1.
configuration:metarule_constraints(m(chain,_P0,P1,P2),fail):-
        P1 == P2.


%!	safe_example(-Example) is nondet.
%
%	Generate a safe scaffold for unlabelled examples.
%
%	For examples with list arguments, generating unlabelled examples
%	during learning can "go infinite". This predicate ensures that
%	list arguments in examples are limited in length.
%
%	This argument should not itself be a generator of ground
%	examples. This is left to the user to avoid.
%
poker_configuration:safe_example(m(s,Ls,[])):-
	between(1,9,L)
	,length(Ls,L).

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

% For Poker
initial_example(s/2,s([a,a,a,b,b,b],[])).

positive_example(s/2,E):-
% Uncomment extra examples to experiment with different combinations
% thereof.
	member(E, [%s([a,b],[])
		  %,s([a,a,b,b],[])
		  s([a,a,a,b,b,b],[])
		  %,s([a,a,a,a,b,b,b,b],[])
		  %,s([a,a,a,a,a,b,b,b,b,b],[])
		  %,s([a,a,a,a,a,a,a,b,b,b,b,b,b,b],[])
		  ]).

:- if(configuration:learner(metagol,_)).
% On the upside (see conditional compilation block) Metagol needs no
% negative examples to learn a correct hypothesis. This is thanks to the
% stronger inductive bias provided by order constraints.
negative_example(s/2,_E):- fail.
:- else.
% Simpleton and Louise both need negative constraints otherwise they
% construct over-general hypotheses.
negative_example(s/2,E):-
	member(E,[s([a,a],[])
		 ,s([b,b],[])
		 ,s([a,a,b],[])
		 ,s([a,b,b],[])
		 ]).
:- endif.

a([a|T],T).
b([b|T],T).
