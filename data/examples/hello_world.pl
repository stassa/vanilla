:-module(hello_world, [background_knowledge/2
		      ,metarules/2
		      ,positive_example/2
		      ,negative_example/2
		      ,ancestor/2
		      ,parent/2
		      ]).

/** <module> A simple example of MIL for Poker and Metagol.

==
?- poker:experiment_data(ancestor/2,_,_Neg,_BK,_MS), learn([ancestor(stathis,stassa)],_Neg,_BK,_MS,_Ps), auxiliaries:print_clauses(_Ps).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true ;
[]
true.
==

*/


%!	background_knowledge(+Target,-Symbols) is semidet.
%
%	Background knowledge Symbols for a learning Target.
%
%	Background knowledge is a set of definite program definitions
%	used to compose a new hypothesis.
%
background_knowledge(ancestor/2,[parent/2]).

%!	metarules(+Target, -Metarules) is semidet.
%
%	IDs of the Metarules for a learning Target.
%
%	Metarules are defined in the configuration file. They can also
%	be defined in experiment files. See
%	data/examples/user_metarules.pl for an example of defining your
%	own metarules.
%
metarules(ancestor/2,[identity,tailrec]).

%!	positive_example(+Target,-Examples) is nondet.
%
%	Generator of positive Examples for a learning Target.
%
positive_example(ancestor/2,ancestor(A,B)):-
	ancestor(A,B).
%positive_example(ancestor/2,ancestor(stathis,stassa)).

%!	negative_example(+Target,-Examples) is nondet.
%
%	Generator of negative Examples for a learning Target.
%
negative_example(ancestor/2,ancestor(A,B)):-
	ancestor(B,A).

parent(stathis,kostas).
parent(stefanos,dora).
parent(kostas,stassa).
parent(alexandra,kostas).
parent(paraskevi,dora).
parent(dora,stassa).

ancestor(stathis,kostas).
ancestor(stefanos,dora).
ancestor(kostas,stassa).
ancestor(alexandra,kostas).
ancestor(paraskevi,dora).
ancestor(dora,stassa).
ancestor(stathis,stassa).
ancestor(stefanos,stassa).
ancestor(alexandra,stassa).
ancestor(paraskevi,stassa).
