:-module(hello_world, [program_signature/3
		      ,background_knowledge/2
		      ,metarules/2
		      ,initial_example/2
		      ,positive_example/2
		      ,negative_example/2
		      ,ancestor/2
		      ,parent/2
		      ,father/2
		      ,mother/2
		      ]).

/** <module> A simple example using Poker.

*/

%!	program_signature(?Target,?Symbols,?Terms) is semidet.
%
%	Program signature for a learning Target.
%
%	Used by Metagol to produce a total ordering of the Herbrand
%	base and ensure termination.
%
%	Symbols is the predicate signature, a total ordering over
%	predicate symbols in the examples, background knowledge and
%	invented predicate symbols.
%
%	Terms is the constant signature, a total ordering over constants
%	and ground terms in the examples and background knowledge.
%
program_signature(p/2,[ancestor,parent,father,mother],[]).


%!	background_knowledge(+Target,-Symbols) is semidet.
%
%	Background knowledge Symbols for a learning Target.
%
%	Background knowledge is a set of definite program definitions
%	used to compose a new hypothesis.
%
background_knowledge(p/2,[parent/2
			 ,mother/2
			 ,father/2
			 %,ancestor/2
			 ]).

%!	metarules(+Target, -Metarules) is semidet.
%
%	IDs of the Metarules for a learning Target.
%
%	Metarules are defined in the configuration file. They can also
%	be defined in experiment files. See
%	data/examples/user_metarules.pl for an example of defining your
%	own metarules.
%
metarules(p/2,[identity,chain]).

%!	initial_example(+Target,-Examples) is nondet.
%
%	Generator of initial Examples for a learning Target.
%
initial_example(p/2,p(stathis,stassa)).

%!	positive_example(+Target,-Examples) is nondet.
%
%	Generator of positive Examples for a learning Target.
%
positive_example(p/2,p(A,B)):-
	ancestor(A,B).

%!	negative_example(+Target,-Examples) is nondet.
%
%	Generator of negative Examples for a learning Target.
%
negative_example(p/2,p(A,B)):-
	ancestor(B,A).


parent(stathis,kostas).
parent(stefanos,dora).
parent(kostas,stassa).
parent(alexandra,kostas).
parent(paraskevi,dora).
parent(dora,stassa).

father(stathis,kostas).
father(stefanos,dora).
father(kostas,stassa).

mother(alexandra,kostas). % mother
mother(paraskevi,dora). % mother
mother(dora,stassa). % mother

grandfather(stathis,stassa). % grandfather
grandfather(stefanos,stassa).% grandfather

grandmother(alexandra,stassa). % grandmother
grandmother(paraskevi,stassa). % grandmother

ancestor(stathis,kostas). % father
ancestor(stefanos,dora). % father
ancestor(kostas,stassa). % father
ancestor(alexandra,kostas). % mother
ancestor(paraskevi,dora). % mother
ancestor(dora,stassa). % mother
ancestor(stathis,stassa). % grandfather
ancestor(stefanos,stassa).% grandfather
ancestor(alexandra,stassa). % grandmother
ancestor(paraskevi,stassa). % grandmother
