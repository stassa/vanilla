:-module(parity, [background_knowledge/2
                 ,metarules/2
                 ,initial_example/2
                 ,positive_example/2
                 ,negative_example/2
                 ,zero/2
                 ,one/2
                 ,empty/2
                 ]).

:-use_module(project_root(configuration),[]).
:-use_module(lib(poker/poker_configuration),[]).

:-use_module(data(examples/test_harness)).

/** <poker> Learn even parity by inventing odd parity with Poker.

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
poker_configuration:safe_example(m(q0,Ls,[])):-
	between(0,4,L)
	,length(Ls,L).

background_knowledge(q0/2,[zero/2
			  ,one/2
			  ,empty/2
			  ]).

metarules(q0/2,[identity,chain]).


% Uncomment when running with test harness.
%initial_example(q0/2,_E):- fail.

% To train with random examples without the test harness.
%initial_example(q0/2,E):-
%	generate_initial(even,25,0,10,Es)
%        ,member(E,Es)
%        %,E \= q0([],[])
%        ,debug(generated_examples,'You are here: ~w',[E]).

% Hand-picked examples.
initial_example(q0/2,E):-
	positive_example(q0/2,E).

positive_example(q0/2,E):-
        member(E, [q0([],[])
		  ,q0([0],[])
		  ,q0([0,0],[])
		  ,q0([1,1],[])
		  ,q0([0,0,0],[])
		  ,q0([0,1,1],[])
		  ,q0([1,0,1],[])
		  ,q0([1,1,0],[])
		  ,q0([1,1,0,0],[])
		  ,q0([1,0,1,0],[])
		  ,q0([1,0,0,1],[])
		  ,q0([0,1,0,1],[])
		  ,q0([0,0,1,1],[])
		  ,q0([0,0,1,1,0,0],[])
		  ,q0([0,0,1,0,0,1],[])
		  ]
              ).

negative_example(q0/2,E):-
        member(E, [q0([1],[])
                  ,q0([0,1],[])
                  ,q0([1,0],[])
                  ,q0([0,0,1],[])
                  ,q0([0,1,0],[])
                  ,q0([1,0,0],[])
                  ,q0([1,1,1],[])
                  ]
              ).

zero([0|T],T).
one([1|T],T).
empty([],[]).
