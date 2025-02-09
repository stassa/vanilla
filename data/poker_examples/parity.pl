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

:-use_module(data(poker_examples/test_harness)).

/** <poker> Learn even parity by inventing odd parity with Poker.

Check your configus and experiment data.

==
1 ?- [load_headless].
Loading poker
Global stack limit 2,147,483,648
Table space 2,147,483,648
true.

2 ?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), gestalt(Gestalt), greedy_generalisation(Greedy), respecialise(Respecialise), strict_clause_limit(Strict), proof_samples(Samples), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)), listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(q0, Ls, [])) :-
    experiment_file:
    (   between(0, 4, L),
        length(Ls, L)
    ).

Fetch = [builtins,bk,metarules],
Table = Greedy, Greedy = Strict, Strict = false,
Untable = Gestalt, Gestalt = Respecialise, Respecialise = true,
Limit = 5,
Invented = 1,
Samples = 1.0,
Unlabelled = 100,
Order = deterministic,
Reduction = none.

3 ?- poker_auxiliaries:list_mil_problem(q0/2).
Initial examples
----------------
q0([],[]).
q0([0],[]).
q0([0,0],[]).
q0([1,1],[]).
q0([0,0,0],[]).
q0([0,1,1],[]).
q0([1,0,1],[]).
q0([1,1,0],[]).
q0([1,1,0,0],[]).
q0([1,0,1,0],[]).
q0([1,0,0,1],[]).
q0([0,1,0,1],[]).
q0([0,0,1,1],[]).
q0([0,0,1,1,0,0],[]).
q0([0,0,1,0,0,1],[]).

Background knowledge (First Order)
----------------------------------
zero/2:
zero([0|A],A).

one/2:
one([1|A],A).

empty/2:
empty(A,B):-A=B.

Background knowledge (Second Order)
-----------------------------------
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(m(identity, P0, P1), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    experiment_file:(P1==P2).

true.
==

Expected result:

==
4 ?- _T = q0/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 686,315,332 inferences, 27.969 CPU in 88.522 seconds (32% CPU, 24538649 Lips)
Hypothesis:
q0(A,B):-empty(A,B).
q0(A,B):-zero(A,C),q0(C,B).
q0(A,B):-one(A,C),inv_1(C,B).
inv_1(A,B):-zero(A,C),inv_1(C,B).
inv_1(A,B):-one(A,C),q0(C,B).
Positive examples:
q0([1,1,1,1],[]).
q0([0,1,1,0],[]).
q0([0,0,0,0],[]).
q0([],[]).
q0([0],[]).
q0([0,0],[]).
q0([1,1],[]).
q0([0,0,0],[]).
q0([0,1,1],[]).
q0([1,0,1],[]).
q0([1,1,0],[]).
q0([1,1,0,0],[]).
q0([1,0,1,0],[]).
q0([1,0,0,1],[]).
q0([0,1,0,1],[]).
q0([0,0,1,1],[]).
q0([0,0,1,1,0,0],[]).
q0([0,0,1,0,0,1],[]).
Negative examples:
q0([1,1,1,0],[]).
q0([1,1,1],[]).
q0([1,1,0,1],[]).
q0([1,0,1,1],[]).
q0([1,0,0,0],[]).
q0([1,0,0],[]).
q0([1,0],[]).
q0([1],[]).
q0([0,1,1,1],[]).
q0([0,1,0,0],[]).
q0([0,1,0],[]).
q0([0,1],[]).
q0([0,0,1,0],[]).
q0([0,0,1],[]).
q0([0,0,0,1],[]).
Ps = 5,
Pos = 18,
Neg = 15.
==
*/

% Avoid left-recursions in pre-terminals.
configuration:metarule_constraints(m(identity,P0,P1),fail):-
        P0 == P1.

% Avoid left-recursions in non-terminals.
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

% Uncomment to train with random examples without the test harness.
initial_example_(q0/2,E):-
	generate_initial(even,25,0,10,Es)
        ,member(E,Es).

% Uncomment to train with hand-picked examples.
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

zero --> [0].
one --> [1].
empty --> [].
