:-module(palindrome,[background_knowledge/2
                    ,metarules/2
                    ,initial_example/2
                    ,zero/2
                    ,one/2
                    ,empty/2
		    ,generate_examples/6
                    ]).

:-use_module(project_root(configuration)).
:-use_module(data(poker_examples/test_harness)).

/** <module> Learn to recognise palindromes with Poker.

To reproduce this experiment make sure you have the same configuration
and learning problem elements, as listed below.

Configuration:

==
1 ?- [load_headless].
Loading poker
Global stack limit 2,147,483,648
Table space 17,179,869,184
true.

2 ?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), flatten_prove_all(Flatten), gestalt(Gestalt), greedy_generalisation(Greedy), respecialise(Respecialise), strict_clause_limit(Strict), proof_samples(Samples), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)), listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(q0, Ls, [])) :-
    experiment_file:
    (   between(0, 8, L),
        length(Ls, L)
    ).

Fetch = all,
Table = Untable, Untable = Flatten, Flatten = true,
Limit = 5,
Invented = 0,
Gestalt = Greedy, Greedy = Respecialise, Respecialise = Strict, Strict = false,
Samples = 1.0,
Unlabelled = 100,
Order = deterministic,
Reduction = plotkins.
==

Learning problem:

==
3 ?- poker_auxiliaries:list_mil_problem(q0/2).
Initial examples
----------------
q0([],[]).
q0([1],[]).
q0([0],[]).
q0([1,1],[]).
q0([0,0],[]).
q0([1,1,1],[]).
q0([1,0,1],[]).
q0([0,1,0],[]).
q0([0,0,0],[]).
q0([1,1,1,1],[]).
q0([1,0,0,1],[]).
q0([0,1,1,0],[]).
q0([0,0,0,0],[]).

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
(Tri-chain) ∃.P,Q,R,S ∀.x,y,z,u: P(x,y)← Q(x,z),R(z,u),S(u,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(A, fail) :-
    experiment_file:
    (   ground(A),
        A=..[m, _, B, B|_]
    ).
metarule_constraints(m(identity, _, A), fail) :-
    experiment_file:(A==q0).
metarule_constraints(m(tri_chain, _, A, B, _), fail) :-
    experiment_file:
    (   ground(A),
        ground(B),
        A==B
    ).
metarule_constraints(m(tri_chain, _, _, A, B), fail) :-
    experiment_file:
    (   ground(A),
        ground(B),
        A==B
    ).
metarule_constraints(m(tri_chain, _, A, B, C), fail) :-
    experiment_file:
    (   ground([A, B, C]),
        memberchk(empty, [A, B, C])
    ).

true.
==

Learning results:
==
4 ?- _T = q0/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 58,357,913 inferences, 4.609 CPU in 15.032 seconds (31% CPU, 12660700 Lips)
Hypothesis:
q0(A,B):-zero(A,B).
q0(A,B):-one(A,B).
q0(A,B):-empty(A,B).
q0(A,B):-zero(A,C),q0(C,D),zero(D,B).
q0(A,B):-one(A,C),q0(C,D),one(D,B).
Positive examples:
q0([1,1,1,1,1,1],[]).
q0([],[]).
q0([1],[]).
q0([0],[]).
q0([1,1],[]).
q0([0,0],[]).
q0([1,1,1],[]).
q0([1,0,1],[]).
q0([0,1,0],[]).
q0([0,0,0],[]).
q0([1,1,1,1],[]).
q0([1,0,0,1],[]).
q0([0,1,1,0],[]).
q0([0,0,0,0],[]).
Negative examples:
q0([1,1,1,1,1,0],[]).
q0([1,1,1,1,1],[]).
q0([1,1,1,1,0,1],[]).
q0([1,1,1,1,0,0],[]).
q0([1,1,1,1,0],[]).
q0([1,1,1,0,1,1],[]).
q0([1,1,1,0,1,0],[]).
q0([1,1,1,0,1],[]).
q0([1,1,1,0,0,1],[]).
q0([1,1,1,0,0,0],[]).
q0([1,1,1,0,0],[]).
q0([1,1,1,0],[]).
q0([1,1,0,1,1,1],[]).
q0([1,1,0,1,1,0],[]).
q0([1,1,0,1,1],[]).
q0([1,1,0,1,0,1],[]).
q0([1,1,0,1,0,0],[]).
q0([1,1,0,1,0],[]).
q0([1,1,0,1],[]).
q0([1,1,0,0,1,1],[]).
q0([1,1,0,0,1,0],[]).
q0([1,1,0,0,1],[]).
q0([1,1,0,0,0,1],[]).
q0([1,1,0,0,0,0],[]).
q0([1,1,0,0,0],[]).
q0([1,1,0,0],[]).
q0([1,1,0],[]).
q0([1,0,1,1,1,1],[]).
q0([1,0,1,1,1,0],[]).
q0([1,0,1,1,1],[]).
q0([1,0,1,1,0,1],[]).
q0([1,0,1,1,0,0],[]).
q0([1,0,1,1,0],[]).
q0([1,0,1,1],[]).
q0([1,0,1,0,1,1],[]).
q0([1,0,1,0,1,0],[]).
q0([1,0,1,0,1],[]).
q0([1,0,1,0,0,1],[]).
q0([1,0,1,0,0,0],[]).
q0([1,0,1,0,0],[]).
q0([1,0,1,0],[]).
q0([1,0,0,1,1,1],[]).
q0([1,0,0,1,1,0],[]).
q0([1,0,0,1,1],[]).
q0([1,0,0,1,0,1],[]).
q0([1,0,0,1,0,0],[]).
q0([1,0,0,1,0],[]).
q0([1,0,0,0,1,1],[]).
q0([1,0,0,0,1,0],[]).
q0([1,0,0,0,1],[]).
q0([1,0,0,0,0,1],[]).
q0([1,0,0,0,0,0],[]).
q0([1,0,0,0,0],[]).
q0([1,0,0,0],[]).
q0([1,0,0],[]).
q0([1,0],[]).
q0([0,1,1,1,1,1],[]).
q0([0,1,1,1,1,0],[]).
q0([0,1,1,1,1],[]).
q0([0,1,1,1,0,1],[]).
q0([0,1,1,1,0,0],[]).
q0([0,1,1,1,0],[]).
q0([0,1,1,1],[]).
q0([0,1,1,0,1,1],[]).
q0([0,1,1,0,1],[]).
q0([0,1,1,0,0],[]).
q0([0,1,1],[]).
q0([0,1,0,1,1],[]).
q0([0,1,0,1,0],[]).
q0([0,1,0,1],[]).
q0([0,1,0,0,1],[]).
q0([0,1,0,0,0],[]).
q0([0,1,0,0],[]).
q0([0,1],[]).
q0([0,0,1,1,1],[]).
q0([0,0,1,1,0],[]).
q0([0,0,1,1],[]).
q0([0,0,1,0,1],[]).
q0([0,0,1,0,0],[]).
q0([0,0,1,0],[]).
q0([0,0,1],[]).
q0([0,0,0,1,1],[]).
q0([0,0,0,1,0],[]).
q0([0,0,0,1],[]).
q0([0,0,0,0,1],[]).
q0([0,0,0,0,0],[]).
Ps = 5,
Pos = 14,
Neg = 86.
==

It's difficult to see just by eyeballing but while the learned
hypothesis is exactly the target theory, a grammar for the language of
palindromes, the labelling is slightly off and the negative examples
include some palindromes, e.g.:

==
q0([1,1,1,1,1],[]).
q0([1,1,0,1,1],[]).
q0([1,0,0,0,1],[]).
q0([1,0,0,0,0,1],[]).
q0([0,1,1,1,1,0],[]).
q0([0,1,1,1,0],[]).
q0([0,0,0,0,0],[]).
==

And possibly others I missed. This is because Poker labels examples once
and then continues specialising its hypothesis, so the labelling
performed during learning improves as time goes by, but may well include
errors made in the earlier stages.

Note that this is not the same as a greedy algorithm: rather than commit
to the early errors and return an over-specialised hypothesis that
rejects the false negatives identified at the early stages of learning,
Poker improves its hypothesis as it goes and is capable of returning a
correct hypothesis, including one that covers the false negatives.

*/

% McCarhtyite constraint
configuration:metarule_constraints(M,fail):-
	ground(M)
	,M =.. [m,_Id,P,P|_Ps].

configuration:metarule_constraints(m(identity,_P0,P1),fail):-
	P1 == q0.

configuration:metarule_constraints(m(tri_chain,_P0,P1,P2,_P3),fail):-
	ground(P1)
	,ground(P2)
	,P1 == P2.

% This eliminates one, q0, one and zero, q0, zero so don't.
%configuration:metarule_constraints(m(tri_chain,_P0,P1,_P2,P1),fail).
configuration:metarule_constraints(m(tri_chain,_P0,_P1,P2,P3),fail):-
	ground(P2)
	,ground(P3)
	,P2 == P3.

% Problem-specific constraint. Eliminates over-general
% tail-recursive clauses. Cheating.
%configuration:metarule_constraints(m(tri_chain,P0,_P1,_P2,P3),fail):-
%	ground(P0)
%	,ground(P3)
%	,P0 == P3.

configuration:metarule_constraints(m(tri_chain,_P0,P1,P2,P3),fail):-
	ground([P1,P2,P3])
        ,memberchk(empty, [P1,P2,P3]).

configuration:tri_chain metarule 'P(x,y):- Q(x,z), R(z,u), S(u,y)'.

poker_configuration:safe_example(m(q0,Ls,[])):-
	between(0,8,L)
	,length(Ls,L).

background_knowledge(q0/2,[zero/2
			  ,one/2
			  ,empty/2
			  ]).

metarules(q0/2,[identity,tri_chain]).

initial_example(q0/2,E):-
	% Uncomment to generate random sets of initial eamples.
        %,generate_initial(palindrome,K,0,5,Es)
	generate_all_initial(palindrome,0,4,Es)
        ,member(E,Es).

zero --> [0].
one --> [1].
empty --> [].

generate_examples(pos,all,palindrome,10,0,4).
generate_examples(neg,all,not_palindrome,10,0,4).
