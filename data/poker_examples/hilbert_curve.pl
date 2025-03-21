:-module(hilbert_curve, [background_knowledge/2
                        ,metarules/2
                        ,labelled_example/2
                        ,unlabelled_example/2
                        ,f/2
                        ,g/2
                        ,x/2
                        ,y/2
                        ,plus/2
                        ,minus/2
                        ,empty/2
                        ,generate_examples/5
                        ]).

:-use_module(l_systems_constraints).
:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_configuration),[]).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(data(poker_examples/test_harness)).

/** <module> Learn an L-System grammar modelling the Hilbert Curve.

The target theory for this experiment is the following L-System grammar:

==
Constants: +, -, f
Variables: x, y
Axiom: f
Rules:
x -> +yf-xfx-fy+
y -> -xf+yfy+fx-
==

Where x is to be interpreted as "turn left 90 degrees", y "turn right 90
degrees" and f "move forward".

The specific notation with x and y as variables is from wikipedia:

https://en.wikipedia.org/wiki/Hilbert_curve#Representation_as_Lindenmayer_system

The L-system rules are from A. Lindenmayer's book The Algorithmic Beauty
of Plants (see Section 1.4.2, page 13).

TODO: instructions on rendering the learned L-System with Turtle
Graphics.

1. Good configs:

==
?- auxiliaries:list_config, nl, poker_auxiliaries:list_poker_config, nl, listing([safe_example/1,initial_example/2]).
encapsulation_predicate(m)
example_clauses(call)
fetch_clauses([builtins,bk,metarules])
invented_symbol_prefix(inv_)
learner(poker,lib(poker/poker))
metarule_formatting(quantified)
metasubstitution_atoms(existential)
table_meta_interpreter(false)
untable_meta_interpreter(true)

clause_limit(8)
experiment_file(data(poker_examples/hilbert_curve.pl),hilbert_curve)
flatten_prove_all(true)
generalise_conjunction(false)
gestalt(false)
greedy_generalisation(false)
listing_limit(15)
max_invented(6)
multithreading(false)
proof_samples(1.0)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
respecialise(false)
strict_clause_limit(false)
unfold_invented(all)
unfolding_depth_limit(500)
unlabelled_examples(100)
unlabelled_examples_order(random)

:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(s, Is, Os, [])) :-
    hilbert_curve:
    (   K=8,
        between(0, K, I),
        length(Is, I),
        between(0, K, J),
        length(Os, J)
    ).

hilbert_curve:initial_example(s/3, A) :-
    generate_initial(hilbert_curve, all, 0, 3, B),
    member(A, B).
hilbert_curve:initial_example(s/3, A) :-
    generate_initial(hilbert_curve, all, 11, 12, B),
    findall(s(C, D, []),
            ( member(s(C, D, []), B),
              (   member(x, C)
              ;   member(y, C)
              )
            ),
            E),
    F=member(A, E),
    limit(8, F).

true.
==


2. MIL problem and Lindenmayer Normal Form with constraints:

==
?- poker_auxiliaries:list_mil_problem(s/3).
Initial examples
----------------
s([],[],[]).
s([+],[+],[]).
s([-],[-],[]).
s([f],[f],[]).
s([+,+],[+,+],[]).
s([+,-],[+,-],[]).
s([+,f],[+,f],[]).
s([-,+],[-,+],[]).
s([-,-],[-,-],[]).
s([-,f],[-,f],[]).
s([f,+],[f,+],[]).
s([f,-],[f,-],[]).
s([f,f],[f,f],[]).
s([+,+,+],[+,+,+],[]).
s([+,+,-],[+,+,-],[]).
% ... 27 more clauses.

Background knowledge (First Order)
----------------------------------
f/2:
f([f|A],A).

g/2:
g([g|A],A).

x/2:
x([x|A],A).

y/2:
y([y|A],A).

plus/2:
plus([+|A],A).

minus/2:
minus([-|A],A).

empty/2:
empty(A,B):-A=B.

Background knowledge (Second Order)
-----------------------------------
(Ls-variable) ∃.P,Q,R ∀.x,y,z,u,v: P(x,y,z)← Q(y,u),R(x,v),P(v,u,z)
(Ls-base) ∃.P,Q ∀.x,y: P(x,y,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Tri-chain) ∃.P,Q,R,S ∀.x,y,z,u: P(x,y)← Q(x,z),R(z,u),S(u,y)
Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(M, fail) :-
    l_systems_constraints:
    (   ground(M),
        M=..[m, _Id, P, P|_Ps]
    ).
metarule_constraints(m(ls_constant, P, _Q), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ target(P)
    ).
metarule_constraints(m(ls_constant, _P, Q), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(ls_variable, P, _Q, _R), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ target(P)
    ).
metarule_constraints(m(ls_variable, _P, Q, _R), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(ls_variable, _P, _Q, R), fail) :-
    l_systems_constraints:
    (   ground(R),
        target(R)
    ).
metarule_constraints(m(ls_base, P, _Q), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ target(P)
    ).
metarule_constraints(m(ls_base, _P, Q), fail) :-
    l_systems_constraints:
    (   ground(Q),
        Q\==empty
    ).
metarule_constraints(M, fail) :-
    l_systems_constraints:
    (   ground(M),
        M=..[m, Id, _P|Ps],
        Id\==ls_base,
        memberchk(empty, Ps)
            ).
metarule_constraints(m(tri_chain, P, _Q, _R, _S), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ invented(P)
    ).
metarule_constraints(m(tri_chain, _P, Q, _R, _S), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(tri_chain, _P, _Q, R, _S), fail) :-
    l_systems_constraints:
    (   ground(R),
        \+ preterminal(R)
    ).
metarule_constraints(m(tri_chain, _P, _Q, _R, S), fail) :-
    l_systems_constraints:
    (   ground(S),
        target(S)
    ).
metarule_constraints(m(tri_chain, P, _Q, _R, S), fail) :-
    l_systems_constraints:
    (   ground(P),
        ground(S),
        P==S
    ).
metarule_constraints(m(tri_chain, P, _Q, _R, S), fail) :-
    l_systems_constraints:
    (   ground(P),
        ground(S),
        (   invented(S)
        ->  S@<P
        ;   false
        )
    ).
metarule_constraints(m(chain, P, _Q, _R), fail) :-
    l_systems_constraints:
    (   ground(P),
        \+ invented(P)
    ).
metarule_constraints(m(chain, _P, Q, _R), fail) :-
    l_systems_constraints:
    (   ground(Q),
        \+ preterminal(Q)
    ).
metarule_constraints(m(chain, _P, _Q, R), fail) :-
    l_systems_constraints:
    (   ground(R),
        target(R)
    ).
metarule_constraints(m(chain, P, _Q, R), fail) :-
    l_systems_constraints:
    (   ground(P),
        ground(R),
        P==R
    ).
metarule_constraints(m(chain, P, _Q, R), fail) :-
    l_systems_constraints:
    (   ground(P),
        ground(R),
        (   invented(R)
        ->  R@<P
        ;   false
        )
    ).

true.
==

3. Learning query:

==
?- _T = s/3, time( poker:learn(_T,_Pos,_Neg,_Ps) ), auxiliaries:print_clauses('Hypothesis:',_Ps), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% Generalising positive examples
% Derived 144 sub-hypotheses (unsorted)
% Derived 40 sub-hypotheses (sorted)
% Derived 10 sub-hypotheses (unfolded)
% 45,219,844 inferences, 0.938 CPU in 2.673 seconds (35% CPU, 48234500 Lips)
Hypothesis:
s(A,B,C):-y(B,D),minus(A,E),x(E,F),f(F,G),plus(G,H),y(H,I),f(I,J),y(J,K),plus(K,L),f(L,M),x(M,N),minus(N,O),s(O,D,C).
s(A,B,C):-x(B,D),plus(A,E),y(E,F),f(F,G),minus(G,H),x(H,I),f(I,J),x(J,K),minus(K,L),f(L,M),y(M,N),plus(N,O),s(O,D,C).
s(A,B,C):-plus(B,D),plus(A,E),s(E,D,C).
s(A,B,C):-minus(B,D),minus(A,E),s(E,D,C).
s(A,B,C):-f(B,D),f(A,E),s(E,D,C).
s(A,B,B):-empty(A,B).
Ps = 6,
Pos = 44,
Neg = 58.
==
*/

% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
l_systems_constraints:target(s).
l_systems_constraints:invented(inv_1).
l_systems_constraints:invented(inv_2).
l_systems_constraints:invented(inv_3).
l_systems_constraints:invented(inv_4).
l_systems_constraints:invented(inv_5).
l_systems_constraints:invented(inv_6).
l_systems_constraints:preterminal(f).
l_systems_constraints:preterminal(g).
l_systems_constraints:preterminal(x).
l_systems_constraints:preterminal(y).
l_systems_constraints:preterminal(plus).
l_systems_constraints:preterminal(minus).
l_systems_constraints:preterminal(empty).

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[8]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[6]).
:-poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).
*/


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
poker_configuration:safe_example(m(s,Is,Os,[])):-
        K = 8
	,between(0,K,I)
	,length(Is,I)
	,between(0,K,J)
	,length(Os,J).

background_knowledge(s/3,[f/2
                         ,g/2
                         ,x/2
                         ,y/2
                         ,plus/2
                         ,minus/2
                         ,empty/2
                         ]).

metarules(s/3,[ls_constant,ls_variable,ls_base,chain,tri_chain]).

labelled_example(s/3,E):-
        generate_initial(hilbert_curve,all,0,3,Es)
        ,member(E,Es).
labelled_example(s/3,E):-
% The first Hilbert Curve string that contains variable symbols has
% length 11.
        generate_initial(hilbert_curve,all,11,12,Es)
        ,findall(s(Is,Os,[])
                ,(member(s(Is,Os,[]),Es)
                 ,( member(x,Is)
                  ; member(y,Is)
                  )
                 )
                ,Vs)
        ,G = member(E,Vs)
        ,limit(8, G).

unlabelled_example(s/3,_):- fail.

f --> [f].
g --> [g].
x --> [x].
y --> [y].
plus --> [+].
minus --> [-].
empty --> [].

% Generate examples for evaluation.
% Examples are generated by test harndes predicates.
%
% TODO: define in test_harness.pl. Not currently working.
%
generate_examples(pos,hilbert_curve,all,0,12).
generate_examples(neg,not_hilbert_curve,all,0,4).
