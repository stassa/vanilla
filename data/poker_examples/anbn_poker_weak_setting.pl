:-module(anbn, [background_knowledge/2
               ,metarules/2
               ,labelled_example/2
               ,unlabelled_example/2
               ,a/2
               ,b/2
               ,empty/2
               ,generate_examples/5
               ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(lib(poker/normal_forms/weak_chomsky_greibach_nf)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn a grammar of the context-free a^nb^n language with Poker.


There are three versions of anbn in data/poker_examples:

1. anbn_poker.pl: no specific assumptions about language or grammar.

2. anbn_poker_weak_setting.pl (this file): assumes only that the target
language is Context Free and can be represented by a grammar in Chomsky
Normal Form.

3. anbn_poker_strong_setting.pl: Assumes that the target language is
Context Free and can be represented by a grammar in a "natural",
informal form.

The three files differen in the set of metarules they define and in the
constraints they impose on those metarules. This may sound a bit
confusing but the differences between the three files, and the
assumptions made therein should help better understand how Poker can be
used, when different kinds or degrees or domain knowledge are available.
-------------------------------------------------------------------------

This version learns an a^nb^n grammar using the "natural" CFG
constraints in the "weak setting". The target theory is as follows:

==
S --> AB
S --> ASB
A --> a
B --> b
==


To reproduce check your configs and MIL problem elements:

==
1 ?- [load_headless].
Loading poker
Loading experiment file module data(poker_examples/anbn_poker_weak_setting.pl) from anbn.
Global stack limit 2,147,483,648
Table space 2,147,483,648

% Uncomment set_poker_configuration_option/2 block and make/0 to set
% the appropriate configuration options.
2 ?- make.
Loading experiment file module data(poker_examples/anbn_poker_weak_setting.pl) from anbn.
% c:/.../vanilla/data/poker_examples/anbn_poker_weak_setting compiled into anbn 0.00 sec, 0 clauses
true.

3 ?- poker_auxiliaries:list_poker_config.
clause_limit(3)
experiment_file(data(poker_examples/anbn_poker_weak_setting.pl),anbn)
flatten_prove_all(true)
gestalt(false)
greedy_generalisation(false)
listing_limit(15)
max_invented(0)
multithreading(false)
proof_samples(1.0)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
respecialise(false)
strict_clause_limit(false)
unlabelled_examples(50)
unlabelled_examples_order(random)
true.
==

Note the slightly increased table space limit. That's need for testing.

Note also that clause_limit(3): is more than necessary for the target
theory. This serves two purposes: first to demonstrate that Poker
doesn't (necessarily) over-generalise if the clause limit is too high
and second to allow some negative examples to be generated. If we set
the clause limit to "2", then the target theory is learned immediately
and there's are no negative examples generated (only new positive
examples).


==
4 ?- poker_auxiliaries:list_mil_problem(s/2).
Initial examples
----------------
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,b,b,b],[]).
s([a,a,a,a,b,b,b,b],[]).
s([a,a,a,a,a,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b,b,b,b,b,b],[]).

Background knowledge (First Order)
----------------------------------
a/2:
a([a|A],A).

b/2:
b([b|A],A).

empty/2:
empty(A,B):-A=B.

Background knowledge (Second Order)
-----------------------------------
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Tri-chain) ∃.P,Q,R,S ∀.x,y,z,u: P(x,y)← Q(x,z),R(z,u),S(u,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(M, fail) :-
    grammar_constraints:
    (   ground(M),
        M=..[m, _Id, P, P|_Ps]
    ).
metarule_constraints(m(identity, P0, _P1), fail) :-
    grammar_constraints:
    (   ground(P0),
        \+ target(P0)
    ).
metarule_constraints(m(identity, _P0, P1), fail) :-
    grammar_constraints:
    (   ground(P1),
        \+ preterminal(P1)
    ).
metarule_constraints(M, fail) :-
    grammar_constraints:
    (   ground(M),
        M=..[m, Id, _P|Ps],
        Id\==identity,
        memberchk(empty, Ps)
    ).
metarule_constraints(m(chain, _P0, P1, _), fail) :-
    grammar_constraints:
    (   ground(P1),
        target(P1)
    ).
metarule_constraints(m(tri_chain, _P0, P1, _, _), fail) :-
    grammar_constraints:
    (   ground(P1),
        target(P1)
    ).
metarule_constraints(m(chain, P0, P1, _), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        invented(P0),
        invented(P1)
    ).
metarule_constraints(m(tri_chain, P0, P1, _, _), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        invented(P0),
        invented(P1)
    ).
metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        P0==P1
    ).
metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    grammar_constraints:
    (   ground(P1),
        ground(P2),
        P1==P2
    ).
metarule_constraints(m(tri_chain, P0, P1, _P2, _P3), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        P0==P1
    ).
metarule_constraints(m(tri_chain, _P0, P1, P2, _P3), fail) :-
    grammar_constraints:
    (   ground(P1),
        ground(P2),
        P1==P2
    ).
metarule_constraints(m(tri_chain, _P0, _P1, P2, P3), fail) :-
    grammar_constraints:
    (   ground(P2),
        ground(P3),
        P2==P3
    ).

true.
==


Expected results:

==
5 ?- _T = s/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 4,217,611 inferences, 0.234 CPU in 0.777 seconds (30% CPU, 17995140 Lips)
Hypothesis:
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
Positive examples:
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,b,b,b],[]).
s([a,a,a,a,b,b,b,b],[]).
s([a,a,a,a,a,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b,b,b,b,b,b],[]).
Negative examples:
s([b,a,a,a,b],[]).
s([a,b,a,b,b],[]).
s([a,b,b,b],[]).
s([a,b,a,a,b],[]).
s([a,a,b,b,a],[]).
s([a,a,a,a],[]).
s([b,a,a],[]).
s([a,b,a],[]).
s([b,a,b,a],[]).
s([a,a,b,a,a],[]).
s([b,a,a,a,a],[]).
s([a,a,b],[]).
s([a,b,b,a,a],[]).
s([b,a,b],[]).
s([b,a,a,b],[]).
s([a,b,a,a,a],[]).
s([a,b,a,b],[]).
s([b,b,a],[]).
s([a,b,b,a,b],[]).
s([b,b],[]).
s([a,a,a,a,b],[]).
s([a,a],[]).
s([a,a,b,a],[]).
s([b,b,b],[]).
s([a,b,b,a],[]).
s([a,a,a,b,a],[]).
s([a,a,a,b,b],[]).
s([a,a,b,a,b],[]).
s([a,a,a,b],[]).
s([a,b,b],[]).
s([b],[]).
s([b,b,a,a],[]).
s([b,b,b,a],[]).
s([a,b,a,b,a],[]).
s([a,b,b,b,a],[]).
s([a],[]).
s([b,b,a,b],[]).
s([b,b,b,b],[]).
s([b,a,a,a],[]).
s([],[]).
s([a,b,b,b,b],[]).
s([a,a,b,b,b],[]).
s([a,a,a,a,a],[]).
s([a,a,a],[]).
s([a,b,a,a],[]).
s([b,a,a,b,a],[]).
s([b,a,b,b],[]).
s([b,a],[]).
Ps = 2,
Pos = 6,
Neg = 48.
==

*/

/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[3]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[0]).
:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[50]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).
*/

% Language alphabet for the constraints defeined
% in grammar_constraints_weak_setting.pl
%
weak_cgnf:target(s).
weak_cgnf:invented(inv_1).
weak_cgnf:preterminal(a).
weak_cgnf:preterminal(b).
weak_cgnf:preterminal(empty).


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
	between(0,9,L)
	,length(Ls,L).

background_knowledge(s/2,[a/2,b/2,empty/2]).

metarules(s/2,[identity,chain,tri_chain]).

labelled_example(s/2,E):-
	generate_initial(anbn,all,0,12,Es)
        ,distinct( member(E,Es) ).

unlabelled_example(s/2,_):- fail.

% The background knowledge is the set of pre-terminals in the language.
a --> [a].
b --> [b].
empty --> [].

generate_examples(pos,anbn,all,0,100).
generate_examples(neg,not_anbn,all,0,10).
