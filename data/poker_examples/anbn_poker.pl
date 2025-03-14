:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,initial_example/2
	       ,a/2
	       ,b/2
               ,empty/2
	       ,generate_examples/5
	       ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn an a^nb^n CFG with Poker.

There are three versions of anbn in data/poker_examples:

1. anbn_poker.pl (this file): no specific assumptions about language or
grammar.

2. anbn_poker_weak_setting.pl: assumes only that the target language is
Context Free and can be represented by a grammar in Chomsky Normal Form.

3. anbn_poker_strong_setting.pl: Assumes that the target language is
Context Free and can be represented by a grammar in a "natural",
informal form.

The three files differen in the set of metarules they define and in the
constraints they impose on those metarules. This may sound a bit
confusing but the differences between the three files, and the
assumptions made therein should help better understand how Poker can be
used, when different kinds or degrees or domain knowledge are available.
-------------------------------------------------------------------------


Check your configs and training data:

==
1 ?- [load_headless].
Loading poker
Global stack limit 2,147,483,648
Table space 2,147,483,648
true.

2 ?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), flatten_prove_all(Flatten), gestalt(Gestalt), greedy_generalisation(Greedy), respecialise(Respecialise), strict_clause_limit(Strict), proof_samples(Samples), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)), listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(s, Ls, [])) :-
    experiment_file:
    (   between(1, 9, L),
        length(Ls, L)
    ).

Fetch = [builtins,bk,metarules],
Table = Flatten, Flatten = Gestalt, Gestalt = Greedy, Greedy = Respecialise, Respecialise = Strict, Strict = false,
Untable = true,
Limit = 5,
Invented = 1,
Samples = 1.0,
Unlabelled = 100,
Order = deterministic,
Reduction = plotkins.

3 ?- poker_auxiliaries:list_mil_problem(s/2).
Initial examples
----------------
s([a,a,a,b,b,b],[]).

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

4 ?- _T = s/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 30,402,150 inferences, 0.969 CPU in 2.573 seconds (38% CPU, 31382865 Lips)
Hypothesis:
s(A,B):-inv_1_8(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_9(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_9(A,B):-s(A,C),b(C,B).
inv_1_8(A,B):-a(A,C),s(C,B).
Positive examples:
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,a,b,b,b,b],[]).
s([a,a,a,b,b,b],[]).
Negative examples:
s([a,b,b,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b],[]).
s([a,b,b,b,b],[]).
s([a,b,b,b],[]).
s([a,b,b],[]).
s([a,a,b,b,b,b,b,b,b],[]).
s([a,a,b,b,b,b,b,b],[]).
s([a,a,b,b,b,b,b],[]).
s([a,a,b,b,b,b],[]).
s([a,a,b,b,b],[]).
s([a,a,b],[]).
s([a,a,a,b,b,b,b,b,b],[]).
s([a,a,a,b,b,b,b,b],[]).
s([a,a,a,b,b,b,b],[]).
s([a,a,a,b,b],[]).
s([a,a,a,b],[]).
s([a,a,a,a,b,b,b,b,b],[]).
s([a,a,a,a,b,b,b],[]).
s([a,a,a,a,b,b],[]).
s([a,a,a,a,b],[]).
s([a,a,a,a,a,b,b,b,b],[]).
s([a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,a,b],[]).
Ps = 5,
Pos = 4,
Neg = 32.
==
*/

%/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

:-poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
:-poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false]).
:-poker_auxiliaries:set_configuration_option(untable_meta_interpreter, [true]).
:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[5]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[1]).
%*/


% Constraints to eliminate left recursions and ensure Chain instances
% have all second-order variables distinct.
configuration:metarule_constraints(m(identity, P0, P1), fail) :-
	P0==P1.
configuration:metarule_constraints(m(chain, P0, P1, _P2), fail) :-
	P0==P1.
configuration:metarule_constraints(m(chain, _P0, P1, P2), fail) :-
	P1==P2.

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

background_knowledge(s/2,[a/2,b/2,empty/2]).

metarules(s/2,[identity,chain]).

% For Poker
initial_example(s/2,s([a,a,a,b,b,b],[])).

% The background knowledge is the set of pre-terminals in the target
% language. a^nb^n does not include the empty string but we include it
% anyway: the learner should be able to not use it if it doesn't need
% it.
a --> [a].
b --> [b].
empty --> [].

generate_examples(pos,anbn,all,4,8).
generate_examples(neg,not_anbn,all,4,8).
