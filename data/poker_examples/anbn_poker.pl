:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,labelled_example/2
	       ,unlabelled_example/2
	       ,set_configs/0
	       ,a/2
	       ,b/2
               ,empty/2
	       ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn an a^nb^n CFG with Poker.

This experiment file shows how to use Poker in a free-style manner,
without a Normal Form. Constraints on the two metarules, Chain and
Identity, are still used to restrict redundancy in the initial set of
hypotheses.

1. Check your configs and training data:

==
1 ?- [load_headless].
Loading poker
Global stack limit 2,147,483,648
Table space 2,147,483,648
true.

% Remember to setup good configuration options:

?- experiment_file:set_configs.
true.

?- auxiliaries:list_config.
encapsulation_predicate(m)
example_clauses(call)
fetch_clauses([builtins,bk,metarules])
invented_symbol_prefix(inv_)
learner(poker,lib(poker/poker))
metarule_formatting(quantified)
metasubstitution_atoms(existential)
table_meta_interpreter(false)
untable_meta_interpreter(true)
true.

?- poker_auxiliaries:list_poker_config.
clause_limit(5)
experiment_file(data(poker_examples/anbn_poker.pl),anbn)
flatten_prove_all(false)
generalise_conjunction(false)
gestalt(false)
greedy_generalisation(false)
listing_limit(15)
max_invented(1)
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
unlabelled_examples_order(deterministic)
true.

% List experiment data:

?- poker_auxiliaries:list_mil_problem(s/2).
Labelled examples
-----------------
s([a,a,a,b,b,b],[]).

Unlabelled examples
-------------------

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

metarule_constraints(m(identity, A, B), fail) :-
    experiment_file:(A==B).
metarule_constraints(m(chain, A, B, _), fail) :-
    experiment_file:(A==B).
metarule_constraints(m(chain, _, A, B), fail) :-
    experiment_file:(A==B).

true.


% safe_example/1 is used to limit the length of strings in generated
% examples to avoid infinite generation.

?- listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(s, A, [])) :-
    experiment_file:
    (   between(1, 9, B),
        length(A, B)
    ).

true.
==


2. Make a learning query.

Expected result:

==
?- _T = s/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 29,810,247 inferences, 0.750 CPU in 1.900 seconds (39% CPU, 39746996 Lips)
Hypothesis:
s(A,B):-a(A,C),s(C,D),b(D,B).
s(A,B):-a(A,C),b(C,B).
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
Ps = 2,
Pos = 4,
Neg = 32.
==

Note how the output hypothesis includes a clause with three body
literals that is not an instance of either of the metarules Chain and
Identity, given as second-order background knowledge:

==
Hypothesis:
s(A,B):-a(A,C),s(C,D),b(D,B).
s(A,B):-a(A,C),b(C,B).
==

In truth the learned hypothesis only includes clauses of Chain and
Identity, none of which has three body literals, like the first clause
above. The learned hypothesis is unfolded to remove invented predicates
and the result is the hypothesis above.

You can see this if you set the option unfold_invented/1 to "false", by
changing it in set_configs/0 like this:

==
poker_auxiliaries:set_poker_configuration_option(unfold_invented,[none])
==

Remember to call set_configs/0 after changing that option:
==
?- experiment_file:set_configs.
true.
==

Now, if you make a learning query again, you can see the underlying
representation, before unfolding, with invented predicates all out in
the open:

==
?- _T = s/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), auxiliaries:print_clauses('Hypothesis:',_Ps), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 29,679,813 inferences, 0.656 CPU in 1.856 seconds (35% CPU, 45226382 Lips)
Hypothesis:
s(A,B):-inv_1_8(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_9(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_9(A,B):-s(A,C),b(C,B).
inv_1_8(A,B):-a(A,C),s(C,B).
Ps = 5,
Pos = 4,
Neg = 32.
==

You can also print out the positive and negative labelled examples to
convince yourself that the two hypotheses, the one with invented
predicates, and the unfolded one, accept and reject the same examples.
This is left as an exercise etc. etc.

*/

set_configs:-
	poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
	,poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false])
	,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[none])
	,poker_auxiliaries:set_configuration_option(untable_meta_interpreter, [true])
	,poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
	,poker_auxiliaries:set_poker_configuration_option(max_invented,[1]).

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

labelled_example(s/2,s([a,a,a,b,b,b],[])).

unlabelled_example(s/2,_):- fail.

a --> [a].
b --> [b].
empty --> [].
