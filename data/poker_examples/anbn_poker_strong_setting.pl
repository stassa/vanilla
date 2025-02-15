:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,initial_example/2
	       ,a/2
	       ,b/2
               ,empty/2
	       ,generate_examples/5
	       ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_configuration),[]).
:-use_module(grammar_constraints).
:-use_module(data(poker_examples/test_harness)).

/** <module> Learn an a^nb^n CFG with Poker.

Variant of anbn_poker.pl with stronger constraints, imposing a
combination of Chomsky Normal Form and Greibach Normal Form to the
instantiation of metarules.

The "strength" of this set of constraints is in the fact that it is not
specific to the problem being solved (learning a grammar of a^nb^n) but
instead reprseents a general category of programs (context-free grammars
in Chomsky Normal Form) and so enforces a self-supervised learning
setting where we assume no knowledge of the target theory.

I mean in truth we know what the target theory is, duh, but we pretend
we don't just for a bit, to check whether stuff still works without that
explicit knowledge. That's generally the done thing in machine learning
research. Otherwise, how can we estimate the error of our systems on
truly unseen data?

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
metarule_constraints(m(chain, P0, P1, _), fail) :-
    grammar_constraints:
    (   ground(P0),
        ground(P1),
        invented(P0),
        invented(P1)
    ).

true.
==


Expected results:

==
4 ?- _T = s/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:','Positive examples:','Negative examples:'],[_Ps,_Pos,_Neg]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 21,034,018 inferences, 10.406 CPU in 34.289 seconds (30% CPU, 2021287 Lips)
Hypothesis:
s(A,B):-inv_1_53(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_53(A,B):-a(A,C),s(C,B).
Positive examples:
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,b,b,b],[]).
Negative examples:
s([b,b,b,b,b,b],[]).
s([b,b,b,b,b],[]).
s([b,b,b,b,a,b],[]).
s([b,b,b,b,a,a],[]).
s([b,b,b,b],[]).
s([b,b,b,a,b,b],[]).
s([b,b,b,a,b],[]).
s([b,b,b,a,a,b],[]).
s([b,b,b,a,a,a],[]).
s([b,b,b,a,a],[]).
s([b,b,b],[]).
s([b,b,a,b,b,b],[]).
s([b,b,a,b,b],[]).
s([b,b,a,b,a,b],[]).
s([b,b,a,b,a,a],[]).
s([b,b,a,b],[]).
s([b,b,a,a,b,b],[]).
s([b,b,a,a,b],[]).
s([b,b,a,a,a,b],[]).
s([b,b,a,a,a,a],[]).
s([b,b,a,a,a],[]).
s([b,b,a,a],[]).
s([b,b],[]).
s([b,a,b,b,b,b],[]).
s([b,a,b,b,b],[]).
s([b,a,b,b,a,b],[]).
s([b,a,b,b,a,a],[]).
s([b,a,b,b],[]).
s([b,a,b,a,b,b],[]).
s([b,a,b,a,b],[]).
s([b,a,b,a,a,b],[]).
s([b,a,b,a,a,a],[]).
s([b,a,b,a,a],[]).
s([b,a,b],[]).
s([b,a,a,b,b,b],[]).
s([b,a,a,b,b],[]).
s([b,a,a,b,a,b],[]).
s([b,a,a,b,a,a],[]).
s([b,a,a,b],[]).
s([b,a,a,a,b,b],[]).
s([b,a,a,a,b],[]).
s([b,a,a,a,a,b],[]).
s([b,a,a,a,a,a],[]).
s([b,a,a,a,a],[]).
s([b,a,a,a],[]).
s([b,a,a],[]).
s([a,b,b,b,b,b],[]).
s([a,b,b,b,b],[]).
s([a,b,b,b,a,b],[]).
s([a,b,b,b,a,a],[]).
s([a,b,b,b],[]).
s([a,b,b,a,b,b],[]).
s([a,b,b,a,b],[]).
s([a,b,b,a,a,b],[]).
s([a,b,b,a,a,a],[]).
s([a,b,b,a,a],[]).
s([a,b,b],[]).
s([a,b,a,b,b,b],[]).
s([a,b,a,b,b],[]).
s([a,b,a,b,a,b],[]).
s([a,b,a,b,a,a],[]).
s([a,b,a,b],[]).
s([a,b,a,a,b,b],[]).
s([a,b,a,a,b],[]).
s([a,b,a,a,a,b],[]).
s([a,b,a,a,a,a],[]).
s([a,b,a,a,a],[]).
s([a,b,a,a],[]).
s([a,a,b,b,b,b],[]).
s([a,a,b,b,b],[]).
s([a,a,b,b,a,b],[]).
s([a,a,b,b,a,a],[]).
s([a,a,b,a,b,b],[]).
s([a,a,b,a,b],[]).
s([a,a,b,a,a,b],[]).
s([a,a,b,a,a,a],[]).
s([a,a,b,a,a],[]).
s([a,a,b],[]).
s([a,a,a,b,b],[]).
s([a,a,a,b,a,b],[]).
s([a,a,a,b,a,a,a],[]).
s([a,a,a,b,a,a],[]).
s([a,a,a,b],[]).
s([a,a,a,a,b,b,b],[]).
s([a,a,a,a,b,b],[]).
s([a,a,a,a,b,a,b],[]).
s([a,a,a,a,b,a,a],[]).
s([a,a,a,a,b],[]).
s([a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a],[]).
s([a,a,a,a,a,a],[]).
s([a,a,a,a,a],[]).
s([a,a,a,a],[]).
s([a,a,a],[]).
s([a,a],[]).
Ps = Pos, Pos = 3,
Neg = 97.
==

*/

% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
grammar_constraints:target(s).
grammar_constraints:invented(inv_1).
grammar_constraints:preterminal(a).
grammar_constraints:preterminal(b).
grammar_constraints:preterminal(empty).


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

initial_example(s/2,s([a,a,a,b,b,b],[])).

% The background knowledge is the set of pre-terminals in the language.
% a^nb^n does not include the empty string.
a --> [a].
b --> [b].
empty --> [].

generate_examples(pos,anbn,all,0,4).
generate_examples(neg,not_anbn,all,0,4).
