:-module(anbm, [background_knowledge/2
	       ,metarules/2
	       ,initial_example/2
	       ,a/2
	       ,b/2
               ,empty/2
	       ,generate_examples/5
	       ]).

:-use_module(grammar_constraints).
:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).

/** <module> Learn a grammar of the context-free language {a^nb^m|n >= m >= 0}.

Work in progress. Currently works with fixed examples:

==
Loading poker
Global stack limit 2,147,483,648
Table space 17,179,869,184


?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), flatten_prove_all(Flatten), gestalt(Gestalt), greedy_generalisation(Greedy), respecialise(Respecialise), strict_clause_limit(Strict), proof_samples(Samples), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)), listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(s, Ls, [])) :-
    anbm:
    (   between(0, 9, L),
        length(Ls, L)
    ).

Fetch = all,
Table = Untable, Untable = Flatten, Flatten = Respecialise, Respecialise = true,
Limit = 4,
Invented = 1,
Gestalt = Greedy, Greedy = Strict, Strict = false,
Samples = 1.0,
Unlabelled = 1500,
Order = random,
Reduction = plotkins.


?- poker_auxiliaries:list_mil_problem(s/2).
Initial examples
----------------
s([],[]).
s([a,a,a,b],[]).
s([a,a,a],[]).

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
?- _T = s/2, time( poker:learn(_T,_Pos,_Neg,_Ps) ), maplist(auxiliaries:print_clauses,['Hypothesis:'],[_Ps]), maplist(length,[_Ps,_Pos,_Neg],[Ps,Pos,Neg]).
% 5,877,251 inferences, 0.672 CPU in 0.855 seconds (79% CPU, 8747536 Lips)
Hypothesis:
s(A,B):-empty(A,B).
s(A,B):-inv_1_47(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
inv_1_47(A,B):-a(A,C),s(C,B).
Ps = 4,
Pos = 30,
Neg = 993.
==

This seems to be close, but no cigar:
==
?- between(0,5,_N), length(Xs,_N), phrase(anbm:s,Xs).
Xs = [] ;
Xs = [a] ;
Xs = [a,b] ;
Xs = [a,a] ;
Xs = [a,a,b] ;
Xs = [a,a,b] ;
Xs = [a,a,a] ;
Xs = [a,a,b,b] ;
Xs = [a,a,a,b] ;
Xs = [a,a,a,b] ;
Xs = [a,a,a,b] ;
Xs = [a,a,a,a] ;
Xs = [a,a,a,b,b] ;
Xs = [a,a,a,b,b] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,b,b] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,a,a] ;
false.
==

This is what the target theory does:
==
?- between(0,5,_N), length(Xs,_N), phrase(test_harness:anbm,Xs).
Xs = [] ;
Xs = [a] ;
Xs = [a,a] ;
Xs = [a,b] ;
Xs = [a,a,a] ;
Xs = [a,a,b] ;
Xs = [a,a,b] ;
Xs = [a,a,a,a] ;
Xs = [a,a,a,b] ;
Xs = [a,a,a,b] ;
Xs = [a,a,a,b] ;
Xs = [a,a,b,b] ;
Xs = [a,a,a,a,a] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,b,b] ;
Xs = [a,a,a,a,b] ;
Xs = [a,a,a,b,b] ;
Xs = [a,a,a,b,b] ;
false.
==


*/

%/*
% Raises error despite importing poker_auxiliaries. Why?
% Best way to use currently is to load file for the first time when this
% is commented out, then uncomment and reload the file (with make/0).

%:-poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
%:-poker_auxiliaries:set_configuration_option(table_meta_interpreter, [false]).
%:-poker_auxiliaries:set_configuration_option(untable_meta_interpreter, [true]).
:-poker_auxiliaries:set_poker_configuration_option(clause_limit,[4]).
:-poker_auxiliaries:set_poker_configuration_option(max_invented,[1]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[1500]).
:-poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
						  ,[random]).

:-poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true]).

%:-poker_auxiliaries:set_poker_configuration_option(gestalt,[true]).
:-poker_auxiliaries:set_poker_configuration_option(respecialise,[true]).

%*/


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
	between(0,9,L)
	,length(Ls,L).

background_knowledge(s/2,[a/2,b/2,empty/2]).

metarules(s/2,[identity,chain]).

initial_example(s/2,s([],[])).
initial_example(s/2,s([a,a,a,b],[])).
initial_example(s/2,s([a,a,a],[])).
initial_example_(s/2,E):-
	generate_initial(anbm,2,0,4,Es)
        ,distinct( member(E,Es) ).


% The background knowledge is the set of pre-terminals in the language.
% a^nb^n does not include the empty string.
a --> [a].
b --> [b].
empty --> [].

generate_examples(pos,anbm,all,0,4).
generate_examples(neg,not_anbm,all,0,4).


/*
% Target theory, with invented predicate.
% Actually not sure this is right.

s(A,B):- empty(A,B).
s(A,B):- a(A,C), s(C,B).
s(A,B):- a(A,C), inv_1(C,B).
inv_1(A,B):- b(A,B).
*/
/*
s(A,B):-empty(A,B).
s(A,B):-inv_1_47(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
inv_1_47(A,B):-a(A,C),s(C,B).
*/
