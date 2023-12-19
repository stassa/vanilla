:-module(vanilla, [refresh_tables/1
		  ,prove/6
		  ,bind_head_literal/3
		  ,check_constraints/1
		  ,free_member/2
		  ,constraints/1
                  ]).

:-use_module(project_root(configuration)).
:-use_module(src(auxiliaries)).

/** <module> An inductive Prolog meta-interpreter.

*/

% Tabling of prove/7 may be overriden by configuration options at
% runtime. See refresh_tables/1.
%
:-table(prove/7).


%!	refresh_tables(+Action) is det.
%
%	Table or untable prove/7.
%
%	The Vanilla meta-interpreter implemented in prove/7 uses
%	tabling, a.k.a. SLG-Resolution to avoid infinite left-recursions
%	during learning. This allows Vanilla to learn programs with
%	arbitrary recursive structure, left-recursive, or not.
%
%	At the same time, tabling execution replaces SLD-Resolution's
%	Depth-First Search of an SLD-tree with a Breadth-First Search
%	(BFS). BFS has exponential _space_ complexity so this can make
%	some programs impossible to learn without large amounts of
%	memory.
%
%	This predicate gives the user the option to table or untable
%	prove/7 depending on their recursion needs. It is meant to be
%	used in one of two ways: either in a learning predicate calling
%	prove/7; or in the configuration.
%
%	__ Used in learning predicates __
%
%	A learning predicate implemented using Vanilla might want to
%	ensure tables are refreshed between learning attempts.
%
%	As an example of this use, refresh_tables/1 is called by learn/5
%	in Poker, as follows:
%	==
%	learn(Pos,Neg,BK,MS,Ps):-
%	        poker_configuration:clause_limit(K)
%	        ,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
%	        ,S = (write_problem(user,[BK_],Refs)
%	             ,refresh_tables(untable)
%		     ,refresh_tables(table)
%	         )
%	         % ... Code calling prove/7
%	        ,C = (erase_program_clauses(Refs)
%		     ,refresh_tables(untable)
%	             )
%	==
%
%	__ Use in configuration __
%
%	To give the user control over tabling and untabling of prove/7,
%	without having to call this predicate at the top-level, the
%	following configuration options can be set:
%	==
%	table_meta_interpreter/1
%	untable_meta_interpreter/1
%	==
%
%	Those configuration options are declared as multifile and
%	dynamic in configuration.pl. They should be defined in the
%	configuration files of learners based on Vanilla.
%
%	Note that there is some slowdown when this predicate is called
%	between learning queries with different clause_limit/1 settings.
%	This may be because adding and removing tabling with table/1 and
%	untable/1, as done in this predicate, leaves behind some
%	garbage. The SWI-Prolog documentationr states that table/1 and
%	untable/1 are meant to be used at the top-level and that
%	abolish_table_subgoals/1 should be used instead to cleanup
%	tables programmatically. However, that predicate seems to cause
%	even more slowdown than table/1 and untable/1, so we're going
%	with the latter.
%
refresh_tables(table):-
	configuration:table_meta_interpreter(true)
	,!
	,table(prove/7)
	,debug(tabling,'Tabled prove/7',[]).
refresh_tables(table):-
	configuration:table_meta_interpreter(false)
	,!
	,debug(tabling,'Not tabling prove/7',[]).
refresh_tables(untable):-
	configuration:untable_meta_interpreter(false)
	,!
	,debug(tabling,'Not un-tabling prove/7',[]).
refresh_tables(untable):-
	configuration:untable_meta_interpreter(true)
	,untable(prove/7)
	,debug(tabling,'Un-tabled prove/7',[]).



%!	prove(?Literals,+Limit,+Metarules,+Sig,+Acc,-Metasubs) is
%!	nondet.
%
%	A vanilla MIL meta-interpreter for Inductive Logic Programming.
%
%	This predicate implements second-order SLD-Resolution prover as
%	a higher-order Prolog meta-interpreter. What makes this
%	meta-interpreter higher order is the ability to fetch from, and
%	resolve with, second-order definite clauses from a list of
%	"Metarules". By unification, the second-order variables in
%	Metarules are bound during resolution thus constructing a
%	first-order program.
%
%	Literals is a set of encapsulated literals (as a "tree" of
%	Prolog terms, in parentheses), the current proof-goals. Goals in
%	Literals are resolved in Last-In, First-Out order, therefore
%	implementing a stack and, consequently, a Depth-First Search.
%	However, if prove/7 is tabled, the execution order switches from
%	Depth-First, to Breadth-First search (and this can make
%	debugging a little bit confusing).
%
%	Limit is the clause limit defined in the configuration option
%	clause_limit/1. Limit restricts the number of distinct clauses
%	in a refutation sequence.
%
%	Metarules is a list of second-order definite clauses to be used
%	in the proof by SLD-Refutation of each member of Literals.
%	Metarules are in "expanded" form, as returned by
%	expanded_metarules/2.
%
%	Sig is the learning signature, a list of predicate symbols (only
%	symbols - without arities!) of the target predicates for a
%	learning attempt. Only symbols in Sig will be allowed to be
%	bound to the second-order variables in head literals of
%	metarules and so only clauses of those predicates will be found
%	in the returned list of metasubstitutions.
%
%	Acc is the accumulator of metasubstitutions.
%
%	Metasubs is a list of metasubstitution terms derived during the
%	proof. Metasubstitution terms are of the form:
%
%	m(Id,Tgt,P1,...,Pn)/m(Id,V1,...,Vm)
%
%	Where Id is the atomic identifier of a metarule in Metarules;
%	Tgt is the predicate symbol (without arity) of one target
%	predicate, each Pi is the symbol of a background predicate
%	(which can also be the target predicate, or an invented
%	predicate, as well as a predicate actually defined in the BK),
%	and each Vi is a ground first-order term.
%
%	Metasubstitution atoms can be applied to the metarule with
%	identifier Id to substitute that metarule's existentially or
%	universally quantified variables with the symbols, or terms, in
%	the two atoms of the metasubstitution term.
%
%	When this predicate is first called, Literals should be a single
%	encapsulated literal, one example of one target predicate. As
%	the SLD-refutation proof proceeds, that original goal of the
%	proof is replaced by goals derived by SLD-Resolution.
%
%	When the proof completes, if it completes successfully,
%	Metasubs should be a list of metasubstitution atoms, each of
%	which can be applied to its corresponding metarule (identified
%	by the metasubstitution atom's first, Id, argument) to produce a
%	first-order clause. In that sense, the clauses of Metasubs are
%	the set of clauses in one refutation sequence of the original
%	example Literal.
%
%	The number of metasubstitution atoms in Metasubs can be at most
%	equal to Limit.
%
%	Since Metasubstitutions is a list of atoms in a refutation
%	sequence, this meta-interpreter is capable of inducing, for each
%	single example, an entire proof-branch refuting that example. In
%	other words, this is is a multi-clause learning inductive
%	meta-interpreter, capable of learning entire programs from one
%	example.
%
%	What is the motivation for multi-clause learning? Traditional
%	ILP systems like Aleph or Progol learn a single "rule" from each
%	positive example (and then remove examples "covered" by the
%	rule). That works fine until an example is found that cannot be
%	covered by a single rule. This is often the case with recursive
%	programs and certainly the case with predicate invention. The
%	ability to learn multiple clauses that resolve with each other
%	to refute a goal-example allows MIL systems based on Vanilla to
%	learn recursive programs and to perform predicate invention
%	without the restrictions of earlier systems.
%
prove(Ls,K,MS,Ss,Subs,Subs_):-
	process_options(Os)
	,debug(fetching,'Fetching options ~w',[Os])
	,prove(Ls,K,MS,Ss,Os,Subs,Subs_).



%!	process_options(-Options) is det.
%
%	Generate a list of clause-fetching Options for clause/8.
%
%	Options is a list of clause-fetching options, as described in
%	the configuration option fetch_clauses/1. Options is a list of
%	the form [BK,Builtins,Hypothesis,Metarules], the elements of
%	which determine the sets of clauses from which clauses are
%	selected to be resolved with goals, in prove/7. See
%	fetch_clauses/1 for more information.
%
process_options(Os):-
	configuration:fetch_clauses(all)
	,!
	,sort([bk, builtins, hypothesis, metarules],Os).
process_options(Os):-
	configuration:fetch_clauses(Ss)
	,maplist(sort,[Ss,[bk, builtins, hypothesis, metarules]],[Ss_s,Ds])
	,process_fetch_options(Ss_s,Ds,[],Os).


%!	process_fetch_options(+Options,+Defaults,+Acc,-Processed) is
%!	det.
%
%	Determine which Options, from a list of Defaults, will be used.
%
%	Options is a list of clause fetching options each of which is a
%	term in: [bk, builtins, hypothesis, metarules].
%
%	Defaults is a list of all those options, in that order (i.e.
%	exactly the list [bk, builtins, hypothesis, metarules]).
%
%	Processed is the list of Defaults, where each option that does
%	not appear in Options is replaced with the same term, with the
%	atom "not_" prepended. Thus, the list Processed will contain the
%	terms in Defaults, or one of the terms in: [not_bk,
%	not_builtins, not_hypothesis, not_metarules], each replacing the
%	corresponding option, if that option was not found in Options.
%
process_fetch_options([O|Ws],[O|Ds],Acc,Bind):-
	!
	,process_fetch_options(Ws,Ds,[O|Acc],Bind).
process_fetch_options(Ws,[O|Ds],Acc,Bind):-
	atomic_concat(not_,O,Not_O)
	,!
	,process_fetch_options(Ws,Ds,[Not_O|Acc],Bind).
process_fetch_options([],_,Acc,Os):-
	reverse(Acc,Os).



%!	prove(?Literals,+Limit,+Metarules,+Sig,+Options,+Acc,-Metasubs)
%!	is nondet.
%
%	Business end of prove/6.
%
%	This is the actual implementation of the Meta-Interpretive
%	Learning meta-interpreter described in prove/6, whereas prove/6
%	is an interface used to set clause-fetching options according
%	to the configuration settings.
%
%	Literals, Limit, Metarules, Sig, Acc and Metasubs are as in
%	prove/6.
%
%	Options is a list of clause-fetching options as described in the
%	configuration option fetch_clauses/1, and used to control the
%	behaviour of clause/8.
%
prove(true,_K,_MS,_Ss,_Os,Subs,Subs):-
	!
        ,debug(prove_steps,'Reached proof leaf.',[])
	,debug(prove_metasubs,'Metasubs so-far: ~w',[Subs]).
prove((L,Ls),K,MS,Ss,Os,Subs,Acc):-
	debug(prove_steps,'Splitting proof at literals ~w -- ~w',[L,Ls])
	,prove(L,K,MS,Ss,Os,Subs,Subs_)
	,prove(Ls,K,MS,Ss,Os,Subs_,Acc).
prove((L),K,MS,Ss,Os,Subs,Acc):-
        L \= (_,_)
	,L \= true
        ,debug(prove_steps,'Proving literal: ~w.',[L])
	,clause(Os,L,K,MS,Ss,Subs,Subs_,Ls)
	,debug(prove_metasubs,'New Metasubs: ~w',[Subs_])
        ,debug(prove_steps,'Proving body literals of clause: ~w',[L:-Ls])
        ,prove(Ls,K,MS,Ss,Os,Subs_,Acc).
/* % Uncomment for richer debugging and logging.
prove(L,_K,_MS,_Ss,_Os,Subs,_Acc):-
	L \= true
        ,debug(prove,'Failed to prove literals: ~w',[L])
	,debug(prove,'Metasubs so-far: ~w',[Subs])
	,fail.
*/



%!	clause(+Options,?Literal,+K,+MS,+Sig,+Subs,-Subs_New,-Body) is
%!	nondet.
%
%	MIL-specific clause/2 variant.
%
%	This predicate is similar to clause/2 except that if the body of
%	a clause with the given Literal as head can't be found in the
%	program database, the metasubstitution store Subs is searched
%	for a known metasubstitution whose encapsulated head literal
%	unifies with Literal. If that fails, a new metasubstitution is
%	constructed and added to the store.
%
%	Options is a list of clause-fetching options, as described in
%	the configuration option fetch_clauses/1. Options is a list of
%	the form [BK,Builtins,Hypothesis,Metarules], the elements of
%	which determine the sets of clauses from which clauses are
%	selected to be resolved with goals, in prove/7. See
%	fetch_clauses/1 for more information.
%
%	Literal is a partially or fully instantiated literal to be
%	proved.
%
%	K is an integer, the clause limit defined in the configuration
%	option clause_limit/1. This limits the number of new
%	metasubstitutions added to the metasubstitution store.
%
%	MS is the set of metarules for the current MIL Problem.
%
%	Sigs is the predicate signature, a list of _atoms_ (not yet
%	predicate identifiers).
%
%	Subs is a list of encapsulated metasubstitution atoms.
%
%	Subs_New is the list Subs with any new metasubstitution
%	constructed.
%
%	Body is the body literals of Literal found in the database, or a
%	metasubstitution already in Subs, or a new one constructed by
%	new_metasub/6.
%
clause(_Os,_L,_K,_MS,_Ss,Subs,_Acc,_Ls):-
	\+ check_constraints(Subs)
	,!
	,fail.
clause([_BK,builtins,_Hypothesis,_Metarules],L,_K,_MS,_Ss,Subs,Subs,true):-
	(   predicate_property(L,foreign)
	;    built_in_or_library_predicate(L)
	)
	,debug(fetch,'Proving built-in literal: ~w', [L])
        ,call(L)
	,debug(fetch,'Proved built-in clause: ~w', [L:-true]).
clause([bk,_Builtins,_Hypothesis,_Metarules],L,_K,_MS,_Ss,Subs,Subs,Ls):-
	\+ predicate_property(L,foreign)
	,\+ built_in_or_library_predicate(L)
	,debug(fetch,'Proving literal with BK: ~w', [L])
        ,clause(L,Ls)
	,debug(fetch,'Trying BK clause: ~w', [L:-Ls]).
clause([_BK,_Builtins,hypothesis,_Metarules],L,_K,MS,_Ss,Subs,Subs,Ls):-
        debug(fetch,'Proving literal with known metasubs: ~w',[L])
        ,known_metasub(L,MS,Subs,Ls).
clause([_BK,_Builtins,_Hypothesis,metarules],L,K,MS,Ss,Subs,Subs_,Ls):-
	length(Subs,N)
	,N < K
        ,debug(fetch,'Proving literal with new metasub: ~w',[L])
        ,new_metasub(L,MS,Ss,Subs,Subs_,Ls).



%!	check_constraints(+Metasubs) is det.
%
%	True if all ground metasubstitutions obey constraints.
%
%	Metasubs is the list of metasubstitutions derived so-far. For
%	each metasubstitution in Metasubs, this predicate checks that it
%	does not violate any constraint defined in a
%	metarule_constraints/2 clause.
%
check_constraints(Subs):-
	forall(member(Sub,Subs)
	      ,constraints(Sub)
	      ).



%!	known_metasub(?Literal,+Subs,-Body) is nondet.
%
%	Selects a known metasubstition whose head unifies with Literal.
%
known_metasub(L,MS,Subs,Ls):-
	member(Sub,Subs)
        ,applied_metasub(MS,Sub,L,Ls)
	,debug(fetch,'Trying known metasub: ~w',[Sub]).


%!	applied_metasub(+Metarules,?Metasubstitution,?Head,-Body)
%!	is nondet.
%
%	Get the encapsulated body literals of a Metasubstitution.
%
applied_metasub(MS, Sub/_, H, B):-
        free_member(Sub/_Sub_U:-(H,B),MS)
	,!.
applied_metasub(MS, Sub/_, L, true):-
	free_member(Sub/_Sub_U:-(L),MS).


%!	free_member(?Element,?List) is nondet.
%
%	member/2 variant that copies elements without unifying them.
%
%	Used to avoid binding all instances of a metarule throughout a
%	proof branch.
%
free_member(Z,Xs):-
	free_member(_X,Xs,Z).

%!	free_member(?Element,?List,?Copy) is nondet.
%
%	Business end of free_member/2.
%
free_member(X,[Y|_],Z):-
	unifiable(X,Y,_)
	,copy_term(Y,Y_)
	% Unifying in copy_term/2 may fail.
	,Y_ = Z.
free_member(X,[_|Ys],Z):-
	free_member(X,Ys,Z).



%!	new_metasub(?Literal,+MS,+Sig,+Subs,-New_Subs,-Body) is nondet.
%
%	Constructs new metasubstitutions whose heads unify with Literal.
%
new_metasub(L,MS,Ss,Subs,[Sub|Subs],Ls):-
        member(M,MS)
        ,applied_metasub(Sub,M,Ss,L,Ls)
	,debug(fetch,'Added new metasub: ~w',[Sub]).


%!	applied(?Metasubstitution,+Metarule,+Sig,?Head,-Body) is
%!	nondet.
%
%	Construct a new Metasubstitution whose head unifies with Head.
%
applied_metasub(Sub, M, Ss, H, Ls):-
	copy_term(M,M_)
	,M_ = (Sub:-(H,Ls))
	,bind_head_literal(H,M_,(Sub:-(H,Ls)))
	,member(S,Ss)
        ,symbol(H,S).
applied_metasub(Sub, M, Ss, H, H):-
	copy_term(M,M_)
	,M_ = (Sub:-(H))
	,bind_head_literal(H,M_,(Sub:-(H)))
	,member(S,Ss)
        ,symbol(H,S).


%!	bind_head_literal(+Example,+Metarule,-Head) is det.
%
%	Bind an Example to the encapsulated Head literal of a Metarule.
%
%	Abstracts the complex patterns of binding examples to the heads
%	of metarules with and without body literals.
%
bind_head_literal(H:-B,(Sub:-(H,B)),(Sub:-(H,B))):-
% Positive or negative example given as a definite clause
% with one or more body literals.
	configuration:example_clauses(bind)
	,!.
bind_head_literal(H:-B,(Sub:-(H,Ls)),(Sub:-(H,Ls))):-
	configuration:example_clauses(call)
	,user:call(B)
	,!.
bind_head_literal(E,M,(H:-(E,Ls))):-
% Positive example given as a unit clause.
	M = (H:-(E,Ls))
	,!.
bind_head_literal(:-E,M,(H:-(E,Ls))):-
% Negative example given as a unit clause
	M = (H:-(E,Ls))
	,!.
bind_head_literal(E,M,(H:-(E,true))):-
% Positive example given as a unit clause.
% M is the Abduce metarule, i.e. body-less clause.
	M = (H:-E)
	,!.
bind_head_literal(:-E,M,(H:-(E,true))):-
% Negative example given as a unit clause.
% M is the Adbuce metarule, i.e. body-less clause.
	M = (H:-E)
	,!.
bind_head_literal(:-(L,Ls),M,(S:-(H,L,Ls))):-
% Negative example given as a Horn goal with no head literal.
% In this case, metasubstitution/3 must fail if the head of the
% metarule is entailed by its body literals.
% Note that binding the example to the body literals of the metarule
% will also bind the shared variables in the head of the metarule.
	M = (S:-(H,L,Ls))
	,!.


%!	symbol(?Literal,+Symbol) is det.
%
%	Instantiate a literal's predicate symbol to the given Symbol.
%
symbol(L,S):-
	configuration:encapsulation_predicate(E)
        ,L =.. [E,S|_As].



%!	constraints(+Metasubstitution) is det.
%
%	Apply a set of constraints to a generalising Metasubstitution.
%
%	Metasubstitution is a generalising metasubstitution considered
%	for addition to the Top program. A generalising metasubstitution
%	is one found during the generalisation step of Top program
%	construction.
%
%	constraints/1 tests Metasubstitution against a set of
%	user-defined constraints. If each applicable constraint is true,
%	then constraints/1 succeeds and Metasubstitution is included in
%	the Top program. Otherwise constraints/1 fails and
%	Metasubstitution is excluded from the Top program.
%
%	Note that only metasubstitutions found to generalise an example
%	are tested for constraints, i.e. metasubstitutions that can not
%	be proven against the MIL problem will be excluded without
%	constraints being tested.
%
%	User-defined constraints
%	------------------------
%
%	User-defined constraints are declared in experiment files as
%	clauses of configuration:metarule_constraints/2:
%
%	==
%	configuration:metarule_constraints(?Metasub,+Goal) is semidet.
%	==
%
%	A metarule_constraints/2 clause can be any Prolog clause. To
%	clarify, it can be a unit clause (a "fact"), or a non-unit
%	clause (a "rule").
%
%	The first argument of metarule_constraints/2 should match the
%	metasubstitution atom of an encapsulated metarule (the functor
%	must be "m" not "metarule"). If a generalising metasubstitution
%	matches this first argument, the matching metarule_constraint/2
%	clause is called. If this first call succeeds, the Prolog goal
%	in the second argument is called to perform the constraint test.
%
%	The second argument of metarule_constraints/2 is an arbitrary
%	Prolog goal. When the first argument of metarule_constraints/2
%	matches a generalising metasubstitution and the initial call
%	to metarule_constraint/2 succeds, the second argument is passed
%	to call/1. If this second call fails, the constraint test fails
%	and the metasubstitution matching the first argument is removed
%	from the generalised Top program. If this second calls succeeds,
%	the cosntraint test passes and the metasubstitution is added to
%	the generalised Top program.
%
%	Example
%	-------
%
%	The following metarule constraint will match a metasubstitution
%	with any metarule Id and with three existentially quantified
%	variables all ground to the same term. When the match succeeds,
%	and given that the constraint is a unit clause (i.e. always
%	true), the second argument of the constraint, fail/0 will be
%	called causing the constraint to fail and the metasubstitution
%	to be discarded:
%
%	==
%	configuration:metarule_constraints(m(_ID,P,P,P),fail).
%	==
%
%	The metarule constraint listed above can be used to exclude
%	left-recursive clauses from the Top program, but only for
%	metarules with exactly two body literals. A more general
%	constraint that will apply to a metarule with an arbitrary
%	number of body literals is as follows:
%
%	==
%	configuration:metarule_constraints(M,fail):-
%	M =.. [m,_Id,P|Ps]
%	,forall(member(P1,Ps)
%	       ,P1 == P).
%	==
%
%	Alternatively, the symbol of a target predicate can be specified
%	so that only metasubstitutions of that predicate are excluded
%	(if they would result in left-recursive clauses):
%
%	==
%	configuration:metarule_constraints(M,fail):-
%	M =.. [m,_Id,ancestor|Ps]
%	,forall(member(P1,Ps)
%	       ,P1 == ancestor).
%	==
%
%	Or the metarule Id can be ground to test only metasubstitutions
%	of a specific metarule, and so on.
%
%
%	Metarule constraints and predicate invention
%	--------------------------------------------
%
%	In the above examples, note the use of ==/2 instead of =/2 to
%	perform the comparison between existentially quantified
%	variables. This is to allow for variables remaining unbound on
%	generalisation during metarule extension by unfolding in the
%	proces of predicate invention.
%
%	@see data(examples/constraints) for examples of using metarule
%	constraints, in particular for the purpose of excluding
%	metasubstitutions resulting in left-recursive clauses from the
%	Top progam.
%
constraints(_Sub):-
	predicate_property(metarule_constraints(_,_), number_of_clauses(0))
	,!.
constraints(Sub):-
	predicate_property(metarule_constraints(_,_), number_of_clauses(N))
	,N > 0
	,copy_term(Sub,Sub_)
	,forall(configuration:metarule_constraints(Sub_, C)
	       ,user:call(C)
	       ).
