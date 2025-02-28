:-module(poker_auxiliaries, [% MIL problem auxiliaries
			     write_encapsulated_problem/1
	                     % Debugging auxiliaries
			     ,list_encapsulated_problem/1
			     ,list_learning_results/0
			     ,list_mil_problem/1
			     ,list_problem_statistics/1
			     ,debug_length/3
			     ,debug_clauses_length/3
			     ,debug_metasubs/5
			     ,debug_all_metasubs/5
	                     % Experiment file auxiliaries
			     ,cleanup_experiment/0
			     ,experiment_data/4
			     ,learning_target/1
			     ,learning_targets/1
			     ,load_experiment_file/0
			     ,edit_experiment_file/0
			     % Configuration auxiliaries
			     ,list_poker_options/1
			     ,debug_poker_config/1
			     ,list_poker_config/0
			     ,print_poker_config/3
			     ,set_configuration_option/2
			     ,set_poker_configuration_option/2
			     % Program auxiliaries
			     ,unifiable_compare/3
			     ,skolem_sort/2
			     ,skolem_sort/4
			    ]).

:-use_module(poker_configuration).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).

/** <module> Auxiliary predicates for Poker.

Predicates in this module support the implementation and usage of
Poker. They include predicates to examine an experiment file in the
format utiliesed in Poker.

*/


% [sec_prob]
% ================================================================================
% MIL problem auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating a MIL problem.


%!	write_encapsulated_problem(+Target) is det.
%
%	Write an encapsulated MIL problem to the dynamic db.
%
%	The MIL problem for Target is obtained from the current
%	experiment file.
%
write_encapsulated_problem(T):-
	experiment_data(T,Pos,BK,MS)
	,write_encapsulated_problem(Pos,[],BK,MS).




% [sec_debug]
% ================================================================================
% Debugging auxiliaries
% ================================================================================
% Predicates to facilitate experiment debugging and data inspection.


%!	list_encapsulated_problem(+Target) is det.
%
%	Pretty-print the encapsulation of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	encapsulated MIL problem to be listed.
%
list_encapsulated_problem(Ts):-
	poker_configuration:listing_limit(L)
	,experiment_data(Ts,Pos,BK,MS)
	,encapsulated_problem(Pos,[],BK,MS,[Pos_,_Neg,_BK_,MS_])
	,format_underlined('Initial examples')
	,print_limited(L,Pos_)
	,nl
	,format_underlined('Background knowledge (First Order)')
	,forall(member(P,BK)
	       ,(encapsulated_bk([P],Ts,Ps)
		,format('~w:~n',[P])
		,print_limited(L,Ps)
		,nl
		)
	       )
	,nl
	,expanded_metarules(MS,MS_)
	,format_underlined('Background knowledge (Second Order)')
	,print_metarules(expanded,MS_).


%!	list_learning_results is det.
%
%	List results for all learning targets.
%
%	Prints to the console the results of training on each learning
%	target defined in the current experiment file.
%
%	Learning targets are obtained with a call to learning_targets/1.
%
%	By default, each learning target is passed to learn/1.
%	Alternatively, the user may declare a clause of the dynamic,
%	multifile predicate learning_predicate/1 to select a different
%	learning predicate.
%
%	Alternative learning predicates must be one of [learn_meta/1,
%	learn_with_examples_invention/2 learn_metarules/1,
%	learn_minimal/1]. learn/2 can also be specified, but it will
%	have the same results as learn/1.
%
%	If a predicate with a symbol other than the above listed
%	alternatives, or with arity other than 1 or 2 is specified, an
%	informative error is raised.
%
%	@see learning_predicate/1, learning_targets/1
%
/*
list_learning_results:-
	configuration:learning_predicate(P)
	,!
	,list_learning_results(P).
*/
list_learning_results:-
	list_learning_results(learn/1).

%!	list_learning_results(+Learning_Predicate) is det.
%
%	Business end of list_learning_results/0.
%
%	Learning_Predicate is a predicate indicator, the symbol and
%	arity of one of the learning predicates in Poker.
%
%	Clauses are selected according to Learning_Predicate. Known
%	learning predicates with arity in [1,2] are called on all
%	learning targets and the results output to console. Predicates
%	with a symbol that is not one of the known learning predicates
%	or an arity other than an integer in [1,2], raise an appropriate
%	error.
%
list_learning_results(P/N):-
	\+ memberchk(P,[learn
		       ])
	,format(atom(A),'Unknown learning predicate: ~w',[P/N])
	,throw(A)
	% Actually needed to raise this error if the next also applies.
	,!.
list_learning_results(P/N):-
	\+ memberchk(N, [1,2])
	,format(atom(A),'Learning predicate arity must be in [1,2]: got ~w',[P/N])
	,throw(A).
list_learning_results(P/1):-
	!
	,learning_targets(Ts)
	,forall(member(T,Ts)
	       ,(call(P,T)
		,nl
		)
	       ).
list_learning_results(P/2):-
	learning_targets(Ts)
	,forall(member(T,Ts)
	       ,(call(P,T,Ps)
		,print_clauses(Ps)
		,nl
		)
	       ).



%!	list_mil_problem(+Target) is det.
%
%	List the elements of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	MIL problem to be listed.
%
list_mil_problem(T):-
	experiment_data(T,Pos,BK,MS)
	,list_mil_problem(Pos,BK,MS)
	,nl
	,print_constraints(MS,metasub).


%!	list_mil_problem(+Pos,+BK,+MS) is det.
%
%	Business end of list_mil_problem/1.
%
list_mil_problem(Pos,BK,MS):-
	poker_configuration:listing_limit(L)
	,format_underlined('Initial examples')
	,print_limited(L,Pos)
	,nl
	,format_underlined('Background knowledge (First Order)')
	,forall(member(P,BK)
	       ,(program(P,experiment_file,Ps)
		,format('~w:~n',[P])
		,print_limited(L,Ps)
		,format('~n',[])
		)
	       )
	,format_underlined('Background knowledge (Second Order)')
	,print_metarules(quantified,MS).


%!	format_underlined(+Atom) is det.
%
%	Print an atom and underline it.
%
format_underlined(A):-
	atom_underline(A,A_)
	,format('~w~n',[A])
	,format('~w~n',[A_]).


%!	atom_underline(+Atom,-Underlined) is det.
%
%	Create an Underline for an Atom.
%
atom_underline(A,A_):-
	atom_length(A, N)
	,findall(-
		,between(1,N,_)
		,Ds)
	,atomic_list_concat(Ds,A_).


%!	print_limited(+Limit,+Clauses) is det.
%
%	Print a list of Clauses up to a Limit.
%
%	Helper for list_mil_problem/4 (and list_encapsulated_problem/4)
%	to print clauses up to a limit, to avoid cluttering the screen
%	with too much output, especially in the presence of large
%	datasets with many examples and lots of extensional BK.
%
print_limited(L,Cs):-
	length(Cs,N)
	,G = member(C,Cs)
	,forall(limit(L,G)
	       ,print_clauses([C])
	       )
	,(   L < N
	 ->  M is N - L
	    ,format('% ... ~w more clauses.~n',[M])
	 ;   true
	 ).


%!	print_constraints(+Metarules,+Constraints) is det.
%
%	Print out information about metarule and order constraints.
%
%	Constraints is one of [order, metasub], denoting whether
%	information about order constraints or metasubstitution
%	constraints (currently defined as clauses
%	of metarule_constraints/2) will be printed.
%
print_constraints(_MS,metasub):-
	predicate_property(configuration:metarule_constraints(_,_), number_of_clauses(N))
	,(   N > 0
	 ->  format_underlined('Metasubstitution constraints')
	    ,listing(configuration:metarule_constraints)
	 ;   true
	 ).


%!	prettify_vars(+Vars,+Type,-Pretty) is det.
%
%	Prettify order constraints variables for pretty-printing.
%
%	@tbd This is a generalisation of two sets of repeating lines of
%	code in pretty_expanded_metarule/2. Perhaps consider replacing
%	the repeating lines in that predicate, too?
%
prettify_vars(Vs,T,Ps):-
	length(Vs,N)
	,auxiliaries:numbered_symbols(N,Vs,T)
	,findall('$VAR'(P)
		,(nth1(I,Vs,P)
		 ,nth1(I,Ps,'$VAR'(P))
		 )
		,Ps).



%!	list_problem_statistics(+Target) is det.
%
%	List statistics of the MIL problem for Target.
%
list_problem_statistics(T):-
	experiment_data(T,Pos,BK,MS)
	,maplist(length,[Pos,BK,MS],[I,K,N])
	,format('Initial examples:  ~w~n', [I])
	,format('Background knowledge: ~w ~w~n', [K,BK])
	,format('Metarules:            ~w ~w ~n', [N,MS]).



%!	debug_length(+Topic,+Message,+List) is det.
%
%	Log a Message about the length of a List.
%
%	Message is an atom passed to format/2 that should have an
%	argument like "~w" to handle the length of the input List.
%
debug_length(T,M,Ls):-
	length(Ls,N)
	,debug(T,M,[N]).



%!	debug_clauses_length(+Topic,+Message,+List) is det.
%
%	Log a Message and the length of a List.
%
%	Like debug_length/3 but also calls debug_clauses/3 on List.
%
debug_clauses_length(T,M,Ls):-
	length(Ls,N)
	,format(atom(A),M,[N])
	,debug_clauses(T,A,Ls).



%!	debug_metasubs(+Topic,+Message,+Subs,+Examples,+Metarules) is
%!      det.
%
%	Log a list of metasubstitutions, applied to their Metarules.
%
%	Subs should be a list of metasubstitutions.
%
%	Applies the given meta Subs to their Metarules, excapsulates
%	them and logs the given Message for the given Topic.
%
%	Examples are used to extract learning targets for the
%	excapsulation.
%
debug_metasubs(T,M,Subs,Es,MS):-
	applied_metarules(Subs,MS,Cs)
	,examples_targets(Es,Ss)
	,excapsulated_clauses(Ss,Cs,Cs_e)
	,debug_clauses(T,M,Cs_e).



%!	debug_all_metasubs(+Topic,+Message,+Subs,+Examples,+Metarules)
%!      is det.
%
%	Debug a list of lists of metasubs applied to their Metarules.
%
%	As debug_metasubs/5 but applies to lists of lists of
%	metasubstitutions, rather than to one list of metasubstitutions.
%
debug_all_metasubs(T,M,Subs,Es,MS):-
	debug(T,'~w',[M])
	,forall(nth1(I,Subs,Subs_i)
	       ,(format(atom(A),'-- Sub-hypothesis ~w --',[I])
		,debug_metasubs(T,A,Subs_i,Es,MS)
		)
	       ).



% [sec_expr]
% ================================================================================
% Experiment file auxiliaries
% ================================================================================
% Auxiliaries for inspecting and manipulating experiment files.


%!	cleanup_experiment is det.
%
%	Clean up after a learning session.
%
%	Currently this only removes clauses of m/n asserted to the
%	dynamic database.
%
%	Remember to run initialise_experiment/0 after this one to
%	re-load any necessary clauses.
%
cleanup_experiment:-
	% Retract encapsulated examples, BK and metarule clauses.
	forall(user:current_predicate(m,H)
	      ,(user:retractall(H)
	       % Clauses in program module are asserted
	       % by predicates in program_reduction module
	       ,program:retractall(H)
	       )
	      )
	% Retract encapsulated clauses of predicates in BK closure.
	,forall(user:current_predicate(p,H)
	      ,(user:retractall(H)
	       ,program:retractall(H)
	       )
	      )
	% Remove tabling for all tabled predicates
	,abolish_all_tables.



%!	experiment_data(+Targets,-Initial,-BK,-Metarules) is det.
%
%	Collect experiment file data for one or more learning Targets.
%
%	Targets is either a single predicate indicator, or a list of
%	predicate indicators of the predicates to be learned.
%
%	experiment_data/4 expects an experiment file to be loaded into
%	memory and will fail without warning otherwise.
%
experiment_data(Ts,_,_,_):-
% A list of learning targets must be ground.
	is_list(Ts)
	,learning_targets(Ls)
	,forall(member(T,Ts)
	       ,(   \+ memberchk(T,Ls)
		->  throw('Unknown learning target':T)
		;   true
		)
	       )
	,fail.
experiment_data(T,_,_,_):-
% A single learning target's predicate indicator must be ground.
	\+ is_list(T)
	,learning_targets(Ts)
	,\+ memberchk(T,Ts)
	,throw('Unknown learning target':T).
experiment_data(T,Es,BK,MS):-
	example_atoms(experiment_file,T,Es_)
	,list_to_set(Es_,Es)
	,bk_or_metarules(background_knowledge,experiment_file,T,BK)
	,bk_or_metarules(metarules,experiment_file,T,MS_)
	,(   (MS_ == [all]
	     ;	 memberchk(all,MS_)
	     )
	 ->  configuration_metarules(MS)
	 ;   MS = MS_
	 ).


%!	example_atoms(+Module,+Targets,-Atoms) is det.
%
%	Collect Atoms of one or more target predicates.
%
%	Module is the module name of the current experiment file.
%
%	Targets is either a list of learning targets' symbols and
%	arities as F/A predicate indicators, or a single predicate
%	indicator.
%
%	Atoms is a list of atoms of all the learning Targets. These
%	atoms can be used as examples to learn a hypothesis that can
%	generate and label new atoms (i.e. assign truth values to them)
%	depending on whether they are consistent with the initial
%	examples.
%
example_atoms(M,Ts,Es):-
% Ts is a list of learning targets.
	is_list(Ts)
	,!
	,C =.. [initial_example,T,Ep]
	,findall(Ep
		,(member(T,Ts)
		 ,M:C
		 )
		,Es_)
	,flatten(Es_,Es).
example_atoms(M,T,Es):-
% T is a single learning target.
	C =.. [initial_example,T,Ep]
	,findall(Ep
		,(M:C
		 )
		,Es).


%!	bk_or_metarules(+Bias,+Module,+Targets,-Delarations) is det.
%
%	Retrieve the BK or metarule Declarations for a MIL problem.
%
%	Bias is one of [background_knowledge,metarules]. Module is the
%	module name of the current experiment file. Targets is either a
%	list of learning targets or a single target, each as an F/A
%	predicate indicator.
%
%	If Bias is "background_knowledge", Declarations is a list of the
%	F/A predicate indicators of predicates declared as background
%	knowledge for each learning target in Targets.
%
%	If Bias is "metarules", Declarations is a list of atomic
%	metarule identifiers.
%
bk_or_metarules(B,M,Ts,Bs):-
% Ts is a list of learning targets.
	is_list(Ts)
	,!
	,C =.. [B,T,Bs_]
	,findall(E_
		,(member(T,Ts)
		 ,M:C
		 ,member(E_,Bs_)
		 )
	      ,Bs_)
	,list_to_set(Bs_, Bs)
	.
bk_or_metarules(B,M,T,Bs):-
% T is a single learning target.
	C =.. [B,T,Bs]
	,M:C.


%!	configuration_metarules(-Metarules) is det.
%
%	Collect the names of all Metarules defined in the configuration.
%
%	Used when the list of metarules for a learning targets includes
%	the atom "all", meaning that all known metarules should be used
%	for that learning target.
%
configuration_metarules(MS):-
	findall(Id
	       ,(configuration:current_predicate(metarule,H)
		,predicate_property(H, implementation_module(configuration))
		,H =.. [metarule,Id|_]
		,clause(H, _B)
		)
	       ,MS).



%!	learning_targets(?Target) is nondet.
%
%	Generate each learning Target in an experiment file.
%
learning_target(T):-
	learning_targets(Ts)
	,member(T,Ts).



%!	learning_targets(-Targets) is det.
%
%	Collect learning Targets defined in an experiment file.
%
%	Targets is the list of predicate symbols and arities of each of
%	the target predicates that have background knowledge
%	declarations in background/2 clauses in the current experiment
%	file.
%
learning_targets(Ts):-
	findall(T
		,experiment_file:background_knowledge(T, _BK)
		,Ts_)
	,flatten(Ts_,Ts).



%!	load_experiment_file is det.
%
%	Load the current experiment file into module user.
%
load_experiment_file:-
	experiment_file(P,_M)
	,user:use_module(P).



%!	edit_experiment_file is det.
%
%	Open the current experiment file in the Swi-Prolog IDE.
%
edit_experiment_file:-
	poker_configuration:experiment_file(P,_M)
	,edit(P).




% [sec_config]
% ================================================================================
% Configuration auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating configuration options.



%!	list_poker_options(+Options) is det.
%
%	List a set of configuration Options for POker.
%
%	Options is a list of predicate indicators, Symbol/Arity, of
%	configuration options defined in poker_configuration.pl.
%
%	This predicate prints to the top-level the configuration options
%	given in Options.
%
%	This predicate can be used in place of list_config/0 to list the
%	values of only a desired subset of configuration options.
%
%	@tbd Shameless copy/pasta from auxiliaries.pl modified for use
%	by Poker because we don't want Vanilla to know anything about
%	the learning systems using it.
%
list_poker_options(Os):-
	\+ is_list(Os)
	,!
	,list_poker_options([Os]).
list_poker_options(Os):-
        forall(member(S/A,Os)
              ,(functor(O,S,A)
               ,poker_configuration:O
               ,format('~w~n',[O])
               )
              ).



%!	debug_poker_config(+Subject) is det.
%
%	Log Poker configuration options to the debug stream for Subject.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the
%	poker_configuration module (i.e. not re-exported from other
%	configuration files) are printed.
%
%	@tbd Shameless copy/pasta from auxiliaries.pl modified for use
%	by Poker because we don't want Vanilla to know anything about
%	the learning systems using it.
%
debug_poker_config(S):-
	print_poker_config(debug,S,main).


%!	list_poker_config is det.
%
%	Print Poker configuration options to the console.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the
%	poker_configuration module (i.e. not re-exported from other
%	configuration files) are printed.
%
%	@tbd Shameless copy/pasta from auxiliaries.pl modified for use
%	by Poker because we don't want Vanilla to know anything about
%	the learning systems using it.
%
list_poker_config:-
	print_poker_config(print,user_output,main).


%!	print_poker_config(+Print_or_Debug,+Stream_or_Subject,+Scope) is det.
%
%	Print or debug current configuration options for Poker.
%
%	Print_or_Debug is one of [print,debug] which should be
%	self-explanatory.
%
%	Stream_or_Subject is either a stream alias or a debug subject,
%	depending on the value of Print_or_Debug.
%
%	Scope is one of [main,all]. If Scope is "main", only
%	configuration options whose implementation module is
%	"poker_configuration" are printed. If Scope is "all", all
%	configuration options re-exported by poker_configuration.pl are
%	printed, which includes options defined elsewhere, e.g.
%	configuration files of libraries that are re-exported by
%	poker_configuration.pl to avoid cluttering it etc.
%
%	If Scope is "all" configuration options are prepended by the
%	name of their implementation module, to help identification.
%
%	If Scope is something other than "main" or "all", print_poker_config/3
%	raised an existence error.
%
%	Configuration options are printed in alphabetical order, which
%	includes the alphabetical order of their implementation modules'
%	names.
%
%	@tbd Shameless copy/pasta from auxiliaries.pl modified for use
%	by Poker because we don't want Vanilla to know anything about
%	the learning systems using it.
%
print_poker_config(T,S,Sc):-
	must_be(oneof([main,all]), Sc)
	,module_property(poker_configuration, exports(Es))
	,findall(M:Opt_
		,(member(F/A,Es)
		 % No need to print those out and they're too verbose.
		 ,\+ memberchk(F/A, [tautology/1
				    ,safe_example/1
				    ])
		 ,functor(Opt,F,A)
		 ,predicate_property(Opt, implementation_module(M))
		 ,call(poker_configuration:Opt)
		 % Convert to list to sort by functor only.
		 % Standard order of terms also sorts by arity.
		 ,Opt =.. Opt_
		 )
		,Opts)
	% Sort alphabetically
	,sort(Opts, Opts_)
	,(   Sc = all
	 ->  true
	 ;   Sc = main
	 ->  Mod = poker_configuration
	 )
	,forall(member(Mod:Opt, Opts_)
	       ,(Opt_ =.. Opt
		,(   Sc = all
		 ->  print_or_debug(T,S,Mod:Opt_)
		 ;   Sc = main
		 ->  print_or_debug(T,S,Opt_)
		 )
		)
	       ).



%!	set_configuration_option(+Option,+Value) is det.
%
%	Change the Value of a configuration Option.
%
%	Option is an atom, the name of a configuration option defined in
%	(or exported to) module configuration.
%
%	Value is the set of the arguments of Option. Iff Option has a
%	single argument, Value can be a single atomic constant.
%	Otherwise, it must be a list.
%
%	set_configuration_option/2 first retracts _all_ clauses of the
%	named Option, then asserts a new clause with the given Value.
%
%	Only configuration options declared as dynamic can be changed
%	using set_configuration_option/2. Attempting to change a static
%	configuration option will raise a permission error.
%
%	@tbd This predicate cannot change configuration options with
%	multiple clauses (or at least can't change any but their first
%	clause). Such functionality may or may not be necessary to add.
%
%	@bug Sorta bug, but if set_configuration_option/2 is used as
%	intended, at the start of an experiment file, to set a necessary
%	configuration option, the configuration option thus changed will
%	remain changed until the option is changed with
%	set_configuration_option/2 or by editing the configuration file.
%	And note that just reloading the configuration file will not
%	reset the option- it will just add an extra clause of it in the
%	database, which will often cause unepxected backtracking. This
%	may cause some confusion, for example when setting the value of
%	extend_metarules/1 to something other than false for one
%	experiment, which then of course affects subsequent experiments.
%	It happened to me, it could happen to you.
%
set_configuration_option(N, V):-
	atomic(V)
	,!
	,set_configuration_option(N,[V]).
set_configuration_option(N, [V]):-
	is_list(V)
	,N \== fetch_clauses
	,!
	,set_configuration_option(N,V).
set_configuration_option(fetch_clauses, V):-
	!
	 % V is the atom 'all'
	,(   V = all
	 ->  Vs = [all]
	 % V is the atom 'all' in a list
	 ;   V = [all]
	 ->  Vs = [all]
	 % V is the atom 'all' in a list in a list
	 % Perpetrated by set_multi_configuration_option/2
	 ;   V = [[all]]
	 ->  Vs = [all]
	 ;   V = [Vs_]
	    ,is_list(Vs_)
	 ->  Vs = [Vs_]
	 )
	,functor(T,fetch_clauses,1)
	,T_ =.. [fetch_clauses|Vs]
	,retractall(configuration:T)
	,assert(configuration:T_).
set_configuration_option(N, Vs):-
	length(Vs, A)
	,functor(T,N,A)
	,T_ =.. [N|Vs]
	,retractall(configuration:T)
	,assert(configuration:T_).


%!	set_poker_configuration_option(+Name,+Value) is det.
%
%	Change the Value of a Poker configuration Option.
%
%	As set_configuration_option/2 but Name must be defined in
%	poker_configuration module.
%
%	@tbd currently calling this and set_configuration_option/2 as a
%	directive from an experiment file raises an existence error.
%	Why?
%
set_poker_configuration_option(N, Vs):-
	length(Vs, A)
	,functor(T,N,A)
	,T_ =.. [N|Vs]
	,retractall(poker_configuration:T)
	,assert(poker_configuration:T_).



% [sec_prog]
% ================================================================================
% Program auxiliaries
% ================================================================================
% Predicates for inspecting and modifying a program.


%!	unifiable_compare(-Delta,+A,+B) is det.
%
%	Comparison predicate for predsort/3.
%
%	If A and B unify Delta is =, otherwise compare(Delta,A,B) is
%	true.
%
%	Used to sort clauses with variables without taking into account
%	variable ages, which can cause unifiable terms to sort as
%	different terms.
%
%	Suggested by Boris on the Swi-Prolog mailing list.
%
unifiable_compare(Delta, A, B) :-
    (   unifiable(A, B, _)
    ->  Delta = (=)
    ;   compare(Delta, A, B)
    ).



%!	skolem_sort(+List,-Sorted) is det.
%
%	Sort a List, ignoring variable age.
%
%	Skolemises each element of list, sorts it, then unskolemises it.
%
skolem_sort(Ls,Ss):-
	setof(L
	     ,Ls^(member(L,Ls)
		 ,numbervars(L)
		 )
	     ,Ss_)
	,findall(Li
		,(member(L_,Ss_)
		 ,varnumbers(L_,Li)
		 )
		,Ss).



%!	skolem_sort(+Key,+Order,+List,-Sorted) is det.
%
%	Sort a List, ignoring variable age.
%
%	Skolemises each element of list, sorts it, then unskolemises it.
%
%	The 4-arity version passes Key and Order to sort/4. Look it up!
%
skolem_sort(K,O,Ls,Ss):-
	findall(Li
	       ,(member(Li,Ls)
		,numbervars(Li)
		)
	       ,Ss_)
	,sort(K,O,Ss_,Ss_s)
	,findall(Lj
		,(member(Lk,Ss_s)
		 ,varnumbers(Lk,Lj)
		 )
		,Ss).
