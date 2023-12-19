:-module(auxiliaries, [% Experiment auxiliaries
	               protocol_experiment/3
		       % MIL problem auxiliaries
		      ,invented_symbol/3
		      ,invented_symbol/2
		      ,invented_symbols/3
		      ,invented_symbols/2
		      ,known_metarules/1
		      ,same_metarule/2
		      ,write_problem/3
		      ,write_encapsulated_problem/4
		      % Configuration auxiliareis
		      ,list_options/1
		      ,debug_config/1
		      ,list_config/0
		      ,print_config/3
		       % Debugging auxiliaries
		      ,print_or_debug/3
		      ,debug_clauses/3
		      ,debug_clauses/2
		      ,print_clauses/2
		      ,print_clauses/1
		      ,print_metarules/1
		      ,print_metarules/2
		      ,debug_metarules/2
		      ,debug_metarules/3
		      ,debug_metarules/4
		      ,debug_msg_metarules/3
		       % Database auxiliaries
		      ,assert_program/3
		      ,erase_program_clauses/1
		      ,eraseall_program_clauses/1
		       % Program auxiliaries
		      ,built_in_or_library_predicate/1
		      ,closure/3
		      ,program_symbols/2
		      ,program/3
		      ,table_program/1
		      ,untable_program/1
	              % Timing auxiliaries
	              ,timing/2
		      ,timing/3
		      ]).

:-use_module(project_root(configuration)).
:-user:use_module(lib(term_utilities/term_utilities)).
:-user:use_module(lib(mathemancy/mathemancy)).
:-use_module(src(mil_problem)).


/** <module> Auxiliary predicates.

Predicates supporting Vanilla and implementations of MIL sytems by
Vanilla.

To facilitate browsing the source code in this module, the different
sections are listed in the ToC below. Each section is assigned a tag
made up of sec_ and a contracted section description, enclosed in square
braces. These tags can be used to jump to the relevant section by
searching the text for the text of a tag.

The ToC below can also help browsing the structured documentation,
rather than the source code, of predicates defined in this module. In
the Swi-Prolog documentation browser, the predicate indicators of
predicates listed under a section's title in the ToC should be rendered
as links that can be navigated-to in the user's browser, by clicking on
them.

These predicates were originally defined for Louise. Some of Louise's
auxiliaries are not included in this file, in particular the ones that
support Louise's concept of an "experiment file" with a specific format.
Vanilla does not use this format in order to avoid strong coupling with
any particular implementation, hence those auxiliaries that are specific
to experiment files are left out. They are currently included in the
auxiliaries for lib(metagol) and lib(poker) that do use the experiment
file format.


Table of Contents
-----------------

1. Experiment auxiliaries [sec_prot]
   * protocol_experiment/3

2. MIL problem auxiliaries [sec_prob]
   * invented_symbol/3
   * invented_symbol/2
   * invented_symbols/3
   * invented_symbols/2
   * known_metarules/1
   * same_metarule/2
   * write_problem/3
   * write_encapsulated_problem/4

3. Configuration auxiliaries [sec_config]
   * list_options/1
   * debug_config/1
   * list_config/0
   * print_config/3

4. Debugging auxiliaries [sec_debug]
   * print_or_debug/3
   * debug_clauses/3
   * debug_clauses/2
   * print_clauses/2
   * print_clauses/1
   * print_metarules/1
   * print_metarules/2
   * debug_metarules/2
   * debug_metarules/3
   * debug_metarules/4
   * debug_msg_metarules/3

5. Database auxiliaries [sec_dynm]
   * assert_program/3
   * erase_program_clauses/1
   * eraseall_program_clauses/1

6. Program auxiliaries [sec_prog]
   * built_in_or_library_predicate/1
   * closure/3
   * program_symbols/2
   * program/3
   * table_program/1
   * untable_program/1

7. Timing auxiliaries [sec_time]
   * timing/2
   * timing/3

*/




% [sec_prot]
% ================================================================================
% Experiment auxiliaries
% ================================================================================
% Predicates to manage experiments logged with protocol/1.


%!	protocol_experiment(+Target,+Protocol,+Goal) is det.
%
%	Run an experimental Goal and log results with protocol/1.
%
%	Target is a learning target in the current experiment file.
%
%	Protocol is the name of a file in which to save output of
%	executing Goal using protocol/1.
%
%	Goal is an arbitrary Prolog goal. It's expected that Goal has
%	something to do with Target.
%
%	Use this predicate to run quick, simple experiments keeping a
%	record of their configuration options, problem statisticts,
%	experiment Goal and result.
%
%	Example query:
%	==
%	?- _T = ancestor/2
%	, _F = 'logs/protocol_testing.log'
%	, _G = learn(_T), protocol_experiment(_T,_F,_G).
%	==
%
%	The query above will execute the goal learn/1 while keeping a
%	copy of the output to the SWI-Prolog terminal to the file
%	logs/protocol_testing.log. The path is relative to the root
%	director of louise's installation. If no path is given, the file
%	is created in the root directory of louise's installation
%	instead.
%
protocol_experiment(T,N,G):-
	S = protocol(N)
	,C = (writeln('Current configuration:')
	     ,list_config
	     ,nl
	     ,writeln('Problem statistics:')
	     ,list_problem_statistics(T)
	     ,nl
	     ,writeln('MIL Problem elements:')
	     ,list_mil_problem(T)
	     ,nl
	     ,writeln('MIL Problem elements (encapsulated):')
	     ,list_encapsulated_problem(T)
	     ,nl
	     ,copy_term(G,G_)
	     ,numbervars(G_)
	     ,writeln('Goal:')
	     ,print(G_)
	     ,nl
	     ,nl
	     ,writeln('Results:')
	     ,user:call(G)
	     )
	,L = noprotocol
	,setup_call_cleanup(S,C,L).




% [sec_prob]
% ================================================================================
% MIL problem auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating a MIL problem.


%!	invented_symbol(+Index,?Arity,?Symbol) is nondet.
%
%	An invented Symbol witn an index in [1,Index].
%
%	Symbol is a predicate indicator, S/A, where S is an invented
%	symbol and A is Arity.
%
%	Use this predicate to generate invented symbols of a given
%	Arity or verify that a Prolog term is an invented symbol of that
%	Arity.
%
invented_symbol(I,A,S/A):-
	configuration:invented_symbol_prefix(F)
	,between(1,I,K)
	,atomic_list_concat([F,K],'',S).



%!	invented_symbol(+Index,?Symbol) is nondet.
%
%	An invented Symbol witn an index in [1,Index].
%
%	As invented_symbol/2 but Symbol is an atomic term without an
%	arity.
%
%	Use this predicate to generate invented symbols or verify that
%	a Prolog term is an invented symbol.
%
invented_symbol(I,S):-
	configuration:invented_symbol_prefix(F)
	,between(1,I,K)
	,atomic_list_concat([F,K],'',S).



%!	invented_symbols(+Index,?Arity,?Symbols) is det.
%
%	A list of invented Symbols up to some maximum Index.
%
%	Symbols is a list of symbols S/A, where each S is an invented
%	symbol and A is Arity.
%
%	Use this predicate to generate invented symbols of a given Arity
%	or verify that a list of Prolog terms is a list of invented
%	symbols of that Arity.
%
invented_symbols(I,A,Ss):-
	findall(S
	       ,invented_symbol(I,A,S)
	       ,Ss).



%!	invented_symbols(+Index,?Symbols) is det.
%
%	A list of invented Symbols up to some maximum Index.
%
%	As invented_symbols/3 but Symbols are atomic terms without
%	arities.
%
%	Use this predicate to generate invented symbols or verify that a
%	list of Prolog terms is a list of invented symbols.
%
invented_symbols(I,Ss):-
	findall(S
	       ,invented_symbol(I,S)
	       ,Ss).



%!	known_metarules(-Ids) is det.
%
%	Collect the Ids of metarules known to the system.
%
known_metarules(Ids):-
	setof(Id
	       ,H^Ps^B^(configuration:current_predicate(metarule, H)
			 ,H =.. [metarule,Id|Ps]
			 ,clause(H,B)
			 )
	     ,Ids).



%!	same_metarule(+Metarule1,+Metarule2) is det.
%
%	True when Metarule1 and Metarule2 are identical.
%
%	To compare the two metarules, first they are skolemised using
%	numbervars/1, then their head and body literals are compared.
%
%	This predicate is useful for debugging experiment files, in
%	particular when metarule extension is turned on. Extended
%	metarules can become too long to easily read and compare by the
%	naked eye.
%
%	Example of use
%	--------------
%	The query below compares metarules expanded from an initial set
%	of chain and inverse to the un-expanded chain metarule. The
%	chain metarule itself is in the output of expanded_metarules/2
%	so the query is true once.
%
%	==
%	?- _Sub1 = metarule(chain,_P,_Q,_R)
%	,clause(_Sub1, _MB), _M1 = (_Sub1:-_MB)
%	,expanded_metarules([chain,inverse], _Ms)
%	, member(_M2, _Ms), same_metarule(_M1, _M2), print_clauses([_M2,_M1]).
%	m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
%	metarule(chain,A,B,C):-configuration:(m(A,D,E),m(B,D,F),m(C,F,E)).
%	true ;
%	false.
%	==
%
same_metarule(M1,M2):-
	skolem_copy(M1,M1_)
	,skolem_copy(M2,M2_)
	,M1_ = M2_
	,!.
same_metarule(M1,M2):-
	skolem_copy(M1,M1_)
	,skolem_copy(M2,M2_)
	,metarule_head_body_name(M1_,Sub,B,Id)
	,metarule_head_body_name(M2_,Sub,B,Id).


%!	skolem_copy(+Term,-Skolemised) is det.
%
%	Copy and skolemise a Prolog Term.
%
%	Term is an arbitrary Prolog term and Skolemised is a copy of
%	this Term with variables replaced by numbered constants with
%	varnumbers/1. Skolemised is copied before passing it to
%	numbervars so the variables in Term remain unaffected by the
%	skolemisation.
%
skolem_copy(T,T_):-
	copy_term(T,T_)
	,numbervars(T_).


%!	metarule_head_body_name(+Metarule,-Metasubstitution,-Body,-Id) is det.
%
%	Extract a metarule's Metasubstitution and Body literals and Id.
%
metarule_head_body_name(M,Sub_,B_,Id):-
	configuration:encapsulation_predicate(E)
	,M = (Sub:-B)
	,strip_module(B,_M,B_)
	,Sub =.. [_F,_|Ps]
	,Sub_ =.. [E,_|Ps]
	,Id = '$metarule'.



%!	write_problem(+Module,+Elements,-Refs) is det.
%
%	Write the Elements of a MIL problem to a Module.
%
%	Refs is a list of references of the clauses assserted to the
%	dynamic database. These are meant to be used later to erase the
%	asserted clauses.
%
write_problem(M,Es,Rs):-
	findall(Rs_i
		,(member(P, Es)
		 ,assert_program(M,P,Rs_i)
		 )
		,Rs_)
	,flatten(Rs_,Rs).



%!	write_encapsulated_problem(+Pos,+Neg,+BK,+Metarules) is det.
%
%	Write an encapsulated MIL problem to the dynamic db.
%
%	Writes to the dynamic database the encapsulated positive and
%	negative examples, BK and metarules given as inputs.
%
%	Useful for debugging purposes. Remember to call
%	cleanup_experiment/0 to remove clauses asserted to the dynamic
%	db.
write_encapsulated_problem(Pos,Neg,BK,MS):-
	encapsulated_problem(Pos,Neg,BK,MS,Es)
	,flatten(Es, Es_)
	,assert_program(user,Es_,_).




% [sec_config]
% ================================================================================
% Configuration auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating configuration options.



%!	list_options(+Options) is det.
%
%	List a set of configuration Options.
%
%	Options is a list of predicate indicators, Symbol/Arity, of
%	configuration options defined in configuration.pl.
%
%	This predicate prints to the top-level the configuration options
%	given in Options.
%
%	This predicate can be used in place of list_config/0 to list the
%	values of only a desired subset of configuration options.
%
list_options(Os):-
	\+ is_list(Os)
	,!
	,list_options([Os]).
list_options(Os):-
        forall(member(S/A,Os)
              ,(functor(O,S,A)
               ,configuration:O
               ,format('~w~n',[O])
               )
              ).



%!	debug_config(+Subject) is det.
%
%	Log configuration options to the debug stream for Subject.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	logged.
%
debug_config(S):-
	print_config(debug,S,main).


%!	list_config is det.
%
%	Print configuration options to the console.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	printed.
%
list_config:-
	print_config(print,user_output,main).


%!	print_config(+Print_or_Debug,+Stream_or_Subject,+Scope) is det.
%
%	Print or debug current configuration options.
%
%	Print_or_Debug is one of [print,debug] which should be
%	self-explanatory.
%
%	Stream_or_Subject is either a stream alias or a debug subject,
%	depending on the value of Print_or_Debug.
%
%	Scope is one of [main,all]. If Scope is "main", only
%	configuration options whose implementation module is
%	"configuration" are printed. If Scope is "all", all
%	configuration options re-exported by configuration.pl are
%	printed, which includes options defined elsewhere, e.g.
%	configuration files of libraries that are re-exported by
%	configuration.pl to avoid cluttering it etc.
%
%	If Scope is "all" configuration options are prepended by the
%	name of their implementation module, to help identification.
%
%	If Scope is something other than "main" or "all", print_config/3
%	raised an existence error.
%
%	Configuration options are printed in alphabetical order, which
%	includes the alphabetical order of their implementation modules'
%	names.
%
print_config(T,S,Sc):-
	must_be(oneof([main,all]), Sc)
	,module_property(configuration, exports(Es))
	,findall(M:Opt_
		,(member(F/A,Es)
		 ,\+ memberchk(F/A, [metarule/2
				    ,metarule_constraints/2
				    ,order_constraints/5
				    ,symbol_range/2
				    ,tautology/1
				    ])
		 ,functor(Opt,F,A)
		 ,predicate_property(Opt, implementation_module(M))
		 ,call(configuration:Opt)
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
	 ->  Mod = configuration
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



%!	reset_defaults is det.
%
%	Reset dynamic configuration options to their defaults.
%
%	This predicate should be called after dynamic configuration
%	options are changed dynamically with set_configuration_option/2
%	to reset the changed options to their defaults.
%
%	Defaults of dynamic configuration options are stored in the file
%	src(defaults).
%
%	Note that only options declared dynamic and whose implementation
%	module is configuration are reset.
%
%	Some options may be originally implemented in a different
%	module, but the configuration module may re-export them. Such
%	options are _not_ rest.
%
%	Some options might be originally implemented in a different
%	module, but the configuration module re-defines them and
%	excludes them from its re-export list. For example, that is the
%	case with the resolutions/1 option, originally defined in
%	reduction_configuration.pl and declared dynamic. Such options
%	_are_ reset to their defaults.
%
%	Consult the default.pl file for a definitive source of the
%	options that should be reset by this predicate.
%
reset_defaults:-
	module_property(configuration, exports(Es))
	,forall(member(F/A, Es)
	       ,(functor(P,F,A)
		,(  \+ memberchk(F, [learning_predicate
				    ,metarule
				    ,metarule_constraints])
		   ,predicate_property(P, dynamic))
		   ,predicate_property(P, implementation_module(configuration))
		->  atom_concat(default_,F,DF)
		   ,functor(D,DF,1)
		   ,findall(Vs
			  ,(call(defaults:D)
			   ,D =.. [_|Vs]
			   )
			   ,VS
			  )
		   ,set_multi_configuration_option(F,VS)
		;  true
		)
	       ).



%!	set_multi_configuration_option(+Option,+Values) is det.
%
%	Set a configuration option possibly with multiple Values.
%
%	As set_configuration_option/2 but allows for options that may
%	have multiple default values, defined in multiple clauses in
%	src/defaults.pl. No option is currently of that type.
%
%	If Option has a single default option, Option and Value are both
%	passed to set_configuration_option/2. This is to allow use of
%	the same predicate in a loop to set options that may have one or
%	more defaults.
%
set_multi_configuration_option(N, [V]):-
% Check this is actually a multi-setting option rather
% than an option with a list-type setting.
	memberchk(N,[fetch_clauses])
	,!
	,set_configuration_option(N,V).
set_multi_configuration_option(N, [V]):-
	is_list(V)
	,!
	,set_configuration_option(N,V).
set_multi_configuration_option(N, [V|Vs]):-
	!
	,length([V|Vs],A)
	,functor(T,N,A)
	,retractall(configuration:T)
	,forall(member([Vi],[V|Vs])
	       ,(T_ =.. [N|Vi]
		,assert(configuration:T_)
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
/*
% Modify when multi-options are added to configuration.
set_configuration_option(N,_Vs):-
	memberchk(N, [recursion_depth_limit])
	,!
	,print_message(warning,multi_option(N))
	,print_message(warning,option_not_set(N)).
*/
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


% Message hook for multi-option warning in first clause of
% set_configuration_option/2.
prolog:message(multi_option(N)) -->
	{ A = 'You\'re attempting to set the multi-clause option ~w to a single value.~n'
	 ,B = 'Warning: Some predicates relying on this option may fail unexpectedly.~n'
	 ,C = 'Warning: Use auxiliaries:set_multi_configuration_option/2 instead \c
	  or set option by hand. ~n'
	}
	,[A-N],[B-[], C-[]].

prolog:message(option_not_set(N)) -->
	['Option ~w not set!~n'-[N]].




% [sec_debug]
% ================================================================================
% Debugging auxiliaries
% ================================================================================
% Predicates to facilitate experiment debugging and data inspection.


%!	print_or_debug(+Print_or_Debug,+Stream_or_Subject,+Atom) is
%!	det.
%
%	Print or debug an Atom.
%
%	Print_or_Debug can be one of: [print,debug,both]. If "print",
%	Stream_or_Subject should be the name or alias of a stream
%	and Atom is printed at that Stream. If "debug",
%	Stream_or_Subject should be a debug subject and Atom is printed
%	to the current debug stream, iff the specified subject is being
%	debugged. If "both", Stream_or_Subject should be a term Str/Sub,
%	where Str the name or alias of a stream and Sub the name of
%	debug topic; then Atom is printed to the specified stream and
%	also to the current debug topic if Sub is being debugged.
%
print_or_debug(debug,S,C):-
	debug(S,'~w',[C]).
print_or_debug(print,S,C):-
	format(S,'~w~n',[C]).
print_or_debug(both,Str/Sub,C):-
	print_or_debug(print,Str,C)
	,print_or_debug(debug,Sub,C).



%!	debug_clauses(+Topic,+Message,-Clauses) is det.
%
%	Log a Message followed by a set of Clauses.
%
debug_clauses(T,M,Cs):-
	debug(T,'~w',[M])
	,debug_clauses(T,Cs).


%!	debug_clauses(+Topic,+Clauses) is det.
%
%	Debug a list of Clauses if Topic is being debugged.
%
debug_clauses(T,[]):-
	!
	,debug(T,'[]',[]).
debug_clauses(T,L):-
	\+ is_list(L)
	,!
	,debug_clauses(T,[L]).
debug_clauses(T,Cs):-
	copy_term(Cs,Cs_)
	,forall(member(C,Cs_)
	      ,(numbervars(C)
	       ,format(atom(A),'~w',[C])
	       ,debug(T,'~w',A)
	       )
	      ).



%!	print_clauses(+Message,-Clauses) is det.
%
%	Print a Message followed by a set of Clauses.
%
print_clauses(M,Cs):-
	format('~w~n',[M])
	,print_clauses(Cs).


%!	print_clauses(+Clauses) is det.
%
%	Print a list of Clauses to standard output.
%
print_clauses([]):-
	!
	,writeln([]).
print_clauses(L):-
	\+ is_list(L)
	,!
	,print_clauses([L]).
print_clauses(Cs):-
	forall(member(C,Cs)
	      ,(copy_term(C,C_)
	       ,numbervars(C_)
	       ,write_term(C_, [fullstop(true)
			       ,nl(true)
			       ,numbervars(true)
			       ,quoted(true)
			       ])
	       )
	      ).



%!	print_metarules(+Metarules) is det.
%
%	Pretty-print a list of Metarules.
%
%	As print_metarules/2 but the choice of formatting is taken from
%	the configuration option metarule_formattting/1.
%
print_metarules(MS):-
	configuration:metarule_formatting(F)
	,print_metarules(F,MS).


%!	print_metarules(+Format,+Metarules) is det.
%
%	Pretty-print a list of metarules.
%
%	Metarules is a list of either metarule ids, or expanded
%	metarules as returned by metarule_expansion/2 or
%	expanded_metarules/2.
%
%	Format is an atom denoting the format in which Metarules will be
%	printed, one of [expanded,quantified,user_friendly], interpreted
%	as follows.
%	* expanded: print metarules in Louise's internal format,
%	encapsulated and expanded, but with pretty variable names.
%	* quantified: print metarules with quantifiers and in the
%	human-readable format found in the MIL literature.
%	* user_friendly: print metarules in the user-level format used
%	in experiment files.
%
%	Ask for "expanded" formatting when you wish to inspect the
%	internal representation of metarules that Louise uses to learn.
%
%	Ask for "quantified" formatting when you want to compare
%	metarules with metarules listed in the literature, or just to
%	get a more formal definition of metarules.
%
%	Ask for "user_friendly" formatting when you want to copy and
%	past metarules to an experiment file to use directly in a
%	learning attempt.
%
print_metarules(F,M):-
	\+ is_list(M)
	,!
	,print_metarules(F,[M]).
print_metarules(F,[I|Is]):-
% Punch metarules are "expanded" into lists of integers.
	integer(I)
	,!
	,min_list([I|Is],Min)
	,max_list([I|Is],Max)
	,output_metarule(F,print,user_output,higher_order(Min,Max)).
print_metarules(F,MS):-
	!
	,forall(member(M,MS)
	       ,output_metarule(F,print,user_output,M)
	       ).



%!	debug_metarules(+Subject,+Metarules) is det.
%
%	Print a list of pretty-printed Metarules to a debug Subject.
%
%	As debug_metarules/3 but the choice of formatting of the printed
%	metarules is taken from the configuration option
%	metarule_formatting/1.
%
debug_metarules(S,MS):-
	configuration:metarule_formatting(F)
	,debug_metarules(F,S,MS).


%!	debug_metarules(+Formatting,+Subject,+Metarules) is det.
%
%	Log a list of pretty-printed Metarules to a debug stream.
%
%	Subject is the name of the debug subject associated with the
%	debugging stream, where the list of Metarules is to be
%	pretty-printed.
%
%	Metarules is a list of metarule IDs or expended metarules to be
%	printed to the debug stream associated with Subject.
%
%	Formatting is one fo [quantified,user_friendly,expanded]. See
%	configuration option metarule_formatting/1 for details.
%
debug_metarules(F,S,M):-
	\+ is_list(M)
	,!
	,debug_metarules(F,S,[M]).
debug_metarules(F,S,[I|Is]):-
% Punch metarules are "expanded" into lists of integers.
	integer(I)
	,!
	,min_list([I|Is],Min)
	,max_list([I|Is],Max)
	,output_metarule(F,debug,S,higher_order(Min,Max)).
debug_metarules(F,S,MS):-
	!
	,forall(member(M,MS)
	       ,output_metarule(F,debug,S,M)
	       ).



%!	debug_metarules(+Formatting,+Subject,+Message,+Metarules) is
%!	det.
%
%	Print a list of pretty-printed Metarules to a debug Subject.
%
%	As debug_metarules/3 but also prints an informative Message to
%	the debug output. The predicate doesn't check that Message is
%	actually informative :P
%
%	@tbd If Message is '', an empty debug line is printed, i.e. an
%	empty line headed with the Prolog comment symbol, %. This can be
%	used to add some space between output that is otherwise too
%	cloase together, to facilitate reading logs.
%
debug_metarules(F,S,M,MS):-
	debug(S,'~w',M)
	,debug_metarules(F,S,MS).



%!	debug_msg_metarules(+Subject,+Message,+Metarules) is det.
%
%	Print a list of pretty-printed Metarules to a debug Subject.
%
%	As debug_metarules/4, but takes the metarule printing format
%	from the configuration option metarule_formatting/1.
%
%	@tbd This breaks the mould of the name of metarule debugging
%	predicates because a three-argument version of
%	debug_metarules/3 already exists and it would make a bit of a
%	mess if we endeavoured to overload it.
%
%
debug_msg_metarules(S,M,MS):-
	configuration:metarule_formatting(F)
	,debug_metarules(F,S,M,MS).



%!	output_metarules(+Format,+How,+Where,+Metarule) is det.
%
%	Pretty-print a Metarule.
%
%	How is one of: [print, debug], denoting whether Metarule is
%	pretty-printed to a debug or print stream.
%
%	Where is the name of the stream to print to if How is "print" or
%	the debug subject, if How is "debug".
%
%	Metarule is either a metarule ID or an expanded metarule, as the
%	output of metarule_expansion/2 or expanded_metarules/2.
%
%	__Motivation__
%
%	This predicate is the base predicate for pretty-printing
%	expanded metarules at the top-level for the purpose of debugging
%	a MIL problem. It is called by print_metarules/[1,2] to print
%	metarules in "expanded" format at the top-level and by
%	debug_metarules/[2,3,4] to debug metarules to a debug stream
%	associated with a degub subject.
%
%	Metarules can also be printed to the top-level using
%	print_clauses/1, however that predicate assigns variable names
%	to metarule variables according to their order. This can make
%	metarules difficult to read especially for the user who has the
%	notation used in MIL literature in mind.
%
%	Example:
%       ==
%       ?- auxiliaries:output_metarule(expanded,print,user_output,chain).
%       m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
%       true.
%       ==
%
%	Note the difference with the alternative of printing metarules
%	using print_clauses/2:
%	==
%	?- expanded_metarules([chain],_MS), print_clauses(_MS).
%	m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
%	true.
%	==
%
%	@tbd Third-order metarules are not expanded into encapsulated
%	clauses with a metasubstitution atom in the head, and so this
%	predicate cannot pretty-print third order metarules when Format
%	is 'expanded'.
%
output_metarule(F,_H,_W,_Id):-
	var(F)
	,throw('Please supply a non-variable metarule formatting argument').
output_metarule(expanded,H,W,Id):-
	!
	,pretty_expanded_metarule(Id,M)
	,output_metarule(H,W,M).
output_metarule(quantified,H,W,Id):-
	!
	,quantified_metarule(Id,M)
	,output_metarule(H,W,M).
output_metarule(user_friendly,H,W,Id):-
	user_friendly_metarule(Id,M)
	,output_metarule(H,W,M).
output_metarule(F,_H,_W,_Id):-
	\+ memberchk(F, [expanded,quantified,user_friendly])
	,throw('Uknown metarule printing format':F).


%!	output_metarule(+How,+Where,+Id) is det.
%
%	Business end of otuput_metarule/4.
%
output_metarule(H,W,M):-
% M may be a list of third-order metarules.
	(   is_list(M)
	->  forall(member(Hom,M)
		  ,print_or_debug(H,W,Hom)
		  )
	;   print_or_debug(H,W,M)
	).


%!	pretty_expanded_metarule(+Id,-Expanded) is det.
%
%	Transform a metarule into a pretty-printing format.
%
%	Id is the id of a metarule, or an expanded metarule.
%
%	Expanded is the metarule with the given Id, expanded by
%	metarule_expansion/2 and with variables replaced with ones
%	defined in the configuration option symbol_range/2. See also
%	numbered_symbols/3.
%
pretty_expanded_metarule(higher_order(M,N),higher_order(M,N)):-
% Punch metarules aren't really expanded, as such.
	!.
pretty_expanded_metarule(Id,M_):-
	atom(Id)
	,!
	,once(metarule_expansion(Id,M))
	,pretty_expanded_metarule(M,M_).
pretty_expanded_metarule(MR,MR_):-
	must_be(nonvar, MR)
	,copy_term(MR,MR_)
	,metarule_variables(MR_,Es,Us)
	,length(Es,N)
	,numbered_symbols(N,Ps,predicate)
	,findall('$VAR'(P)
		,(nth1(I,Ps,P)
		 ,nth1(I,Es,'$VAR'(P))
		 )
		,Es)
	,length(Us,M)
	,numbered_symbols(M,Vs,variable)
	,findall('$VAR'(V)
		,(nth1(I,Vs,V)
		 ,nth1(I,Us,'$VAR'(V))
		 )
		,Us).


%!	metarule_variables(+Metarule,-Second_order,-First_order) is det.
%
%	Collect variables in an encapsulated Metarule.
%
%	Second_order is the list of second-order existentially
%	quantified variables in Metarule. First_order is the list of
%	first-order existentially and universally quantified variables
%	in Metarule.
%
%	The two output lists, Second_order and First_order only include
%	each variable once and the variables are in the order in which
%	they were originally in the metarule's literals. Unfortunately
%	this means that sort/2 cannot be used to remove duplicates
%	because it also imposes a new order on the variables in each
%	list, which may not be their original order. For this reason,
%	two auxiliaries, second_order/3 and first_order/3 go through
%	a list of variables each time one or more variables must be
%	added to it, and ensure that only new variables are added. This
%	of course is rather expensive (it's O(N^2)) so this is not a
%	predicate that should ever be in the transitive closure of a
%	heavy-lifting predicate.
%
%	@tbd Well, add this warning at print_metarules/[1,2], no?
%
metarule_variables(_A:-M,Ss,Fs):-
	metarule_variables(M,[],Ss_,[],Fs_)
	,maplist(reverse,[Ss_,Fs_],[Ss,Fs]).

%!	metarule_variables(+Literals,+Ss_Acc,-Ss,+Fs_Acc,-Fs) is det.
%
%	Business end of metarule_variables/3.
%
metarule_variables((L,Ls),Ss_Acc,Ss_Bind,Fs_Acc,Fs_Bind):-
	! % Avoid backtracking here for more of the same.
	,configuration:encapsulation_predicate(E)
	,L =.. [E,P|Fs]
	,second_order(P,Ss_Acc,Ss_Acc_)
	,first_order(Fs,Fs_Acc,Fs_Acc_)
	,metarule_variables(Ls,Ss_Acc_,Ss_Bind,Fs_Acc_,Fs_Bind).
metarule_variables(L,Ss_Acc,Ss,Fs_Acc,Fs):-
	configuration:encapsulation_predicate(E)
	,L \= (_,_)
	,L =.. [E,P|Fs_L]
	,second_order(P,Ss_Acc,Ss)
	,first_order(Fs_L,Fs_Acc,Fs).


%!	second_order(+Variable,+Acc,-New) is det.
%
%	Add a Variable to an Accumulator if it is not already there.
%
second_order(P,Acc,[P|Acc]):-
	\+ in_vars(P,Acc)
	,!.
second_order(_P,Acc,Acc).


%!	first_order(+Variables,+Acc,-New) is det.
%
%	Add a list of Variables to an Accumulator if not already there.
%
first_order([],Fs,Fs):-
	!.
first_order([V|Fs],Acc,Bind):-
	\+ in_vars(V,Acc)
	,!
	,first_order(Fs,[V|Acc],Bind).
first_order([_V|Fs],Acc,Bind):-
	first_order(Fs,Acc,Bind).


%!	in_vars(?Variable,?Variables) is det.
%
%	True when a Variable is in a list of Variables.
%
%	Version of memeber/2 that avoids unfiying Variable with every
%	other variable in Variables and thereby making an awful
%	mish-mashed mess of unexpectedly identical variables. We need to
%	preserve Variables and their bindings throughout the project.
%
%	@tbd This is copied verbatim from metagen module. Maybe add it
%	to er, this module and make public? Or perhaps in
%	library(term_utilities)?
%
in_vars(V,[V1|_Vs]):-
	V == V1
	,!.
in_vars(V,[_|Vs]):-
	in_vars(V,Vs).



%!	numbered_symbols(+N,-Symbols,+Type) is det.
%
%	Generate a list of N predicate Symbols of the given Type.
%
%	N is an integer, the number of Symbols to generate.
%
%	Type is one of [predicate, variable], denoting whether Symbols
%	should be predicate symbols or variable names.
%
%	Symbols is a list of atoms representing predicate or variable
%	symbols to be used as the names of variables in a metarule when
%	pretty-printing the metarule. Predicate symbols are assigned to
%	second-order existentially quantified variables and variable
%	symbols to first-order universally and existentially quantified
%	variables.
%
%	Generating predicate symbols
%	----------------------------
%
%	When T = predicate, Symbols is a list of atomic predicate
%	symbols to be assigned to the second-order existentially
%	quantified variables of a metarule.
%
%	Predicate symbols are given names from an initial set of M
%	symbols, Ns, generated by symbol_range/3 when its first agrument
%	is the atom 'predicate'. M is the "symbol range" i.e. the
%	cardinality of Ns. When more than M symbols are generated, each
%	symbol afte the first M is indexed by a number, according to how
%	many times the same symbol has been used before.
%
%	As an example of how numbers are assigned to predicate symbols,
%	suppose that Ns = [P,Q,R,S,T] so M = 5.
%
%	Then, for N in [1,5] predicate symbols are given the names in Ns
%	so that the first element in Symbols is 'P', the second is 'Q',
%	etc.
%
%	For N > 6, predicate symbols are given the names in Ns, numbered
%	with the division of 5 in which N falls. For example, the N's in
%	[6,10] fall within the first division of 5, the N's in [11,15]
%	fall in the second division of 5, etc.
%
%	This is illustrated with an example, below.
%
%	Example:
%	==
%	% Generating predicate symbols for varying N.
%	% Assuming:
%	% symbol_range(predicate, ['P','Q','R','S','T'], 5).
%
%	?- auxiliaries:predicate_symbols(5, S).
%	S = ['P', 'Q', 'R', 'S', 'T']. % N in 0'th division of 5
%
%	?- auxiliaries:predicate_symbols(10, S).
%	S = ['P', 'Q', 'R', 'S', 'T'  % N in 'th division of 5
%	,'P1', 'Q1', 'R1', 'S1', 'T1' % N in 1st division of 5
%	].
%
%	?- auxiliaries:predicate_symbols(15, S).
%	S = ['P', 'Q', 'R', 'S', 'T'  % N in 0'th division of 5
%	,'P1', 'Q1', 'R1', 'S1', 'T1' % N in 1st division of 5
%	,'P2', 'Q2', 'R2', 'S2', 'T2' % N in 2nd division of 5
%	].
%
%	?- auxiliaries:predicate_symbols(20, S).
%	S = ['P', 'Q', 'R', 'S', 'T'  % N in 0'th division of 5
%	,'P1', 'Q1', 'R1', 'S1', 'T1' % N in 1st division of 5
%	,'P2', 'Q2', 'R2', 'S2', 'T2' % N in 2nd division of 5
%	,'P3', 'Q3', 'R3', 'S3', 'T3' % N in 3d division of 5
%	].
%
%	% etc
%	==
%
%	Generating variable names
%	-------------------------
%
%	When T = variable, Symbols is a list of atomic variable names to
%	be assigned to the first-order existentially and universally
%	quantified variables of a metarule.
%
%	Variable symbols are given names from an initial set of M
%	symbols, Ns, generated by symbol_range/3 when its first argument
%	is the atom 'variable'. As for predicate symbols, M is the
%	symbol range for variables, i.e. the cardinality of Ns. When
%	more than M symbols are generated, each symbol after the first M
%	is indexed by a number according to how many times that symbol
%	has been used before.
%
%	As an example of how numbers are assigned to variable symbols,
%	suppose that Ns = [X,Y,Z] and M = 3.
%
%	Then, for N in [1,3], variable symbols are given the names in Ns
%	so that the first element in Symbols is 'X', the second 'Y' and
%	the third is 'Q'.
%
%	For N > 3, variable symbols are given the names in Ns, numbered
%	with the division of 3 in which N falls. For example, the N's in
%	[4,6] fall in the first division of 3, the N's in [7,9] fall in
%	the second division of 3, etc.
%
%	This is illustrated with an example, below.
%
%	Example:
%	==
%	% Generating variable symbols for varying N.
%	% Assuming:
%	%symbol_range(variable, ['X','Y','Z'], 3).
%
%	?- auxiliaries:numbered_symbols(3, S, variable).
%	S = ['X', 'Y', 'Z']. % N in 0'th division of 3
%
%	?- auxiliaries:numbered_symbols(6, S, variable).
%	S = ['X', 'Y', 'Z' % N in 0'th division of 3
%	,'X1', 'Y1', 'Z1'  % N in 1st division of 3
%	].
%
%	?- auxiliaries:numbered_symbols(9, S, variable).
%	S = ['X', 'Y', 'Z' % N in 0'th division of 3
%	,'X1', 'Y1', 'Z1'  % N in 1st division of 3
%	,'X2', 'Y2', 'Z2'  % N in 2nd division of 3
%	].
%
%	?- auxiliaries:numbered_symbols(12, S, variable).
%	S = ['X', 'Y', 'Z' % N in 0'th division of 3
%	,'X1', 'Y1', 'Z1'  % N in 1st division of 3
%	,'X2', 'Y2', 'Z2'  % N in 2nd division of 3
%	,'X3', 'Y3', 'Z3'  % N in 3d division of 3
%	].
%
%	% etc.
%	==
%
numbered_symbols(N,Ss,T):-
	% Off-by-one correction for symbols' indices.
	succ(N, N_)
	,numbered_symbols(T,1,N_,[],Ss).

%!	numbered_symbols(+Type,+Current,+Max,+Acc,-Symbols) is det.
%
%	Business end of numbered_symbols/3.
%
numbered_symbols(_T,N,N,Acc,Ss):-
	!
	,reverse(Acc, Ss).
numbered_symbols(T,I,K,Acc,Bind):-
	once(numbered_symbol(T,I,S))
	,succ(I,I_)
	,numbered_symbols(T,I_,K,[S|Acc],Bind).


%!	numbered_symbol(+Type,+Index,-Symbol) is nondet.
%
%	Generate a Symbol of the given Type.
%
%	Index is the index of the symbol in the list of symbols
%	generated by numbered_symbols/3. Index is used to determine
%	whether, and how, to number Symbol to distinguish it from other
%	symbols with the same letter generated so far. See
%	numbered_symbols/3 for an explanation of how symbol numbering
%	works.
%
numbered_symbol(T,I,S):-
	symbol_range(T,Ss,_M)
	,nth1(I,Ss,S)
	,!.
numbered_symbol(T,I,S):-
	symbol_range(T,Ss,M)
	,% K'th division of M, counting from 0
	K is ceiling(I/M) - 1
	% I_'th item in K'th division of M
	, I_ is I - M * K
	,nth1(I_,Ss,S_)
	% Or:
	%,numbered_symbol(T,I_,S_)
	,atomic_list_concat([S_,K],'',S).


%!	symbol_range(+Type,-Symbols,-N) is det.
%
%	The list of Symbols for a Type.
%
%	Type is one of [predicate,variable], denoting the type of
%	symbols in the currenr range.
%
%	Symbols is a list of symbols of the given Type. The value of
%	Symbols is taken from the configuration option symbol_range/2.
%
%	N is the number of symbols in Symbols. This is used by
%	numbered_symbol/3 to index symbols accordig to how many times
%	they have already been used.
%
symbol_range(T,Ss,N):-
	configuration:symbol_range(T,Ss)
	,length(Ss,N).



%!	user_friendly_metarule(+Metarule,-Atom) is det.
%
%	Transform a Metarule into a user-friendly pretty-printing Atom.
%
user_friendly_metarule(higher_order(M,N),higher_order(M,N)):-
% Punch metarules don't really have a more user-friendly format.
	!.
user_friendly_metarule(Id,A):-
	quantified_metarule(Id,M)
	,atom(M)
	,atomic_list_concat([_Qs,C],': ',M)
	,metarule_id(Id,Id_)
	,(   atomic_list_concat([Hd,Bd],'\u2190 ',C)
	 ->  format(atom(A),'configuration:~w metarule \'~w:- ~w\'.',[Id_,Hd,Bd])
	 ;   atom_concat(Hd,'\u2190',C)
	    ,format(atom(A),'configuration:~w metarule \'~w\'.',[Id_,Hd])
	 ).


%!	quantified_metarule(+Id,-Atom) is det.
%
%	Transform a metarule into a pretty-printing atom.
%
%	Business end of output_quantified_metarule/3.
%
quantified_metarule(higher_order(Min,Max),Hs):-
% Pretty-print third-order metarules.
	!
	,findall(M_
	       ,(between(Min,Max,N)
		,length(Ps,N)
		,variables_symbols(predicate,Ps)
		,once(list_tree(Ps,Vs))
		,(   Vs = (Hd,B)
		 ->  format(atom(M),'~w \u2190 ~w',[Hd,B])
		 ;   M = Vs
		 )
		,maplist(arg(1),Ps,Es)
		,atomic_list_concat(Es,',',Es_)
		,format(atom(Es_A),'\u2203.~w',[Es_])
		,format(atom(M_),'(TOM-~w) ~w: ~w',[N,Es_A,M])
		)
	       ,Hs).
quantified_metarule(Id,A):-
	atom(Id)
	,!
	,once(metarule_expansion(Id,M))
	,quantified_metarule(M,A).
quantified_metarule(M,A):-
	metarule_symbols(M)
	,excapsulated_metarule(M,M_)
	,metarule_id(M, Id)
	,metarule_quantifiers(M,Es,Us)
	,pretty_metarule_id(Id,Id_)
	,format(atom(A),'(~w) ~w~w: ~w',[Id_,Es,Us,M_]).


%!	metarule_symbols(+Metarule) is det.
%
%	Assign common names to the variables in a Metarule.
%
%	This is a very poorly named predicate but I couldn't really find
%	anything better. What it does is that it binds the variables in
%	Metarule to appropriate symbols, depending on whether they're
%	first- or second-order and existentially or universally
%	quantified. "Appropriate" symbols then are the symbols that are
%	typically assigned to the relevant kind of variable in the MIL
%	literature.
%
%	The specific symbols used are defined by the user in the
%	configuration option symbol_range/2 however this predicate
%	enforces lower-casing of first-order, universally quantified
%	variables. However, capitalisation of any-order existentially
%	quantified variables is not enforced.
%
%	@bug The decision to not force capitalisation of existentially
%	quantified variables might well lead to ugly results if the user
%	chooses a symbol_range/2 that doesn't agree with this scheme.
%	e.g. a symbol range for variables where all variables are lower-
%	case, like symbol_range(variable, [x,y,z]) will cause
%	existentially quantified first-order variables to be printed in
%	lower-case.
%
metarule_symbols(M):-
	metarule_variables(M,Ss,Fs)
	,variables_symbols(predicate,Ss)
	,variables_symbols(variable,Fs)
	,existential_vars(M,Es)
	,quantification_case(Es,Fs).


%!	variables_symbols(+Type, ?Variables) is det.
%
%	Bind Variables to appropriate symbols of the given Type.
%
variables_symbols(T,Vs):-
	length(Vs,N)
	,numbered_symbols(N,Ps,T)
	,findall('$VAR'(P)
		,(nth1(I,Ps,P)
		 ,nth1(I,Vs,'$VAR'(P))
		 )
		,Vs).


%!	existential_vars(+Metarule, -Existential) is det.
%
%	Collect Existentially quantified variables from a Metarule.
%
existential_vars(Sub_E/_Sub_U:-_M,Es):-
	configuration:encapsulation_predicate(E)
	,Sub_E =.. [E,_Id|Es].


%!	quantification_case(+Existential,+First_Order) is det.
%
%	Ensure variables' symbols are the appropriate case.
%
%	Existential is the list of existentially quantified variables in
%	a metarule, which may include first- and second-order variables.
%	First_Order is a list of the first-order variables in a
%	metarule. Both lists of variables are already ground to
%	'$VAR'(S) terms where S is a meaningful symbol depending on the
%	variable's order and quantification.
%
%	What this predicate does then is to downcase first-order,
%	universally quantified variables, which are the variables in
%	First_Order that are not also in Existential.
%
%	@tbd This predicate is very, very naughty. Because Existential
%	and First_Order are both lists of "Skolem" terms, like
%	'$VAR'(S), and in order to avoid having to re-construct the
%	metarule thse variables have come from, it uses arg/3 and
%	nb_setarg/3 to modify the symbols in '$VAR'(S) terms where
%	appropriate, by downcasing them.
%
quantification_case(_,[]):-
	!.
quantification_case(Es,[V|Fs]):-
	\+ memberchk(V,Es)
	,!
	,arg(1,V,S)
	,downcase_atom(S,S_)
	,nb_setarg(1,V,S_)
	,quantification_case(Es,Fs).
quantification_case(Es,[_V|Fs]):-
	quantification_case(Es,Fs).


%!	excapsulated_metarule(+Metarule,-Atomic) is det.
%
%	Excapsulate a metarule into an Atomic representation.
%
%	Metarule is an expanded metarule with variables bound '$VAR'(S)
%	terms where each S is an appropriate name for a variable
%	depending on its order and quantification.
%
%	Atomic is the same metarule in a second-order representation,
%	although at this point not yet with quantifiers. The
%	representatin is an atom, because Prolog cannot represent
%	second-order terms otherwise.
%
excapsulated_metarule((_A:-M),A):-
	clause_literals(M,Ls)
	,excapsulated_literals(Ls,[],Ls_)
	,once(list_tree(Ls_,C))
	,(   C = (H,B)
	->   format(atom(A),'~w\u2190 ~w',[H,B])
	     % Unit clause metarule.
	 ;   format(atom(A),'~w\u2190',[C])
	 ).


%!	excapsulated_literals(+Literals,+Acc,-Excapsulated) is det.
%
%	Excapsulate each of the Literals of an expanded metarule.
%
excapsulated_literals([],Acc,Ls):-
	!
	,reverse(Acc,Ls).
excapsulated_literals([L|Ls],Acc,Bind):-
	configuration:encapsulation_predicate(E)
	,L =.. [E|As]
	,findall(S
		,member('$VAR'(S),As)
		,Ss)
	,S_ =.. Ss
	,excapsulated_literals(Ls,[S_|Acc],Bind).


%!	metarule_quantifiers(+Metarule,-Existential,-Universal) is det.
%
%	Collect quantifiers and quantified variables in a Metarule.
%
%	Metarule is an expanded metarule.
%
%	Existential is an atomic represention of the existentially
%	quantified variables in Metarule, preceded by an existential
%	quantifier, in the form \exists.P,Q,R... etc.
%
%	Univerasl is a film studio in Hollywood. Also, in this
%	predicate, it's an atomic representation of the universally
%	quantified variables in Metarule, preceeded by a universal
%	quantifier, in the form \forall.X,Y,Z... etc.
%
metarule_quantifiers(M,Es,Us):-
	metarule_variables(M,_Ss,Fs)
	,existential_vars(M,Es_)
	,subtract(Fs,Es_,Us_)
	,maplist(arg(1),Es_,Ss_E)
	,maplist(arg(1),Us_,Ss_U)
	,atomic_list_concat(Ss_E,',',Es_1)
	,atomic_list_concat(Ss_U,',',Us_1)
	,format(atom(Es),'\u2203.~w',[Es_1])
	,(   Ss_U = [_]
	 ->  format(atom(Us),' \u2200~w',[Us_1])
	 ;   Ss_U = [_|_]
	 ->  format(atom(Us),' \u2200.~w',[Us_1])
	 ;   Ss_U = []
	 ->  Us = ''
	 ).


%!	pretty_metarule_id(+Id,-Pretty) is det.
%
%	Prettify a metarule Id.
%
%	The prettified Id has its first letter up-cased and each
%	underscore turned to a hyphen.
%
%	@tbd  Other prettifications may need to be added later.
%
pretty_metarule_id(Id,Id_):-
	atom_chars(Id, [C|Cs])
	,upcase_atom(C,C_)
	,findall(Ck
		,(member(Ci,Cs)
		 ,(   Ci == '_'
		  ->  Ck = -
		  ;   Ck = Ci
		  )
		 )
		,Cs_)
	,atom_chars(Id_,[C_|Cs_]).



%!	metarule_id(+Metarule,-Id) is det.
%
%	Extract the Id of a Metarule.
%
%	Metarule may be an atomic Id of a metarule or a metarule clause.
%	Id is either the atom Id, or the atomic ide of the metarule in
%	the clause.
%
%	@tbd this is going to be needed elsewhere. Maybe modify
%	mil_problem's metarule_parts/5 so it actually works?
%
metarule_id(Id,Id):-
	atom(Id)
	,!.
metarule_id(Sub_E/_Sub_U:-_M,Id):-
	configuration:encapsulation_predicate(E)
	,Sub_E =.. [E,Id|_].




% [sec_dynm]
% ================================================================================
% Database auxiliaries
% ================================================================================
% Predicates for manipulating the Prolog dynamic database.
% Remember: the dynamic database is evil.


%!	assert_program(+Module,+Program,-Clause_References) is det.
%
%	As assert_program/2 but also binds a list of Clause_References.
%
assert_program(M,Ps,Rs):-
	assert_program(M,Ps,[],Rs).

assert_program(_,[],Rs,Rs):-
	!.
assert_program(M,[A|P],Acc,Bind):-
	copy_term(A,A_)
	,numbervars(A_)
	,clause(M:A_,true)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[C|P],Acc,Bind):-
	copy_term(C,H:-B)
	,numbervars(H:-B)
	,clause(M:H,B)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[C|P],Acc,Bind):-
	assert(M:C,Ref)
	,assert_program(M,P,[Ref|Acc],Bind).



%!	erase_program_clauses(-Clause_References) is det.
%
%	Erase a list of Clause_References from the dynamic database.
%
%	Clause_References is meant to be a list of references of a
%	program's clauses asserted to the dynamic database with
%	assert_program/3.
%
%	The purpose of this predicate is to allow a set of clauses
%	previously asserted by invoking assert_program/3 to be removed
%	from the dynamic database without stumbling over module scoping
%	that can be complicated when a predicate is declared in one
%	module and then clauses of it are added in another module.
%
%	For example, the following is what you should expect to see in
%	the dynamic database after a theory of father/2 is learned and
%	asserted in the dynamic database, while there is also background
%	knowledge of father/2:
%
%	==
%	% Example copied from Thelma.
%	[debug] [1]  ?- listing(thelma:father/2).
%	:- dynamic tiny_kinship:father/2.
%
%	tiny_kinship:father(stathis, kostas).
%	tiny_kinship:father(stefanos, dora).
%	tiny_kinship:father(kostas, stassa).
%	tiny_kinship:father(A, C) :-
%	    thelma:
%	    (   father_1(A, B),
%	        parent(B, C)
%	    ).
%
%	true.
%	==
%
%	This happens whenever new clauses of a previous defined
%	predicate are asserted in a different module than the
%	predicate's original implementation module. The reason we may
%	wish to do that is to create "multiple worlds" each with
%	different definitions of a predicate. For example, Thelma, where
%	the above example is taken from, asserts a learned hypothesis to
%	the dynamic database in order to test it against the negative
%	examples. However, the clauses of the learned hypothesis can get
%	mixed up with the examples of the target predicate. This creates
%	an unholy mess that is very fiddly to manage.
%	erase_program_clauses/1 helps a little, but, ultimately, one
%	must never forget that the dynamic database is evil.
%
erase_program_clauses([]):-
	!.
erase_program_clauses([Ref|Rs]):-
	erase(Ref)
	,erase_program_clauses(Rs).



%!	eraseall_program_clauses(+Refs) is det.
%
%	retractall-like clause erasure.
%
%	"Retractall-like" in the sense that if a reference in Refs is
%	now found, this doesn't fail and simply continues with the next
%	reference.
%
eraseall_program_clauses([]):-
	!.
eraseall_program_clauses([Ref|Rs]):-
	erase(Ref)
	,!
	,eraseall_program_clauses(Rs).
eraseall_program_clauses([_Ref|Rs]):-
	eraseall_program_clauses(Rs).




% [sec_prog]
% ================================================================================
% Program auxiliaries
% ================================================================================
% Predicates for inspecting a program.


%!	built_in_or_library_predicate(+Predicate) is det.
%
%	True for a built-in or autoloaded Predicate.
%
%	Thin wrapper around predicate_property/2. Used to decide what
%	programs to collect with closure/3 and what programs to
%	encapsulate.
%
built_in_or_library_predicate(H):-
	predicate_property(H, built_in)
	,!.
built_in_or_library_predicate(H):-
	predicate_property(H, autoload(_)).



%!	closure(+Progam_Symbols,+Module,-Closure) is det.
%
%	Collect all clauses of a program and its Closure.
%
%	As program/3, but also collects the definitions of programs in
%	the closure of a progam.
%
%	Progam_Symbols is a list of predicate symbols and arities, F/A,
%	of clauses in a program. Closure is the set of definitions of
%	the Symbols in Program_Symbols, and the definitions of the
%	programs in the closure of each program in Program_Symbols.
%
%	Module is the definition module of each program in Closure, or
%	a module importing that module. To ensure each program in
%	Closure is accessible the best thing to do is to export
%	everything to the user module.
%
%	Example
%	-------
%	==
%	?- closure([ancestor/2],user,_Cs),forall(member(P,_Cs),print_clauses(P)).
%
%	ancestor(A,B):-parent(A,B).
%	ancestor(A,B):-parent(A,C),ancestor(C,B).
%	parent(A,B):-father(A,B).
%	parent(A,B):-mother(A,B).
%	father(stathis,kostas).
%	father(stefanos,dora).
%	father(kostas,stassa).
%	mother(alexandra,kostas).
%	mother(paraskevi,dora).
%	mother(dora,stassa).
%	true.
%	==
%
%	@tbd Why is closure/3 returning a list of lists rather than a
%	flat list? In mil_problem:encapsulated_bk/2 it's flattened after
%	being called. Why do we need to do that?
%
closure(Ss,M,Cs):-
	closure(Ss,[],_Ps,M,[],Cs_)
	,reverse(Cs_, Cs).

%!	closure(+Symbols,+Path_Acc,-Path,+Module,+Acc,-Closure) is det.
%
%	Business end of closure/3.
%
closure([],Ps,Ps,_M,Cs,Cs):-
	!.
closure([F/A|Ss],Ps_Acc,Ps_Bind,M,Acc,Bind):-
	functor(S,F,A)
	,built_in_or_library_predicate(S)
	,!
	,closure(Ss,Ps_Acc,Ps_Bind,M,Acc,Bind).
closure([S|Ss],Ps_Acc,Ps_Bind,M,Acc,Bind):-
	\+ memberchk(S,Ps_Acc)
	,!
	,debug(closure,'Looking for definition of ~w in module ~w.', [S,M])
	,program(S,M,Cs)
	,(   Cs \= []
	 ->  debug(closure,'Found definition of ~w in module ~w.~n', [S,M])
	 ;   debug(closure,'Found empty definition of ~w in module ~w.~n', [S,M])
	 )
	,closure(Ss,[S|Ps_Acc],Ps_Acc_,M,[Cs|Acc],Acc_)
	,program_symbols(Cs,Ss_)
	,closure(Ss_,Ps_Acc_,Ps_Bind,M,Acc_,Bind).
closure([_S|Ss],Ps,Ps_Acc,M,Acc,Bind):-
	closure(Ss,Ps,Ps_Acc,M,Acc,Bind).



%!	program_symbols(+Program,-Symbols) is det.
%
%	Collect symbols of body literals in a Program.
%
program_symbols(Ps,Ss):-
	clauses_literals(Ps,Ls)
	,setof(F/A
	      ,L^Ls^(member(L,Ls)
		    ,functor(L,F,A)
		    )
	      ,Ss).



%!	program(+Symbols,+Module,-Program) is det.
%
%	Collect all clauses of a Program.
%
%	Symbols is the list of predicate indicators, F/A, of clauses in
%	Program.
%
%	Module is the definition module for Progam. This can be set to
%	user if the Program is not defined in a module.
%
%	Program is a list of all the clauses of the predicates in
%	Symbols.
%
%	@tbd This doesn't attempt to sort the list of Symbols to exclude
%	duplicates- if the same Symbol is passed in more than once, the
%	same definition will be included that many times in Programs.
%
program(F/A,M,Ps):-
	!
	,program([F/A],M,Ps).
program(Ss,M,Ps):-
	findall(P
	       ,(member(F/A,Ss)
		,functor(H,F,A)
		,M:clause(H,B)
		,(   B == true
		 ->  P = H
		 ;   P = (H:-B)
		 )
		)
	       ,Ps).



%!	table_program(+Symbols) is det.
%
%	Table each of a list of predicates given their Symbols.
%
table_program(Ss):-
	forall(member(F/A,Ss)
	      ,(table(program:F/A)
	       ,debug(tabling,'Tabled ~w',[F/A])
	       )
	      ).



%!	untable_program(+Symbols) is det.
%
%	Untable a list of predicates given their Symbols.
%
untable_program(Ss):-
	forall(member(F/A,Ss)
	      ,(untable(program:F/A)
	       ,debug(tabling,'Removed tabling for ~w',[F/A])
	       )
	      ).




% [sec_time]
% ================================================================================
% Timing auxiliaries
% ================================================================================
% Predicates to time the execution of a goal.

%!	timing(+Goal,-Time) is det.
%
%	Call a Goal and report the Time it took.
%
%	@tbd: If Goal fails, no Time is reported. Wrap in a conditional?
%
timing(G, T):-
	S is cputime
	,call(G)
	,E is cputime
	,T is E - S.



%!	timing(+Goal,+Limit,-Time) is det.
%
%	Call a Goal with a time Limit and report the Time it took.
%
%	@tbd: If Goal fails, no Time is reported. Wrap in a conditional?
%
timing(G, L, T):-
	S is cputime
	,C = call_with_time_limit(L,G)
	,R = debug(learning_curve_full,'Exceeded Time limit: ~w sec',[L])
	,catch(C,time_limit_exceeded,R)
	,E is cputime
	,T is E - S.
