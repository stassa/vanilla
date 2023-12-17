:-module(metagol_auxiliaries, [% MIL problem auxiliaries
	                       write_encapsulated_problem/1
                               % Debugging auxiliaries
                              ,list_encapsulated_problem/1
                              ,list_learning_results/0
                              ,list_mil_problem/1
                              ,list_problem_statistics/1
                               % Experiment file auxiliaries
                              ,cleanup_experiment/0
                              ,experiment_data/5
                              ,learning_target/1
                              ,learning_targets/1
                              ,load_experiment_file/0
                              ,edit_experiment_file/0
                              ]).

:-use_module(metagol_configuration).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).

/** <module> Auxiliary predicates for Metagol.

Predicates in this module support the implementation and usage of
Metagol. They include predicates to examine an experiment file in the
format utiliesed in Louise, the project from which these predicates were
originally copied.

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
	experiment_data(T,Pos,Neg,BK,MS)
	,write_encapsulated_problem(Pos,Neg,BK,MS).




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
	metagol_configuration:listing_limit(L)
	,experiment_data(Ts,Pos,Neg,BK,MS)
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,_BK_,MS_])
	,format_underlined('Positive examples')
	,print_limited(L,Pos_)
	,nl
	,format_underlined('Negative examples')
	,print_limited(L,Neg_)
	,nl
	,format_underlined('Background knowledge')
	,forall(member(P,BK)
	       ,(encapsulated_bk([P],Ts,Ps)
		,format('~w:~n',[P])
		,print_limited(L,Ps)
		,nl
		)
	       )
	,nl
	,expanded_metarules(MS,MS_)
	,format_underlined('Metarules')
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
%	arity of one of the learning predicates in Louise.
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
	experiment_data(T,Pos,Neg,BK,MS)
	,list_mil_problem(Pos,Neg,BK,MS)
	,nl
	,print_constraints(MS,metasub).


%!	list_mil_problem(+Pos,+Neg,+BK,+MS) is det.
%
%	Business end of list_mil_problem/1, list_mil_problem_thelma/1.
%
list_mil_problem(Pos,Neg,BK,MS):-
	metagol_configuration:listing_limit(L)
	,format_underlined('Positive examples')
	,print_limited(L,Pos)
	,nl
	,format_underlined('Negative examples')
	,print_limited(L,Neg)
	,nl
	,format_underlined('Background knowledge')
	,forall(member(P,BK)
	       ,(program(P,experiment_file,Ps)
		,format('~w:~n',[P])
		,print_limited(L,Ps)
		,format('~n',[])
		)
	       )
	,format_underlined('Metarules')
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
print_constraints(MS,order):-
	!
	,format_underlined('Order constraints')
	,forall(member(Id,MS)
	       ,(metagol_configuration:order_constraints(Id,Ss,Fs,PS,CS)
		,prettify_vars(Ss,predicate,Ss_)
		,prettify_vars(Fs,variable,Fs_)
		,print_or_debug(print,user_output,order_constraints(Id,Ss_,Fs_,PS,CS))
		)
	       ).
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



%!	list_mil_problem_thelma(+Targets) is det.
%
%	List the elements of a MIL Problem for Thelma.
%
%	As list_mil_problem/1 but also prints out information about
%	metarule constraints and order constraints.
%
%	@tbd Needs to be renamed or removed.
%
list_mil_problem_thelma(Ts):-
	experiment_data(Ts,Pos,Neg,BK,MS)
	,list_mil_problem(Pos,Neg,BK,MS)
	,nl
	,print_constraints(MS,order).



%!	list_problem_statistics(+Target) is det.
%
%	List statistics of the MIL problem for Target.
%
%	Currently this only lists the numbers of positive and negative
%	examples, background definitions and metarules in the initial
%	MIL problem for Target (i.e. before any automatic modifications
%	such as metarule extension).
%
list_problem_statistics(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,maplist(length,[Pos,Neg,BK,MS],[I,J,K,N])
	,format('Positive examples:    ~w~n', [I])
	,format('Negative examples:    ~w~n', [J])
	,format('Background knowledge: ~w ~w~n', [K,BK])
	,format('Metarules:            ~w ~w ~n', [N,MS]).




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



%!	experiment_data(+Targets,-Positive,-Negative,-BK,-Metarules) is
%!	det.
%
%	Collect experiment file data for one or more learning Targets.
%
%	Targets is either a single predicate indicator, or a list of
%	predicate indicators of the predicates to be learned.
%
%	experiment_data/5 expects an experiment file to be loaded into
%	memory and will fail without warning otherwise.
%	initialise_experiment/0 should be called before it, and
%	cleanup_experiment/0 after it if cleanup is required between
%	experiments.
%
experiment_data(Ts,_,_,_,_):-
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
experiment_data(T,_,_,_,_):-
% A single learning target's predicate indicator must be ground.
	\+ is_list(T)
	,learning_targets(Ts)
	,\+ memberchk(T,Ts)
	,throw('Unknown learning target':T).
experiment_data(T,Pos,Neg,BK,MS):-
	signed_examples(positive,experiment_file,T,Pos_)
	,signed_examples(negative,experiment_file,T,Neg_)
	,maplist(list_to_set,[Pos_,Neg_],[Pos,Neg])
	,bk_or_metarules(background_knowledge,experiment_file,T,BK)
	,bk_or_metarules(metarules,experiment_file,T,MS_)
	,(   (MS_ == [all]
	     ;	 memberchk(all,MS_)
	     )
	 ->  configuration_metarules(MS)
	 ;   MS = MS_
	 ).


%!	signed_examples(+Sign,+Module,+Targets,-Examples) is det.
%
%	Collect positive or negative Examples of one or more Targets.
%
%	Sign is one of [positive,negative] denoting the kind of
%	examples to collect. Module is the module name of the current
%	experiment file. Targets is either a list of learning targets'
%	symbols and arities as F/A predicate indicators, or a single
%	predicate indicator.
%
%	Examples is a list of examples of all the learning Targets. If
%	Sign is "positive", Examples is a list of positive examples
%	(ground unit clauses). If Sign is "negative", Examples is a list
%	of negative examples (ground unit clauses prefixed with ":-").
%
signed_examples(S,M,Ts,Es):-
% Ts is a list of learning targets.
	is_list(Ts)
	,!
	,atom_concat(S,'_example',F)
	,C =.. [F,T,Ep]
	,findall(Ep_
		,(member(T,Ts)
		 ,M:C
		 ,signed_example(S,Ep,Ep_)
		 )
		,Es_)
	,flatten(Es_,Es).
signed_examples(S,M,T,Es):-
% T is a single learning target.
	atom_concat(S,'_example',F)
	,C =.. [F,T,Ep]
	,findall(Ep_
		,(M:C
		 ,signed_example(S,Ep,Ep_)
		 )
		,Es).


%!	signed_example(+Example,-Signed) is nondet.
%
%	Ensure an Example is Signed if nessary.
%
%	Negative examples declared in an experiment file as ground unit
%	clauses or sets of literals must be prefixed with ":-" (so that
%	they are properly definite goals).
%
%	Negative examples can also be definite clauses with a head
%	literal, in which case it's not necessary to change them.
%
%	Positive examples don't need to be signed.
%
signed_example(_,H:-B,H:-B):-
	!.
signed_example(positive,E,E).
signed_example(negative,E,:-E).


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
	metagol_configuration:experiment_file(P,_M)
	,edit(P).
