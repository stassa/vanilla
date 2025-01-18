:-module(louise, [learn/1
		 ,learn/2
		 ,learn/5
		 ]).

:-use_module(src(vanilla)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(lib(louise/louise_auxiliaries)).
:-use_module(lib(louise/program_reduction/program_reduction)).
:-use_module(lib(louise/louise_configuration)).

/** <module> Meta-Interpretive Learning by Top program construction and reduction.

*/

%!	learn(+Targets) is det.
%
%	Learn a deafinition of one or more learning Targets.
%
learn(Ts):-
	learn(Ts,Ps)
	,print_clauses(Ps).



%!	learn(+Targets,-Definition) is det.
%
%	Learn a definition of one or more learning Targets.
%
learn(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn/2: non-ground target symbol!')
	;   fail
	).
learn(Ts,Ps):-
	experiment_data(Ts,Pos,Neg,BK,MS)
	,learn(Pos,Neg,BK,MS,Ps).



%!	learn(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn a Progam from a MIL problem.
%
learn([],_Neg,_BK,_MS,_Ts):-
	throw('learn/5: No positive examples found. Cannot train.').
learn(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('learn/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('learn/5: unbound negative examples list!')
	;   var(BK)
	->  throw('learn/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn/5: unbound metarule IDs list!')
	;   fail
	).
learn(Pos,Neg,BK,MS,Ps):-
	debug(learn,'Encapsulating problem...',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(learn,'Constructing Top program...',[])
	,top_program(Pos_,Neg_,BK_,MS_,Ms)
	,debug(learn,'Reducing Top program...',[])
	,reduced_top_program(Pos_,BK_,MS_,Ms,Rs)
	,examples_targets(Pos,Ss)
	,debug(learn,'Excapsulating hypothesis...',[])
	,excapsulated_clauses(Ss,Rs,Ps).



%!	top_program(+Pos,+Neg,+BK,+Metarules,-Top) is det.
%
%	Construct the Top program for a MIL problem.
%
%	Clauses are selected according to the configuration setting
%	theorem_prover/1.
%
%	If theorem_prover/1 is set to "resolution", Top program
%	construction is performed in a top-down manner using SLD
%	resolution to decide entailment, which is faster (because it
%	hands off to the Prolog interpreter), but not guaranteed to
%	terminate (for example, it may go infinite given the
%	left-recursive nature of encapsulated metarules).
%
%	If the value of theorem_prover/1 is "tp", Top program
%	construction is performed in a bottom-up manner, using a TP
%	operator. This is slower (because it's implemented in Prolog)
%	but it's guaranteed to terminate. Note also that the TP operator
%	only works for datalog definite programs.
%
%	@bug Top program specialisation using the TP operator is still a
%	work in progress and may not fully eliminate too-general
%	metasubstitutions.
%
top_program(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('top_program/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('top_program/5: unbound negative examples list!')
	;   var(BK)
	->  throw('top_program/5: unbound background symbols list!')
	;   var(MS)
	->  throw('top_program/5: unbound metarule IDs list!')
	;   fail
	).
top_program(Pos,Neg,BK,MS,Ts):-
% Uses the Prolog engine and avoids using the dynamic db too much.
	louise_configuration:clause_limit(K)
	,(   K =< 1
	 ->  Bs = [Pos,BK]
	 ;   Bs = [BK]
	 )
	,S = (write_problem(user,Bs,Refs)
	     ,refresh_tables(untable)
	     ,refresh_tables(table)
	     )
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,generalise(Pos,MS,Ts_)
	     ,debug_clauses(top_program,'Generalised Top program',Ts_)
	     ,specialise(Ts_,MS,Neg,Ts)
	     ,debug_clauses(top_program,'Specialised Top program',Ts)
	     )
	,C = (erase_program_clauses(Refs)
	     ,refresh_tables(untable)
	     )
	,setup_call_cleanup(S,G,C)
	% Fail if Top Program is empty.
	,Ts \= []
	,!.
top_program(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
% This is meant to send a clear message that learning failed.
	debug(top_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).



%!	generalise(+Positive,+Metarules,-Generalised) is det.
%
%	Generalisation step of Top program construction.
%
%	Generalises a set of Positive examples by finding each
%	metasubstitution of a metarule that entails a positive example.
%
%	Positive is a set of positive examples of a learning target.
%
%	Metarules is a set of expanded metarules.
%
%	The form of Generalised depends on the configuration option
%	clause_limit/1.
%
%	If clause_limit(0) is set, Generalised is a set of key-value
%	pairs where the keys are ground metasubstitution atoms and the
%	values are a copy, with free variables, of the encapsulated head
%	and body literals of the metarule corresponding to the
%	metasubsitution.
%
%	If clause_limit(K) is set, and K > 0, Generalised is a list _of
%	lists_ of key-value pairs of ground metasubstitution atoms and
%	their corresponding metarules.
%
%	The key-value pairs of metarules returned by generalise/3 can be
%	passed to applied_metarules/3 to apply the metasubstitutions to
%	their metarules to obtain first-order clauses.
%
%	@tbd Give examples of the key-value-pairs of ground
%	metasubstitutions and metarules returned by generalise/3.
%
generalise(Pos,MS,Ss_Pos):-
% Hands proofs to Vanilla inductive meta-interpreter.
	louise_configuration:clause_limit(K)
	% Used to name invented predicates apart in rename_invented/3.
	,reset_gensym('_')
	,findall(Subs
		,(member(Ep,Pos)
		 ,debug_clauses(examples,'Positive example:',Ep)
		 ,metasubstitutions(Ep,K,MS,Subs)
		 ,forall(member(Sub-_M,Subs)
			,constraints(Sub)
			)
		 ,debug_clauses(generalise,'Passed metasub constraints:',[Subs])
		 )
		,Ss_Pos_)
	,predsort(unifiable_compare,Ss_Pos_,Ss_Pos)
	,debug_length(generalise,'Derived ~w sub-hypotheses (sorted)',Ss_Pos).



%!	specialise(+Generalised,+Metarules,+Negatives,-Specialised) is
%!	det.
%
%	Specialisation step of Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
%	Unlike the original, one-clause TPC version this one specialises
%	sub-hypotheses derived by generalise/3.
%
specialise(Ss_Pos,_MS,[],Ss_Pos):-
	!.
specialise(Ss_Pos,MS,Neg,Ss_Neg):-
	louise_configuration:clause_limit(K)
	,findall(Subs
	       ,(member(Subs,Ss_Pos)
		,findall(Sub
			,member(Sub-_M,Subs)
			,Subs_)
		,debug_clauses(metasubstitutions,'Ground metasubstitutions:',[Subs_])
		,\+((member(En,Neg)
		    ,debug_clauses(examples,'Negative example:',En)
		    ,once(metasubstitutions(En,K,MS,Subs_))
		    ,debug_clauses(examples,'Proved negative example:',En)
		    )
		   )
		)
	       ,Ss_Neg).



%!	metasubstitutions(+Example,+Limit,+Metarules,-Metasubstitutions)
%!	is nondet.
%
%	Derive all possible Metasubstitutions entailing an Example.
%
%	Limit is the clause limit specified in the configuration option
%	clause_limit/1.
%
metasubstitutions(:-En,K,MS,Subs):-
	 !
	,signature(En,Ss)
	,debug(signature,'Signature: ~w',[Ss])
	,S = setup_negatives(Fs,T,U)
	,G = prove(En,K,MS,Ss,Subs,Subs)
	,C = cleanup_negatives(Fs,T,U)
	,setup_call_cleanup(S,G,C)
	,debug(metasubstitutions,'Proved Example: ~w',[:-En])
	,debug_clauses(metasubstitutions,'With Metasubs:',[Subs]).
metasubstitutions(Ep,K,MS,Subs):-
	signature(Ep,Ss)
	,debug(signature,'Signature: ~w',[Ss])
        ,vanilla:prove(Ep,K,MS,Ss,[],Subs_)
	,debug(metasubstitutions,'Proved Example: ~w',[Ep])
	,Subs_ \= []
	,sort(Subs_,Subs_s)
	,debug_clauses(metasubstitutions,'Proved Metasubs:',[Subs_s])
	,gensym('_',GS)
	,findall(Sub_-M
		,(member(Sub,Subs_s)
		 ,rename_invented(Sub,GS,Sub_)
		 ,metasub_metarule(Sub,MS,M)
		 )
		,Subs).


%!	setup_negatives(-Fetch,-Table,-Untable) is det.
%
%       Setup tabling to test a hypothesis with negative examples.
%
%	Fetch, Table and Untable are the current values of the
%	configuration options fetch_clauses/1, table_meta_interpreter/1
%	and untable_meta_interpreter/1, respectively.
%
%	This predicate ensures tabling is set for prove/7 in vanilla.pl
%	before testing a (sub) hypothesis against the negative examples.
%
%	When we test with the negative examples, prove/7 needs to
%	resolve with clauses in the hypothesis (otherwise it can't test
%	the hypothesis, duh) but not in the second-order background
%	knowledge. That means that fetch_clauses/1 must be set to
%	[builtins, bk, metarules].
%
%	Resolving with the hypothesis allows unconstrained recursion and
%	this may cause it to go into an infinite recursion. This is
%	particularly the case if the hypothesis was learned _without_
%	'hypothesis' in the list of fetch_clause/1 options.
%
%	To avoid such infinite recursions we must set
%	table_meta_interpreter/1 and untable_meta_interpreter/1 so as to
%	turn tabling on through the interface of refresh_tables/1. Then
%	we can test the hypothesis in relative certainty it won't just
%	"go infinite".
%
%	The current values of all three configuration options are bound
%	to the output so they can be re-set after testing with the
%	negative examples.
%
setup_negatives(Fs,T,U):-
	configuration:fetch_clauses(Fs)
	,configuration:table_meta_interpreter(T)
	,configuration:untable_meta_interpreter(U)
	,set_configuration_option(fetch_clauses, [[builtins,bk,hypothesis]])
	,set_configuration_option(table_meta_interpreter, [true])
	,refresh_tables(untable)
	,refresh_tables(table).


%!	cleanup_negatives(+Fetch,+Table,+Untable) is det.
%
%	Reset tabling options after testing with negative examples.
%
%	Sets the three configuration options manipulated by
%	setup_negatives/3 to their original values, as read from the
%	configuration. See setup_negatives/3 for details.
%
cleanup_negatives(Fs,T,U):-
	set_configuration_option(fetch_clauses, [[Fs]])
	,set_configuration_option(table_meta_interpreter, [T])
	,set_configuration_option(untable_meta_interpreter, [U])
	,refresh_tables(untable)
	,refresh_tables(table).


%!	signature(+Example,-Signature) is det.
%
%	Return the predicate Signature for one example's predicate.
%
signature(L,[T|Ss]):-
        configuration:encapsulation_predicate(E)
        ,louise_configuration:max_invented(N)
        ,findall(S
                ,invented_symbol(N,S)
                ,Ss)
        ,L =.. [E,T|_].


%!	rename_invented(+Metasub,+Gensym,-Renamed) is det.
%
%	Ensure invented predicates in a Metasub are uniquely named.
%
%	Metasub is a ground metasubstitution atom as returned by
%	prove/7.
%
%	Gensym is a an atom with prefix _ and a numeric suffix generated
%	by gensym('_',Gensym).
%
%	Renamed is the metasubstitution atom in Metasub with all
%	invented symbols renamed by appending Gensym to each of them as
%	a new suffix.
%
%	gensym/2 is reset in generalise/4 and called once before calling
%	this predicate on each new list of metasubstitutions returned by
%	prove/7, so each new set of metasubstitutions will have fresh
%	suffixes but each invented symbol in the set will have the same
%	suffix throughout the set.
%
%	The purpose of this renaming is to name invented predicate
%	symbols apart in order to avoid them getting all mixed up in the
%	Top Program, which can lead to overgeneralisation.
%
%	For example, consider the Top Program learned by Louise from
%	data/examples/anbn.pl. Without renaming invented predicates
%	apart that Top Program would look like this:
%	==
%	?- louise:learn(s/2).
%	inv_1(A,B):-a(A,C),s(C,B).
%	inv_1(A,B):-s(A,C),b(C,B).
%	s(A,B):-a(A,C),b(C,B).
%	s(A,B):-a(A,C),inv_1(C,B).
%	s(A,B):-inv_1(A,C),b(C,B).
%	true.
%	==
%
%	That Top Program includes two clauses of one invented predicate,
%	inv_1/2. In truth these two clauses belong to two invented
%	predicates that were constructed from different branches of the
%	inductive proof performed by prove/7 on backtracking. But that
%	distinction is lost if we name them all the same.
%
%	This predicate ensures that invented predicates constructed in
%	different branches of a prove/7 proof are named appart, so that
%	they are different predicates. For example, this is how the Top
%	Program for anbn looks like when this predicate is used:
%	==
%	?- louise:learn(s/2).
%	inv_1_16(A,B):-a(A,C),s(C,B).
%	inv_1_17(A,B):-s(A,C),b(C,B).
%	s(A,B):-a(A,C),b(C,B).
%	s(A,B):-a(A,C),inv_1_17(C,B).
%	s(A,B):-inv_1_16(A,C),b(C,B).
%	true.
%	==
%
%	The new Top Program includes the definitions of two different
%	invented predicates, which now cannot call each other (unless
%	explicitly defined to do so) and so cannot over-generalise by
%	getting tangled up in each other, unlike in the first example
%	with the single inv_1/2 predicate above.
%
rename_invented(Sub,_GS,Sub):-
	max_invented(0)
	,!.
rename_invented(Sub,GS,Sub_):-
	configuration:encapsulation_predicate(M)
	,Sub =.. [M|Ss]
	,findall(S_
		,(member(S,Ss)
		 ,(   invented_symbol_(_I,S)
		  ->  atom_concat(S,GS,S_)
		  ;   S_ = S
		  )
		 )
		,Ss_)
	,Sub_ =.. [M|Ss_].


%!	invented_symbol_(?Index,+Symbol) is det.
%
%	True when Symbol is an invented predicate symbol.
%
%	@tbd Add as second clause to auxiliaries invented_symbol/2 to
%	avoid using between/3 for checks rather than generation.
%
invented_symbol_(I,S):-
	configuration:invented_symbol_prefix(F)
	,atom_concat(F,A,S)
	,atom_number(A,I).


%!	metasub_metarule(+Sub,+Metarules,-Metarule) is det.
%
%	Retrieve an expanded metarule matching a metasub atom.
%
%	Sub is an encapsulated metasubtitution atom.
%
%	Metarules is the list of expanded metarules.
%
%	Metarules is an expanded metarule in Metarules with a metarule
%	Id matching the metarule Id of Sub.
%
%	Used by metasubstitutions/3 to retrieve the encapsulated
%	metarule matching the metarule id of a ground metasubstitution
%	atom without binding variables in the encapsulated metarule.
%
metasub_metarule(Sub,MS,Sub_:-M):-
	configuration:encapsulation_predicate(E)
        ,Sub =.. [E,Id|As]
	,length(As,N)
	,length(As_,N)
	,Sub_ =.. [E,Id|As_]
	,free_member(Sub_:-M,MS).



%!	reduced_top_program(+Pos,+BK,+Metarules,+Program,-Reduced)
%!	is det.
%
%	Reduce the Top Program.
%
%	Clauses are selected according to the value of the configuration
%	option recursive_reduction/1. If this is set to true, the Top
%	program is reduced recursively, by passing the output of each
%	reduction step to the next, as input. If recursive_reduction/1
%	is set to false a single reduction step is performed.
%
%	Recursive reduction is useful when the Top program is large, or
%	recursive, and a large number of resolution steps are required
%	to reduce it effectively. In such cases, recursive reduction can
%	result in a stronger reduction of the Top program (i.e. result
%	in fewer redundant clauses in the learned hypothesis) in a
%	shorter amount of time, without increasing the number of
%	resolution steps in the program reduction meta-interpreter.
%
reduced_top_program(_Pos,_BK,MS,Ps,Ps_):-
	louise_configuration:reduction(none)
	,!
	,flatten_top_program_and_apply_metasubs(Ps,MS,Ps_)
	,debug(reduction,'reduction/1 is "none". The Top program is not reduced.',[]).
reduced_top_program(_Pos,_BK,MS,Ps,Rs):-
	louise_configuration:reduction(subhypotheses)
	,!
	,debug(reduction,'Splitting Top Program to subhypotheses...',[])
	,member(Ps_i,Ps)
	,applied_metarules(Ps_i,MS,Rs)
	,debug_clauses(reduction,'Applied metasubstitutions to subhypothesis:',Rs).

reduced_top_program(Pos,BK,MS,Ps,Rs):-
	louise_configuration:recursive_reduction(true)
	,!
	,flatten_top_program_and_apply_metasubs(Ps,MS,Ps_a)
	,flatten([Pos,BK,Ps_a,MS],Fs)
	,list_to_set(Fs, Fs_)
	,debug(reduction,'Reducing Top program recursively...',[])
	,program_reduction(Fs_,Rs_,_)
	,length(Fs_,M)
	,length(Rs_,N)
	,debug(reduction,'Initial reduction: ~w to ~w',[M,N])
	,reduced_top_program_(N,Rs_,BK,MS,Rs)
	,debug_clauses(reduction,'Reduced Top program:',Rs)
	% program_reduction module leaves behind garbage
	% in program module. Why?
	,cleanup_experiment.
reduced_top_program(Pos,BK,MS,Ps,Rs):-
	louise_configuration:recursive_reduction(false)
	,flatten_top_program_and_apply_metasubs(Ps,MS,Ps_a)
	,flatten([Pos,BK,Ps_a,MS],Fs)
	,list_to_set(Fs,Fs_)
	,debug(reduction,'Reducing Top program by Plotkin\'s algorithm...',[])
	,program_reduction(Fs_,Rs,_)
	,debug_clauses(reduction,'Reduced Top program:',Rs)
	,cleanup_experiment.


%!	flatten_top_program_and_apply_metasubs(+Top,+MS,-Reduced) is
%!	det.
%
%	What it says on the tin.
%
%	Helper to take the union of clauses in the Top Program by
%	flattening, sort it, and apply metasubstitution, to avoid
%	repetition of the same three calls in clauses of
%	reduced_top_program/5.
%
flatten_top_program_and_apply_metasubs(Ps,MS,Ps_a):-
	flatten(Ps,Ps_f)
	,sort(1,@<,Ps_f,Ps_s)
	,applied_metarules(Ps_s,MS,Ps_a)
	,debug_clauses(top_program,'Applied metasubstitutions:',Ps_a).


%!	reduced_top_program_(+N,+Prog,+BK,+Metarules,-Reduced) is
%!	det.
%
%	Business end of reduced_top_program/6
%
%	Recursively reduces the Top Program, by feeding back the result
%	of each call to program_reduction/2 to itself, a process known
%	as "doing feedbacksies".
%
reduced_top_program_(N,Ps,BK,MS,Bind):-
	program_reduction(Ps,Rs,_)
	,length(Rs, M)
	,debug(reduction,'New reduction: ~w to ~w',[N,M])
	,M < N
	,!
	,reduced_top_program_(M,Rs,BK,MS,Bind).
reduced_top_program_(_,Rs,_BK,_MS,Rs):-
	length(Rs, N)
	,debug(reduction,'Final reduction: ~w',[N]).
