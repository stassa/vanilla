:-module(poker, [learn/1
		,learn/4
		,learn/6
		]).

:-use_module(src(vanilla)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/program_reduction/program_reduction)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(lib(poker/sampling/sampling)).

/** <module> Self-supervised Meta-Interpretive Learning.

Poker is a version of Louise that learns from an initial set of atoms
it assumes are positive examples of one or more target predicates. A
single assumed-positive example is enough to begin learning.

During learning, a hypothesis is derived and used to generate more
examples and assign truth values to them, a process that we call
"labelling" (as in "examples are labelled as positive or negative").

This labelling is achieved by initially assuming that each generated
example is a negative example of one of the predicates of the atoms in
the initial, assumed-positive, set of atoms. These assumed-negative
examples are first used to specialise the hypothesis so far, then if the
hypothesis, specialised by the assumed-negative examples, is found to no
longer cover some of the assumed-positive examples, the assumed-negative
examples are labelled as positive and added to the set of
assumed-positive examples. Otherwise, the assumed-negative examples are
labelled as negative.

The sets of positive and negative examples derived during the proof are
returned at the end.


*/


%!	learn(+Targets) is det.
%
%	Learn a deafinition of one or more learning Targets.
%
learn(Ts):-
	learn(Ts,Pos,Neg,Ps)
	,print_clauses('Hypothesis:',Ps)
	,print_clauses('Positive examples:',Pos)
	,print_clauses('Negative examples:',Neg).



%!	learn(+Targets,-Definition) is det.
%
%	Learn a definition of one or more learning Targets.
%
learn(Ts,_Pos,_Neg,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn/2: non-ground target symbol(s)!')
	;   fail
	).
learn(T,Pos,Neg,Ps):-
	T = _F/_A
	,!
	,experiment_data(T,As,BK,MS)
	,learn(As,BK,MS,Pos,Neg,Ps).
learn([F/A|Ts],Pos,Neg,Ps):-
	!
	,experiment_data([F/A|Ts],As,BK,MS)
	,learn(As,BK,MS,Pos,Neg,Ps).
learn([A|As],Pos,Neg,Ps):-
	!
        ,compound(A)
	,functor(A,F,N)
	,experiment_data(F/N,_,BK,MS)
	,learn([A|As],BK,MS,Pos,Neg,Ps).
learn(A,Pos,Neg,Ps):-
	!
        ,compound(A)
	,functor(A,F,N)
	,experiment_data(F/N,_,BK,MS)
	,learn([A],BK,MS,Pos,Neg,Ps).



%!	learn(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn a Progam from a MIL problem.
%
learn([],_BK,_MS,_Pos,_Neg,_Ts):-
	throw('learn/5: No example atoms found. Cannot train.').
learn(As,BK,MS,_Pos,_Neg,_Ts):-
	(   var(As)
	->  throw('learn/5: unbound example atoms list!')
	;   var(BK)
	->  throw('learn/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn/5: unbound metarule IDs list!')
	;   fail
	).
learn(As,BK,MS,Pos,Neg,Ps):-
	debug(learn,'Encapsulating problem...',[])
	,encapsulated_problem(As,[],BK,MS,[As_,_,BK_,MS_])
	,debug(learn,'Constructing Top program...',[])
	,top_program(As_,BK_,MS_,Pos_,Neg_,Ms)
	,debug(learn,'Reducing Top program...',[])
	,reduced_top_program(As_,BK_,MS_,Ms,Rs)
	,examples_targets(As,Ss)
	,debug(learn,'Excapsulating hypothesis...',[])
	,excapsulated_clauses(Ss,Rs,Ps)
	,debug(learn,'Excapsulating labelled examples...',[])
	,excapsulated_clauses(Ss,Pos_,Pos)
	,findall(En
		,member(:-En,Neg_)
		,Neg_c)
	,excapsulated_clauses(Ss,Neg_c,Neg).


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
top_program(As,BK,MS,_Pos,_Neg,_Ts):-
	(   var(As)
	->  throw('top_program/5: unbound positive examples list!')
	;   var(BK)
	->  throw('top_program/5: unbound background symbols list!')
	;   var(MS)
	->  throw('top_program/5: unbound metarule IDs list!')
	;   fail
	).
top_program(As,BK,MS,Pos,Neg,Ts):-
% Uses the Prolog engine and avoids using the dynamic db too much.
	poker_configuration:clause_limit(K)
	,(   K =< 1
	 ->  Bs = [As,BK]
	 ;   Bs = [BK]
	 )
	,S = (write_problem(user,Bs,Refs)
	     ,refresh_tables(untable)
	     ,refresh_tables(table)
	     )
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,label(As,MS,K,Pos,Neg,Ts)
	     )
	,C = (erase_program_clauses(Refs)
	     ,refresh_tables(untable)
	     )
	,setup_call_cleanup(S,G,C)
	% Fail if Top Program is empty.
	,Ts \= []
	,!.
top_program(_As,_BK,_MS,[],[],[]):-
% If Top program construction fails return an empty program.
% This is meant to send a clear message that learning failed.
	debug(top_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).


%!	label(+Atoms,+Metarules,+Limit,-Positives,-Negatives,-Hypothsis)
%	is det.
%
%	Construct a Hypothesis and label a set of Negative examples.
%
%	Atoms is a set of atoms assumed to be positive examples of a
%	target predicate.
%
%	Metarules is a set of second-order definite clauses.
%
%	Limit is the value of the configuration option clause_limit/1.
%
%	Positives is a list of atoms labelled as positive according to
%	the Hypothesis learned by Atom.
%
%	Negatives is a list of atoms labelled as negative according to
%	the Hypothesis learned by Atom.
%
%	Hypothesis is a hypothesis learned by Atom, and with Negatives
%	as negative examples.
%
label(Ep,MS,K,Pos,Neg,Ps):-
	poker_configuration:greedy_generalisation(false)
	,poker_configuration:unlabelled_examples(N)
	,debug_clauses(label,'Initial Examples:',Ep)
	,generalise(Ep,MS,Subs_)
	,respecialise(Subs_,Ep,MS,Subs)
	,debug_length(label,'Constructed ~w initial sub-hypotheses.',Subs)
	,debug_clauses(label_full,'Initial hypothesis:',Subs)
	,generate(N,Ep,K,MS,Subs,As)
	,label(Ep,As,MS,K,Subs,Pos,[],Neg,Ps)
	,debug_length(label,'Kept ~w final sub-hypotheses.',Ps)
	,debug_clauses(label_full,'Final hypothesis:',Ps).
label(Ep,MS,K,Pos,Neg,Ps):-
        poker_configuration:greedy_generalisation(true)
	,poker_configuration:unlabelled_examples(N)
	,debug_clauses(label,'Initial Examples:',Ep)
	,generalise_greedy(Ep,Ep,K,MS,Subs_)
	,respecialise(Subs_,Ep,MS,Subs)
	,debug_length(label,'Constructed ~w initial sub-hypotheses.',Subs)
	,debug_clauses(label_full,'Initial hypothesis:',Subs)
	,generate(N,Ep,K,MS,Subs,As)
	,label(Ep,As,MS,K,Subs,Pos,[],Neg,Ps)
	,debug_length(label,'Kept ~w final sub-hypotheses.',Ps)
	,debug_clauses(label_full,'Final hypothesis:',Ps).


%!	generalise_greedy(+Es,+Cs,+K,+Metarules,-Subs) is det.
%
%	Gonstruct a single Top Program from one of the initial examples.
%
%	Es is the set of initial examples.
%
%	Cs is the set of initial examples in Es, kept unchanged through
%	execution.
%
%	K is the value of poker_configuration:clause_limit/1.
%
%	Metarules is the set of metarules for the learning problem.
%
%	Subs is a Top Program learned by one of the examples in Es.
%
%	This program walks through the list of examples in Es and
%	constructs a Top Program from each, with a call to genralise/3
%	(but not specialise/4). Then it tests this Top Program against
%	all the initial examples kept constant in Cs, with a call to
%	prove_all/6. If this succeeds, the Top Program constructed is
%	returned, as the initial hypothesis, to label/6. If the test
%	fails, the Top Prograam is discarded and the next example in Es
%	is tried, until there are no more examples remaining.
%
%	If no Top Program can be learned this way, generalise_greedy/5
%	returns an empty Top Program.
%
%	@tbd That's not great when it's not possible to create a
%	complete Top Program. Maybe make it so it's possible to return a
%	Top Program that allows some examples to not be covered?
%
generalise_greedy([],_Pos,_K,_MS,[]):-
	!
	,debug(generalise_greedy,'Greedy generalisation failed.',[]).
generalise_greedy([Ep|_Es],Pos,K,MS,Subs):-
	debug(generalise_greedy,'Greedy-generalising example ~w',[Ep])
	,generalise([Ep],MS,Subs)
	,prove_all(false,Pos,K,MS,[],Subs)
	,debug(generalise_greedy,'Proved all examples.',[])
	,debug_clauses(generalise_greedy_full,'With greedy hypothesis:',Subs)
	,!.
generalise_greedy([Ep|Es],Pos,K,MS,Subs):-
	debug(generalise_greedy,'Dropped example ~w',[Ep])
	,generalise_greedy(Es,Pos,K,MS,Subs).



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
	debug(generalise,'Generalising positive examples',[])
	,poker_configuration:clause_limit(K)
	% Used to name invented predicates apart in rename_invented/3.
	,reset_gensym('_')
	,findall(Subs
		,(member(Ep,Pos)
		 ,debug(examples,'Positive example: ~w',[Ep])
		 ,metasubstitutions(Ep,K,MS,Subs)
		 ,forall(member(Sub-_M,Subs)
			,constraints(Sub)
			)
		 ,debug_metasubs(generalise_full,'Passed metasub constraints:',Subs,Pos,MS)
		 )
		,Ss_Pos_)
	,debug_length(generalise,'Derived ~w sub-hypotheses (unsorted)',Ss_Pos_)
	,once( skolem_sort(Ss_Pos_,Ss_Pos_s) )
	,debug_length(generalise,'Derived ~w sub-hypotheses (sorted)',Ss_Pos_s)
	,rename_all_invented(Ss_Pos_s,Ss_Pos).


%!	respecialise(+Metasubs,+Pos,+MS,-Specialised) is det.
%
%	Strongly specialise the Top Program against positive examples.
%
%	Second step of specialisation that specialises the
%	already-specialised Top Program further by removing each
%	sub-hypothesis that does not entail all the positive examples.
%
%	This is useful when the Top Program contains many over-special
%	sub-hypotheses and only a few ones that are sufficiently general
%	to cover all the positive examples.
%
%	This specialisation operation is applied only if the
%	poker_configuration option respecialise/1 is set to "true".
%
respecialise(Ss_Neg,_,_MS,Ss_Neg):-
	poker_configuration:respecialise(false)
	,!.
respecialise(Ss_Neg,[E0|Pos],MS,Ss_Neg_):-
	poker_configuration:respecialise(true)
	,poker_configuration:clause_limit(K)
	,signature(E0,Ss)
	,debug_length(respecialise,'Respecialising ~w sub-hypotheses',Ss_Neg)
	,S = setup_negatives(Fs,T,U)
	,G = findall(Subs
		    ,(member(Subs, Ss_Neg)
		     ,findall(Sub
			     ,member(Sub-_M,Subs)
			     ,Subs_)
		     ,debug_clauses(respecialise_full,'Ground metasubstitutions:',Subs_)
		     ,forall(member(Ep,[E0|Pos])
			    ,(debug(examples,'Positive example: ~w',[Ep])
			     ,vanilla:prove(Ep,K,MS,Ss,Subs_,Subs_)
			     ,debug(examples,'Proved positive example: ~w',[Ep])
			     )
			    )
		     ,debug_clauses(respecialise_full,'Proved metasubstitutions:',Subs_)
		     )
		    ,Ss_Neg_)
	,C = cleanup_negatives(Fs,T,U)
	,setup_call_cleanup(S,G,C)
	,debug_length(respecialise,'Kept ~w sub-hypotheses',Ss_Neg_).


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
	,prove(En,K,MS,Ss,Subs,Subs)
	,debug(metasubstitutions,'Proved Example: ~w',[:-En])
	,debug_clauses(metasubstitutions_full,'With Metasubs:',[Subs]).
metasubstitutions(Ep,K,MS,Subs):-
	poker_configuration:strict_clause_limit(S)
	,poker_configuration:proof_samples(P)
	,signature(Ep,Ss)
	,debug(signature,'Signature: ~w',[Ss])
        ,G = prove_with_clause_limit(S,Ep,K,MS,Ss,Subs_)
	,goal_sample(P,poker,G,_)
	,debug(metasubstitutions,'Proved Example: ~w',[Ep])
	,debug_length(metasubstitutions,'Derived ~w Metasubs.',Subs_)
	,debug_clauses(metasubstitutions_full,'Proved Metasubs:',[Subs_])
	,findall(Sub-M
		,(member(Sub,Subs_)
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
	,set_configuration_option(untable_meta_interpreter, [true])
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
	set_configuration_option(fetch_clauses, [Fs])
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
        ,poker_configuration:max_invented(N)
        ,findall(S
                ,invented_symbol(N,S)
                ,Ss)
        ,L =.. [E,T|_].


%!	prove_with_clause_limit(+Strict,+Ep,+Limit,+MS,+Sig,-Subs) is
%!	nondet.
%
%	Prove an example with a limit on the size of a hypothesis.
%
%	Wrapper around prove/7 to call it according to the value of
%	strict_clause_limit/1.
%
%	Strict is a boolean that determines whether the clause limit in
%	Limit is to be treatred as an exact size or an upper bound.
%	Strict inherits its value from the poker configuration option
%	strict_clause_limit/1.
%
%	Limit is an integer, either an exact length of Subs or an upper
%	bound, depending on Strict.
%
%	Ep is a positive example, MS is a set of metarules and Sig is
%	the program signature, as returned by signature/1. Ep MS and Sig
%	are passed to prove/7.
%
%	Subs is a list of metasubstitutions derived by prove/7 from Ep.
%
%	Limit is measured on Subs only once Subs is sorted. This is to
%	remove duplicaters that may be included when fetch_clauses/1
%	does not include "hypothesis", in which case multiple copies of
%	the same metasubstitution atom indicate recursion over the same
%	instances of metarules.
%
%	Subs _is_ sorted when Strict is "false".
%
prove_with_clause_limit(true,Ep,K,MS,Ss,Subs_s):-
	!
	,vanilla:prove(Ep,K,MS,Ss,[],Subs)
	,sort(Subs,Subs_s)
	,length(Subs_s,K)
        ,debug_length(prove_limit,'Derived ~w clause hypothesis.',Subs_s).
prove_with_clause_limit(false,Ep,K,MS,Ss,Subs_s):-
	vanilla:prove(Ep,K,MS,Ss,[],Subs)
	,sort(Subs,Subs_s).



%!	rename_all_invented(+Metasubs,-Renamed) is det.
%
%	Name apart invented predicates in a list of Metasubs.
%
%	Metasubs is a list of metasubstitutions derived by generalise/4,
%	possibly with invented predicate symbols.
%
%	Renamed is the list of lists of metasubstitutions in Metasubs
%	with their invented predicates renamed by appending a gensym'd
%	atom to each copy of an invented symbol in the same list of
%	metasubs as a new suffix.
%
%	Note that invented predicates are renamed in this way only if
%	reduction(subhypotheses) is selected.
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
rename_all_invented(Subs,Subs_r):-
	poker_configuration:gestalt(false)
	,!
	,findall(Ss_r
		,(member(Ss,Subs)
		 ,gensym('_',GS)
		 ,findall(Sub_r-M
			 ,(member(Sub_i-M,Ss)
			  ,rename_invented(Sub_i,GS,Sub_r)
			  )
			 ,Ss_r)
		 )
		,Subs_r).
rename_all_invented(Subs,Subs).


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
%	Note that the example Top Programs above are learned with
%	Louise, not Poker, but the same principle applies to Poker also.
%	This predicate is copied from Louise where it was originally
%	implemented.
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


%!	generate(+N,+Atoms,+Limit,+MS,+Sig,+Subs,-Neg) is det.
%
%	Generate a set assumed negative examples of a target predicate.
%
%	N is the maximum number of Atoms to generate.
%
%	Atoms is a set of atoms assumed to be positive examples of a
%	target predicate.
%
%	Limit is the value of the configuration option clause_limit/1.
%
%	MS is a set of second-order definite clauses.
%
%	Subs is a set of metasubstitutions, an initial hypothesis
%	derived from Atoms.
%
%	Neg is a set of atoms of the same predicates as Atoms, initially
%	assumed to be negative examples of those predicates.
%
%	If Atoms include arguments that are lists the generation can go
%	infinite. In that case the user must define safe_example/1
%	(dynamic and multifile in module experiment_file) to limit the
%	length of generated list arguments. It's up to the user to not
%	abuse this mechanism to generate specific examples.
%
generate(N,[Ep|Pos],K,MS,Subs,Neg):-
	debug(generate,'Generating new atoms...',[])
	,poker_configuration:unlabelled_examples_order(O)
	,S = setup_negatives(Fs,T,U)
	,(   poker_configuration:safe_example(_)
	 ->  G = generate(list_safe,N,[Ep|Pos],K,MS,Subs,Neg_)
	 ;   G = generate(atomic,N,[Ep|Pos],K,MS,Subs,Neg_)
	 )
	,C = cleanup_negatives(Fs,T,U)
	,setup_call_cleanup(S,G,C)
	,(   O == deterministic
	 ->  Neg = Neg_
	 ;   O == random
	 ->  random_permutation(Neg_,Neg)
	 )
	,debug_length(generate,'Generated ~w new atoms.',Neg)
	,debug_clauses(generate_full,'Generated new atoms:',Neg).


%!	generate(+How,+N,+Atoms,+Limit,+MS,+Sig,+Subs,-Neg) is det.
%
%	Business end of generate/6.
%
%	Clauses are selected according to the value of How, which can be
%	one of: [list_safe, atomic].
%
%	If How is "list_safe" it means there is a definition of
%	safe_example/1 in the current experiment file. In that case,
%	that definintion will be used to safely generate examples with
%	list arguments, without generating infinite list arguments.
%
%	If How is "atomic" then there is no need to worry about list
%	arguments or generating infinite lists and generation will not
%	take safe_example/1 into account.
%
generate(list_safe,N,[Ep|Pos],K,MS,Subs,Neg_s):-
        !
	,flatten(Subs,Subs_f)
        ,setof(Sub
               ,M^Subs_f^member(Sub-M,Subs_f)
               ,Subs_)
        ,findall(:-En
		,(G = ( poker_configuration:safe_example(En)
		      ,prove(En,K,MS,[],Subs_,Subs_)
		      )
		 ,limit(N,G)
		 ,\+ memberchk(En,[Ep|Pos])
		 ,debug_clauses(generate_full,'Generated new atom:',:-En)
		 )
		,Neg)
	,sort(Neg, Neg_s).
generate(atomic,N,[Ep|Pos],K,MS,Subs,Neg_s):-
        configuration:encapsulation_predicate(Enc)
        ,Ep =.. [Enc,S|_Args0]
        ,functor(Ep,F,A)
        ,functor(En,F,A)
        ,En =.. [Enc,S|_Args1]
	,flatten(Subs,Subs_f)
        ,setof(Sub
               ,M^Subs_f^member(Sub-M,Subs_f)
               ,Subs_)
        ,findall(:-En
		,(G = prove(En,K,MS,[],Subs_,Subs_)
		 ,limit(N,G)
		 ,\+ memberchk(En,[Ep|Pos])
		 ,debug_clauses(generate_full,'Generated new atom:',:-En)
		 )
		,Neg)
	,sort(Neg, Neg_s).


%!	label(+Pos,+Neg,+MS,+K,+Prog,+Acc1,+Acc2,-Neg,-Ps) is det.
%
%	Business end of label/5
%
%	Pos is a set of assumed-positive examples of one or more target
%	predicates.
%
%	Neg is a list of assumed-negative examples of one or more
%	target predicates.
%
%	MS is a set of second-order definite clauses in the background
%	theory.
%
%	K is the value of the configuration option clause_limit/1.
%
%	Prog is a hypothesis learned from the examples in Pos
%	specialised by the examples in Neg, and used to derive more
%	positive examples in Pos and negative examples in Neg.
%
%	Acc1 is the accumulator of positive examples derived during the
%	execution of this predicate.
%
%	Acc2 is the accumulator of negative examples derived during etc.
%
%	Ps is the final hypothesis, specialised by the assumed
%	negative examples in Neg.
%
label(Pos,[],_MS,_K,Ps,Pos,Neg,Neg,Ps):-
        debug_clauses(label_results,'Final hypothesis:',Ps)
        ,debug_clauses(label_results,'Positive examples:',Pos)
        ,debug_clauses(label_results,'Negative examples:',Neg)
	,!.
label(Pos,[En|Neg],MS,K,Subs,Pos_Bind,Neg_Acc,Neg_Bind,Ps):-
        debug_length(label,'Specialising ~w sub-hypotheses.',Subs)
	,debug_clauses(label_full,'Specialising hypothesis:',Subs)
	,debug(label,'With negative example: ~w',[En])
	,specialise(Subs,MS,[En],Subs_S)
        ,debug_clauses(label_full,'Specialised hypothesis:',Subs_S)
        ,prove_all(false,Pos,K,MS,[],Subs_S)
        ,!
        ,debug(label,'Keeping negative example: ~w',[En])
        ,label(Pos,Neg,MS,K,Subs_S,Pos_Bind,[En|Neg_Acc],Neg_Bind,Ps).
label(Pos,[:-Ep|Neg],MS,K,Subs,Pos_Bind,Neg_Acc,Neg_Bind,Ps):-
        debug_clauses(label_full,'Keeping hypothesis:',Subs)
        ,debug_length(label,'Keeping ~w sub-hypotheses:',Subs)
        ,debug(label,'Keeping as positive example: ~w',[Ep])
	,label([Ep|Pos],Neg,MS,K,Subs,Pos_Bind,Neg_Acc,Neg_Bind,Ps).


%!	prove_all(+Flatten,+Pos,+K,+MS,+Sig,+Subs) is det.
%
%	Prove a set of atoms with a set of metasubstitutions.
%
%	Flatten is a boolean that determines whether the Top Program is
%	flattened into one big union of sub-hypotheses, or not, before
%	proving the positive examples. Clauses of prove_all/6 are
%	selected according to the value of Flatten.
%
%	Pos is a set of atoms assumed to be positive examples of a
%	target predicate.
%
%	K is the value of the configuration option clause_limit/1.
%
%	MS is a set of second-order definite clauses.
%
%	Sig is the predicate signature of Atoms.
%
%	Subs is a set of metasubstitutions, an initial hypothesis
%	derived from Atoms.
%
%	This predicate proves all of the atoms in Pos, with the
%	metasubstitutions in Subs, and fails if it can't do that, Dave.
%
prove_all(_F,_Pos,_K,_MS,_Ss,[]):-
        debug(prove_all,'Empty hypothesis. Proof fails',[])
	,!
	,fail.
prove_all(true,Pos,K,MS,Ss,Subs):-
	debug_clauses(prove_all,'Re-proving positive examples:',Pos)
	,flatten(Subs,Subs_f)
	,setof(Sub
	      ,M^Subs_f^member(Sub-M,Subs_f)
	      ,Subs_)
	,debug_clauses(prove_all_full,'With current metasubs: ',Subs_)
	,S = setup_negatives(Fs,T,U)
	,G = forall(member(Ep,Pos)
		   ,prove(Ep,K,MS,Ss,Subs_,Subs_)
		   )
	,C = cleanup_negatives(Fs,T,U)
	,setup_call_cleanup(S,G,C)
	,debug(prove_all,'Proof succeeded',[]).
prove_all(false,Pos,K,MS,Ss,Subs):-
	debug_clauses(prove_all,'Re-proving positive examples:',Pos)
	,S = setup_negatives(Fs,T,U)
	,G = forall(member(Sub,Subs)
		   ,(setof(Sub_
			  ,M^Sub^member(Sub_-M,Sub)
			  ,Subs_)
		    ,debug_clauses(prove_all_full,'With current metasubs: ',Subs_)
		    ,forall(member(Ep,Pos)
			   ,prove(Ep,K,MS,Ss,Subs_,Subs_)
			   )
		    )
		   )
	,C = cleanup_negatives(Fs,T,U)
	,setup_call_cleanup(S,G,C)
	,debug(prove_all,'Proof succeeded',[]).


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
	!
       ,debug(examples,'No negative examples. Can\'t specialise',[]).
specialise(Ss_Pos,MS,Neg,Ss_Neg):-
	debug_length(specialise,'Specialising with ~w negative examples.',Neg)
	,poker_configuration:clause_limit(K)
	,S = setup_negatives(Fs,T,U)
	,G = findall(Subs
		    ,(member(Subs,Ss_Pos)
		     ,findall(Sub
			     ,member(Sub-_M,Subs)
			     ,Subs_)
		     ,debug_clauses(specialise_full,'Ground metasubstitutions:',[Subs_])
		     ,\+((member(En,Neg)
			 ,debug(examples,'Negative example: ~w',[En])
			 ,once(metasubstitutions(En,K,MS,Subs_))
			 ,debug(examples,'Proved negative example: ~w',[En])
			 )
			)
		     )
		    ,Ss_Neg)
	,C = cleanup_negatives(Fs,T,U)
	,setup_call_cleanup(S,G,C).


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
reduced_top_program(_Pos,_BK,_MS,[],[]):-
	!
       ,debug(reduction,'Empty Top program. Nothing to reduce',[]).
reduced_top_program(_Pos,_BK,MS,Ps,Ps_):-
	poker_configuration:reduction(none)
	,!
	,flatten_top_program_and_apply_metasubs(Ps,MS,Ps_)
	,debug(reduction,'reduction/1 is "none". The Top program is not reduced.',[]).
reduced_top_program(_Pos,_BK,MS,Ps,Rs):-
	poker_configuration:reduction(subhypotheses)
	,!
	,debug(reduction,'Splitting Top Program to subhypotheses...',[])
	,member(Ps_i,Ps)
	,applied_metarules(Ps_i,MS,Rs)
	,debug_clauses(reduction,'Applied metasubstitutions to subhypothesis:',Rs).
reduced_top_program(Pos,BK,MS,Ps,Rs):-
	poker_configuration:recursive_reduction(true)
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
	poker_configuration:recursive_reduction(false)
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
flatten_top_program_and_apply_metasubs(Ps,MS,Ps_s):-
	flatten(Ps,Ps_f)
	,applied_metarules(Ps_f,MS,Ps_a)
	,skolem_sort(0,@>,Ps_a,Ps_s)
	,debug_clauses(top_program,'Applied metasubstitutions:',Ps_s).


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
