:-module(poker, [learn/1
                ,learn/2
                ,learn/5
                ]).

:-use_module(src(vanilla)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(poker_configuration).

/** <module> Simple MIL-learner based on Vanilla.

*/


%!      learn(+Target) is nondet.
%
%       Learn a program for a learning Target and print to the console.
%
%       Target is a predicate indicator, Symbol/Arity, of a learning
%       target, or a list of predicate indicators of learning targets.
%
%       Examples and background knowledge, including metarules, for
%       Target are taken from currently loaded experiment file, defined
%       in poker_configuration.pl.
%
learn(T):-
        learn(T,Ps)
        ,print_clauses(Ps).


%!      learn(+Target,-Program) is nondet.
%
%       Learn a Program for a learning Target.
%
%       Target is a predicate indicator, Symbol/Arity, of a learning
%       target, or a list of predicate indicators of learning targets.
%
%       Program is a list of clauses learned from the examples and
%       background knowledge given for Target.
%
%       Examples and background knowledge, including metarules, for
%       Target are taken from currently loaded experiment file, defined
%       in poker_configuration.pl.
%
learn(T,Ps):-
        experiment_data(T,Pos,Neg,BK,MS)
        ,learn(Pos,Neg,BK,MS,Ps).


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



%!      learn(+Pos,+Neg,+B_FO,+B_SO,-Program) is nondet.
%
%       Target is a predicate indicator, Symbol/Arity, of a learning
%       target, or a list of predicate indicators of learning targets.
%
%       Pos, Neg, B_FO, and B_SO are the positive and negative examples,
%       and first-order and second-order background knowledge,
%       respectively. Pos are ground logical atoms, Neg are ground goals
%       (:-G), B_FO is a list of predicate indicators, Symbol/Arity, of
%       first-order background predicates. B_SO is a list of atomic
%       names of second-order background predicate, i.e. the metarules.
%
%       Program is a list of clauses learned from the examples and
%       background knowledge given for Target.
%
%       All programs that entail the positive examples, and none of the
%       negative examples, with respect to the first-order background
%       knowledge in B_FO are generated on backtracking.
%
learn(Pos,Neg,BK,MS,Ps):-
        poker_configuration:clause_limit(K)
        ,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
        ,S = (write_problem(user,[BK_],Refs)
             ,refresh_tables(untable)
	     ,refresh_tables(table)
         )
        ,G = (generalise(Pos_,K,MS_,Subs)
             ,specialise(Neg_,K,MS_,Subs)
             ,sort(1,@<,Subs,Subs_s)
             ,applied_metarules(Subs_s,MS_,Ps_)
             )
        ,C = (erase_program_clauses(Refs)
	     ,refresh_tables(untable)
             )
        ,setup_call_cleanup(S,G,C)
        ,Ps_ \= []
        ,examples_targets(Pos,Ts)
        ,excapsulated_clauses(Ts,Ps_,Ps).
learn(_Pos,_Neg,_BK,_MS,[]).


%!      generalise(+Pos,+K,+B_SO,-Metasubs) is nondet.
%
%       Generalise a set of posiitve examples.
%
generalise(Pos,K,MS,Subs):-
        member(Ep,Pos)
        ,debug(generalise,'Ep: ~w',[Ep])
        ,metasubstitutions(Ep,K,MS,Subs)
        ,debug(generalise,'Subs: ~w',[Subs]).


%!      specialise(+Neg,+K,+B_SO,+Metasubs) is det.
%
%       Specialise a set of Metasubstitutions using Negative examples.
%
specialise(Neg,K,MS,Subs):-
        forall(member(En,Neg)
              ,\+ metasubstitutions(En,K,MS,Subs)
              ).


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
        ,prove(En,K,MS,[],Subs,Subs)
	,debug(metasubstitutions,'Proved Example: ~w',[:-En])
	%,debug_clauses(metasubstitutions,'With Metasubs:',[Subs])
        .
metasubstitutions(Ep,K,MS,Subs):-
	signature(Ep,Ss)
	,debug(signature,'Signature: ~w',[Ss])
        ,vanilla:prove(Ep,K,MS,Ss,[],Subs_)
	,debug(metasubstitutions,'Proved Example: ~w',[Ep])
	,Subs_ \= []
	,sort(Subs_,Subs_s)
	%,debug_clauses(metasubstitutions,'Proved Metasubs:',[Subs_s])
	,findall(Sub-M
		,(member(Sub,Subs_s)
		 ,metasub_metarule(Sub,MS,M)
		 )
		,Subs).


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
