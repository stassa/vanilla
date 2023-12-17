:-module(poker, [learn/1
                ,learn/2
                ,learn/5
                ]).

:-use_module(src(vanilla)).
:-use_module(src(auxiliaries)).
:-use_module(lib(poker/poker_auxiliaries)).
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
