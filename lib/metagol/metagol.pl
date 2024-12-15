:-module(metagol, [learn/1
                  ,learn/2
                  ,learn/5
                  ]).

:-use_module(src(vanilla)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(lib(term_utilities/term_utilities)).
:-use_module(metagol_auxiliaries).
:-use_module(metagol_configuration).


/** <module> An implemenation of Metagol based on Vanilla.

*/

% Metagol does not use tabling and instead controls recursion by means
% of lexicographic and interval order constraints.
%
configuration:table_meta_interpreter(false).
configuration:untable_meta_interpreter(true).


%!      learn(+Target) is nondet.
%
%       Learn a program for a learning Target and print to the console.
%
%       Target is a predicate indicator, Symbol/Arity, of a learning
%       target, or a list of predicate indicators of learning targets.
%
%       Examples and background knowledge, including metarules, for
%       Target are taken from currently loaded experiment file, defined
%       in metagol_configuration.pl.
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
%       in metagol_configuration.pl.
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
%       background knowledge given for Target. Specifically, Program
%       entails the conjunction of the positive examples, and none of
%       the negative examples, with respect to first-order background
%       knowledge in B_FO.
%
%       All programs that entail the positive examples, and none of the
%       negative examples, with respect to the first-order background
%       knowledge in B_FO are generated on backtracking.
%
learn(Pos,Neg,BK,MS,Ps):-
        metagol_configuration:depth_limits(K,J)
        ,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
        ,S = (write_problem(user,[BK_],Refs)
             ,refresh_tables(untable)
	     ,refresh_tables(table)
         )
        ,G = (generalise(Pos_,K,J,MS_,Subs)
             ,specialise(Neg_,J,MS_,Subs)
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



%!      generalise(+Pos,+Min,+Max,+B_SO,-Metasubs) is nondet.
%
%       Generalise a set of posiitve examples as a conjunction.
%
%       Performs a Depth-First Search with Iterative Deepening over the
%       cardinality of the ste of derived Metasubs, where the depth
%       of the search ranges from Min to Max.
%
generalise(Pos,K,J,MS,Subs):-
        debug(generalise,'Generalising examples',[])
        ,list_tree(Pos,Pos_)
        ,between(K,J,I)
        ,debug(depth,'Depth: ~w',[I])
        ,metasubstitutions(Pos_,I,MS,Subs)
        ,debug_clauses(generalise,'New metasubs:',[Subs]).



%!      specialise(+Neg,+K,+B_SO,+Metasubs) is det.
%
%       Specialise a set of Metasubstitutions using Negative examples.
%
specialise(Neg,K,MS,Subs):-
        debug_clauses(specialise,'Specialising metasubs:',[Subs])
        % prove/7 only needs metasubstitution terms.
        ,findall(Sub
               ,(member(Sub-_M,Subs)
                )
               ,Subs_)
        ,debug_clauses(specialise,'Checking constraints on metasubs:',[Subs_])
        ,check_constraints(Subs_)
        ,forall(member(En,Neg)
              ,\+ metasubstitutions(En,K,MS,Subs_)
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
	,signature(En,Ss)
	,debug(signature,'Signature (neg): ~w',[Ss])
	,debug(metasubstitutions,'Proving negative example: ~w',[:-En])
        ,prove(En,K,MS,Ss,Subs,Subs)
	,debug(metasubstitutions,'Proved example: ~w',[:-En])
	,debug_clauses(metasubstitutions,'With metasubs:',[Subs])
        .
metasubstitutions(Ep,K,MS,Subs):-
	signature(Ep,Ss)
	,debug(signature,'Signature: ~w',[Ss])
	,debug(metasubstitutions,'Proving positive example: ~w',[Ep])
        ,vanilla:prove(Ep,K,MS,Ss,[],Subs_)
	,debug(metasubstitutions,'Proved Example: ~w',[Ep])
	,Subs_ \= []
	,sort(Subs_,Subs_s)
	,debug_clauses(metasubstitutions,'Proved Metasubs:',[Subs_s])
	,findall(Sub-M
		,(member(Sub,Subs_s)
		 ,metasub_metarule(Sub,MS,M)
		 )
		,Subs).


%!	signature(+Example,-Signature) is det.
%
%	Return the predicate Signature for one example's predicate.
%
signature((L,_),Ss):-
        !
        ,signature(L,Ss).
signature(L,[T|Ss]):-
        configuration:encapsulation_predicate(E)
        ,metagol_configuration:max_invented(N)
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
metasub_metarule(Sub_E/Sub_U,MS,Sub_E_/Sub_U_:-M):-
	configuration:encapsulation_predicate(E)
        ,metasub_atom(E,Sub_E,Sub_E_)
        ,metasub_atom(E,Sub_U,Sub_U_)
        ,free_member(Sub_E_/Sub_U_:-M,MS).


%!      metasub_atom(+Symbol,+Atom,-New) is det.
%
%       Construct a Metasubstitution Atom.
%
%       Symbol is the encapsulation predicate symbol defined in the
%       configuration as encapsulation_predicate/1.
%
%       Atom is a metasubstitution atom, with existentially or
%       universally quantified variables.
%
%       New is the given metasubstitution Atom with ground symbols or
%       terms replaced by fresh variables.
%
metasub_atom(E,Sub,Sub_):-
	Sub =.. [E,Id|As]
	,length(As,N)
	,length(As_,N)
	,Sub_ =.. [E,Id|As_].



%!      metarule_constraints(+Metasub,-Truth) is det.
%
%       Implements lexicographic order constraints for Metagol
%
%       Metasub is a metasubstitution atom of the form S(Id,T,P1,..,Pn)
%       where S is the encapsulation symbol defined in the
%       configuration, Id is the identifier of a metarule, T is a
%       learning target and P1, ..., Pn are predicate symbols. T and Ps
%       may be unbound at the point of call, i.e. variables.
%
%       Truth is one of "true" or "false", denoting whether the
%       constraint is passed or not. For this
%
%       Called by constraints/1. Checks order constraints defined in
%       metagol_configuration.pl for the metarule identified in Metasub.
%       Order constraints must be consistent with the lexicographic
%       ordering declared for the learning target in Metasub are taken
%       from the experiment file where the training data for that target
%       are defined.
%
%       See data/examples/hello_world.pl for an example of declaring a
%       lexicographic ordering for a learning target.
%
configuration:metarule_constraints(M_E/M_U,B):-
	debug_clauses(constraints,'Testing constraint for metasub:',M_E/M_U)
        ,configuration:encapsulation_predicate(S)
        ,copy_term(M_E/M_U,M_E_c/M_U_c)
        ,M_E_c =.. [S,Id|[T|Ps_E]]
        ,M_U_c =.. [S,Id|Ts_U]
        ,experiment_file:program_signature(T/_,PS,CS)
	,debug(constraints,'Predicate signature: ~w',[PS-CS])
        ,metagol_configuration:order_constraints(Id,[T|Ps_E],Ts_U,STs,FTs)
	,debug(constraints,'Order constraints: ~w-~w',[STs,FTs])
        ,(   order_tests(PS,CS,STs,FTs)
	 ->  B = true
	    ,debug(constraints,'Passed constraint test!',[])
	 ;   B = false
	    ,debug(constraints,'Failed constraint test!',[])
	 ).


%!	order_tests(+Predicates,+Constants,+First_Order,+Second_Order)
%!	is det.
%
%	Test the order constraints associated with a metarule.
%
%	Predicates is the program signature.
%
%	Constants is the constant signature.
%
%	First_order and Second_order are the lexicographic and interval
%	inclusion order constraints imposed by a metarule.
%
order_tests(_PS,_CS,[],[]):-
	!.
order_tests(_PS,CS,[],Us):-
	!
       ,ordered_list(Us,CS).
order_tests(PS,_CS,Es,[]):-
	!
       ,ordered_list(Es,PS).
order_tests(PS,CS,Es,Us):-
	ordered_list(Es,PS)
        ,ordered_list(Us,CS).


%!	ordered_list(?List,+Ordering) is det.
%
%	A Sublist order according to a total Ordering of its elements.
%
ordered_list([X>Y],Os):-
	above(X,Y,Os)
	,!.
ordered_list([X>Y|Ls],Os):-
	above(X,Y,Os)
	,ordered_list(Ls,Os).


%!	above(?Above,+Below,+Ordering) is det.
%
%	True when Above is above Below in a total Ordering.
%
above(_S1,S2,_Ss):-
        var(S2)
        ,!.
above(S1,S2,Ss):-
	previous(S1,S2,Ss)
	,!.
above(S1,S3,Ss):-
	previous(S1,S2,Ss)
	,!
	,above(S2,S3,Ss).
above(S1,S2,[_|Ss]):-
	above(S1,S2,Ss).


%!	previous(?First,?Next,?List) is det.
%
%	True when First and Next are the first two elements of List.
%
previous(S1,S2,[S1,S2|_Ss]).
