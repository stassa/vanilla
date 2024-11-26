:-module(mil_problem, [expanded_metarules/2
		      ,metarule_expansion/2
		      ,encapsulated/1
		      ,encapsulated_problem/5
		      ,encapsulated_bk/3
		      ,examples_targets/2
		      ,encapsulated_clauses/3
		      ,applied_metarules/3
		      ,excapsulated_clauses/3
		      ,target_or_invention/2
		      ]).

:-use_module(project_root(configuration)).
:-use_module(lib(term_utilities/term_utilities)).
:-use_module(lib(mathemancy/mathemancy)).
:-use_module(src(auxiliaries)).
:-use_module(src(metarules_parser)).

/** <module> Predicates to transform a MIL problem.

*/


%!	expanded_metarules(?Ids,-Encapsulated) is det.
%
%	Encapsulate a set of metarules.
%
%	Metarules is a list of metarule definitions to be expanded by
%	metarule_expansion/2.
%
%	If Ids is a list of metarule names, only definitions of the
%	named metarules are bound to Metarules.
%
%	If Ids is a free variable, it is bound to a list of the names of
%	all Metarules known to the system.
%
%	@tbd The mode (?,-) seems a little dangerous and in any case
%	there's the "all" option that does the same thing more or less.
%	Is it really necessary to allow Ids to be unbound on entry?
%
expanded_metarules(MS,MS):-
% Already expanded and encapsulated.
	\+ var(MS)
	,encapsulated(MS)
	,!.
expanded_metarules(Ids,Ms):-
	var(Ids)
	,!
	,findall(Id-M
	       ,metarule_expansion(Id,M)
	       ,Ids_Ms)
	,pairs_keys_values(Ids_Ms,Ids,Ms).
expanded_metarules(Ids,Ms):-
	is_list(Ids)
	,findall(M
	       ,(member(Id,Ids)
		,metarule_expansion(Id,M)
		)
	       ,Ms_)
	% Higher order metarule numbers end up [[caged]]
	,flatten(Ms_,Ms).



%!	metarule_expansion(?Id,-Metarule) is nondet.
%
%	Expand a Metarule with the given Id.
%
%	Performs a translation beween the user-level, user-friendly
%	atomic representation of metarules in metarule/2 clauses, and
%	Vanilla's internal representation as an expanded metarule with a
%	metasubstitution atom as a head literal and a set of
%	encapsualted body literals.
%
%	@tbd I've been meaning to change the name of this predicate, and
%	of expanded_metarules/1 to "metarule_translation/2" and
%	"translated_metarules/1" for a while now to avoid confusion with
%	metarule extension predicates' names.
%
%	@tbd Obviously this is just a renaming of parsed_metarule/2.
%	Another reason to rename it, or just get rid of it completely.
%
metarule_expansion(higher_order(Min,Max), M):-
% Higher order metarules are transformed to a list of integers.
	!
	,series(Min,Max,M).
metarule_expansion(N, N):-
% Something is asking for already-transformed higher order metarules to
% be expanded again- ignore it, they're just a list of integers.
	integer(N)
	,!.
metarule_expansion(Id, M):-
	parsed_metarule(Id,M).



%!	encapsulated(+Terms) is det.
%
%	True when Terms is a list of encapsulated Prolog terms.
%
%	Used by auxiliaries of encapsulated_problem/5 to allow the
%	elements of an already encapsulated MIL problem being passed to
%	learning predicates.
%
%	@tbd Similar analysis of a (Prolog) term to see if it's
%	encapsulaed is done ad-hoc in many places in the code. Perhaps
%	find them and replace them? For instance, see symbol/2 which
%	extracts the predicate symbol of a possible encapsulated term.
%	This could definitely be combined with this predicate, for
%	insance this predicate could return the encapsulated predicate
%	symbol.
%
encapsulated(Ts):-
	Ts = [H:-_B|_]
	,functor(H,m,_)
	,!.
encapsulated(Ts):-
% A negated term - probably a negative example.
	Ts = [:-T|_]
	,functor(T,m,_)
	,!.
encapsulated(Ts):-
	Ts = [T|_]
	,functor(T,m,_).



%!	encapsulated_problem(+Pos,+Neg,+BK,+MS,-Ps)
%!	is det.
%
%	Encapsualte a MIL problem.
%
%	Pos and Neg are lists of example atoms; Pos are positive
%	examples and Neg are negative examples, of the form :-E, where E
%	an atom.
%
%	BK is a list of predicate symbols and arities of BK predicates.
%
%	Metarules is a list of constants, the names of metarules in the
%	problem.
%
%	Ps is a list [Pos_, Neg_, BK_, MS_] where elements are the
%	encapsulations of the positive and negative examples, BK
%	definitions, and Metarules, respectively.
%
%	@tbd Encapsulated forms need documentation.
%
encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_]):-
	examples_targets(Pos,Ss)
	,encapsulated_bk(BK,Ss,BK_)
	,expanded_metarules(MS,MS_)
	,encapsulated_clauses(Pos,Ss,Pos_)
	% Shouldn't negative examples have their own Ss?
	,encapsulated_clauses(Neg,Ss,Neg_).


%!	encapsulated_bk(+Background,+Symbols,-Encapsulated) is det.
%
%	Encapsulate a list of Background definitions.
%
%	Symbols is the set of predicate symbols of target predicates
%	(including invented predicates), used to ensure clauses of
%	target predicates in the closure of BK predicates are also
%	encapsulated.
%
encapsulated_bk(BK,Ss,Es):-
	\+ is_list(Ss)
	,!
	,encapsulated_bk(BK,[Ss],Es).
encapsulated_bk([],_Ss,[]):-
	!.
encapsulated_bk(BK,_Ss,BK):-
% Already encapsulated.
	encapsulated(BK)
	,!.
encapsulated_bk(BK,Ts,Es):-
	(   closure(BK, user, Ps)
	 ->  true
	 ;   closure(BK, experiment_file, Ps)
	 ->  true
	 ;   throw('Missing BK definition of predicate in':BK)
	 )
	,append(BK,Ts,Ss)
	,findall(Cs_
	       ,(member(Cs, Ps)
		,encapsulated_clauses(Cs,Ss,Cs_)
		)
	       ,Es_)
	,flatten(Es_, Es).




%!	examples_targets(+Examples,-Targets) is det.
%
%	Extract symbols and arities from Examples of Targets.
%
%	Examples is a list of positive or negative examples,
%	encapsulated or not. Targets is a list of the predicate symbols
%	and arities of the target predicates in that list of examples.
%
examples_targets(Es,Ss):-
        setof(S
             ,E^Es^(member(E,Es)
                   ,symbol(E,S)
              )
             ,Ss).


%!	symbol(+Example,-Symbol) is det.
%
%	Derive the predicate symbol of an Example.
%
%	Example is a positive or negative example. It might be
%	encapsulated, or not. Symbol is a compound Symbol/Arity, the
%	symbol and arity of the Example's predicate (not m/n if
%	encapsulated).
%
symbol(H:-_B,F/A):-
	!
	,symbol(H,F/A).
symbol(:-E,F/A):-
% Encapsulated negative example.
        configuration:encapsulation_predicate(Enc)
	,E =.. [Enc,F|As]
        ,!
        ,length(As,A).
symbol(:-H,F/A):-
% Not encapsulated negative example.
        functor(H,F,A)
        ,!.
symbol(E,F/A):-
% Encapsulated example.
        configuration:encapsulation_predicate(Enc)
	,E =.. [Enc,F|As]
        ,!
        ,length(As,A).
symbol(H,F/A):-
% Not encapsulated example.
        functor(H,F,A).



%!	encapsulated_clauses(+Clauses,+Symbols,-Encapsulated) is det.
%
%	Encapsulate a list of Clauses.
%
%       Symbols is a list of predicate symbols and arities of literals
%       to be encapsulated. Literals with symbols and arities not in
%       Symbols will not be encapsulated.
%
encapsulated_clauses(Cs,_Ss,Cs):-
% Already encapsulated.
	encapsulated(Cs)
	,!.
encapsulated_clauses(Cs,Ss,Cs_):-
	encapsulated_clauses(Cs,Ss,[],Cs_).

%!	encapsulated_clauses(+Clauses,+Acc,-Encapsulated) is det.
%
%	Business end of encapsulated_clauses/2.
%
encapsulated_clauses([],_Ss,Acc,Cs):-
	reverse(Acc, Cs)
	,!.
encapsulated_clauses([C|Cs],Ss,Acc,Bind):-
	encapsulated_clause(C,Ss,C_)
	,encapsulated_clauses(Cs,Ss,[C_|Acc],Bind).


%!	encapsulated_clause(+Clause,+Symbols,-Encapsulated) is det.
%
%	Encapsulate a Clause.
%
encapsulated_clause(:-C,Ss,:-C_):-
% Negated clause: a negative example.
	encapsulated_clause(C,Ss,[],C_)
        ,!.
encapsulated_clause(C,Ss,C_):-
	encapsulated_clause(C,Ss,[],C_).

%!	encapsulated_clause(+Clause,+Symbols,+Acc,-Encapsulated) is det.
%
%	Business end of encapsulated_clause/2.
%
encapsulated_clause(H:-Bs,Ss,Acc,H_:-Bs_):-
% Clause with a body; H is the head literal.
	!
        ,encapsulated_literal(H,Ss,H_)
	,encapsulated_clause(Bs,Ss,Acc,Bs_).
encapsulated_clause((L,Ls),Ss,Acc,C_):-
% L is the next in a set of literals.
	!
        ,encapsulated_literal(L,Ss,L_)
	,encapsulated_clause(Ls,Ss,[L_|Acc],C_).
encapsulated_clause(L,Ss,[],L_):-
% Last literal: the accumulator is empty; probably a single atom.
	!
        ,encapsulated_literal(L,Ss,L_).
encapsulated_clause(L,Ss,Acc,Ls_):-
% Last literal: the accumulator is not empty.
        encapsulated_literal(L,Ss,L_)
	,reverse([L_|Acc], Ls)
	,once(list_tree(Ls,Ls_)).


%!	encapsulated_literal(+Literal,+Symbols,-Encapsulated) is det.
%
%	Encapsulate one literal.
%
encapsulated_literal(L,Ss,L):-
        \+ encapsulable(L,Ss)
        ,!.
encapsulated_literal(L,_Ss,L_):-
        configuration:encapsulation_predicate(S)
        ,L =.. [F|As]
        ,L_ =.. [S|[F|As]].


%!	encapsulable(+Literal,+Symbols) is det.
%
%	True when Literal must be encapsulated.
%
%	Literal must be encapsulated when its predicate symbol is in the
%	list of Symbols to be encapsulated.
%
encapsulable(L,_Ss):-
        built_in_or_library_predicate(L)
        ,!
        ,fail.
encapsulable(L,Ss):-
        functor(L,F,A)
        ,memberchk(F/A,Ss).



%!	applied_metarules(+Metasubstitutions,+Metarules,-Applied) is
%!	det.
%
%	Apply a list of Metasubstitutions to corresponding Metarules.
%
%	The list of Metasubstitutions is normally the specialised Top
%	program.
%
applied_metarules(Ss,_MS,Ms):-
	findall(Msub
		,(member(Sub,Ss)
		 ,applied_metasubstitution(Sub,Msub)
		 )
		,Ms).


%!	applied_metasubstitution(+Metasubstitution,-Applied) is det.
%
%	Apply a Metasubstitution atom to its corresponding metarule.
%
%	Abstracts the application of a Metasubstitution atom to its
%	corresponding metarule which may or may not have a body.
%
%	Allows for metasubstitutions in both known representations,
%	depending on the setting of the configuration option
%	metasubstitution_atoms/1.
%
applied_metasubstitution(Sub/_Sub_U-(Sub/_:-(H,B)), H:-B):-
	!.
applied_metasubstitution(Sub/_Sub_U-(Sub/_:-(L)), L):-
	!.
applied_metasubstitution(Sub-(Sub:-(H,B)), H:-B):-
	!.
applied_metasubstitution(Sub-(Sub:-(L)), L).


%!	excapsulated_clauses(+Target, +Clauses, -Excapsulated) is det.
%
%	Remove encapsulation from a list of Clauses.
%
%	Only clauses of the Target predicate or invented predicates
%	based on the Target predicate, are excapsulated- clauses of
%	metarules and background definitions are dropped silently.
%
excapsulated_clauses(T, Cs, Cs_):-
	excapsulated_clauses(T,Cs,[],Cs_).

%!	excapsulated_clauses(+Target,+Acc,-Excapsulated) is det.
%
%	Business end of excapsulated_clauses/3.
%
excapsulated_clauses(_T,[],Acc,Es):-
	reverse(Acc, Es)
	,!.
excapsulated_clauses(T,[C|Cs],Acc,Bind):-
	excapsulated_clause(T,C,C_)
	,!
	,excapsulated_clauses(T,Cs,[C_|Acc],Bind).
excapsulated_clauses(T,[_C|Cs],Acc,Bind):-
	excapsulated_clauses(T,Cs,Acc,Bind).


%!	excapsulated_clause(+Targets,+Clause,-Excapsulated) is det.
%
%	Excapsulate a single Clause of all learning Targets.
%
excapsulated_clause(T,C,C_):-
	excapsulated_clause(T,C,[],C_).

%!	excapsulated_clause(+Targets,+Clause,+Acc,-Excapsulated) is det.
%
%	Business end of excapsulated_clause/3.
%
excapsulated_clause(Ts,H:-Bs,Acc,Bind):-
% Definite clause; H is the head literal.
	configuration:encapsulation_predicate(E)
	,H =.. [E|[S|As]]
	,length(As,A)
	,target_or_invention(Ts,S/A)
	,!
	,H_ =.. [S|As]
	,excapsulated_clause(Ts,Bs,[H_|Acc],Bind).
excapsulated_clause(Ts,(L,Ls),Acc,Bind):-
% Definite clause: L is the next body literal.
	!
	,configuration:encapsulation_predicate(E)
	,L =.. [E|[F|As]]
	,L_ =.. [F|As]
	,excapsulated_clause(Ts,Ls,[L_|Acc],Bind).
excapsulated_clause(Ts,L,[],L_):-
% Unit clause: the accumulator is empty.
	!
        ,configuration:encapsulation_predicate(E)
	,L =.. [E|[S|As]]
	,length(As, A)
	,target_or_invention(Ts,S/A)
	,ground(S)
	,L_ =.. [S|As].
excapsulated_clause(_Ts,(L),Acc,(H:-Bs)):-
% Definite clause: L is the last literal.
% We don't need to check the symbol again.
	configuration:encapsulation_predicate(E)
	,L =.. [E|[F|As]]
	,L_ =.. [F|As]
	,reverse([L_|Acc],Ls)
	,once(list_tree(Ls,(H,Bs))).


%!	target_or_invention(+Targets,+Symbols) is det.
%
%	True when Symbols are Targets or inventions from Targets.
%
%	@tbd This expects that invented predicates' symbols will have
%	the same functor as the Target predicate but with a numeric
%	index appended to it by an underscore, '_'. This could be
%	enforced a little more strictly through the project.
%
%	TODO: pluralise.
%
target_or_invention(Ts,F/A):-
        memberchk(F/A,Ts)
	,!.
target_or_invention(_,S/_):-
	configuration:invented_symbol_prefix(F)
	,atom_concat(F,K,S)
	,atom_number(K,_N).
