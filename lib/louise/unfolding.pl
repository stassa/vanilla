:-module(unfolding, [unfold_invented/3
		    ,index_and_sort/2
		    ,indexed_mergesort/2
                    ]).

:-use_module(lib(term_utilities/term_utilities)).
:-use_module(src(vanilla)).
:-use_module(src(auxiliaries)).

/** <module> Unfold learned hypotheses to remove invented predicates.

*/


%!	unfold_invented(+Program,+Targets,-Unfolded) is det.
%
%	Unfold a Program to remove invented predicates.
%
%	Program is a set of first-order definite program clauses,
%	possibly including the definitions of invented predicates.
%	Program is a hypothesis formed by generalisation of a set of
%	positive examples.
%
%	Targets is the list of predicate indicators, F/A, of the
%	predicates defined in Program that are learning targets.
%
%	Unfolded is the list of clauses in Program unfolded so that only
%	the predicates in the list Targets are defined.
%
%	The use of this predicate is to unfold programs with invented
%	predicates to eliminate the literals of invented predicates by
%	resolution.
%
%	Unfolding to remove invented predicates is best explained with
%	an example. Below, inv_1/2 is treated as an invented predicate:
%	==
%	?- _Cs = [(p(X,Y):- inv_1(X,Z), r(Z,Y)), (inv_1(X2,Y2):- s(X2,Y2)) ].
%	true.
%
%	?- auxiliaries:print_clauses($_Cs).
%	p(A,B):-inv_1(A,C),r(C,B).
%	inv_1(A,B):-s(A,B).
%	true.
%
%	?- unfold_invented($_Cs,[p/2],_Us), auxiliaries:print_clauses(_Us).
%	p(A,B):-s(A,C),r(C,B).
%	true.
%	==
%
%	In the example, notice how the literal inv_1(A,C) in the body of
%	the clause of p/2 is replaced, by unfolding, by the body literal
%	s(A,B), in the definitinon of inv_1/2, and the definition of
%	inv_1/2 eliminated.
%
%	With a bit of squinting it should be obvious that unfolding is
%	resolution. More precisely, unfold_invented/3, implements
%	unfolding as SLD-Resolution, by means of a meta-interpreter.
%
unfold_invented(Ps,Ts,Us):-
	debug_clauses(unfold_invented,'Unfolding program:',Ps)
	,debug(unfold_invented,'With target symbols: ~w',[Ts])
	,copy_term(Ps,Ps_c)
	,program_invented(Ps_c,Ts,Cs,Is)
	,invented_symbols_(Is,Ss)
	,!
	,unfold_clauses(Cs,Ss,Is,[],Us_)
	,(   is_list(Us_)
	    ,\+ memberchk([depth_limit_exceeded],Us_)
	 ->  flatten(Us_,Us)
	 ;   Us = Ps
	 )
	,debug_clauses(unfold_invented,'Unfolded program:',Us).
unfold_invented(Ps,Ts,Ps):-
	program_invented(Ps,Ts,_Cs,[]).



%!	program_invented(+Program,+Targets,-Clauses,-Invented) is det.
%
%	Partition a Program to clauses of target or Invented Predicates.
%
%	Program is a set of first-order definite program clauses,
%	possibly including the definitions of invented predicates.
%	Program is a hypothesis formed by generalisation of a set of
%	positive examples.
%
%	Targets is the list of predicate symbols and arities of the
%	target predicates in Program, as F/A predicate indicators.
%
%	Clauses is the list of clauses of predicates in Targets, i.e.
%	clauses with head literals having the predicate symbol and arity
%	of a predicate indicator in Targets.
%
%	Invented is the list of clauses of invented predicates, i.e.
%	clauses with head literals having an invented predicate symbol,
%	as defined in the configuration option invented_symbol_prefix/1.
%
program_invented(Ps,Ts,Cs,Is):-
	program_invented(Ps,Ts,[],Cs_,[],Is_)
	,maplist(reverse,[Cs_,Is_],[Cs,Is])
	,debug_clauses(program_invented,'Targets:',Cs)
	,debug_clauses(program_invented,'Invented:',Is).

%!	program_invented(+Program,+Targets,+Acc1,-Clauses,+Acc2,-Invented)
%	is det.
%
%	Business end of program_invented/4.
%
program_invented([],_Ts,Cs,Cs,Is,Is):-
	!.
program_invented([C|Ps],Ts,Cs_Acc,Cs_Bind,Is_Acc,Is_Bind):-
	clause_of(C,Ts)
	,!
	,program_invented(Ps,Ts,[C|Cs_Acc],Cs_Bind,Is_Acc,Is_Bind).
program_invented([C|Ps],Ts,Cs_Acc,Cs_Bind,Is_Acc,Is_Bind):-
	program_invented(Ps,Ts,Cs_Acc,Cs_Bind,[C|Is_Acc],Is_Bind).


%!	clause_of(+Clause,+Signature) is det.
%
%	True when Clause is a clause of a predicate in Signature.
%
%	Clause is a definite clause. Signature is a list of predicate
%	symbols and arities, inherited from the second argument of
%	unfold_invented, i.e. it is the set of predicate indicators of
%	target predicates defined in a learned hypothesis.
%
%	A call clause_of(C,Ss) succeeds when the symbol and arity of the
%	head literal in clause C is S/A and S/A is in Ss.
%
clause_of(C,Ts):-
% Encapsulated clause
        clause_symbol(C,F/A)
	,memberchk(F/A,Ts).


%!	clause_symbol(+Clause,-Symbol) is det.
%
%	The predicate Symbol and arity of a Clause.
%
%	@tbd This could be useful elsewhere in the project. Consider
%	adding to auxiliaries, probably.
%
%	TODO: Un-hardcode the invented symbol.
%
clause_symbol(H:-_B,F/N):-
% Encapsulated clause
	functor(H,m,_A)
	,H =.. [m,F|As]
	,length(As, N)
	,!.
clause_symbol(L,F/N):-
% Encapsulated unit clause
	functor(L,m,_A)
	,L =.. [m,F|As]
	,length(As,N)
	,!.
clause_symbol(H:-_B,F/A):-
% Definite clause
	functor(H,F,A)
	,!.
clause_symbol(L,F/A):-
% Unit clause
	functor(L,F,A).


%!	invented_symbols_(+Invented,-Symbols) is det.
%
%	Collect the Symbols of a list of clauses of Invented predicates.
%
%	Invented is a list of clauses of invented predicates'
%	definitions as returned by program_invented/4. Symbols is a list
%	of F/A terms where each F is the symbol of an invented predicate
%	in Invented and each A is its arity.
%
%	@tbd there is a predicate, invented_symbols/2 in the auxiliaries
%	module that does this kind of thing. The difference with this
%	program is that invented_symbols/2 generates invented symbols
%	directly from the configuration option max_invented/2 and so
%	there may be situations in which Invented may contain symbols
%	not generated by invented_symbols/2. This may happen, for
%	example, if unfold_invented/3 is called manually with some
%	arbitrary user-defined program, for testing. That said, do
%	consider using invented_symbols/2 instead of this predicate.
%
%	@tbd More to the point, this can be done while partitioning an
%	input program to target and invented predicates, in
%	program_invented/4 and that would avoid the overhead of having
%	to re-process invented predicates' clauses.
%
invented_symbols_(Is,Ss):-
	setof(F/A
	     ,B^Is^H^(member(H:-B,Is)
		     ,clause_symbol(H,F/A)
		     )
	     ,Ss).


%!	unfold_clauses(+Clauses,+Symbols,+Invented,+Acc,-Unfolded) is
%!	det.
%
%	Unfold a set of Clauses with a set of Invented definitions.
%
%	Clauses is a list of clauses, the definition of the target
%	predicates in a learned hypothesis.
%
%	Symbols is the list of predicate indicators, F/A, of the target
%	predicates defined in Targets.
%
%	Invented is a list of clauses, the definitions of invented
%	predicates in a learned hypothesis.
%
%	Acc is the accumulator of unfolded claues, initially the empty
%	list [].
%
%	Unfolded is a list of clauses, the set of resolvents of each
%	clause in Clauses with the clauses in Invented. Unfolding
%	replaces the sets of clauses in Clauses and Invented with a
%	set of clauses that define the predicates in Targets without any
%	body literals of invented predicates.
%
unfold_clauses([],_Ss,_Is,Us,Us):-
	!.
unfold_clauses([C|Cs],Ss,Is,Acc,Bind):-
	findall(C_
	       ,unfold_clause(C,Ss,Is,C_)
	       ,Us)
	,unfold_clauses(Cs,Ss,Is,[Us|Acc],Bind).


%!	unfold_clause(+Clause,+Symbols,+Invented,-Unfolded) is nondet.
%
%	Auxiliary to unfold_clauses/5.
%
unfold_clause(H:-B,Ss,Is,C):-
	louise_configuration:unfolding_depth_limit(L)
	,must_be(nonvar,H)
	,must_be(nonvar,B)
	,debug_clauses(unfold_clause,'Unfolding Clause:',[H:-B])
	,G = unfold_literals(B,Ss,Is,(H),U_)
	,call_with_depth_limit(G,L,R)
	,(   R == depth_limit_exceeded
	 ->  debug(unfold_clause,'Proof exceeded depth limit ~w',[L])
	    ,C = R
	 ;   debug(unfold_clause,'unfold_clause/4 succeeded at depth ~w',[R])
	   ,treeverse(U_,(H,B_))
	   ,C = (H:-B_)
	   ,debug_clauses(unfold_clause,'Unfolded:',[H:-B_])
	 ).


%!	unfold_literals(+Literals,+Symbols,+Invented,+Acc,-Unfolded) is
%!	nondet.
%
%	Business end of unfold_clause/4.
%
%	Unfoldss a set of Literals to remove invented predicates.
%
%	Literals is a list of literals, the doby literals of a clause
%	being unfolded, received from unfold_clause/4.
%
%	Acc is initialised to the head litearl of the clauses being
%	unfolded, i.e. the clause whose body literals are in Literals.
%
%	Unfolded is the clause with its head in Acc and body in
%	Literals, unfolded by resolution with predicates in Invented, to
%	remove the body literals of invented predicates from Literals.
%
/*
% Uncomment for a more readable version without all the debugs.
unfold_literals(true,_Ss,_Is,Us,Us):-
	!.
unfold_literals((L,Ls),Ss,Is,Acc,Bind):-
	!
	,unfold_literals(L,Ss,Is,Acc,Acc1)
	,unfold_literals(Ls,Ss,Is,Acc1,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
	head_body(L,Is,Ls)
	,!
	,unfold_literals(Ls,Ss,Is,Acc,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
	\+ clause_of(L,Ss)
	,unfold_literals(true,Ss,Is,(L,Acc),Bind).
*/
%/*
% Uncomment for better logging
unfold_literals(true,_Ss,_Is,Us,Us):-
	debug(unfold_literals,'Current stack: ~w',[true])
	,debug_clauses(unfold_literals,'Reached Leaf. Unfolded:',Us)
	,!.
unfold_literals((L,Ls),Ss,Is,Acc,Bind):-
	!
        ,debug(unfold_literals,'Current stack: ~w',[(L,Ls)])
	,debug(unfold_literals,'Proving literal: ~w',[L])
	,unfold_literals(L,Ss,Is,Acc,Acc1)
	,debug(unfold_literals,'Proving literals: ~w',[Ls])
	,unfold_literals(Ls,Ss,Is,Acc1,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
        debug(unfold_literals,'Current stack: ~w',[(L)])
	,head_body(L,Is,Ls)
	,debug(unfold_literals,'Bound head literal: ~w',[L])
	,debug(unfold_literals,'And body literals: ~w',[Ls])
	,!
	,unfold_literals(Ls,Ss,Is,Acc,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
	debug(unfold_literals,'Current stack: ~w',[(L)])
	,\+ clause_of(L,Ss)
	,debug(unfold_literals,'Clause of: ~w',[L])
	,unfold_literals(true,Ss,Is,(L,Acc),Bind).
%*/


%!	head_body(+Clause,+Literal,-Body) is det.
%
%	Auxiliary to unfold_literals/5.
%
%	Bind the head of a Clause to a Literal and return its Body.
%
%	The purpose of this is similar to a call to clause/2 in a
%	classic Prolog meta-interpreter: we want to recursively evaluate
%	the body literals of a clause, so for every body literal we find
%	its parent clause and replace the literal with the body literals
%	of the parent.
%
head_body(L,Is,B):-
% Avoid binding variables in the clause to terms in the literal.
	free_member(L:-B,Is)
	,!.
head_body(L,Is,true):-
	free_member(L,Is).



		/*******************************
		*           SORTING            *
		*******************************/


%!	index_and_sort(+Programs,-Sorted) is det.
%
%	Sort a list of programs to remove duplicates.
%
%	Programs is a list of first-order definite programs.
%
%	Sorted is the list of Programs, sorted by indexed_mergesort/2.
%
%	This predicate first indexes the programs in Programs as
%	required by indexed_mergesort, before handing them over to
%	indexed_mergesort.
%
index_and_sort(Ps,Ss):-
	findall(I-Cs
	       ,(nth1(I,Ps,Cs)
		)
	       ,Is)
	,indexed_mergesort(Is,Ss_s)
	,de_indexed_list(Ss_s,Ps,Ss).


%!	de_indexed_list(+Indexed,+Original,-Selected) is det.
%
%	Map the elements of an indexed list to a list.
%
de_indexed_list(Is,Ls,Ls_):-
	keysort(Is,Is_ks)
	,de_indexed_list(Is_ks,1,Ls,[],Ls_).

%!	de_indexed_list(+Indexed,+Count,+Original,+Acc,-Selected) is
%!	det.
%
%	Business end of de_indexed_list/3.
%
de_indexed_list([],_,_,Ls,Ls):-
	!.
de_indexed_list([I-_Xs|Is],I,[Ls_i|Ls],Acc,Bind):-
	succ(I,J)
	,!
	,de_indexed_list(Is,J,Ls,[Ls_i|Acc],Bind).
de_indexed_list(Is,I,[_Ls_i|Ls],Acc,Bind):-
	succ(I,J)
	,de_indexed_list(Is,J,Ls,Acc,Bind).




%!	indexed_mergesort(+Indexed,-Sorted) is det.
%
%	Mergesort implementation for key-value pairs.
%
%	Indexed is a list of key-value-pairs, K-V, where K an integer
%	key and V a value. We want to sort Indexed to remove elements
%	with duplicate values while preserving their order. To do this
%	we sort by comparing only the values, while ignoring the keys.
%
%	This is similar to what can be achieved by keysort/2, however we
%	want to compare literals ignoring their variables, i.e. we
%	test whether two literals are equal up to renaming of their
%	variables. This is handled by merge_compare/3.
%
%	The purpose of this predicate is to compare sub-hypotheses
%	learned by Poker and unfoled by unfold_invented/2, to reduce the
%	number of programs that are success-set equivalent, but have
%	different structure, in particular with respect to the positions
%	and names of invented predicates, that are returned by
%	generalise/3. There can be a very large number of those programs
%	depending on the value of clause_limit/1 but we only need to
%	keep one, both for efficiency and to avoid the Top Program
%	looking like a horrible, unreadable mess.
%
%	The purpose of the indexing is that programs returned by
%	genrealise/3 are in the form of lists of metasubstitutions. We
%	want to sort the _programs_ constructed from each of those
%	lists, but then return the lists again, so that they can be
%	further processed by the rest of Poker's sub-routines, that
%	expect metasubstitutions, rather than expanded clauses. To do
%	this, we use the indices as in Programs as handles to match them
%	to their pre-expansion form.
%
%	Note that in order to sort the indexed programs correctly,
%	clauses in the program, and literals in each clause must be
%	sorted to the standard order of terms.
%
%	Mergesort adapted from:
%
%	http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
%
indexed_mergesort([],[]):-
	!.
indexed_mergesort([I-Xs],[I-Xs]):-
	!.
indexed_mergesort(Ls,Ss):-
        % Ignore variable age during sorting.
	maplist(numbervars,Ls)
	,indexed_mergesort_sk(Ls,Ss_sk)
	,maplist(varnumbers,Ss_sk,Ss).

%!	indexed_mergesort_sk(+List,-Sorted) is det.
%
%	Business end of indexed_mergesort/2.
%
%	Sorts skolemised lists to ignore the age of Prolog variables.
%
indexed_mergesort_sk([],[]):-
	!.
indexed_mergesort_sk([I-Xs],[I-Xs]):-
	!.
indexed_mergesort_sk(Ls,Ss):-
	!
	,split(Ls,Ls_R,Ls_L)
	,indexed_mergesort_sk(Ls_R,Ss_1)
	,indexed_mergesort_sk(Ls_L,Ss_2)
	,indexed_merge(Ss_1,Ss_2,Ss).


%!	split(+List,-Right,-Left) is det.
%
%	Split a list in two halves.
%
%	Divide step of indexed_mergesort/2. List should be a list of
%	key-value pairs, K-V, where K is an integer kay and V a list of
%	literals. Right and Left are the two halves of List.
%
split(Ls,Ls_R,Ls_L):-
	in_half(Ls,Ls,Ls_R,Ls_L).

%!	split(+List,+Buff,-Right,-Left) is det.
%
%	Business end of split/3.
%
in_half([],Rs,[],Rs):-
% for lists of even length
	!.
in_half([_],Rs,[],Rs):-
% for lists of odd length
	!.
in_half([_,_|T],[X|Ls],[X|Ls_R],Rs):-
	in_half(T,Ls,Ls_R,Rs).


%!	indexed_merge(+List1,+List2,-Merged) is det.
%
%	Merge two lists of key-value pairs.
%
%	Merge step of indexed_mergesort/2. List1 and List2 should be
%	lists of key-value pairs, K-V, where K is an integer key and V a
%	list of literals.
%
indexed_merge([],Ls,Ls):-
	!.
indexed_merge(Ls,[],Ls):-
	!.
indexed_merge([I-X|T1],[_K-Y|T2],[I-X|T]):-
% We could avoid comparison by comparing indices...
	merge_compare(X,Y,=)
	,!
	,indexed_merge(T1,T2,T).
indexed_merge([I-X|T1],[K-Y|T2],[I-X|T]):-
	merge_compare(X,Y,=<)
	,!
	,indexed_merge(T1,[K-Y|T2],T).
indexed_merge([I-X|T1],[K-Y|T2],[K-Y|T]):-
	merge_compare(X,Y,>)
	,indexed_merge([I-X|T1],T2,T).


%!	merge_compare(+Term1,+Term2,-Delta) is det.
%
%	Compare two terms up to renaming of variables.
%
merge_compare(A,B,D):-
	(   unifiable(A,B,_)
	->  D = (=)
	;   A @< B
	->  D = (=<)
	;   A @> B
	->  D = >
	).
