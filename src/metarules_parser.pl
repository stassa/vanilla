:-module(metarules_parser, [parsed_metarule/2
			   ]).

/** <module> Parser for atomic metarules.

The single predicate exported by this module, parsed_metarule/2,
implements a transducer to translate metarules in Vanilla's
user-friendly, high-level metarule format to its less user-friendly,
internal representation.

Metarules in the user-friendly high-level format are clauses of the
predicate metarule/2. The atom "metarule" is declared as an infix
operator so that users can define metarules like the following:

==
chain metarule 'P(x,y):- Q(x,z), R(z,y)'.
==

This format is then tranformed by parsed_metarule/3 to Vanilla's
internal reprsentation, in one of two forms, depending on the setting of
the configuration option metasubstitution_atoms, as follows:

==
?- metasubstitution_atoms(S), parsed_metarule(chain, _M), print_metarules([_M]).
m(chain,P,Q,R)/m(chain,X,Y,Z):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
S = existential_and_universal.

?- configuration:metasubstitution_atoms(S), metarules_parser:parsed_metarule(chain, _M), auxiliaries:print_metarules([_M]).
m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
S = existential.
==

More specifically.

If metasubstitution_atoms/1 is set to "existential_and_universal", then
the internal representation of metarules is a clause of the form Atom :-
Literal_1, ..., Literal_n where Atom is a term E/U, and each Literal_i
is a literal in a metarule. In the term E/U, E and U are encapsulated
metasubstitution atoms of the form S(Id,Var_1, ..., Var_n) where S is
the encapsulation predicate symbol set in the configuration option
encapsulation_predicate/1, Id is the atomic identifier of a metarule,
and each Var_i are the existentially or univesally quantified variables
in the metarule, for E and U, respectively.

If metasubstitution_atoms/1 is set to "existential" (i.e. existential
only) then the head of the encapsulatead metarule is a single
encapsulated metasubstitution atom, as E above, with only the
existentially quantified variables in the metarule.

The motivation for having two different representations is efficiency:
when metasubstitution atoms are compound terms E/U, there will be one or
more U's for each E, each of which will take some space in memory. For
_very_ _large_ programs this can be an issue, particularly when turning
on tabling.

The choice of representation should be left to the implement-or of a
learing system. For this reason the metasubstitution_atoms/1 option is
made multifile and dynamic in the configuration to allow it to be set in
individual learner system configurations.


User-level metarule format
--------------------------

User level metarules must conform to the following format:

==
<Id> metarule <atomic metarule>
==

Where:

Id is an atomic identifier for the metarule used as a reference to find
its definition in the program database. Id must be unique to avoid
unexpected results when learning.

metarule is the metarule/2 functor, which is declared as an infix
operator to allow the above syntax.

And, "atomic metarule" is an atomic representation of a second-order
metarule, as in the Chain example, in the previous section, above.

The atomic representatin of a second-order metarule in a metarule/2
clause must obey the following rules:

* Each existentially quantified, second-order variable must be
  represented by a single upper-case alphabetic character.

* Each existentially quantified, first-order variable must be
  represented as a single, upper-case alphabetic character.

* Each universally quantified, first-order variable must be represented
  as a single, lower-case alphabetic characer.

* The sets of characters used for each type of variable: existentially
  or universally quantified, first- or second-order, must be disjoint.

Note that the user-level representation does not depend on any options
chosen for the internal representation, such as
metasubstitution_atoms/1.

*/


%!	parsed_metarule(+Id,-Metarule) is det.
%
%	Parse an Atomic Metarule.
%
%	Id is the metarule identifier of a metarule/2 clause in the
%	program database.
%
%	Metarule is a transformation of Atomic to an expanded metarule
%	in Vanilla's representation, as a clause with a metasubstitution
%	atom as its head literal and the encapsulated literals of the
%	metarule in Atomic as its body literals. In the encapsulated
%	literals of the expanded metarule predicate symbols, constants
%	and variables in Atomic have been replaced with appropriate
%	first-order, universally quantified variables and universally
%	and existentially quantified variables are collected in the
%	arguments of the metasubstitution atom at the head of the
%	clause.
%
%	Example:
%	==
%	?- metarule(chain, M).
%	M = 'P(x,y):- Q(x,z), R(z,y)'.
%
%	?- metarules_parser:parsed_metarule(chain, _M), print_metarules([_M]).
%	m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
%	true.
%	==
%
parsed_metarule(Id,M):-
	(   \+ configuration:metarule(Id, _)
	->  throw('Unknown metarule ID':Id)
	;   configuration:metarule(Id, M_)
	)
	,parsed_metarule(Id,M_,M).


%!	parsed_metarule(+Id,+Atomic,-Metarule) is det.
%
%	Business end of parsed_metarule/2.
%
%	Parses an Atomic Metarule.
%
%	Id is the metarule identifier of a metarule/2 clause in the
%	program database.
%
%	Atomic is an atom reprsenting a metarule, e.g. 'P(x,y):- Q(y,x)'
%	etc, the atomic metarule in the metarule/2 clause with the given
%	Id.
%
%	Metarule is a transformation of Atomic to an expanded metarule
%	in Vanilla's internal representation with Id as its identifier.
%
%	The form of Metarule depends on the setting of the configuration
%	option metasubstitution_atoms/1. When that option is set to
%	"existential_and_universal", expanded metarules have
%	metasubstitution atoms in their heads that store the
%	substitutions of both existentially and universally quantified
%	varables. When the option is set to "existential" only
%	existentially quantified variables' substitutions are kept.
%
%	@tbd This predicate can be used, inadvertently, to rename
%	metarules. Since this will probably not be the intention of
%	calling this predicate, it should be accessed only through the
%	interface of parsed_metarule/2, that ensures the correct
%	identifier is used, i.e. the identifier associated with Atomic
%	in a metarule/2 clause in the program database.
%
parsed_metarule(Id,M,M1):-
	configuration:metasubstitution_atoms(existential_and_universal)
	,!
	,configuration:encapsulation_predicate(E)
	,atom_chars(M,Cs)
	,remove_whitespace(Cs,Cs_)
	,once(phrase(clause_(Ls),Cs_))
	,existential_vars(Ls,Es)
	,universal_vars(Ls,Us)
	,maplist(args_vars,[Es,Us],[Es_,Us_])
	,Eh =.. [E,Id|Es_]
	,Uh =.. [E,Id|Us_]
	,literals_clause(Ls, M_)
	,varnumbers(Eh/Uh:-M_, M1).
parsed_metarule(Id,M,M1):-
	configuration:metasubstitution_atoms(existential)
	,configuration:encapsulation_predicate(E)
	,atom_chars(M,Cs)
	,remove_whitespace(Cs,Cs_)
	,once(phrase(clause_(Ls),Cs_))
	,existential_vars(Ls,Es)
	,args_vars(Es,Es_)
	,A =.. [E,Id|Es_]
	,literals_clause(Ls, M_)
	,varnumbers(A:-M_, M1).



%!	remove_whitespace(+Chars,-Cleaned) is det.
%
%	Remove whitespace characters from a list of characters.
%
remove_whitespace(Cs,Cs_):-
	findall(C
	       ,(member(C,Cs)
		,char_type(C,graph)
		)
	       ,Cs_).


%!	existential_vars(+Literals,-Existential) is det.
%
%	Collect Existentially quantified variables in Literals.
%
%	Literals is a list of lists [L1,...,Ln], where each sublist Li
%	is a list [S,A1,...,Am], representing a literal where S is an
%	atom representing the literal's predicate symbol and each Ai is
%	an atom representing one of its arguments.
%
%	Existential is the list of existentially-quantified variables in
%	Literals.
%
%	This predicate collects the upper-case atoms in Literals, under
%	the assumption that the atomic metarule from which those
%	literals were extracted represents existentially quantified
%	terms in upper-case and universally quantified terms in
%	lower-case, and there is no overlap between the two.
%
existential_vars(Ls,Es):-
	flatten(Ls, Ls_)
	,args_of_case(Ls_,upper,Es).


%!	universal_vars(+Literals,-Universal) is det.
%
%	Collect Universally quantified variables in Literals.
%
%	Counterpart to existential_vars/2 for universallyq uantified
%	variables.
%
%	Literals is a list of lists [L1,...,Ln], where each sublist Li
%	is a list [S,A1,...,Am], representing a literal where S is an
%	atom representing the literal's predicate symbol and each Ai is
%	an atom representing one of its arguments.
%
%	Universal is the list of universally-quantified variables in
%	Literals.
%
%	This predicate collects the lower-case atoms in Literals, under
%	the assumption that the atomic metarule from which those
%	literals were extracted represents existentially quantified
%	terms in upper-case and universally quantified terms in
%	lower-case, and there is no overlap between the two.
%
universal_vars(Ls,Us):-
	flatten(Ls, Ls_)
	,args_of_case(Ls_,lower,Us).


%!	args_of_case(+Characters,+Case,-Cased) is det.
%
%	Collect Characters of the given Case.
%
%	Characters is a list of atomic characters. Case is one of
%	[upper, lower]. Cased is the list of characters in Characters
%	of the specified case.
%
%	Used to collect existentially and universally quantified
%	variables from an atomic metarule.
%
args_of_case(As,C,As_C):-
	args_of_case(As,C,[],As_C).

%!	args_of_case(+Chars,+Case,+Acc,-Cased) is det.
%
%	Business end of args_of_case/3.
%
args_of_case([],_UL,Acc,Cs):-
	!
	,reverse(Acc,Cs).
args_of_case([A|As],UL,Acc,Bind):-
% C is a letter of case UL
% The rest, we don't care
	atom_codes(A,[C|_Cs])
	,char_type(C,UL)
	,\+ memberchk(A,Acc)
	,!
	,args_of_case(As,UL,[A|Acc],Bind).
args_of_case([_A|As],UL,Acc,Bind):-
	args_of_case(As,UL,Acc,Bind).


%!	literals_clause(+Literals,-Encapsulated) is det.
%
%	Convert between a list of Literals and a Clause.
%
%	Literals is a list of lists: [L1,...,Ln] representing the
%	literals of a metarule, as returned by clause_//1. Each
%	sub-list is a list: [S,A1,...Am] representing a literal, where S
%	is the predicate symbol of the literal and each Ai are its
%	arguments.
%
%	Encapsulated is a conjunction of encapsulated literals (L1,...,Ln)
%	where each Li is an atom m(S,A1,...,Am), such that S is the
%	predicate symbol in the sub-list of Literals representing the
%	literal Li and each Ai is one of its arguments.
%
literals_clause(Ls,C):-
	encapsulated_literals(Ls, [], Ls_)
	,once(list_tree(Ls_, C)).

%!	encapsulated_literals(+Literals,+Acc,-Encapsulated) is det.
%
%	Business end of literals_clause/2.
%
encapsulated_literals([],Acc,Ls):-
	!
	,reverse(Acc,Ls).
encapsulated_literals([L|Ls],Acc,Bind):-
	configuration:encapsulation_predicate(E)
	,args_vars(L,Vs)
	,L_ =.. [E|Vs]
	,encapsulated_literals(Ls,[L_|Acc],Bind).



%!	args_vars(+Literal,-Variables) is det.
%
%	Skolemise the Variables of a Literal.
%
%	Literal is a list, [S,A1,...,An], representing a literal, such
%	that S is an atom reprsenting the predicate symbol, and each Ai
%	is an atom representing an argument, of the literal.
%
%	Variables is a list of terms '$VAR'(C) where C is the up-cased
%	atom at the same position in the list Literal as the term
%	'$VAR'(C).
%
%	Use this predicate to replace each atomic term in Literal with a
%	'$VAR'(C) terms that can be later converted to a variable with
%	the same name as the atomic term.
%
args_vars(Ls,Vs):-
	args_vars(Ls,[],Vs).

%!	args_vars(+Literals,+Acc,-Vars) is det.
%
%	Business end of args_vars/2.
%
args_vars([],Acc,Vs):-
	!
	,reverse(Acc,Vs).
args_vars([A|As],Acc,Vs):-
	upcase_atom(A,U)
	,atom_codes(U,[C])
	,args_vars(As,['$VAR'(C)|Acc],Vs).


/* The following is a grammar for atomic metarules in metarule/2 clauses
*/

%!	clause_(Literals)// is nondet.
%
%	A list of Literals in a clause.
%
%	Start symbol of atomic metarule grammar.
%
%	@tbd: Add some logic to figure out why this failed, if it
%	ifails, and raise an informative error to help the user define
%	the metarule they are trying to define.
%
clause_([L|Ls]) --> head_(L), neck, body_(Ls).
% Unit clause metarule
% Again, parse multi-literal clause first.
clause_([L]) --> head_(L).

%!	head// is semidet.
%
%	A head literal.
%
head_(L) --> literal(L).

%!	neck// is semidet.
%
%	The ":-" neck symbol connecting a clause head and body.
%
%	The neck symbol may be surrounded by spaces on either side, or
%	both.
%
neck --> [:,-].

%!	body// is nondet.
%
%	A body literal
%
body_(Ls) --> literals(Ls).

%!	literals// is nondet.
%
%	A set of literals in a clause.
%
literals([L|Ls]) --> literal(L), comma, literals(Ls).
% Parse all literals first!
literals([L]) --> literal(L).

%!	literal// is det.
%
%	A literal in a clause.
%
literal([S|As]) --> symbol(S), left, args(As), right.

%!	symbol// is det.
%
%	A predicate symbol.
%
symbol(S) --> [S], { atom(S) }.

%!	args// is det.
%
%	The list of arguments of a literal.
%
args([A|As]) --> arg_(A), comma, args(As).
args([A]) --> arg_(A).

%!	arg_// is det.
%
%	An argument of a literal.
%
arg_(A) --> [A], { atom(A) }.

%!	left// is semidet.
%
%	An opening, left-bracket.
%
left --> ['('].

%!	left// is semidet.
%
%	A closing, right-bracket.
%
right --> [')'].

%!	comma// is semidet.
%
%	A comma.
%
comma --> [','].
