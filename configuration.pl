:-module(configuration, [encapsulation_predicate/1
                        ,example_clauses/1
                        ,fetch_clauses/1
                        ,invented_symbol_prefix/1
                        ,learner/2
                        ,metarule/2
                        ,metarule_constraints/2
                        ,metarule_formatting/1
			,symbol_range/2
                        ,table_meta_interpreter/1
                        ,untable_meta_interpreter/1
			,op(100,xfx,metarule)
                        ]).

/** <module> Configuration options for Vanilla.

*/

% Allows experiment files to define their own, special metarules.
:-multifile metarule/2
           ,metarule_constraints/2
           ,table_meta_interpreter/1
           ,untable_meta_interpreter/1.

%!      encapsulation_predicate(+Symbol) is semidet.
%
%       The Symbol used in encapsulation predicates.
%
%       Symbol is an atom used as the symbol of encapsulated literals in
%       clauses of examples, background knowledge, metarules and
%       invented predicates, alike.
%
encapsulation_predicate(m).


%!      example_clauses(?What) is semidet.
%
%       What to do with example clauses.
%
%       This option determines how Vanilla treats examples that are
%       given as definite clauses with one or more body literals (rather
%       than ground atoms), i.e. "example clauses".
%
%       What is one of [bind,call].
%
%       If What is "bind", example clauses are bound to each instance of
%       a metarule where that is possible.
%
%       If What is "call", the head literal of each example clause is
%       bound to the enapsulated head literal of a metarule, then the
%       body literals of the example clause are called. This may
%       result in the universally quantified variables in the head of
%       the clause, and so the encapsulated head literal of the
%       metarule, to be bound.
%
%       Use "bind" when you have a set of definite clauses that you want
%       to transform to instances of a metarule.
%
%       Use "call" when you want to use a set of definite clauses with
%       bodies to generate examples.
%
%example_clauses(bind).
example_clauses(call).


%!      fetch_clauses(?Whence) is semidet.
%
%       Where to fetch clauses from during meta-interpretation.
%
%       Clauses are "fetched" during meta-interpretation with Vanilla by
%       the predicate clause/7. A different clause of this predicate
%       fetches clauses from different sources and this option is used
%       to control which sources will be allowed in a learning attempt.
%
%       Whence can be any subset of: [builtins,bk,hypothesis,metarules].
%       These are interpreted as follows:
%
%       * builtins: clauses will be fetched from built-in and library
%       predicates loaded into memory. This option should be needed most
%       of the time when built-ins and library predicates are used in
%       background knowledge definitions.
%
%       * bk: clauses will be fetched from the set of clauses of the
%       definitions of predicates listed in the second argument of
%       background_knowledge/2 in the current experiment file.
%
%       * hypothesis: clauses will be fetched from the set of clauses
%       added to a hypothesis so-far. These clauses are stored in the
%       metasubstitution accumulator in prove/6 as metasubstitution
%       atoms and must be expanded to be used in a proof (this is
%       handled internally by prove/6). Enabling this setting allows
%       arbitrary recursion between clauses in the learned hypothesis
%       and all other sources where clauses are fetched from.
%
%       * metarules: clauses will be fetched from the list of metarules
%       given to prove/6. Enabling this setting allows construction of
%       new clauses by the Vanilla meta-interpreter by resolution with
%       metarules. This setting should probably never be removed
%       because without it clause construction is not possible. This
%       includes clauses of invented predicates.
%
fetch_clauses(all).
%fetch_clauses([builtins,bk,hypothesis,metarules]).
%fetch_clauses([bk,hypothesis,metarules]).
%fetch_clauses([builtins,hypothesis,metarules]).
%fetch_clauses([builtins,bk,metarules]).
%fetch_clauses([builtins,bk,hypothesis]).
%fetch_clauses([hypothesis]).
%fetch_clauses([metarules]).


%!      invented_symbol_prefix(?Prefix) is semidet.
%
%       Prefix used to form invnented predicates' symbols.
%
%       Invented predicate symbols are created automatically by
%       prepending Prefix to a number between 1, and the value of the
%       configuration option max_invented/1.
%
%       The default-ish Prefix for this concatenation is the dollar
%       symbol, '$'. This character is also defined as a prefix operator
%       with precedence 1 and this can cause trouble in conjunction with
%       the tokenisation in SWI-Prolog. This configuration option allows
%       the user to set their own invented predicate symbol.
%
invented_symbol_prefix('$').


%!	metarule(?Id,?Name,?Clause) is semidet.
%
%	A named Metarule.
%
%       Metarules are second-order definite clauses without function
%       symbols other than constants, i.e. datalog. They are given as
%       part of a second-order background theory along with a set of
%       first-order definite clauses (i.e. a Prolog program). The
%       background theory is used in an SLD-refutation of a set of
%       example atoms, performed by prove/7 in Vanilla.
%
%       Metarules are second-order because they have variables in place
%       of predicate symbols of their literals. In particular, those are
%       second-order variables existentially quantified over the set of
%       predicate symbols. During Resolution, by unification, the
%       second-order variables in the metarules are bound to predicate
%       symbols in the first-order background theory, the examples, and
%       any invented predicate symbols. The result of this binding is a
%       first-order program that entails (logically implies) the refuted
%       example atoms with respect to the first-order background
%       knowledge.
%
abduce metarule 'P(X,Y)'.
unit metarule 'P(x,y)'.
projection_21 metarule 'P(x,x):- Q(x)'.
projection_12 metarule 'P(x):- Q(x,x)'.
identity metarule 'P(x,y):- Q(x,y)'.
inverse metarule 'P(x,y):- Q(y,x)'.
chain metarule 'P(x,y):- Q(x,z), R(z,y)'.
tailrec metarule 'P(x,y):- Q(x,z), P(z,y)'.
precon metarule 'P(x,y):- Q(x), R(x,y)'.
postcon metarule 'P(x,y):- Q(x,y), R(y)'.
switch metarule 'P(x,y):- Q(x,z), R(y,z)'.
swap metarule 'P(x,y):- Q(z,x), R(z,y)'.
% Metarules with abductible first-order existentially quantified
% variables. Also see abduce metarule above.
chain_abduce_x metarule 'P(X,y):- Q(X,z), R(z,y)'.
chain_abduce_y metarule 'P(x,Y):- Q(x,z), R(z,Y)'.
chain_abduce_z metarule 'P(x,y):- Q(x,Z), R(Z,y)'.
projection_21_abduce metarule 'P(X,X):- Q(X)'.
projection_12_abduce metarule 'P(X):- Q(X,X)'.
precon_abduce metarule 'P(X,y):- Q(X), R(X,y)'.
postcon_abduce metarule 'P(x,Y):- Q(x,Y), R(Y)'.


%!      learner(?Name,?Path) is semidet.
%
%       The learning system to load at startup.
%
learner('Metagol',lib(metagol/metagol)).
%learner('Poker',lib(poker/poker)).


%!	metarule_constraints(+Metasubstitution,+Goal) is nondet.
%
%	A Goal to be called when Metasubstitution is matched.
%
%       This option is declared dynamic and multifile that constraints
%       may be declared individually by experiment files, as needed. A
%       few examples are given below.
%
:- dynamic metarule_constraints/2.
/*
% Simple constraint excluding left-recursive clauses that are instances of
% a metarule with any Id and having two existentially quantified
% variables. Matches e.g. Tailrec and Identity:
%
configuration:metarule_constraints(m(_Id,P,P),fail).
*/
/*
% Simple constraint excluding left-recursive clauses that are instances of
% a metarule with any Id and having three existentially quantified
% variables. Matches e.g. Chain, Switch, Swap:
%
configuration:metarule_constraints(m(_Id,P,P,_),fail).
*/
/*
% Anti-recursion constraint - excludes recursive clauses
% Does not take into account invented or metarules with existentially
% quantified secod-order variables:
%
configuration:metarule_constraints(m(tailrec,_,_),fail).
configuration:metarule_constraints(M,fail):-
	configuration:encapsulation_predicate(E)
        ,M =.. [E,Id,P|Ps]
        ,\+ memberchk(Id,[abduce
			 ,unit
			 ,projection_21
			 ,projection_12])
	,memberchk(P,Ps).
*/
/*
% McCarthyite constraint - excludes left-recursive metasubstitutions
% Named after the other McCarthy. The senator, not the computer
% scientist.
%
configuration:metarule_constraints(M,fail):-
	configuration:encapsulation_predicate(E)
        ,M =.. [E,Id,P,P|_Ps]
        ,\+ memberchk(Id,[abduce
                      ,unit
                      ,projection_21
                      ,projection_12]).

*/
/*
% Lexicographic order constraint.
% Imposes total ordering on the Herbrand base.
% Calls src/subsystems/thelma/thelma_configuration:order_constraints/5.
% Needs problem-specific ordering of the predicate signature.
%
configuration:metarule_constraints(M,B):-
	configuration:encapsulation_predicate(E)
        ,debug(lex,'Testing constraint for metasub: ~w',M)
        ,M =.. [E,Id|Ps]
        %#REPLACE WITH PROBLEM-SPECIFIC ORDERING OF PREDICATE SIGNATURE#
        ,PS = [s,a,b] % Example ordering for a^nb^n
	,debug(lex,'Predicate signature: ~w',[PS])
        ,metagol_configuration:order_constraints(Id,Ps,Fs,STs,FTs)
	,debug(lex,'Order constraints: ~w-~w',[STs,FTs])
        ,(   metagol:order_tests(PS,Fs,STs,FTs)
	 ->  B = true
	    ,debug(lex,'Passed constraint test!',[])
	 ;   B = false
	    ,debug(lex,'Failed constraint test!',[])
	 ).
*/


%!      metarule_formatting(?How) is semidet.
%
%       How to print metarules learned with new_metarules/1.
%
%       How is one of: [quantified, user_friendly, expanded].
%
%       Option "quantified" prints metarules with quantifiers and in
%       formal notation found in the MIL literature. Use this option to
%       compare metarules with the ones in the literature, or just to
%       get a more clear explanation of a metarule.
%
%       Option "user_friendly" prints metarules in Louise's user-level,
%       and user-friendly format of metarules in experiment files. Use
%       this option when you want to copy a metarule and later paste it
%       to an experiment file. For example, this option is handy when
%       you learn metarules with TOIL and you want to reuse them in an
%       experiment file.
%
%       Option "expanded" prints metarules in Louise's internal format,
%       encapsulated and expanded, with an encapsulated metasubstitution
%       atom in the head. Use this option to inspect what Louise
%       actually sees when you declare a metarule.
%
%       Note that the metarules printed with option "expanded" cannot be
%       directly copy/pasted into an experiment file. Or, well, sure
%       they can... but they won't be picked up by experiment_data/5 and
%       you will probably see errors.
%
metarule_formatting(quantified).
%metarule_formatting(user_friendly).
%metarule_formatting(expanded).


%!	symbol_range(?Type,?Symbols) is semidet.
%
%	A list of Symbols to pretty-print predicates or variables.
%
%	Type is one of [predicate,variable], denoting the type of
%	symbols in the currenr range.
%
%	Symbols is a list of symbols of the given Type.
%
%	The atoms in list Symbols is used to assign names to the
%	variables in a metarule for pretty-printing.
%
%	Warning:
%	--------
%
%	symbol_range/2 must have exactly two clauses: one for the
%	symbols to be used as names for second-order existentially
%	quantified variables, and one to be used as names for
%	first-order existentially and universally quantified variables.
%
%	You can change each Symbols list as you see fit, but _do not
%	remove or add clauses_ to symbol_range/2!
%
%	Used by
%	-------
%
%	This predicate is used by predicates in the transitive closure
%	of print_metarules/1 and print_metarule/1, in particular,
%	numbered_symbol/3, which uses this to generate lists of
%	predicate symbols to be assigned to variables in metarules
%	according with their (first- or second-) order.
%
symbol_range(predicate, ['P','Q','R','S','T']).
symbol_range(variable, ['X','Y','Z','U','V','W']).
% Silly. Don't use.
%symbol_range(predicate, ['Alice','Bob','Carol']).
%symbol_range(variable, ['Smith','Brown','Carpenter','Miller','Green']).


%!      table_meta_interpreter(?Bool) is semidet.
%
%       Whether to table the Vanilla meta-interpreter, or not.
%
%       Checked by refresh_tables/1 to decide whether to table or
%       untable the prove/7 Vanilla meta-interpreter, or not.
%
%       This option and untable_meta_interpreter/1 are made available so
%       that the user doesn't have to edit the source code of learning
%       predicates to control tabling and untabling behaviour. They are
%       declared dynamic and multifile so that they can be defined
%       separately in the configuration file of a MIL-learner
%       implemented with Vanilla.
%
%       See refresh_tables/1 for more context.
%
:-dynamic(table_meta_interpreter/1).


%!      untable_meta_interpreter(?Bool) is semidet.
%
%       Whether to untable Vanilla between learning queries.
%
%       This predicate is checked by refresh_tables/1 to decide whether
%       to table or untable the prove/7 Vanilla meta-interpreter, or
%       not.
%
%       This option and table_meta_interpreter/1 are made available so
%       that the user doesn't have to edit the source code of learning
%       predicates to control tabling and untabling behaviour. They are
%       declared dynamic and multifile so that they can be defined
%       separately in the configuration file of a MIL-learner
%       implemented with Vanilla.
%
%       See refresh_tables/1 for more context.
%
:- dynamic(untable_meta_interpreter/1).


:-learner(N,P)
  ,format('Loading ~w~n',[N])
  ,use_module(P).
