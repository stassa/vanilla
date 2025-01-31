:-module(metagol_configuration, [depth_limits/2
                                ,experiment_file/2
                                ,max_invented/1
                                ,listing_limit/1
                                ,order_constraints/5
                                ]).

:-use_module(load_experiment_file).

/** <module> Configuration options for Metagol.

*/

:-multifile order_constraints/5.

%!      clause_limit(?Min,?Max) is semidet.
%
%       Limit the number of resolving clauses learned from each example.
%
%       Min and Max should be natural numbers, including 0, or the atom
%       'inf' representing positive infinity if a limit is not required.
%
depth_limits(0,2).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file(data('examples/hello_world.pl'),hello_world).
%experiment_file(data('examples/anbn.pl'),anbn).
%experiment_file(data('examples/even_odd.pl'),even_odd).
%experiment_file(data('examples/parity.pl'),parity).


%!	max_invented(?Number) is semidet.
%
%	Maximum number of invented predicates.
%
%	Must be at most Max - 1 for depth_limits(Min,Max).
%
max_invented(0).


%!      listing_limit(?Limit) is semidet.
%
%       Limit the clauses printed when a MIL problem is listed.
%
%       Limit is a number, limiting the number of clauses of examples
%       and BK that will be printed to the output when a MIL problem is
%       listed. Affects list_mil_problem/1 and
%       list_encapsulated_problem/1.
%
%       Limit should be a positive integer. It can also be the atom
%       'inf' representing positive infinity. If Limit is 'inf', then no
%       limit is imposed on the printed information.
%
listing_limit(10).


%!      order_constraints(+Id,+Existential,+FO,+Lexicographic,+Interval)
%!      is semidet.
%
%	Metarule order constraints for Metagol.
%
%       Id is an atom, the id of a metarule for which these constraints
%       are defined.
%
%       Existential is the list of existentially quantified variables in
%       the identified metarule. This list must include both
%       second-order and first-order existentially quantified variables
%       (if any of the latter are included in the identified metarule).
%
%       FO is the list of first-order variables in the identified
%       metarule. This list must include both universally and
%       existentially quantified first-order variables (if any of the
%       latter are included in the identified metarule).
%
%       Lexicographic is a list of lexicographic order constraints over
%       the second-order varibles in Existential. Lexicographic order
%       constraints are defined as pairs P>Q where P, Q are second-order
%       variables included in the list Existentials. The meaning of P>Q
%       is that any predicate symbol bound to P during learning is above
%       any predicate symbol bound to Q, in the lexicographic order of
%       predicate symbols.
%
%       Interval is a list of interval order constraints over the
%       first-order variables in a metarule. Interval order constraints
%       are defined as pairs X>Y, with the same meaning as for
%       lexicographic order constraints, except here X and Y are
%       first-order variables that bind to constants, rather than
%       predicate symbols, during learning.
%
%       __Motivation__
%
%       Order constraints are used in the originally described version
%       of Metagol to ensure termination when learning recursion. They
%       impose a total ordering over the Herbrand base used to restrict
%       the instantiations of variables in metarules.
%
%       Lexicographic order constraints ensure termination of the
%       learning procedure when learning recursive hypotheses. Interval
%       order constraints are necessary when hypotheses include
%       mutually recursive predicates, most notably when an auxiliary
%       predicate that is mutually recursive with the target predicate
%       must be invented in order to learn the target.
%
%       Lexicographic and interval order constraints are more fully
%       explained in the following reference:
%       ==
%       Meta-interpretive learning of higher-order dyadic datalog:
%       predicate invention revisited
%       Mach Learning (2015) 100:49-73
%       Muggleton, et al.
%       ==
%
%       @tbd Order constraints require a total ordering over the
%       predicate symbols and constants in a learning problem. Such an
%       ordering is derived automatically from the order in which
%       background predicates' definitions and examples are declared in
%       an experiment file. This is achieved by a call to
%       order_constraints/3. See that predicate for more information,
%       including the ability to manually define a total ordering.
%       Ouch.
%
%       @bug Currently, only lexicographic order constraints are
%       implemented in Metagol. Defining interval order constraints
%       won't have any effect at all until this is resolved.
%
order_constraints(unit,_Ss,_Fs,[],[]).
order_constraints(projection_21,[P,Q],_Fs,[P>Q],[]).
order_constraints(projection_12,[P,Q],_Fs,[P>Q],[]).
order_constraints(inverse,[P,Q],_Fs,[P>Q],[]).
order_constraints(identity,[P,Q],_Fs,[P>Q],[]).
order_constraints(chain,[P,Q,R],[X,Y,Z],[P>Q,P>R,Q>R],[X>Z,Z>Y]).
order_constraints(tailrec,[P,Q],[_X,_Y,_Z],[P>Q],[]).
order_constraints(precon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(postcon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(switch,[P,Q,R],_Fs,[P>Q,P>R],[]).


%!      metasubstitution_atoms(?What) is semidet.
%
%       What variables to store in metasubstitution atoms.
%
configuration:metasubstitution_atoms(existential_and_universal).


%!      table_meta_interpreter(?Bool) is semidet.
%
%       Whether to table the Vanilla meta-interpreter, or not.
%
configuration:table_meta_interpreter(false).


%!      untable_meta_interpreter(?Bool) is semidet.
%
%       Whether to untable Vanilla between learning queries.
%
configuration:untable_meta_interpreter(true).


% Opens this configuration file and the current experiment file in the
% SWI-Prolog IDE or your system's $EDITOR if one is set.
/*
:- edit(metagol)
  ,edit(metagol_configuration)
  ,metagol_configuration:experiment_file(P,_)
  ,edit(P).
*/

% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:- experiment_file(P,_)
  ,load_experiment_file(P).
