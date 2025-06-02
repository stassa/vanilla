:-module(simpleton_configuration, [clause_limit/1
                                  ,experiment_file/2
                                  ,max_invented/1
                                  ,listing_limit/1
                                  ]).

:-use_module(load_experiment_file).

/** <module> Configuration options for Simpleton.

*/

% Dynamic options can be set with set_simpleton_configuration_option/2.
:-dynamic clause_limit/1
         ,max_invented/1.

%!      clause_limit(?Limit) is semidet.
%
%       Limit the number of resolving clauses learned from each example.
%
%       Limit should be a natural number, including 0, or the atom 'inf'
%       representing positive infinity if a limit is not required.
%
clause_limit(2).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file(data('examples/hello_world.pl'),hello_world).
%experiment_file(data('simpleton_examples/anbn.pl'),anbn).
%experiment_file(data('simpleton_examples/even_odd.pl'),even_odd).


%!	max_invented(?Number) is semidet.
%
%	Maximum number of invented predicates.
%
%	Assumes clause_limit(K) where K > 1.
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


%!      metasubstitution_atoms(?What) is semidet.
%
%       What variables to store in metasubstitution atoms.
%
configuration:metasubstitution_atoms(existential).


%!      table_meta_interpreter(?Bool) is semidet.
%
%       Whether to table the Vanilla meta-interpreter, or not.
%
configuration:table_meta_interpreter(true).


%!      untable_meta_interpreter(?Bool) is semidet.
%
%       Whether to untable Vanilla between learning queries.
%
configuration:untable_meta_interpreter(true).


% Opens this configuration file and the current experiment file in the
% SWI-Prolog IDE or your system's $EDITOR if one is set.
/*
:- edit(simpleton)
  ,edit(simpleton_configuration)
  ,simpleton_configuration:experiment_file(P,_)
  ,edit(P).
*/

% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:- experiment_file(P,_)
  ,load_experiment_file(P).
