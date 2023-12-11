:-module(metagol_configuration, [clause_limits/2
                                ,experiment_file/2
                                ,max_invented/1
                                ]).

:-use_module(load_experiment_file).

/** <module> Configuration options for Metagol.

*/

%!      clause_limit(?Limit) is semidet.
%
%       Limit the number of resolving clauses learned from each example.
%
%       Limit should be a natural number, including 0, or the atom 'inf'
%       representing positive infinity if a limit is not required.
%
clause_limits(1,2).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file(data('examples/hello_world.pl'),hello_world).


%!	max_invented(?Number) is semidet.
%
%	Maximum number of invented predicates.
%
%	Assumes clause_limit(K) where K > 1.
%
max_invented(0).

% Opens this configuration file and the current experiment file in the
% SWI-Prolog IDE or your system's $EDITOR if one is set.
:- edit(metagol)
  ,edit(metagol_configuration)
  ,metagol_configuration:experiment_file(P,_)
  ,edit(P).

% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:- experiment_file(P,_)
  ,load_experiment_file(P).
