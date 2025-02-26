:-module(louise_configuration, [clause_limit/1
                               ,experiment_file/2
                               ,gestalt/1
                               ,listing_limit/1
                               ,max_invented/1
                               ,max_error/2
                               ,recursive_reduction/1
                               ,reduction/1
                               ,resolutions/1
                               ,respecialise/1
                               ,tautology/1
                              ]).

:-use_module(load_experiment_file).
:-reexport(lib(louise/program_reduction/reduction_configuration),
	   except([resolutions/1])).

/** <module> Configuration options for Louise.

*/

%!      clause_limit(?Limit) is semidet.
%
%       Limit the number of resolving clauses learned from each example.
%
%       Limit should be a natural number, including 0, or the atom 'inf'
%       representing positive infinity if a limit is not required.
%
clause_limit(1).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file(data('examples/hello_world.pl'),hello_world).
%experiment_file(data('examples/anbn.pl'),anbn).
%experiment_file(data('examples/even_odd.pl'),even_odd).
%experiment_file(data('benchmarks/robots.pl'),robots).


%!      gestalt(?Bool) is semidet.
%
%       Whether to gestalt the Top Program or not.
%
%       This option controls whether invented predicates in the Top
%       program are named apart using rename_all_invented/2, in
%       louise.pl, or not. Setting this option to "true" leaves invented
%       predicates unchanged while setting it to "false" renames them.
%
%       The effect of setting this option to "true" is that the Top
%       Program is more general than the set of its sub-hypotheses,
%       while the effect of setting this option to "false" is that the
%       Top program is exactly as general as the set of its
%       sub-hypotheses.
%
%       The reason for the shift in generality is that, when this option
%       is true, invented predicates in sub-hypotheses of the Top
%       Program share the same set of symbols. This means that clauses
%       of invented predicates constructed independently in different
%       branches of the inductive proof, can now resolve with each other
%       thus potentially increasing the success set of their
%       combination.
%
%       Conversely, when this option is false, invented predicates
%       constructed independently, in different branches of the
%       inductive proof, are named apart with an additional numerical
%       suffix, so they cannot resolve with clauses of each other and
%       cannot generalise beyond their individual success set.
%
%       When to gestalt the Top Program
%       -------------------------------
%
%       When the Top Program consists of a set of sub-hypotheses that
%       each have the same success-set as the others the option
%       gestalt(false) is safest because the Top Program then has eactly
%       the success set of each of its sub-hypotheses. Setting
%       gestalt(true) in that case risks over-generalising by creating
%       interactions between sub-hypotheses not derived from the
%       training examples.
%
%       Conversely, when the Top Program consists of sub-hypotheses that
%       do not all have the same success-set, it can be safe to set
%       gestalt(true). In that case, allowing sub-hypotheses to resolve
%       with each other can increase the generality of the Top Program
%       beyond what can only be derived from the training examples. This
%       can also help learn a more accurate Top Program when a smaller
%       clause_limit is selected, which may not be large enough to learn
%       a sub-hypothesis from each positive example. In that case, the
%       combined success-set of the gestalt Top Program can be general
%       enough to "cover" all positive examples.
%
%       As a downside, when gestalt(true) is selected it is possible
%       that the generality of the Top Program increases so that it now
%       "covers" _negative_ examples not previously covered by any
%       sub-hypothesis. Thus, there is a tradeoff in setting this option
%       and it may have to be fine-tuned in an iterative process.
%
%       What is a gestalt?
%       ------------------
%
%       In psychology, the word "gestalt" is used to mean that "the
%       whole is greater than the sum of its parts". In this option it
%       is used to describe how the Top Program can be more general than
%       the sum of its parts, i.e. its component sub-hypotheses.
%
gestalt(true).
%gestalt(false).


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


%!	max_invented(?Number) is semidet.
%
%	Maximum number of invented predicates.
%
%	Assumes clause_limit(K) where K > 1.
%
max_invented(0).


%!      max_error(?Hypothesis,?Clause) is det.
%
%       Maximum error allowed for a Hypothesis and each Clause in it.
%
%       "Error" in this context means the number of negative examples
%       entailed by a clause, or a Hypothesis, with respect to
%       background knowledge. Errors of that type are more formally
%       known as "inconsistencies" in ILP terminology.
%
%       Hypothesis takes precedence over Clause. What this means is that
%       if Clause > Hypothesis, then the maximum number of negative
%       examples a clause is allowed to entail, with respect to
%       background knowledge, before it is discarded, is equal to
%       Hypothesis, while the maximum number of negative examples
%       entailed by a hypothesis remains equal to Hypothesis.
%
%       This option allows Louise to behave similar to Aleph, and
%       similar systems, with the setting "error" set to something other
%       than 0. Its purpose is to allow more natural results when
%       learning from datasets designed for propositional learners.
%
%       @tbd This option affects specialise/3 but is currently taken
%       into account only with clause_limit(K=1). If K > 1, this option
%       has no effect. This makes sense because clause_limit(1) is
%       sufficient for propositional-style learning problems, whereas
%       clause_limit(K > 1) is for more relational-style problems where
%       errors are much less tolerable and noise is not that common
%       anyway. This may change in a future version if there is a need
%       for it.
%
max_error(0,0).


%!	recursive_reduction(?Bool) is semidet.
%
%	Whether to reduce the Top program recursively or not.
%
%	Setting Bool to true enables recursie reduction of the Top
%	program. Recursive reduction means that the result of each
%	reduction step is given as input to the reduction algorithm in
%	the next step (also known as "doing the feedbacksies").
%
%	Recursive reduction can result in a stronger reduction in less
%	time, with a lower setting for resolutions/1 (in fact, the same
%	amount of reduction can take less time exactly because the
%	resolutions/1 setting can be set to a lower value).
%
%	Recursive reduction is more useful when the Top program is large
%	and many resolution steps are required to remove all redundancy
%	from it.
%
recursive_reduction(false).
%recursive_reduction(true).


%!	reduction(?Method) is semidet.
%
%	Select a Method for Top program reduction.
%
%	One of:
%	* none: no reduction.
%	* plotkins: discard logically redundant clauses by application
%	  of Plotkin's program reduction.
%       * subhypotheses: separate each sub-hypothesis included in the
%         Top program and return it on backtracking.
%
%reduction(none).
reduction(plotkins).
%reduction(subhypotheses).


%!	resolutions(?Resolutions) is semidet.
%
%	Maximum number of resolutions.
%
%	Used with solve_to_depth/3.
%
%resolutions(500_000_000_000).
%resolutions(20_500_000).
%resolutions(10_500_000).
%resolutions(5_500_000).
%resolutions(500_000).
%resolutions(250_000).
%resolutions(30_000).
%resolutions(10_000).
resolutions(5000).
%resolutions(100).
%resolutions(15).
%resolutions(0).


%!      respecialise(?Bool) is semidet.
%
%       Strongly specialise the Top Program against positive examples.
%
%       When Bool is true, each of the Top Program's sub-hypotheses must
%       entail _all_ positive examples with respect to background
%       knowledge. When Bool is false some sub-hypotheses in the Top
%       Program may not entail all positive examples, with respect to
%       background knowledge.
%
%       Setting this option to "true" is useful when the Top Program
%       contains many over-specialised sub-hypotheses and only one or a
%       few onets that fully generalise to the positive examples, or
%       nearly. In such cases the Top Program might grow too large and
%       unwieldy.
%
respecialise(false).


%!      tautology(+Clause) is det.
%
%       True when Clause is a tautology.
%
%       This configuration option formalises the concept of a
%       tautological clause as it is used in Louise. In short, a clause
%       is a tautology if it is a definite clause with one or more body
%       literals and all its literals are identical.
%
%       For example, the following clause is considered to be a
%       tautology:
%       ==
%       p(A,B):- p(A,B), p(A,B)
%       ==
%
%       Whereas the following clauses are not considered to be
%       tautologies:
%       ==
%       p(a,b)
%       p(A,B):- p(B,A)
%       ==
%
%       And so on. Formalising the concept of tautology in Louise is
%       useful because of the way the Top Proram Construction (TPC)
%       algorithm works. TPC adds to the background knowledge the set of
%       positive examples, which then functions as an extensional,
%       partial definition of each target predicate. TPC then
%       generalises each example to a clause that entails the example
%       with respect to the background knowledge. Since the background
%       knowledge includes each positive example, it is possible and in
%       fact common to end up with clauses generalising an example by
%       creating an implication of an atom of the example's predicate by
%       one or more instances of itself. For example, if p(a,b) is a
%       positive example, p(A,B):- p(A,B) is a clause tautologically
%       expressing the fact that each atom of p/2 entails itself.
%
%       Such tautologies are removed from the Top program by Plotkin's
%       program reduction algorithm since they are always entailed by
%       the rest of the Top program (and by anything else, ever really).
%       However, alternative reduction methods, such as subhypothesis
%       selection or minimal program learning do not rely on entailment
%       of a clause by the rest of the program and so may not be able to
%       get rid of tautologies as simply as Plotkin's reduction. For
%       such reduction methods, a tautology check is needed. This
%       predicate forms the basis of such a check.
%
%       Note that tautological clause generally only arise when the
%       Identity metarule, or one of its specialisations is in the set
%       of metarules for a MIL problem. The Identity metarule is P(x,y)
%       :- Q(x,y) where {P,Q} are second-order existentially quantified
%       variables that are not constrainted to be different. If P = Q
%       then the resulting clause is a tautology, but this is not always
%       the case, so Identity is generally useful (in fact,
%       indispensible, given that it represents one third of the
%       properties necessary to construct an equivalence relation). A
%       specialisation of Identity is a metarule with multiple clauses
%       having identical literals up to renaming of their second order
%       existentially quantified variables, for example the following is
%       a specialisation of Identity: P(x,y):- Q(x,y), R(x,y) and if P =
%       Q = R the resulting clause would be a tautology as defined by
%       this predicate.
%
tautology(H:-B):-
        copy_term(H:-B,C_)
        ,clause_literals(C_,Ls)
        ,numbervars(Ls)
        ,sort(Ls,[_]).


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
:- edit(louise)
  ,edit(louise_configuration)
  ,edit(louise_auxiliaries)
  ,louise_configuration:experiment_file(P,_)
  ,edit(P).
*/

% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:- experiment_file(P,_)
  ,load_experiment_file(P).
