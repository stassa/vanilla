:-module(poker_configuration, [clause_limit/1
                              ,experiment_file/2
                              ,gestalt/1
                              ,greedy_generalisation/1
                              ,listing_limit/1
                              ,max_invented/1
                              ,proof_samples/1
                              ,recursive_reduction/1
                              ,reduction/1
                              ,resolutions/1
                              ,respecialise/1
                              ,safe_example/1
                              ,strict_clause_limit/1
                              ,tautology/1
                              ,unlabelled_examples/1
                              ,unlabelled_examples_order/1
                              ]).

:-use_module(load_experiment_file).
:-reexport(lib(poker/program_reduction/reduction_configuration),
	   except([resolutions/1])).

/** <module> Configuration options for Poker.

*/

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
experiment_file(data('examples/hello_world_poker.pl'),hello_world_poker).
%experiment_file(data('examples/anbn.pl'),anbn).
%experiment_file(data('examples/parity.pl'),parity).


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
gestalt(false).
%gestalt(true).


%!      greedy_generalisation(?Bool) is semidet.
%
%       Whether to construct an initial hypothesis greedily or not.
%
%       This option is used to select the clauses of label/6.
%
%       When this option is set to "true", label/6 will only construct
%       the Top Program for a single example in the set of initial
%       examples, by looping through the examples, constructing a Top
%       Program and discarding it, until it can construct a Top Program
%       that covers all the initial examples. The first Top Program that
%       satisfies this condition will be used as the initial hypothesis
%       to be specialised by generation of new examples.
%
%       When this option is set to "false", label/6 will construct a Top
%       Program for each of the initial examples and take their
%       conjunction as the initial hypothesis.
%
greedy_generalisation(false).
%greedy_generalisation(true).


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
listing_limit(15).


%!	max_invented(?Number) is semidet.
%
%	Maximum number of invented predicates.
%
%	Assumes clause_limit(K) where K > 1.
%
max_invented(0).


%!      proof_samples(?Sample) is semidet.
%
%       Proportion of proofs sampled from prove/7 results.
%
%       Sample is a number, either a float in [0.0,1.0], interpreted as
%       a probability, or an integer in [0,100], interpreted as a
%       percentile chance.
%
%       Sample determines the probability that each set of
%       metasubstitutions derived by an inductive proof with prove/7
%       will be added to the Top Program or not.
%
%       This option is meant to reduce the number of sub-hypotheses in
%       the initial Top Program. This number can be very, very large
%       and when it is, Poker can run out of RAM.
%
proof_samples(1.0).


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
%	of Plotkin's program reduction.
%	* subhypothesis: select one hypothesis entailed by the Top
%	program.
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
%respecialise(true).


%!     safe_example(-Example) is nondet.
%
%      Generate a safe scaffold for unlabelled examples.
%
%      For examples with list arguments, generating unlabelled examples
%      during learning can "go infinite". This predicate ensures that
%      list arguments in examples are limited in length.
%
%      This argument should not itself be a generator of ground
%      examples. This is left to the user to avoid.
%
:-dynamic safe_example/1.
:-multifile safe_example/1.


%!      strict_clause_limit(?Bool) is semidet.
%
%       Whether clause_limit/1 is an exact value or an upper bound.
%
%       When Bool is true sub-hypotheses in the Top Program will all
%       have exactly the number of clauses defined in clause_limit/1.
%
%       When Bool is false, sub-hypotheses in the Top Program will all
%       have a number of clauses from 1 to the number defined lin
%       clause_limit/1.
%
strict_clause_limit(false).
%strict_clause_limit(true).


%!      tautology(+Clause) is det.
%
%       True when Clause is a tautology.
%
%       This configuration option formalises the concept of a
%       tautological clause as it is used in Poker. In short, a clause
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
%       And so on. Formalising the concept of tautology in Poker is
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


%!      unlabelled_examples(?Number) is semidet.
%
%       The number of unlabelled examples to generate for labelling.
%
%       This option determines the number of atoms of an initial
%       hypothesis, learned from a single, assumed-positive, example,
%       generated at the start of a labelling loop in Poker. The
%       generated examples are assumed-negative and the assumption
%       checked by Poker's labelling loop.
%
unlabelled_examples(100).


%!      unlabelled_examples(?Order) is semidet.
%
%       What Order to generate unlabelled examples in.
%
%       Order is one of: [deterministic, random]. "deterministic" leaves
%       unlabelled examples in the order they are generated (or given).
%       "random" randomly permutes the list of generated (or given)
%       examples once.
%
unlabelled_examples_order(deterministic).
%unlabelled_examples_order(random).


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
:- edit(poker)
  ,edit(poker_configuration)
  ,edit(poker_auxiliaries)
  ,poker_configuration:experiment_file(P,_)
  ,edit(P).
*/

% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:- experiment_file(P,_)
  ,load_experiment_file(P).
