Metagol - search-based Meta-Interpretive Learning
-------------------------------------------------

This README file is a Work-In-Progress.

Metagol is the original implementation of MIL. 

It uses an SLD-refutation proof of its positive examples to construct clauses
but bolts-on at the top of it a Depth-First Search of the space of logic
programs with Iterative Deepening over their cardinalities for a program that
entails the conjunction of its positive examples, and none of its negative
examples, with respect to first-order background knowledge.

The iterative depeening search in Metagol will first visit all the one-clause
hypotheses that can be constructed from the positive examples, then all the
two-clause hypotheses, then all the three-clause hypotheses- and so on, up to
the maximum cardinality allowed. The cardinality of logic programs at each step
corresponds to the "depth" in the iterative deepening search.

The implementation of Metagol in this directory takes the maximum, and also a
minimum, depth for the iterative deepening search. When a hypothesis is found at
some depth k, that entails all the positive examples with respect to the
first-order background knowledge, the hypothesis is tested at the maximum depth
against the negative examples. If any negative example is entailed by the
hypothesis, the hypothesis is rejected. Karl Popper would be proud (but maybe
not if he saw Poker).

Metagol uses a set of order constraints over the instantiations of second- and
first-order variables in the metarules, lexicographic and interval order
constraints respectively, to control recursion and ensure termination. Note that
currently only lexicographic order constraints are implemented.

The following is an example of learning a logic program for the `ancestor`
relation with Metagol. Training data is taken from
`data/examples/hello_world.pl`:

```prolog
% Remember to load metagol first!
?- use_module(lib(metagol/metagol)).true.

?- learn(ancestor/2).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
[]
true.
```

In the example above, a single program is returned, because that's the only
program that entails the training examples, with respect to the background
knowledge and without breaking the order constraints defined for the metarules
in the learning problem, and the ordering of predicate symbols in
`hello_world.pl`. The order constraints are defined as follows:

```prolog
% Order constraints for the two metarules in hello_world.pl:
order_constraints(identity,[P,Q],_Fs,[P>Q],[]).
order_constraints(tailrec,[P,Q],[X,Y,Z],[P>Q],[X>Z,Z>Y]).

% Lexicographic ordering for ancestor/2; interval order is missing:
program_signature(ancestor/2,[ancestor,parent],[]).
```

The lexicographic ordering of the predicate symbols `ancestor` and `parent` in
the `program_signature/3` clause, in conjunction with the lexicographic order
constraints in the `order_constraints/5` clause forces the construction of
clauses of the two metarules, `identity` and `tailrec`, where the symbol
`ancestor` is only used in variables earlier in the clause than `parent`. This
way, left-recursive clauses are eliminated.

Suppose we remove the lexicographic constraints from the `tailrec` metarule,
like this:

```prolog
order_constraints(tailrec,_,_,[],[]).
```

Now, let's see what happens when we try to learn with Metagol:

```prolog
?- learn(ancestor/2).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
ERROR: Stack limit (1.0Gb) exceeded
ERROR:   Stack sizes: local: 0.5Gb, global: 0.5Gb, trail: 26.0Mb
ERROR:   Stack depth: 1,362,336, last-call: 0%, Choice points: 2,043,493
ERROR:   Possible non-terminating recursion:
ERROR:     [1,362,327] vanilla:prove(<compound m/3>, 2, [length:2], [length:1], [length:4], [length:2], _122619138)
ERROR:     [1,362,326] vanilla:prove(<compound (',')/2>, 2, [length:2], [length:1], [length:4], [length:2], _122619198)
   Exception: (1,360,894) vanilla:prove((m(ancestor, stathis, _122489934), m(ancestor, _122489934, _122489754)), 2, [(m(identity, _3880, _3882):-m(_3880, _3894, _3896), m(_3882, _3894, _3896)), (m(tailrec, _3824, _3826):-m(_3824, _3838, _3840), m(_3826, _3838, _3854), m(_3824, _3854, _3840))], [ancestor], [bk, builtins, hypothesis, metarules], [m(tailrec, ancestor, ancestor), m(identity, ancestor, parent)], _122489824) ? abort
% Execution Aborted
```

Ooops. We've run out of stack space. After a first set of clauses is constructed
that is not left-recursive, the missing constraint allows a left-recursive
hypothesis to be constructed, and the Depth-First Search in SLD-Resolution goes
into an infinite recursion and blows up the stack.
