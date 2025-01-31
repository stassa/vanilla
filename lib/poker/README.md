Poker: Self-supervised learning of recursive logic programs with invented predicates  
======================================================================================

Poker is a self-supervised Meta-Interpretive Learning system based on
[Louise](https://github.com/stassa/vanilla/tree/master/lib/louise) and its Top
Program Construction algorithm. It learns a maximally special hypothesis that
generalises an initial set of examples by deriving sets of new positive and
negative examples that are consistent with the initial examples; hence
"self-supervised": because it makes up its own positive and negative examples as
it goes.

Poker is named not after the card game but after [Wittgenstein's
Poker](https://en.wikipedia.org/wiki/Wittgenstein%27s_Poker).

How Poker works
---------------

Poker starts with one or more initial examples assumed to be positive examples
of some unknown predicate. Poker invokes the Top Program Construction algorithm
to learn an initial, maximally general hypothesis from these examples, then
proceeds to generate new examples from that initial hypothesis. Each new
generated example is assumed to be negative and used to specialise the
hypothesis so-far by removing sub-hypotheses that cover it. If the hypothesis no
longer covers any of the positive examples found so-far, the new example is
added to the set of the positive examples and the hypothesis put back the way it
was. Otherwise, the example is kept in the set of negative examples. This
process repeats until no more examples can be generated; a user can select a
limit on the number of examples to be generated to avoid generating infinite
examples. 

The result of the process described above is a set of clauses that comprise the
most specific hypothesis that generalises the initial examples, with respect to
the background knowledge. To avoid confusion with the Least General
Generalisation of a clause, we call this program the Most Specific
Generalisation of a set of atoms (the initial examples), abbreviated to MSG; not
to be confused with the food additive, or the Heavy Rock group.

Example session with Poker
==========================

In the following section Poker is shown learning a grammar for the context-free
`a^nb^n` language from the string `aaabbb` (given in Definite Clause Grammars
form) as an initial example. 

The classic grammar for `a^nb^n` is a recursive grammar defined as follows:

```
S → ab
S → aSb
```

The target theory for this experiment will be a representation of the classical
grammar in Prolog's Definite Clause Grammars form (without syntactic sugar),
replacing the two terminals `a` and `b` in the classical grammar with
pre-terminals `a([a|A],A)` and `b([b|A],A)`. We list it below:

```Prolog
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(A,C),b(C,B).
a([a|A],A).
b([b|A],A).
```

To learn this grammar Poker needs the pre-terminals in the grammar as background
knowledge. It also needs as second-order background knowledge the _metarule_
called `Chain` that corresponds to a production rule in Definite Clause Grammars
form. The complete elements of the MIL problem are listed below with a query to
Poker's `list_mil_problem/1` predicate, that shows what is loaded in the
system's memory:

```Prolog
?- poker_auxiliaries:list_mil_problem(s/2).
Initial examples
----------------
s([a,a,a,b,b,b],[]).

Background knowledge (First Order)
----------------------------------
a/2:
a([a|A],A).

b/2:
b([b|A],A).

Background knowledge (Second Order)
-----------------------------------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(m(identity, P0, P1), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    experiment_file:(P1==P2).

true.
```

The MIL problem also includes a set of standard, `McCarthyite` constraints on
the instantiations of the existentially quantified variables in metarules, used
to avoid left-recursions. Left recursions are eliminated for efficiency reasons:
the target theory does not need to be left-recursive, but left-recursion is more
expensive to prove; so we kill it in its infancy with constraints.

The result of a learning query with Poker's `learn/1` predicate are shown below: 

```Prolog
?- poker:learn(s/2).
Hypothesis:
s(A,B):-inv_1_4(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_5(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_5(A,B):-s(A,C),b(C,B).
inv_1_4(A,B):-a(A,C),s(C,B).
Positive examples:
s([a,a,a,a,b,b,b,b],[]).
s([a,a,b,b],[]).
s([a,b],[]).
s([a,a,a,b,b,b],[]).
Negative examples:
s([a,a,a,a,a,a,a,b],[]).
s([a,a,b,b,b],[]).
s([a,a,a,a,a,b],[]).
s([a,a,a,a,b,b,b],[]).
s([a,a,a,a,b],[]).
s([a,a,a,a,a,b,b],[]).
s([a,a,a,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b],[]).
s([a,a,a,a,a,a,a,a,b],[]).
s([a,a,b,b,b,b,b],[]).
s([a,a,a,a,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,a,b,b],[]).
s([a,a,b,b,b,b,b,b],[]).
s([a,b,b],[]).
s([a,a,a,b,b,b,b,b],[]).
s([a,a,b],[]).
s([a,b,b,b],[]).
s([a,a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,b,b,b,b],[]).
s([a,a,b,b,b,b],[]).
s([a,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,b],[]).
s([a,a,a,b],[]).
s([a,a,a,a,a,b,b,b],[]).
s([a,b,b,b,b],[]).
s([a,a,a,b,b,b,b],[]).
s([a,a,b,b,b,b,b,b,b],[]).
s([a,a,a,a,b,b],[]).
s([a,a,a,b,b],[]).
true.
```

From its initial example and background theory Poker learned a _Top Program_
consisting of two sub-programs, each with recursive clauses and invented
predicates. It also identified three new positive examples, including the "base
case" of the recursive target theory (corresponding to the string `ab`).
Finally, it identified a set of 32 negative examples.

Recursion and predicate invention
---------------------------------

Let's look at the Top Program learned by Poker more closely:

```Prolog
s(A,B):-inv_1_4(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_5(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_5(A,B):-s(A,C),b(C,B).
inv_1_4(A,B):-a(A,C),s(C,B).
```

A Top Program is the most general correct hypothesis that is consistent with a
set of examples and background predicates; it is "most general" in the sense
that it includes, as sub-sets, each other correct hypothesis. The Top Program
for `a^nb^n` above includes two such _sub-hypotheses_ listed below:

```Prolog
s(A,B):-a(A,C),b(C,B).
s(A,B):-inv_1_4(A,C),b(C,B).
inv_1_4(A,B):-a(A,C),s(C,B).

s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_5(C,B).
inv_1_5(A,B):-s(A,C),b(C,B).
```

The reader can confirm that these two sub-hypotheses are both equivalent to the
target theory for an `a^nb^n` grammar, except in "folded" form, with two
invented predicates, `inv_1_4` and `inv_1_5` acting as connections for two
halves of the second, recursive clause. 

If we unfold either sub-hypothesis, resolving-away the invented predicate
symbols, we find our target theory:

```Prolog
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(A,C),b(C,B).
```

Poker learns a hypothesis in folded form because it starts with a second-order
definite clause that only has three literals, the `Chain` _metarule_:

```Prolog
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
```

Poker, like all MIL learners, learns by proving positive examples by
SLD-Refutation with the first- and second- order background theory. During the
Resolution proof, by unification, the second-order variables `P`, `Q`, and `R`
in the case of our example, are bound to predicate symbols to construct
first-order clauses. Negative examples are proved in the same way but used to
_specialise_ the learned set of clauses, by removing any that entail negative
examples.

The reader can verify that the target theory listed above is indeed the most
specific _generalisation_ of the initial example, the string `aaabbb`, with
respect to the background theory. In particular, there exist more specific
hypotheses that are consistent with `aaabbb` but do not generalise it: any
hypothesis that entails exactly `aaabbb` and nothing more is an over-special
hypothesis.


Discovering new positive and negative examples
----------------------------------------------

Poker is self-supervised in the sense that it makes up new positive and negative
examples as it learns. In our example above, Poker identified 35 new examples
and labeled three of them as positive and the rest as negative. The following
are the 3 positive examples identified this way:

```Prolog
s([a,a,a,a,b,b,b,b],[]).
s([a,a,b,b],[]).
s([a,b],[]).
```

And here are the 32 negative examples:

```Prolog
s([a,a,a,a,a,a,a,b],[]).
s([a,a,b,b,b],[]).
s([a,a,a,a,a,b],[]).
s([a,a,a,a,b,b,b],[]).
s([a,a,a,a,b],[]).
s([a,a,a,a,a,b,b],[]).
s([a,a,a,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b],[]).
s([a,a,a,a,a,a,a,a,b],[]).
s([a,a,b,b,b,b,b],[]).
s([a,a,a,a,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,a,b,b],[]).
s([a,a,b,b,b,b,b,b],[]).
s([a,b,b],[]).
s([a,a,a,b,b,b,b,b],[]).
s([a,a,b],[]).
s([a,b,b,b],[]).
s([a,a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,b,b,b,b],[]).
s([a,a,b,b,b,b],[]).
s([a,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,b],[]).
s([a,a,a,b],[]).
s([a,a,a,a,a,b,b,b],[]).
s([a,b,b,b,b],[]).
s([a,a,a,b,b,b,b],[]).
s([a,a,b,b,b,b,b,b,b],[]).
s([a,a,a,a,b,b],[]).
s([a,a,a,b,b],[]).
```

The reader can verify that the above examples are labeled correctly with
respect to the target theory of `a^nb^n` ("n a's followed by n b's").

The negative examples derived by Poker during learning are necessary to learn
a Top Program that only includes correct sub-hypotheses and so is the Most
Specific Generalisation of the initial example. By contrast here is the Top
Program learned by Louise from the same initial example and background knowledge
but _without any negative examples_:

```Prolog
?- louise:learn(s/2).
inv_1_11(A,B):-a(A,C),a(C,B).
inv_1_12(A,B):-a(A,C),a(C,B).
inv_1_13(A,B):-a(A,C),a(C,B).
inv_1_13(A,B):-inv_1_13(A,C),b(C,B).
inv_1_14(A,B):-a(A,C),inv_1_14(C,B).
inv_1_14(A,B):-b(A,C),b(C,B).
inv_1_15(A,B):-b(A,C),b(C,B).
inv_1_16(A,B):-b(A,C),b(C,B).
inv_1_6(A,B):-a(A,C),s(C,B).
inv_1_7(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),a(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_11(C,B).
s(A,B):-a(A,C),inv_1_13(C,B).
s(A,B):-a(A,C),inv_1_7(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-b(A,C),b(C,B).
s(A,B):-b(A,C),inv_1_16(C,B).
s(A,B):-b(A,C),s(C,B).
s(A,B):-inv_1_12(A,C),a(C,B).
s(A,B):-inv_1_14(A,C),b(C,B).
s(A,B):-inv_1_15(A,C),b(C,B).
s(A,B):-inv_1_6(A,C),b(C,B).
s(A,B):-s(A,C),a(C,B).
s(A,B):-s(A,C),b(C,B).
true.
```

It should be obvious that this Top Program is indeed an over-general definition
of our `a^nb^n` target theory.

Learning demonstration
----------------------

The following is a complete learning session with Poker and the `a^nb^n` MIL
problem described above.

The experiment file used for this demonstration is in the following path:

`data/examples/anbn_poker.pl`. 

That experiment file also holds positive and negative examples used with the
other MIL systems included with Vanilla. Those labeled examples are _not_ used
to train Poker. They are only there for easy comparison with other systems.

### Generating new examples

The number of new examples that Poker will generate, and try to separate into
positive and negative, is set by the user in a configuration option called
`unlabelled_examples/1`. For example:

```Prolog
unlabelled_examples(100).
```

This setting tells Poker to try and generate up to 100 new examples once it has
an initial hypothesis. The number actually generated depends on the initial
hypothesis- if this is not general enough to generate 100 examples, then fewer
will be generated.

### Avoiding infinite lists

`a^nb^n` strings in Definite Clause Grammars form contain lists and Poker
imposes a limit on the length of lists in generated examples to avoid generating
infinitely many, infinite-length lists. The limit is set in the predicate
`safe_example/1` defined in the experiment file holding the background theory
used in learning. We list its current definition (with `anbn_poker.pl` loaded)
below:

```Prolog
?- listing(safe_example/1).
:- dynamic poker_configuration:safe_example/1.
:- multifile poker_configuration:safe_example/1.

poker_configuration:safe_example(m(s, Ls, [])) :-
    experiment_file:
    (   between(1, 9, L),
        length(Ls, L)
    ).

true.
```

This definition of `safe_example/1` means that new examples will be generated
with lists of length up to 9 in the first argument. Note that `safe_example/1`
only generates lists whose elements are uninstantiated variables, like the list
`Ls` above. These lists serve as templates for the generation of new examples by
Poker, or more precisely, by the initial hypothesis learned by Poker; it's this
initial hypothesis that will instantiate the variables in the list-templates
specified by `safe_example/1`.

### Learning session

Below is a big friendly dump of the entire learning session for the user's
delectation.

```Prolog
% List the MIL problem elements.
?- poker_auxiliaries:list_mil_problem(s/2).
Initial examples
----------------
s([a,a,a,b,b,b],[]).

Background knowledge (First Order)
----------------------------------
a/2:
a([a|A],A).

b/2:
b([b|A],A).

Background knowledge (Second Order)
-----------------------------------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(m(identity, P0, P1), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    experiment_file:(P1==P2).

true.

% List the definition of safe_example/1 to show the length limit imposed on
% lists in generated examples.
anbn:safe_example(m(s, Ls, [])) :-
    between(1, 9, L),
    length(Ls, L).

% Mysterious and magickal configuration options
% Think of them as hyperparameters of the learning algorithm.
% Make sure they match your config if repeating this experiment.
?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), gestalt(Gestalt), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)).
Fetch = all,
Table = Untable, Untable = true,
Limit = 4,
Invented = 1,
Gestalt = false,
Unlabelled = 100,
Order = random,
Reduction = plotkins.

% Learn you a grammar of anbn, one-shot and self-supervisedly:
?- time( poker:learn(s/2) ).
Hypothesis:
s(A,B):-inv_1_4(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_5(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_5(A,B):-s(A,C),b(C,B).
inv_1_4(A,B):-a(A,C),s(C,B).
Positive examples:
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,a,b,b,b,b],[]).
s([a,a,a,b,b,b],[]).
Negative examples:
s([a,a,a,b,b],[]).
s([a,a,b,b,b],[]).
s([a,b,b],[]).
s([a,a,a,b,b,b,b,b,b],[]).
s([a,a,a,a,a,b,b,b,b],[]).
s([a,a,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,b],[]).
s([a,b,b,b,b,b,b],[]).
s([a,a,a,b,b,b,b],[]).
s([a,a,a,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,a,a,b,b],[]).
s([a,b,b,b,b,b,b,b,b],[]).
s([a,a,b,b,b,b],[]).
s([a,a,a,a,b,b],[]).
s([a,a,b,b,b,b,b,b],[]).
s([a,b,b,b,b],[]).
s([a,a,a,a,b,b,b],[]).
s([a,a,b,b,b,b,b,b,b],[]).
s([a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,b,b],[]).
s([a,a,a,a,b,b,b,b,b],[]).
s([a,b,b,b,b,b],[]).
s([a,a,b],[]).
s([a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,a,b],[]).
s([a,a,a,b],[]).
s([a,b,b,b],[]).
% 3,172,231 inferences, 1.453 CPU in 4.662 seconds (31% CPU, 2183041 Lips)
true.

% Learn again but separate sub-hypotheses with reduction(subhypotheses):
?- poker_configuration:reduction(R).
R = subhypotheses.

?- time( poker:learn(s([a,a,a,b,b,b],[]),_Pos,_Neg,_Ps) ), auxiliaries:print_clauses(_Ps).
% 3,035,284 inferences, 3.250 CPU in 4.399 seconds (74% CPU, 933934 Lips)
inv_1_4(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-inv_1_4(A,C),b(C,B).
true ;
% 753 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_5(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_5(C,B).
true.
```

Wow, you made it all the way down here! As a reward for your patience here is
some testing of the Top Program learned by Poker:

```Prolog
% Assert the learned program and BK at the Prolog top-level.
?- [user].
|: :-table(s/2). % Avoids generating the same strings multiple times.
|: s(A,B):-inv_1_4(A,C),b(C,B).
|: s(A,B):-a(A,C),inv_1_5(C,B).
|: s(A,B):-a(A,C),b(C,B).
|: inv_1_5(A,B):-s(A,C),b(C,B).
|: inv_1_4(A,B):-a(A,C),s(C,B).
|: a([a|T],T).
|: b([b|T],T).
|: 
% user://1 compiled 0.00 sec, 9 clauses
true.

% Test away:
?- between(1,40,_K), length(_Ss,_K), anbn:s(_Ss,[]), atomic_list_concat(_Ss,'',Ss).
Ss = ab ;
Ss = aabb ;
Ss = aaabbb ;
Ss = aaaabbbb ;
Ss = aaaaabbbbb ;
Ss = aaaaaabbbbbb ;
Ss = aaaaaaabbbbbbb ;
Ss = aaaaaaaabbbbbbbb ;
Ss = aaaaaaaaabbbbbbbbb ;
Ss = aaaaaaaaaabbbbbbbbbb ;
Ss = aaaaaaaaaaabbbbbbbbbbb ;
Ss = aaaaaaaaaaaabbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaabbbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaaabbbbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaaaabbbbbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbb ;
Ss = aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb.
```

The reader can verify that the last, longest-generated, string consists of 20
a's followed by 20 b's, comprising in total 40 characters, as given in the
between/3 call.

Further discussion
==================

We showed Poker learning from a single example of `a^nb^n`, the one for n = 3.
Yes but why `aaabbb` though? The big, dirty secret of Poker's self-supervised
learning can now be revealed: the more specific an example, the more specific
the hypotheses that can be learned. Let's see what happens if we learn with
`aabb` as an example:

```Poker
?- time( poker:learn(s([a,a,b,b],[]),_Pos,_Neg,_Ps) ), auxiliaries:print_clauses(_Ps).
% 1,795,372 inferences, 0.453 CPU in 0.742 seconds (61% CPU, 3962200 Lips)
s(A,B):-inv_1_3(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_4(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_4(A,B):-s(A,C),b(C,B).
inv_1_3(A,B):-a(A,C),s(C,B).
true.
```

Oh, er. Nothing changes. Well, that's interesting, isn't it? How about a more
specific, i.e longer, example?

```Poker
?- time( poker:learn(s([a,a,a,a,b,b,b,b],[]),_Pos,_Neg,_Ps) ), auxiliaries:print_clauses(_Ps).
% 3,788,623 inferences, 3.047 CPU in 4.877 seconds (62% CPU, 1243445 Lips)
s(A,B):-inv_1_5(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_6(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_6(A,B):-s(A,C),b(C,B).
inv_1_5(A,B):-a(A,C),s(C,B).
true.
```

Darn. That one works too. But I thought. OK, how about we try with more than one
example? Surely that will cause over-generalisation?

```Poker
?- time( poker:learn([s([a,a,b,b],[]),s([a,a,a,b,b,b],[]),s([a,a,a,a,b,b,b,b],[])],_Pos,_Neg,_Ps) ), auxiliaries:print_clauses(_Ps).
% 8,059,285 inferences, 18.500 CPU in 29.910 seconds (62% CPU, 435637 Lips)
s([a,a,b,b],[]).
s(A,B):-inv_1_8(A,C),b(C,B).
s(A,B):-inv_1_7(A,C),b(C,B).
s(A,B):-inv_1_6(A,C),b(C,B).
s(A,B):-inv_1_5(A,C),b(C,B).
s(A,B):-inv_1_4(A,C),b(C,B).
s(A,B):-inv_1_3(A,C),b(C,B).
s(A,B):-inv_1_2(A,C),b(C,B).
s(A,B):-inv_1_10(A,C),b(C,B).
s(A,B):-inv_1_1(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),inv_1_9(C,B).
s(A,B):-a(A,C),b(C,B).
inv_1_9(A,B):-s(A,C),b(C,B).
inv_1_8(A,B):-a(A,C),s(C,B).
inv_1_7(A,B):-s(A,C),b(C,B).
inv_1_7(A,B):-a(A,C),inv_1_7(C,B).
inv_1_6(A,B):-a(A,C),b(C,B).
inv_1_5(A,B):-s(A,C),b(C,B).
inv_1_5(A,B):-a(A,C),b(C,B).
inv_1_4(A,B):-a(A,C),s(C,B).
inv_1_4(A,B):-a(A,C),b(C,B).
inv_1_3(A,B):-a(A,C),inv_1_3(C,B).
inv_1_3(A,B):-a(A,C),b(C,B).
inv_1_2(A,B):-s(A,C),b(C,B).
inv_1_2(A,B):-a(A,C),inv_1_2(C,B).
inv_1_2(A,B):-a(A,C),b(C,B).
inv_1_10(A,B):-s(A,C),b(C,B).
inv_1_1(A,B):-a(A,C),s(C,B).
inv_1_1(A,B):-a(A,C),inv_1_1(C,B).
inv_1_1(A,B):-a(A,C),b(C,B).
true.
```

Ugh, what's this horror? Let's make that a little more clear:

```Poker
?- poker_configuration:reduction(R).
R = subhypotheses.

?- time( poker:learn([s([a,a,b,b],[]),s([a,a,a,b,b,b],[]),s([a,a,a,a,b,b,b,b],[])],_Pos,_Neg,_Ps) ), auxiliaries:print_clauses(_Ps).
% 7,238,044 inferences, 9.172 CPU in 30.908 seconds (30% CPU, 789156 Lips)
inv_1_1(A,B):-a(A,C),b(C,B).
inv_1_1(A,B):-a(A,C),inv_1_1(C,B).
inv_1_1(A,B):-a(A,C),s(C,B).
s(A,B):-inv_1_1(A,C),b(C,B).
true ;
% 755 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_2(A,B):-a(A,C),b(C,B).
inv_1_2(A,B):-a(A,C),inv_1_2(C,B).
inv_1_2(A,B):-s(A,C),b(C,B).
s(A,B):-inv_1_2(A,C),b(C,B).
true ;
% 702 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_3(A,B):-a(A,C),b(C,B).
inv_1_3(A,B):-a(A,C),inv_1_3(C,B).
s(A,B):-inv_1_3(A,C),b(C,B).
true ;
% 752 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_4(A,B):-a(A,C),b(C,B).
inv_1_4(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-inv_1_4(A,C),b(C,B).
true ;
% 752 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_5(A,B):-a(A,C),b(C,B).
inv_1_5(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-inv_1_5(A,C),b(C,B).
true ;
% 699 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_6(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-inv_1_6(A,C),b(C,B).
true ;
% 752 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_7(A,B):-a(A,C),inv_1_7(C,B).
inv_1_7(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-inv_1_7(A,C),b(C,B).
true ;
% 699 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_8(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-inv_1_8(A,C),b(C,B).
true .
```

Aha! _Now_ we get over-generalisation. Yet in a twist of fate the Top Program we
learn here is over-general because many of its sub-hypotheses are
_over-specialised_. We can avoid this if we set the configuration option
`respecialise/1` to "true".  Like this:

```Poker
?- poker_configuration:respecialise(R).
R = true.

?- time( poker:learn([s([a,a,b,b],[]),s([a,a,a,b,b,b],[]),s([a,a,a,a,b,b,b,b],[])],_Pos,_Neg,_Ps) ), auxiliaries:print_clauses(_Ps).
% 4,680,124 inferences, 4.234 CPU in 6.907 seconds (61% CPU, 1105269 Lips)
inv_1_8(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-inv_1_8(A,C),b(C,B).
true ;
% 584 inferences, 0.000 CPU in 0.000 seconds (0% CPU, Infinite Lips)
inv_1_9(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_9(C,B).
true.
```

`respecialise(true)` tells Poker to keep only those sub-hypotheses in its
initial Top Program that are consistent with _all_ its initial examples; when
`respecialisation(false)` is set, Poker will add to the Top Program
sub-hypotheses that only entail _some_ of the initial examples. Those are, by
definition, over-speicific. 

Learning Parity
---------------

`a^nb^n` is, in an intuitive sense, easy to learn because its pattern is so
regular: `ab, aabb, aaabbb, ... ` and so on. Anyone can see after a few examples
where this is going. Counter-intuitively, this intuition that holds for the
human mind, does not hold for computers. Despite its apparent regularity,
`a^nb^n` is a Context-Free language, so it is in fact _hard_ to learn only from
examples. In fact it is _impossible_ to learn only from a finite number of
positive examples; in our demonstration above we also have a second-order
definite clause acting as background knowledge, that imposes a very strong
inductive bias to the point that it fully specifies the form of hypotheses we
can learn; otherwise it wouldn't be possible to learn.

The knowledge that Context-Free grammars are hard to learn, even ones so
seemingly simple as `a^nb^n` comes from an old result in Inductive Inference,
called _Gold's Result_, after Mark E. Gold who derived it in a paper called
`Language Identification in the Limit`. See the [wikipedia
article](https://en.wikipedia.org/wiki/Language_identification_in_the_limit) for
a short introduction.

Gold's result is that automata that are no more than Regular (corresponding to
regular grammars) can only be precisely learned after observing a number of
positive examples approaching positive infinity, and automata that are above
Regular also need a number of negative examples approaching infinity.  Hence,
automata can only be precisely learned from examples "in the limit".

As discussed, Poker and other MIL systems buck this trend thanks to the strong
inductive bias imposed by their background knowledge. Other modern machine
learning algorithms also impose their own inductive biases, like the
architectures of Artificial Neural Nets or the kernels of Kernel Machines.
Additionally, in the modern machine learning paradigm, informed by PAC-Learning,
we allow _approximate_ learning, with some probability of some degree of error.

Perhaps surprisingly when we "fix" the background knowledge, e.g allowing just
the pre-terminals in the language and second-order clauses that correspond to a
grammar form, the ability to learn depends entirely on the generality of the
examples. In particular, more general examples are consistent with more
hypotheses and the job of identifying a correct hypothesis becomes harder. Even
more so when the only examples we have to begin with are positive.

The following then is an example of Poker learning a grammar that is _harder to
learn_ in practice than `a^nb^n`: a grammar for the language of even parity.
This language consists of strings of `0`s' and `1`'s where the only identifying
feature is that each string has an even number of `1`s.

Poker cannot learn this language from a single example! We start by
demonstrating this:

```Poker
% Ye olde magickal configuration options; use these to reproduce the experiment
?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), gestalt(Gestalt), respecialise(Respecialise), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)).
Fetch = [builtins,bk,metarules],
Table = Untable, Untable = false,
Limit = 5,
Invented = 1,
Gestalt = Respecialise, Respecialise = true,
Unlabelled = 100,
Order = deterministic,
Reduction = plotkins.

% For this experiment we'll take the initial example from the user's input.
% We also give an additional second-order clause, `Identity`. The second-order
% background theory now corresponds _roughly_ to Chomsky Normal Form (except
% that it allows recursion over the start symbol of the grammar).
?- poker_auxiliaries:list_mil_problem(q0/2).
Initial examples
----------------

Background knowledge (First Order)
----------------------------------
zero/2:
zero([0|A],A).

one/2:
one([1|A],A).

empty/2:
empty([],[]).

Background knowledge (Second Order)
-----------------------------------
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(m(identity, P0, P1), fail) :-
    parity:(P0==P1).
metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    parity:(P0==P1).
metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    parity:(P1==P2).

true.

% Give an example and let learn.
?- time( poker:learn(q0([1,0,1,0,0],[])) ).
Hypothesis:
q0(A,B):-inv_1(A,C),zero(C,B).
q0(A,B):-inv_1(A,C),q0(C,B).
inv_1(A,B):-one(A,C),zero(C,B).
Positive examples:
q0([1,0,0],[]).
q0([1,0,1,0,0],[]).
Negative examples:
q0([1,1,1,1],[]).
q0([1,1,1,0],[]).
q0([1,1,1],[]).
q0([1,1,0,1],[]).
q0([1,1,0,0],[]).
q0([1,1,0],[]).
q0([1,1],[]).
q0([1,0,1,1],[]).
q0([1,0,1,0],[]).
q0([1,0,1],[]).
q0([1,0,0,1],[]).
q0([1,0,0,0],[]).
q0([1,0],[]).
q0([1],[]).
q0([0,1,1,1],[]).
q0([0,1,1,0],[]).
q0([0,1,1],[]).
q0([0,1,0,1],[]).
q0([0,1,0,0],[]).
q0([0,1,0],[]).
q0([0,1],[]).
q0([0,0,1,1],[]).
q0([0,0,1,0],[]).
q0([0,0,1],[]).
q0([0,0,0,1],[]).
q0([0,0,0,0],[]).
q0([0,0,0],[]).
q0([0,0],[]).
q0([0],[]).
% 11,753,184 inferences, 1.297 CPU in 1.678 seconds (77% CPU, 9062696 Lips)
true.
```

For this experiment we gave Poker the string `10100`. This is a string in the
even parity language (it has two `1`'s) but that example is consistent with many
different hypotheses: for instance, the hypothesis that the language accepts
"just the string `10100`"or "all strings composed of `1`'s and `0`'s". 

If we load the hypothesis above into memory we can test the grammar learned by
Poker, which then reveals itself to be "the set of strings of alternating `1`'s
and `0`'s that end in `100`:

```Poker
?- between(1,10,_K), length(Xs,_K), parity:q0(Xs,[]).
Xs = [1,0,0] ;
Xs = [1,0,1,0,0] ;
Xs = [1,0,1,0,1,0,0] ;
Xs = [1,0,1,0,1,0,1,0,0] ;
false.
```

This hypothesis is, indeed, consistent with our initial example and it is
impossible to distinguish it from our intended interpretation of that example,
just by looking at that example alone. We're gonna need more examples.

```Poker
% Magickal hyperparameters.
?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), gestalt(Gestalt), respecialise(Respecialise), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)).
Fetch = [builtins,bk,metarules],
Table = Untable, Untable = false,
Limit = 5,
Invented = 1,
Gestalt = Respecialise, Respecialise = true,
Unlabelled = 100,
Order = deterministic,
Reduction = plotkins.

% Hand-picked initial examples.
?- poker_auxiliaries:list_mil_problem(q0/2).
Initial examples
----------------
q0([],[]).
q0([0],[]).
q0([0,0],[]).
q0([1,1],[]).
q0([0,0,0],[]).
q0([0,1,1],[]).
q0([1,0,1],[]).
q0([1,1,0],[]).
q0([1,1,0,0],[]).
q0([1,0,1,0],[]).
q0([1,0,0,1],[]).
q0([0,1,0,1],[]).
q0([0,0,1,1],[]).
q0([0,0,1,1,0,0],[]).
q0([0,0,1,0,0,1],[]).

Background knowledge (First Order)
----------------------------------
zero/2:
zero([0|A],A).

one/2:
one([1|A],A).

empty/2:
empty([],[]).

Background knowledge (Second Order)
-----------------------------------
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

Metasubstitution constraints
----------------------------
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

metarule_constraints(m(identity, P0, P1), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, P0, P1, _P2), fail) :-
    experiment_file:(P0==P1).
metarule_constraints(m(chain, _P0, P1, P2), fail) :-
    experiment_file:(P1==P2).

true.

% Learning even parity.
?- time( poker:learn(q0/2) ).
Hypothesis:
q0(A,B):-empty(A,B).
q0(A,B):-zero(A,C),q0(C,B).
q0(A,B):-one(A,C),inv_1(C,B).
inv_1(A,B):-zero(A,C),inv_1(C,B).
inv_1(A,B):-one(A,C),q0(C,B).
Positive examples:
q0([1,1,1,1],[]).
q0([0,1,1,0],[]).
q0([0,0,0,0],[]).
q0([],[]).
q0([0],[]).
q0([0,0],[]).
q0([1,1],[]).
q0([0,0,0],[]).
q0([0,1,1],[]).
q0([1,0,1],[]).
q0([1,1,0],[]).
q0([1,1,0,0],[]).
q0([1,0,1,0],[]).
q0([1,0,0,1],[]).
q0([0,1,0,1],[]).
q0([0,0,1,1],[]).
q0([0,0,1,1,0,0],[]).
q0([0,0,1,0,0,1],[]).
Negative examples:
q0([1,1,1,0],[]).
q0([1,1,1],[]).
q0([1,1,0,1],[]).
q0([1,0,1,1],[]).
q0([1,0,0,0],[]).
q0([1,0,0],[]).
q0([1,0],[]).
q0([1],[]).
q0([0,1,1,1],[]).
q0([0,1,0,0],[]).
q0([0,1,0],[]).
q0([0,1],[]).
q0([0,0,1,0],[]).
q0([0,0,1],[]).
q0([0,0,0,1],[]).
% 38,069,358 inferences, 2.594 CPU in 5.757 seconds (45% CPU, 14677343 Lips)
true.
```

Let's look at the hypothesis learned by Poker this time:

```Poker
q0(A,B):-empty(A,B).
q0(A,B):-zero(A,C),q0(C,B).
q0(A,B):-one(A,C),inv_1(C,B).
inv_1(A,B):-zero(A,C),inv_1(C,B).
inv_1(A,B):-one(A,C),q0(C,B).
```

This hypothesis corresponds to the following grammar:

```
q0 → ε
q0 → 0q0
q0 → 1q1
q1 → 0q1
q1 → 1q0
```

Where `q1` is replaced by `inv_1`. The reader can take a minute to convince
him or her-self that the grammar above accepts only strings with an even number
of `1`'s; it does that by skipping over `0`'s and terminating at the empty
string, `ε`. 

More interestingly, this grammar includes a sub-grammar that only accepts
strings with an _odd_ number of `1`'s:

```
q1 → 0q1
q1 → 1q0
```

This might begin to explain why learning a grammar of even parity is harder for
Poker, even though parity is a regular task: the definition we allow Poker to
learn, constrained by its background knowledge, requires Poker to _invent_ a
definition of odd parity, and one that is mutually recursive with the even
parity definition, at that.

Of course a simpler way to say all this is that it is very difficult to
describe the concept of even parity, without simultaneously describing the
concept of odd parity. 

At least that is the case if we want a concept built up only from the terminals
in the language, `1`'s and `0`'s. We can obviously define even parity more
easily if we allow division, let alone exact division, as background knowledge.
But this presupposes knowledge of the definition of even parity ("exactly
divisible by 2") and that is usually what we don't know, and want to find out,
in real situations.

A more strict experiment
------------------------

In the experiment above we let Poker learn even parity from a set of hand-picked
examples. In particular, our examples included the empty string (`q0([],[])`)
which is in a sense, giving the game away (it is also the base-case of the
recursive definition learned).

In this section we measure Poker's performance over multiple attempts to learn
parity from randomly generated examples (which may or may not include the empty
string). The experiment uses the predicates in the module
`data/examples/test_harness.pl` which includes a correct definition of even (and
odd) parity, used to generate training examples and correctly label new examples
identified by Poker. The experiment code measures the means, over all learning
attempts, of accuracy, true negative, and true positive rate of the set of
examples generated from the final hypothesis learned by Poker in each attempt,
according to the labeling of the same examples by the correct definition of
parity.

```Prolog
% Magickal hyperparameters.
% Order = random scrambles the set of initial examples. This is useful because
% Poker is sensitive to the order of initial examples and can overfit to it.
?- configuration:(fetch_clauses(Fetch), table_meta_interpreter(Table), untable_meta_interpreter(Untable)), poker_configuration:(clause_limit(Limit), max_invented(Invented), gestalt(Gestalt), respecialise(Respecialise), unlabelled_examples(Unlabelled), unlabelled_examples_order(Order), reduction(Reduction)).
Fetch = [builtins,bk,metarules],
Table = Untable, Untable = false,
Limit = 5,
Invented = 1,
Gestalt = Respecialise, Respecialise = true,
Unlabelled = 100,
Order = random,
Reduction = plotkins.

% Do this to log on screen new generated examples and learned hypotheses.
% Omitted from this experiment for brevity.
% ?- debug(experiment_full).
% true.

% `experiments(even,10,25,0,10,[Acc,TPR,TNR])` means: "with `even` as the true
% theory run 10 experiments, generating 25 initial examples for each, with a
% random number of digits between 0 and 10. 
% Initial examples are true positives of the target theory.
% Acc, TPR and TNR return the means of accuracy, true positive rate and true
% negative reate, respectively, measured on the new examples generated by each
% learned hypothesis (i.e. not from the target theory).
% 
?- test_harness:experiments(even,10,25,0,10,[Acc,TPR,TNR]).
% Experiment 1 of 10
% 61,318,916 inferences, 4.109 CPU in 10.501 seconds (39% CPU, 14921713 Lips)
% Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Experiment 2 of 10
% 44,941,852 inferences, 1.719 CPU in 8.442 seconds (20% CPU, 26147987 Lips)
% Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Experiment 3 of 10
% 60,520,169 inferences, 4.016 CPU in 13.880 seconds (29% CPU, 15071170 Lips)
% Measured Acc: 0.6809 TPR: 0.6809 TNR: 0
% Experiment 4 of 10
% 84,042,867 inferences, 6.266 CPU in 23.160 seconds (27% CPU, 13413325 Lips)
% Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Experiment 5 of 10
% 95,984,652 inferences, 7.312 CPU in 26.190 seconds (28% CPU, 13126106 Lips)
% Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Experiment 6 of 10
% 69,709,471 inferences, 5.703 CPU in 17.929 seconds (32% CPU, 12223031 Lips)
% Measured Acc: 0.6939 TPR: 0.6939 TNR: 0
% Experiment 7 of 10
% 70,344,550 inferences, 4.109 CPU in 18.810 seconds (22% CPU, 17118065 Lips)
% Measured Acc: 0.6875 TPR: 0.6875 TNR: 0
% Experiment 8 of 10
% 82,028,814 inferences, 7.328 CPU in 22.223 seconds (33% CPU, 11193697 Lips)
% Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
% Experiment 9 of 10
% 59,268,977 inferences, 4.422 CPU in 15.399 seconds (29% CPU, 13403585 Lips)
% Measured Acc: 0.7 TPR: 0.7 TNR: 0
% Experiment 10 of 10
% 80,133,553 inferences, 4.766 CPU in 22.337 seconds (21% CPU, 16814909 Lips)
% Measured Acc: 1.0 TPR: 1.0 TNR: 1.0
Acc = TPR, TPR = 0.8762,
TNR = 0.6.
```

That is not perfect, but not that bad given the difficulty of the task. Note in
particular that Poker is not a classifier, but learns arbitrary programs. Poker
is the only system its author (me!) knows that can do that. Modern Large
Language Models (LLMs) can of course generate precise parity definitions on
demand, even zero-shot, but for an LLM to be able to generate any program it
must be trained on examples of similar programs. Poker is an inductive program
synthesis system that never sees examples of the _programs_ it has to learn
during training; only examples of their inputs and outputs.
