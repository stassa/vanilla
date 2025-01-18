Poker: Self-supervised Meta-Interpretive Learning 
==================================================

Poker is a self-supervised Meta-Interpretive Learning system based on
[Louise](https://github.com/stassa/vanilla/tree/master/lib/louise) and its Top
Program Construction algorithm. It learns a maximally special hypothesis that
generalises an initial example by deriving sets of new positive and negative
examples that are consistent with the initial example; hence "self-supervised":
because it generates its own positive and negative examples as it goes.

Poker is named not after the card game but after [Wittgenstein's
Poker](https://en.wikipedia.org/wiki/Wittgenstein%27s_Poker).

How Poker works
---------------

Poker starts with one initial example assumed to be a positive example of some
unknown predicate. Poker invokes the Top Program Construction algorithm to learn
an initial, maximally general hypothesis from this example, then proceeds to
generate new examples from that initial hypothesis. Each new generated example
is assumed to be negative and used to specialise the hypothesis so-far by
removing sub-hypotheses that cover it. If the hypothesis no longer covers any of
the positive examples found so-far, the new example is added to the set of the
positive examples and the hypothesis put back the way it was. Otherwise, the
example is kept in the set of negative examples. This process repeats until no
more examples can be generated; a user can select a limit on the number of
examples to be generated to avoid generating infinite examples. 

The result of the process described above is a set of clauses that comprise the
most specific hypothesis that generalises the initial example, with respect to
the background knowledge. To avoid confusion with the Least General
Generalisation of a clause, we call this program the Most Specific
Generalisation of an atom, abbreviated to MSG; not to be confused with the food
additive, or the Heavy Rock group.

Example session with Poker
--------------------------

Below, Poker is shown learning a grammar for the context-free `a^nb^n` language
from the string `aaabbb` (given in Definite Clause Grammars form) as an initial
example. From this example Poker learns a Top Program consisting of two
sub-programs, each with recursive clauses and invented predicates. We list this
Top Program below:

```
inv_1_7(A,B):-a(A,C),s(C,B).
inv_1_8(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_8(C,B).
s(A,B):-inv_1_7(A,C),b(C,B).
```

In the procerss Poker generates 1018 new examples and labels three of them as
positive and the rest as negative. The following are the 3 positive examples
generated this way:

```
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,a,b,b,b,b],[]).
```

And here are the first 10 of the negative examples generated:

```
s([b,b,b,b,b,b,b,b,b],[]).
s([b,b,b,b,b,b,b,b,a],[]).
s([b,b,b,b,b,b,b,b],[]).
s([b,b,b,b,b,b,b,a,b],[]).
s([b,b,b,b,b,b,b,a,a],[]).
s([b,b,b,b,b,b,b,a],[]).
s([b,b,b,b,b,b,b],[]).
s([b,b,b,b,b,b,a,b,b],[]).
s([b,b,b,b,b,b,a,b,a],[]).
s([b,b,b,b,b,b,a,b],[]).
```

The reader can verify that the above examples are labelled correctly with
respect to the target theory of `a^nb^n` ("n a's followed by n b's").

Both sub-programs and their union, the Top Program, are  MSGs of the initial
example. They are also correct hypotheses in the sense that they cover the
initial example, and none of the negative examples derived during learning.

`a^nb^n` strings in Definite Clause Grammars form contain lists and Poker
imposes a limit on the length of lists in generated examples to avoid generating
infinitely many, infinite-length lists. The limit is set in the predicate
`safe_example/1` defined in the experiment file holding the background theory
used in learning.

The experiment file used for this demonstration is in the following path:

`data/examples/anbn_poker.pl`. 

That experimnet file also holds positive and negative examples used with the
other MIL systems included with Vanilla. Those labelled examples are _not_ used
to train Poker. They are only there for easy comparison with other systems.

Below is a big friendly dump of the entire learning session for the user's
delectation.

```
% List the MIL problem elements, including a single initial example.
?- poker_auxiliaries:list_mil_problem(s/2).
Unlabelled examples
-------------------
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

true.

% List the definition fo safe_example/1 to show the length limit imposed on
% lists in generated examples.
anbn:safe_example(m(s, Ls, [])) :-
    between(1, 9, L),
    length(Ls, L).

% Show number of new examples generated during learning.
?- debug(generate).
true.

% Learn you a grammar of anbn:
?- time( poker:learn(s([a,a,a,b,b,b],[])) ).

% Generated 1018 new atoms:
Hypothesis:
inv_1_7(A,B):-a(A,C),s(C,B).
inv_1_8(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_8(C,B).
s(A,B):-inv_1_7(A,C),b(C,B).
Positive examples:
s([a,b],[]).
s([a,a,b,b],[]).
s([a,a,a,a,b,b,b,b],[]).
s([a,a,a,b,b,b],[]).
Negative examples:
s([b,b,b,b,b,b,b,b,b],[]).
s([b,b,b,b,b,b,b,b,a],[]).
s([b,b,b,b,b,b,b,b],[]).
s([b,b,b,b,b,b,b,a,b],[]).
s([b,b,b,b,b,b,b,a,a],[]).
s([b,b,b,b,b,b,b,a],[]).
s([b,b,b,b,b,b,b],[]).
s([b,b,b,b,b,b,a,b,b],[]).
s([b,b,b,b,b,b,a,b,a],[]).
s([b,b,b,b,b,b,a,b],[]).
s([b,b,b,b,b,b,a,a,b],[]).
s([b,b,b,b,b,b,a,a,a],[]).
s([b,b,b,b,b,b,a,a],[]).
s([b,b,b,b,b,b,a],[]).
s([b,b,b,b,b,b],[]).
s([b,b,b,b,b,a,b,b,b],[]).
s([b,b,b,b,b,a,b,b,a],[]).
s([b,b,b,b,b,a,b,b],[]).
s([b,b,b,b,b,a,b,a,b],[]).
s([b,b,b,b,b,a,b,a,a],[]).
s([b,b,b,b,b,a,b,a],[]).
s([b,b,b,b,b,a,b],[]).
s([b,b,b,b,b,a,a,b,b],[]).
s([b,b,b,b,b,a,a,b,a],[]).
s([b,b,b,b,b,a,a,b],[]).
s([b,b,b,b,b,a,a,a,b],[]).
s([b,b,b,b,b,a,a,a,a],[]).
s([b,b,b,b,b,a,a,a],[]).
s([b,b,b,b,b,a,a],[]).
s([b,b,b,b,b,a],[]).
s([b,b,b,b,b],[]).
s([b,b,b,b,a,b,b,b,b],[]).
s([b,b,b,b,a,b,b,b,a],[]).
s([b,b,b,b,a,b,b,b],[]).
s([b,b,b,b,a,b,b,a,b],[]).
s([b,b,b,b,a,b,b,a,a],[]).
s([b,b,b,b,a,b,b,a],[]).
s([b,b,b,b,a,b,b],[]).
s([b,b,b,b,a,b,a,b,b],[]).
s([b,b,b,b,a,b,a,b,a],[]).
s([b,b,b,b,a,b,a,b],[]).
s([b,b,b,b,a,b,a,a,b],[]).
s([b,b,b,b,a,b,a,a,a],[]).
s([b,b,b,b,a,b,a,a],[]).
s([b,b,b,b,a,b,a],[]).
s([b,b,b,b,a,b],[]).
s([b,b,b,b,a,a,b,b,b],[]).
s([b,b,b,b,a,a,b,b,a],[]).
s([b,b,b,b,a,a,b,b],[]).
s([b,b,b,b,a,a,b,a,b],[]).
s([b,b,b,b,a,a,b,a,a],[]).
s([b,b,b,b,a,a,b,a],[]).
s([b,b,b,b,a,a,b],[]).
s([b,b,b,b,a,a,a,b,b],[]).
s([b,b,b,b,a,a,a,b,a],[]).
s([b,b,b,b,a,a,a,b],[]).
s([b,b,b,b,a,a,a,a,b],[]).
s([b,b,b,b,a,a,a,a,a],[]).
s([b,b,b,b,a,a,a,a],[]).
s([b,b,b,b,a,a,a],[]).
s([b,b,b,b,a,a],[]).
s([b,b,b,b,a],[]).
s([b,b,b,b],[]).
s([b,b,b,a,b,b,b,b,b],[]).
s([b,b,b,a,b,b,b,b,a],[]).
s([b,b,b,a,b,b,b,b],[]).
s([b,b,b,a,b,b,b,a,b],[]).
s([b,b,b,a,b,b,b,a,a],[]).
s([b,b,b,a,b,b,b,a],[]).
s([b,b,b,a,b,b,b],[]).
s([b,b,b,a,b,b,a,b,b],[]).
s([b,b,b,a,b,b,a,b,a],[]).
s([b,b,b,a,b,b,a,b],[]).
s([b,b,b,a,b,b,a,a,b],[]).
s([b,b,b,a,b,b,a,a,a],[]).
s([b,b,b,a,b,b,a,a],[]).
s([b,b,b,a,b,b,a],[]).
s([b,b,b,a,b,b],[]).
s([b,b,b,a,b,a,b,b,b],[]).
s([b,b,b,a,b,a,b,b,a],[]).
s([b,b,b,a,b,a,b,b],[]).
s([b,b,b,a,b,a,b,a,b],[]).
s([b,b,b,a,b,a,b,a,a],[]).
s([b,b,b,a,b,a,b,a],[]).
s([b,b,b,a,b,a,b],[]).
s([b,b,b,a,b,a,a,b,b],[]).
s([b,b,b,a,b,a,a,b,a],[]).
s([b,b,b,a,b,a,a,b],[]).
s([b,b,b,a,b,a,a,a,b],[]).
s([b,b,b,a,b,a,a,a,a],[]).
s([b,b,b,a,b,a,a,a],[]).
s([b,b,b,a,b,a,a],[]).
s([b,b,b,a,b,a],[]).
s([b,b,b,a,b],[]).
s([b,b,b,a,a,b,b,b,b],[]).
s([b,b,b,a,a,b,b,b,a],[]).
s([b,b,b,a,a,b,b,b],[]).
s([b,b,b,a,a,b,b,a,b],[]).
s([b,b,b,a,a,b,b,a,a],[]).
s([b,b,b,a,a,b,b,a],[]).
s([b,b,b,a,a,b,b],[]).
s([b,b,b,a,a,b,a,b,b],[]).
s([b,b,b,a,a,b,a,b,a],[]).
s([b,b,b,a,a,b,a,b],[]).
s([b,b,b,a,a,b,a,a,b],[]).
s([b,b,b,a,a,b,a,a,a],[]).
s([b,b,b,a,a,b,a,a],[]).
s([b,b,b,a,a,b,a],[]).
s([b,b,b,a,a,b],[]).
s([b,b,b,a,a,a,b,b,b],[]).
s([b,b,b,a,a,a,b,b,a],[]).
s([b,b,b,a,a,a,b,b],[]).
s([b,b,b,a,a,a,b,a,b],[]).
s([b,b,b,a,a,a,b,a,a],[]).
s([b,b,b,a,a,a,b,a],[]).
s([b,b,b,a,a,a,b],[]).
s([b,b,b,a,a,a,a,b,b],[]).
s([b,b,b,a,a,a,a,b,a],[]).
s([b,b,b,a,a,a,a,b],[]).
s([b,b,b,a,a,a,a,a,b],[]).
s([b,b,b,a,a,a,a,a,a],[]).
s([b,b,b,a,a,a,a,a],[]).
s([b,b,b,a,a,a,a],[]).
s([b,b,b,a,a,a],[]).
s([b,b,b,a,a],[]).
s([b,b,b,a],[]).
s([b,b,b],[]).
s([b,b,a,b,b,b,b,b,b],[]).
s([b,b,a,b,b,b,b,b,a],[]).
s([b,b,a,b,b,b,b,b],[]).
s([b,b,a,b,b,b,b,a,b],[]).
s([b,b,a,b,b,b,b,a,a],[]).
s([b,b,a,b,b,b,b,a],[]).
s([b,b,a,b,b,b,b],[]).
s([b,b,a,b,b,b,a,b,b],[]).
s([b,b,a,b,b,b,a,b,a],[]).
s([b,b,a,b,b,b,a,b],[]).
s([b,b,a,b,b,b,a,a,b],[]).
s([b,b,a,b,b,b,a,a,a],[]).
s([b,b,a,b,b,b,a,a],[]).
s([b,b,a,b,b,b,a],[]).
s([b,b,a,b,b,b],[]).
s([b,b,a,b,b,a,b,b,b],[]).
s([b,b,a,b,b,a,b,b,a],[]).
s([b,b,a,b,b,a,b,b],[]).
s([b,b,a,b,b,a,b,a,b],[]).
s([b,b,a,b,b,a,b,a,a],[]).
s([b,b,a,b,b,a,b,a],[]).
s([b,b,a,b,b,a,b],[]).
s([b,b,a,b,b,a,a,b,b],[]).
s([b,b,a,b,b,a,a,b,a],[]).
s([b,b,a,b,b,a,a,b],[]).
s([b,b,a,b,b,a,a,a,b],[]).
s([b,b,a,b,b,a,a,a,a],[]).
s([b,b,a,b,b,a,a,a],[]).
s([b,b,a,b,b,a,a],[]).
s([b,b,a,b,b,a],[]).
s([b,b,a,b,b],[]).
s([b,b,a,b,a,b,b,b,b],[]).
s([b,b,a,b,a,b,b,b,a],[]).
s([b,b,a,b,a,b,b,b],[]).
s([b,b,a,b,a,b,b,a,b],[]).
s([b,b,a,b,a,b,b,a,a],[]).
s([b,b,a,b,a,b,b,a],[]).
s([b,b,a,b,a,b,b],[]).
s([b,b,a,b,a,b,a,b,b],[]).
s([b,b,a,b,a,b,a,b,a],[]).
s([b,b,a,b,a,b,a,b],[]).
s([b,b,a,b,a,b,a,a,b],[]).
s([b,b,a,b,a,b,a,a,a],[]).
s([b,b,a,b,a,b,a,a],[]).
s([b,b,a,b,a,b,a],[]).
s([b,b,a,b,a,b],[]).
s([b,b,a,b,a,a,b,b,b],[]).
s([b,b,a,b,a,a,b,b,a],[]).
s([b,b,a,b,a,a,b,b],[]).
s([b,b,a,b,a,a,b,a,b],[]).
s([b,b,a,b,a,a,b,a,a],[]).
s([b,b,a,b,a,a,b,a],[]).
s([b,b,a,b,a,a,b],[]).
s([b,b,a,b,a,a,a,b,b],[]).
s([b,b,a,b,a,a,a,b,a],[]).
s([b,b,a,b,a,a,a,b],[]).
s([b,b,a,b,a,a,a,a,b],[]).
s([b,b,a,b,a,a,a,a,a],[]).
s([b,b,a,b,a,a,a,a],[]).
s([b,b,a,b,a,a,a],[]).
s([b,b,a,b,a,a],[]).
s([b,b,a,b,a],[]).
s([b,b,a,b],[]).
s([b,b,a,a,b,b,b,b,b],[]).
s([b,b,a,a,b,b,b,b,a],[]).
s([b,b,a,a,b,b,b,b],[]).
s([b,b,a,a,b,b,b,a,b],[]).
s([b,b,a,a,b,b,b,a,a],[]).
s([b,b,a,a,b,b,b,a],[]).
s([b,b,a,a,b,b,b],[]).
s([b,b,a,a,b,b,a,b,b],[]).
s([b,b,a,a,b,b,a,b,a],[]).
s([b,b,a,a,b,b,a,b],[]).
s([b,b,a,a,b,b,a,a,b],[]).
s([b,b,a,a,b,b,a,a,a],[]).
s([b,b,a,a,b,b,a,a],[]).
s([b,b,a,a,b,b,a],[]).
s([b,b,a,a,b,b],[]).
s([b,b,a,a,b,a,b,b,b],[]).
s([b,b,a,a,b,a,b,b,a],[]).
s([b,b,a,a,b,a,b,b],[]).
s([b,b,a,a,b,a,b,a,b],[]).
s([b,b,a,a,b,a,b,a,a],[]).
s([b,b,a,a,b,a,b,a],[]).
s([b,b,a,a,b,a,b],[]).
s([b,b,a,a,b,a,a,b,b],[]).
s([b,b,a,a,b,a,a,b,a],[]).
s([b,b,a,a,b,a,a,b],[]).
s([b,b,a,a,b,a,a,a,b],[]).
s([b,b,a,a,b,a,a,a,a],[]).
s([b,b,a,a,b,a,a,a],[]).
s([b,b,a,a,b,a,a],[]).
s([b,b,a,a,b,a],[]).
s([b,b,a,a,b],[]).
s([b,b,a,a,a,b,b,b,b],[]).
s([b,b,a,a,a,b,b,b,a],[]).
s([b,b,a,a,a,b,b,b],[]).
s([b,b,a,a,a,b,b,a,b],[]).
s([b,b,a,a,a,b,b,a,a],[]).
s([b,b,a,a,a,b,b,a],[]).
s([b,b,a,a,a,b,b],[]).
s([b,b,a,a,a,b,a,b,b],[]).
s([b,b,a,a,a,b,a,b,a],[]).
s([b,b,a,a,a,b,a,b],[]).
s([b,b,a,a,a,b,a,a,b],[]).
s([b,b,a,a,a,b,a,a,a],[]).
s([b,b,a,a,a,b,a,a],[]).
s([b,b,a,a,a,b,a],[]).
s([b,b,a,a,a,b],[]).
s([b,b,a,a,a,a,b,b,b],[]).
s([b,b,a,a,a,a,b,b,a],[]).
s([b,b,a,a,a,a,b,b],[]).
s([b,b,a,a,a,a,b,a,b],[]).
s([b,b,a,a,a,a,b,a,a],[]).
s([b,b,a,a,a,a,b,a],[]).
s([b,b,a,a,a,a,b],[]).
s([b,b,a,a,a,a,a,b,b],[]).
s([b,b,a,a,a,a,a,b,a],[]).
s([b,b,a,a,a,a,a,b],[]).
s([b,b,a,a,a,a,a,a,b],[]).
s([b,b,a,a,a,a,a,a,a],[]).
s([b,b,a,a,a,a,a,a],[]).
s([b,b,a,a,a,a,a],[]).
s([b,b,a,a,a,a],[]).
s([b,b,a,a,a],[]).
s([b,b,a,a],[]).
s([b,b,a],[]).
s([b,b],[]).
s([b,a,b,b,b,b,b,b,b],[]).
s([b,a,b,b,b,b,b,b,a],[]).
s([b,a,b,b,b,b,b,b],[]).
s([b,a,b,b,b,b,b,a,b],[]).
s([b,a,b,b,b,b,b,a,a],[]).
s([b,a,b,b,b,b,b,a],[]).
s([b,a,b,b,b,b,b],[]).
s([b,a,b,b,b,b,a,b,b],[]).
s([b,a,b,b,b,b,a,b,a],[]).
s([b,a,b,b,b,b,a,b],[]).
s([b,a,b,b,b,b,a,a,b],[]).
s([b,a,b,b,b,b,a,a,a],[]).
s([b,a,b,b,b,b,a,a],[]).
s([b,a,b,b,b,b,a],[]).
s([b,a,b,b,b,b],[]).
s([b,a,b,b,b,a,b,b,b],[]).
s([b,a,b,b,b,a,b,b,a],[]).
s([b,a,b,b,b,a,b,b],[]).
s([b,a,b,b,b,a,b,a,b],[]).
s([b,a,b,b,b,a,b,a,a],[]).
s([b,a,b,b,b,a,b,a],[]).
s([b,a,b,b,b,a,b],[]).
s([b,a,b,b,b,a,a,b,b],[]).
s([b,a,b,b,b,a,a,b,a],[]).
s([b,a,b,b,b,a,a,b],[]).
s([b,a,b,b,b,a,a,a,b],[]).
s([b,a,b,b,b,a,a,a,a],[]).
s([b,a,b,b,b,a,a,a],[]).
s([b,a,b,b,b,a,a],[]).
s([b,a,b,b,b,a],[]).
s([b,a,b,b,b],[]).
s([b,a,b,b,a,b,b,b,b],[]).
s([b,a,b,b,a,b,b,b,a],[]).
s([b,a,b,b,a,b,b,b],[]).
s([b,a,b,b,a,b,b,a,b],[]).
s([b,a,b,b,a,b,b,a,a],[]).
s([b,a,b,b,a,b,b,a],[]).
s([b,a,b,b,a,b,b],[]).
s([b,a,b,b,a,b,a,b,b],[]).
s([b,a,b,b,a,b,a,b,a],[]).
s([b,a,b,b,a,b,a,b],[]).
s([b,a,b,b,a,b,a,a,b],[]).
s([b,a,b,b,a,b,a,a,a],[]).
s([b,a,b,b,a,b,a,a],[]).
s([b,a,b,b,a,b,a],[]).
s([b,a,b,b,a,b],[]).
s([b,a,b,b,a,a,b,b,b],[]).
s([b,a,b,b,a,a,b,b,a],[]).
s([b,a,b,b,a,a,b,b],[]).
s([b,a,b,b,a,a,b,a,b],[]).
s([b,a,b,b,a,a,b,a,a],[]).
s([b,a,b,b,a,a,b,a],[]).
s([b,a,b,b,a,a,b],[]).
s([b,a,b,b,a,a,a,b,b],[]).
s([b,a,b,b,a,a,a,b,a],[]).
s([b,a,b,b,a,a,a,b],[]).
s([b,a,b,b,a,a,a,a,b],[]).
s([b,a,b,b,a,a,a,a,a],[]).
s([b,a,b,b,a,a,a,a],[]).
s([b,a,b,b,a,a,a],[]).
s([b,a,b,b,a,a],[]).
s([b,a,b,b,a],[]).
s([b,a,b,b],[]).
s([b,a,b,a,b,b,b,b,b],[]).
s([b,a,b,a,b,b,b,b,a],[]).
s([b,a,b,a,b,b,b,b],[]).
s([b,a,b,a,b,b,b,a,b],[]).
s([b,a,b,a,b,b,b,a,a],[]).
s([b,a,b,a,b,b,b,a],[]).
s([b,a,b,a,b,b,b],[]).
s([b,a,b,a,b,b,a,b,b],[]).
s([b,a,b,a,b,b,a,b,a],[]).
s([b,a,b,a,b,b,a,b],[]).
s([b,a,b,a,b,b,a,a,b],[]).
s([b,a,b,a,b,b,a,a,a],[]).
s([b,a,b,a,b,b,a,a],[]).
s([b,a,b,a,b,b,a],[]).
s([b,a,b,a,b,b],[]).
s([b,a,b,a,b,a,b,b,b],[]).
s([b,a,b,a,b,a,b,b,a],[]).
s([b,a,b,a,b,a,b,b],[]).
s([b,a,b,a,b,a,b,a,b],[]).
s([b,a,b,a,b,a,b,a,a],[]).
s([b,a,b,a,b,a,b,a],[]).
s([b,a,b,a,b,a,b],[]).
s([b,a,b,a,b,a,a,b,b],[]).
s([b,a,b,a,b,a,a,b,a],[]).
s([b,a,b,a,b,a,a,b],[]).
s([b,a,b,a,b,a,a,a,b],[]).
s([b,a,b,a,b,a,a,a,a],[]).
s([b,a,b,a,b,a,a,a],[]).
s([b,a,b,a,b,a,a],[]).
s([b,a,b,a,b,a],[]).
s([b,a,b,a,b],[]).
s([b,a,b,a,a,b,b,b,b],[]).
s([b,a,b,a,a,b,b,b,a],[]).
s([b,a,b,a,a,b,b,b],[]).
s([b,a,b,a,a,b,b,a,b],[]).
s([b,a,b,a,a,b,b,a,a],[]).
s([b,a,b,a,a,b,b,a],[]).
s([b,a,b,a,a,b,b],[]).
s([b,a,b,a,a,b,a,b,b],[]).
s([b,a,b,a,a,b,a,b,a],[]).
s([b,a,b,a,a,b,a,b],[]).
s([b,a,b,a,a,b,a,a,b],[]).
s([b,a,b,a,a,b,a,a,a],[]).
s([b,a,b,a,a,b,a,a],[]).
s([b,a,b,a,a,b,a],[]).
s([b,a,b,a,a,b],[]).
s([b,a,b,a,a,a,b,b,b],[]).
s([b,a,b,a,a,a,b,b,a],[]).
s([b,a,b,a,a,a,b,b],[]).
s([b,a,b,a,a,a,b,a,b],[]).
s([b,a,b,a,a,a,b,a,a],[]).
s([b,a,b,a,a,a,b,a],[]).
s([b,a,b,a,a,a,b],[]).
s([b,a,b,a,a,a,a,b,b],[]).
s([b,a,b,a,a,a,a,b,a],[]).
s([b,a,b,a,a,a,a,b],[]).
s([b,a,b,a,a,a,a,a,b],[]).
s([b,a,b,a,a,a,a,a,a],[]).
s([b,a,b,a,a,a,a,a],[]).
s([b,a,b,a,a,a,a],[]).
s([b,a,b,a,a,a],[]).
s([b,a,b,a,a],[]).
s([b,a,b,a],[]).
s([b,a,b],[]).
s([b,a,a,b,b,b,b,b,b],[]).
s([b,a,a,b,b,b,b,b,a],[]).
s([b,a,a,b,b,b,b,b],[]).
s([b,a,a,b,b,b,b,a,b],[]).
s([b,a,a,b,b,b,b,a,a],[]).
s([b,a,a,b,b,b,b,a],[]).
s([b,a,a,b,b,b,b],[]).
s([b,a,a,b,b,b,a,b,b],[]).
s([b,a,a,b,b,b,a,b,a],[]).
s([b,a,a,b,b,b,a,b],[]).
s([b,a,a,b,b,b,a,a,b],[]).
s([b,a,a,b,b,b,a,a,a],[]).
s([b,a,a,b,b,b,a,a],[]).
s([b,a,a,b,b,b,a],[]).
s([b,a,a,b,b,b],[]).
s([b,a,a,b,b,a,b,b,b],[]).
s([b,a,a,b,b,a,b,b,a],[]).
s([b,a,a,b,b,a,b,b],[]).
s([b,a,a,b,b,a,b,a,b],[]).
s([b,a,a,b,b,a,b,a,a],[]).
s([b,a,a,b,b,a,b,a],[]).
s([b,a,a,b,b,a,b],[]).
s([b,a,a,b,b,a,a,b,b],[]).
s([b,a,a,b,b,a,a,b,a],[]).
s([b,a,a,b,b,a,a,b],[]).
s([b,a,a,b,b,a,a,a,b],[]).
s([b,a,a,b,b,a,a,a,a],[]).
s([b,a,a,b,b,a,a,a],[]).
s([b,a,a,b,b,a,a],[]).
s([b,a,a,b,b,a],[]).
s([b,a,a,b,b],[]).
s([b,a,a,b,a,b,b,b,b],[]).
s([b,a,a,b,a,b,b,b,a],[]).
s([b,a,a,b,a,b,b,b],[]).
s([b,a,a,b,a,b,b,a,b],[]).
s([b,a,a,b,a,b,b,a,a],[]).
s([b,a,a,b,a,b,b,a],[]).
s([b,a,a,b,a,b,b],[]).
s([b,a,a,b,a,b,a,b,b],[]).
s([b,a,a,b,a,b,a,b,a],[]).
s([b,a,a,b,a,b,a,b],[]).
s([b,a,a,b,a,b,a,a,b],[]).
s([b,a,a,b,a,b,a,a,a],[]).
s([b,a,a,b,a,b,a,a],[]).
s([b,a,a,b,a,b,a],[]).
s([b,a,a,b,a,b],[]).
s([b,a,a,b,a,a,b,b,b],[]).
s([b,a,a,b,a,a,b,b,a],[]).
s([b,a,a,b,a,a,b,b],[]).
s([b,a,a,b,a,a,b,a,b],[]).
s([b,a,a,b,a,a,b,a,a],[]).
s([b,a,a,b,a,a,b,a],[]).
s([b,a,a,b,a,a,b],[]).
s([b,a,a,b,a,a,a,b,b],[]).
s([b,a,a,b,a,a,a,b,a],[]).
s([b,a,a,b,a,a,a,b],[]).
s([b,a,a,b,a,a,a,a,b],[]).
s([b,a,a,b,a,a,a,a,a],[]).
s([b,a,a,b,a,a,a,a],[]).
s([b,a,a,b,a,a,a],[]).
s([b,a,a,b,a,a],[]).
s([b,a,a,b,a],[]).
s([b,a,a,b],[]).
s([b,a,a,a,b,b,b,b,b],[]).
s([b,a,a,a,b,b,b,b,a],[]).
s([b,a,a,a,b,b,b,b],[]).
s([b,a,a,a,b,b,b,a,b],[]).
s([b,a,a,a,b,b,b,a,a],[]).
s([b,a,a,a,b,b,b,a],[]).
s([b,a,a,a,b,b,b],[]).
s([b,a,a,a,b,b,a,b,b],[]).
s([b,a,a,a,b,b,a,b,a],[]).
s([b,a,a,a,b,b,a,b],[]).
s([b,a,a,a,b,b,a,a,b],[]).
s([b,a,a,a,b,b,a,a,a],[]).
s([b,a,a,a,b,b,a,a],[]).
s([b,a,a,a,b,b,a],[]).
s([b,a,a,a,b,b],[]).
s([b,a,a,a,b,a,b,b,b],[]).
s([b,a,a,a,b,a,b,b,a],[]).
s([b,a,a,a,b,a,b,b],[]).
s([b,a,a,a,b,a,b,a,b],[]).
s([b,a,a,a,b,a,b,a,a],[]).
s([b,a,a,a,b,a,b,a],[]).
s([b,a,a,a,b,a,b],[]).
s([b,a,a,a,b,a,a,b,b],[]).
s([b,a,a,a,b,a,a,b,a],[]).
s([b,a,a,a,b,a,a,b],[]).
s([b,a,a,a,b,a,a,a,b],[]).
s([b,a,a,a,b,a,a,a,a],[]).
s([b,a,a,a,b,a,a,a],[]).
s([b,a,a,a,b,a,a],[]).
s([b,a,a,a,b,a],[]).
s([b,a,a,a,b],[]).
s([b,a,a,a,a,b,b,b,b],[]).
s([b,a,a,a,a,b,b,b,a],[]).
s([b,a,a,a,a,b,b,b],[]).
s([b,a,a,a,a,b,b,a,b],[]).
s([b,a,a,a,a,b,b,a,a],[]).
s([b,a,a,a,a,b,b,a],[]).
s([b,a,a,a,a,b,b],[]).
s([b,a,a,a,a,b,a,b,b],[]).
s([b,a,a,a,a,b,a,b,a],[]).
s([b,a,a,a,a,b,a,b],[]).
s([b,a,a,a,a,b,a,a,b],[]).
s([b,a,a,a,a,b,a,a,a],[]).
s([b,a,a,a,a,b,a,a],[]).
s([b,a,a,a,a,b,a],[]).
s([b,a,a,a,a,b],[]).
s([b,a,a,a,a,a,b,b,b],[]).
s([b,a,a,a,a,a,b,b,a],[]).
s([b,a,a,a,a,a,b,b],[]).
s([b,a,a,a,a,a,b,a,b],[]).
s([b,a,a,a,a,a,b,a,a],[]).
s([b,a,a,a,a,a,b,a],[]).
s([b,a,a,a,a,a,b],[]).
s([b,a,a,a,a,a,a,b,b],[]).
s([b,a,a,a,a,a,a,b,a],[]).
s([b,a,a,a,a,a,a,b],[]).
s([b,a,a,a,a,a,a,a,b],[]).
s([b,a,a,a,a,a,a,a,a],[]).
s([b,a,a,a,a,a,a,a],[]).
s([b,a,a,a,a,a,a],[]).
s([b,a,a,a,a,a],[]).
s([b,a,a,a,a],[]).
s([b,a,a,a],[]).
s([b,a,a],[]).
s([a,b,b,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,b,a],[]).
s([a,b,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b,b,a,b],[]).
s([a,b,b,b,b,b,b,a,a],[]).
s([a,b,b,b,b,b,b,a],[]).
s([a,b,b,b,b,b,b],[]).
s([a,b,b,b,b,b,a,b,b],[]).
s([a,b,b,b,b,b,a,b,a],[]).
s([a,b,b,b,b,b,a,b],[]).
s([a,b,b,b,b,b,a,a,b],[]).
s([a,b,b,b,b,b,a,a,a],[]).
s([a,b,b,b,b,b,a,a],[]).
s([a,b,b,b,b,b,a],[]).
s([a,b,b,b,b,b],[]).
s([a,b,b,b,b,a,b,b,b],[]).
s([a,b,b,b,b,a,b,b,a],[]).
s([a,b,b,b,b,a,b,b],[]).
s([a,b,b,b,b,a,b,a,b],[]).
s([a,b,b,b,b,a,b,a,a],[]).
s([a,b,b,b,b,a,b,a],[]).
s([a,b,b,b,b,a,b],[]).
s([a,b,b,b,b,a,a,b,b],[]).
s([a,b,b,b,b,a,a,b,a],[]).
s([a,b,b,b,b,a,a,b],[]).
s([a,b,b,b,b,a,a,a,b],[]).
s([a,b,b,b,b,a,a,a,a],[]).
s([a,b,b,b,b,a,a,a],[]).
s([a,b,b,b,b,a,a],[]).
s([a,b,b,b,b,a],[]).
s([a,b,b,b,b],[]).
s([a,b,b,b,a,b,b,b,b],[]).
s([a,b,b,b,a,b,b,b,a],[]).
s([a,b,b,b,a,b,b,b],[]).
s([a,b,b,b,a,b,b,a,b],[]).
s([a,b,b,b,a,b,b,a,a],[]).
s([a,b,b,b,a,b,b,a],[]).
s([a,b,b,b,a,b,b],[]).
s([a,b,b,b,a,b,a,b,b],[]).
s([a,b,b,b,a,b,a,b,a],[]).
s([a,b,b,b,a,b,a,b],[]).
s([a,b,b,b,a,b,a,a,b],[]).
s([a,b,b,b,a,b,a,a,a],[]).
s([a,b,b,b,a,b,a,a],[]).
s([a,b,b,b,a,b,a],[]).
s([a,b,b,b,a,b],[]).
s([a,b,b,b,a,a,b,b,b],[]).
s([a,b,b,b,a,a,b,b,a],[]).
s([a,b,b,b,a,a,b,b],[]).
s([a,b,b,b,a,a,b,a,b],[]).
s([a,b,b,b,a,a,b,a,a],[]).
s([a,b,b,b,a,a,b,a],[]).
s([a,b,b,b,a,a,b],[]).
s([a,b,b,b,a,a,a,b,b],[]).
s([a,b,b,b,a,a,a,b,a],[]).
s([a,b,b,b,a,a,a,b],[]).
s([a,b,b,b,a,a,a,a,b],[]).
s([a,b,b,b,a,a,a,a,a],[]).
s([a,b,b,b,a,a,a,a],[]).
s([a,b,b,b,a,a,a],[]).
s([a,b,b,b,a,a],[]).
s([a,b,b,b,a],[]).
s([a,b,b,b],[]).
s([a,b,b,a,b,b,b,b,b],[]).
s([a,b,b,a,b,b,b,b,a],[]).
s([a,b,b,a,b,b,b,b],[]).
s([a,b,b,a,b,b,b,a,b],[]).
s([a,b,b,a,b,b,b,a,a],[]).
s([a,b,b,a,b,b,b,a],[]).
s([a,b,b,a,b,b,b],[]).
s([a,b,b,a,b,b,a,b,b],[]).
s([a,b,b,a,b,b,a,b,a],[]).
s([a,b,b,a,b,b,a,b],[]).
s([a,b,b,a,b,b,a,a,b],[]).
s([a,b,b,a,b,b,a,a,a],[]).
s([a,b,b,a,b,b,a,a],[]).
s([a,b,b,a,b,b,a],[]).
s([a,b,b,a,b,b],[]).
s([a,b,b,a,b,a,b,b,b],[]).
s([a,b,b,a,b,a,b,b,a],[]).
s([a,b,b,a,b,a,b,b],[]).
s([a,b,b,a,b,a,b,a,b],[]).
s([a,b,b,a,b,a,b,a,a],[]).
s([a,b,b,a,b,a,b,a],[]).
s([a,b,b,a,b,a,b],[]).
s([a,b,b,a,b,a,a,b,b],[]).
s([a,b,b,a,b,a,a,b,a],[]).
s([a,b,b,a,b,a,a,b],[]).
s([a,b,b,a,b,a,a,a,b],[]).
s([a,b,b,a,b,a,a,a,a],[]).
s([a,b,b,a,b,a,a,a],[]).
s([a,b,b,a,b,a,a],[]).
s([a,b,b,a,b,a],[]).
s([a,b,b,a,b],[]).
s([a,b,b,a,a,b,b,b,b],[]).
s([a,b,b,a,a,b,b,b,a],[]).
s([a,b,b,a,a,b,b,b],[]).
s([a,b,b,a,a,b,b,a,b],[]).
s([a,b,b,a,a,b,b,a,a],[]).
s([a,b,b,a,a,b,b,a],[]).
s([a,b,b,a,a,b,b],[]).
s([a,b,b,a,a,b,a,b,b],[]).
s([a,b,b,a,a,b,a,b,a],[]).
s([a,b,b,a,a,b,a,b],[]).
s([a,b,b,a,a,b,a,a,b],[]).
s([a,b,b,a,a,b,a,a,a],[]).
s([a,b,b,a,a,b,a,a],[]).
s([a,b,b,a,a,b,a],[]).
s([a,b,b,a,a,b],[]).
s([a,b,b,a,a,a,b,b,b],[]).
s([a,b,b,a,a,a,b,b,a],[]).
s([a,b,b,a,a,a,b,b],[]).
s([a,b,b,a,a,a,b,a,b],[]).
s([a,b,b,a,a,a,b,a,a],[]).
s([a,b,b,a,a,a,b,a],[]).
s([a,b,b,a,a,a,b],[]).
s([a,b,b,a,a,a,a,b,b],[]).
s([a,b,b,a,a,a,a,b,a],[]).
s([a,b,b,a,a,a,a,b],[]).
s([a,b,b,a,a,a,a,a,b],[]).
s([a,b,b,a,a,a,a,a,a],[]).
s([a,b,b,a,a,a,a,a],[]).
s([a,b,b,a,a,a,a],[]).
s([a,b,b,a,a,a],[]).
s([a,b,b,a,a],[]).
s([a,b,b,a],[]).
s([a,b,b],[]).
s([a,b,a,b,b,b,b,b,b],[]).
s([a,b,a,b,b,b,b,b,a],[]).
s([a,b,a,b,b,b,b,b],[]).
s([a,b,a,b,b,b,b,a,b],[]).
s([a,b,a,b,b,b,b,a,a],[]).
s([a,b,a,b,b,b,b,a],[]).
s([a,b,a,b,b,b,b],[]).
s([a,b,a,b,b,b,a,b,b],[]).
s([a,b,a,b,b,b,a,b,a],[]).
s([a,b,a,b,b,b,a,b],[]).
s([a,b,a,b,b,b,a,a,b],[]).
s([a,b,a,b,b,b,a,a,a],[]).
s([a,b,a,b,b,b,a,a],[]).
s([a,b,a,b,b,b,a],[]).
s([a,b,a,b,b,b],[]).
s([a,b,a,b,b,a,b,b,b],[]).
s([a,b,a,b,b,a,b,b,a],[]).
s([a,b,a,b,b,a,b,b],[]).
s([a,b,a,b,b,a,b,a,b],[]).
s([a,b,a,b,b,a,b,a,a],[]).
s([a,b,a,b,b,a,b,a],[]).
s([a,b,a,b,b,a,b],[]).
s([a,b,a,b,b,a,a,b,b],[]).
s([a,b,a,b,b,a,a,b,a],[]).
s([a,b,a,b,b,a,a,b],[]).
s([a,b,a,b,b,a,a,a,b],[]).
s([a,b,a,b,b,a,a,a,a],[]).
s([a,b,a,b,b,a,a,a],[]).
s([a,b,a,b,b,a,a],[]).
s([a,b,a,b,b,a],[]).
s([a,b,a,b,b],[]).
s([a,b,a,b,a,b,b,b,b],[]).
s([a,b,a,b,a,b,b,b,a],[]).
s([a,b,a,b,a,b,b,b],[]).
s([a,b,a,b,a,b,b,a,b],[]).
s([a,b,a,b,a,b,b,a,a],[]).
s([a,b,a,b,a,b,b,a],[]).
s([a,b,a,b,a,b,b],[]).
s([a,b,a,b,a,b,a,b,b],[]).
s([a,b,a,b,a,b,a,b,a],[]).
s([a,b,a,b,a,b,a,b],[]).
s([a,b,a,b,a,b,a,a,b],[]).
s([a,b,a,b,a,b,a,a,a],[]).
s([a,b,a,b,a,b,a,a],[]).
s([a,b,a,b,a,b,a],[]).
s([a,b,a,b,a,b],[]).
s([a,b,a,b,a,a,b,b,b],[]).
s([a,b,a,b,a,a,b,b,a],[]).
s([a,b,a,b,a,a,b,b],[]).
s([a,b,a,b,a,a,b,a,b],[]).
s([a,b,a,b,a,a,b,a,a],[]).
s([a,b,a,b,a,a,b,a],[]).
s([a,b,a,b,a,a,b],[]).
s([a,b,a,b,a,a,a,b,b],[]).
s([a,b,a,b,a,a,a,b,a],[]).
s([a,b,a,b,a,a,a,b],[]).
s([a,b,a,b,a,a,a,a,b],[]).
s([a,b,a,b,a,a,a,a,a],[]).
s([a,b,a,b,a,a,a,a],[]).
s([a,b,a,b,a,a,a],[]).
s([a,b,a,b,a,a],[]).
s([a,b,a,b,a],[]).
s([a,b,a,b],[]).
s([a,b,a,a,b,b,b,b,b],[]).
s([a,b,a,a,b,b,b,b,a],[]).
s([a,b,a,a,b,b,b,b],[]).
s([a,b,a,a,b,b,b,a,b],[]).
s([a,b,a,a,b,b,b,a,a],[]).
s([a,b,a,a,b,b,b,a],[]).
s([a,b,a,a,b,b,b],[]).
s([a,b,a,a,b,b,a,b,b],[]).
s([a,b,a,a,b,b,a,b,a],[]).
s([a,b,a,a,b,b,a,b],[]).
s([a,b,a,a,b,b,a,a,b],[]).
s([a,b,a,a,b,b,a,a,a],[]).
s([a,b,a,a,b,b,a,a],[]).
s([a,b,a,a,b,b,a],[]).
s([a,b,a,a,b,b],[]).
s([a,b,a,a,b,a,b,b,b],[]).
s([a,b,a,a,b,a,b,b,a],[]).
s([a,b,a,a,b,a,b,b],[]).
s([a,b,a,a,b,a,b,a,b],[]).
s([a,b,a,a,b,a,b,a,a],[]).
s([a,b,a,a,b,a,b,a],[]).
s([a,b,a,a,b,a,b],[]).
s([a,b,a,a,b,a,a,b,b],[]).
s([a,b,a,a,b,a,a,b,a],[]).
s([a,b,a,a,b,a,a,b],[]).
s([a,b,a,a,b,a,a,a,b],[]).
s([a,b,a,a,b,a,a,a,a],[]).
s([a,b,a,a,b,a,a,a],[]).
s([a,b,a,a,b,a,a],[]).
s([a,b,a,a,b,a],[]).
s([a,b,a,a,b],[]).
s([a,b,a,a,a,b,b,b,b],[]).
s([a,b,a,a,a,b,b,b,a],[]).
s([a,b,a,a,a,b,b,b],[]).
s([a,b,a,a,a,b,b,a,b],[]).
s([a,b,a,a,a,b,b,a,a],[]).
s([a,b,a,a,a,b,b,a],[]).
s([a,b,a,a,a,b,b],[]).
s([a,b,a,a,a,b,a,b,b],[]).
s([a,b,a,a,a,b,a,b,a],[]).
s([a,b,a,a,a,b,a,b],[]).
s([a,b,a,a,a,b,a,a,b],[]).
s([a,b,a,a,a,b,a,a,a],[]).
s([a,b,a,a,a,b,a,a],[]).
s([a,b,a,a,a,b,a],[]).
s([a,b,a,a,a,b],[]).
s([a,b,a,a,a,a,b,b,b],[]).
s([a,b,a,a,a,a,b,b,a],[]).
s([a,b,a,a,a,a,b,b],[]).
s([a,b,a,a,a,a,b,a,b],[]).
s([a,b,a,a,a,a,b,a,a],[]).
s([a,b,a,a,a,a,b,a],[]).
s([a,b,a,a,a,a,b],[]).
s([a,b,a,a,a,a,a,b,b],[]).
s([a,b,a,a,a,a,a,b,a],[]).
s([a,b,a,a,a,a,a,b],[]).
s([a,b,a,a,a,a,a,a,b],[]).
s([a,b,a,a,a,a,a,a,a],[]).
s([a,b,a,a,a,a,a,a],[]).
s([a,b,a,a,a,a,a],[]).
s([a,b,a,a,a,a],[]).
s([a,b,a,a,a],[]).
s([a,b,a,a],[]).
s([a,b,a],[]).
s([a,a,b,b,b,b,b,b,b],[]).
s([a,a,b,b,b,b,b,b,a],[]).
s([a,a,b,b,b,b,b,b],[]).
s([a,a,b,b,b,b,b,a,b],[]).
s([a,a,b,b,b,b,b,a,a],[]).
s([a,a,b,b,b,b,b,a],[]).
s([a,a,b,b,b,b,b],[]).
s([a,a,b,b,b,b,a,b,b],[]).
s([a,a,b,b,b,b,a,b,a],[]).
s([a,a,b,b,b,b,a,b],[]).
s([a,a,b,b,b,b,a,a,b],[]).
s([a,a,b,b,b,b,a,a,a],[]).
s([a,a,b,b,b,b,a,a],[]).
s([a,a,b,b,b,b,a],[]).
s([a,a,b,b,b,b],[]).
s([a,a,b,b,b,a,b,b,b],[]).
s([a,a,b,b,b,a,b,b,a],[]).
s([a,a,b,b,b,a,b,b],[]).
s([a,a,b,b,b,a,b,a,b],[]).
s([a,a,b,b,b,a,b,a,a],[]).
s([a,a,b,b,b,a,b,a],[]).
s([a,a,b,b,b,a,b],[]).
s([a,a,b,b,b,a,a,b,b],[]).
s([a,a,b,b,b,a,a,b,a],[]).
s([a,a,b,b,b,a,a,b],[]).
s([a,a,b,b,b,a,a,a,b],[]).
s([a,a,b,b,b,a,a,a,a],[]).
s([a,a,b,b,b,a,a,a],[]).
s([a,a,b,b,b,a,a],[]).
s([a,a,b,b,b,a],[]).
s([a,a,b,b,b],[]).
s([a,a,b,b,a,b,b,b,b],[]).
s([a,a,b,b,a,b,b,b,a],[]).
s([a,a,b,b,a,b,b,b],[]).
s([a,a,b,b,a,b,b,a,b],[]).
s([a,a,b,b,a,b,b,a,a],[]).
s([a,a,b,b,a,b,b,a],[]).
s([a,a,b,b,a,b,b],[]).
s([a,a,b,b,a,b,a,b,b],[]).
s([a,a,b,b,a,b,a,b,a],[]).
s([a,a,b,b,a,b,a,b],[]).
s([a,a,b,b,a,b,a,a,b],[]).
s([a,a,b,b,a,b,a,a,a],[]).
s([a,a,b,b,a,b,a,a],[]).
s([a,a,b,b,a,b,a],[]).
s([a,a,b,b,a,b],[]).
s([a,a,b,b,a,a,b,b,b],[]).
s([a,a,b,b,a,a,b,b,a],[]).
s([a,a,b,b,a,a,b,b],[]).
s([a,a,b,b,a,a,b,a,b],[]).
s([a,a,b,b,a,a,b,a,a],[]).
s([a,a,b,b,a,a,b,a],[]).
s([a,a,b,b,a,a,b],[]).
s([a,a,b,b,a,a,a,b,b],[]).
s([a,a,b,b,a,a,a,b,a],[]).
s([a,a,b,b,a,a,a,b],[]).
s([a,a,b,b,a,a,a,a,b],[]).
s([a,a,b,b,a,a,a,a,a],[]).
s([a,a,b,b,a,a,a,a],[]).
s([a,a,b,b,a,a,a],[]).
s([a,a,b,b,a,a],[]).
s([a,a,b,b,a],[]).
s([a,a,b,a,b,b,b,b,b],[]).
s([a,a,b,a,b,b,b,b,a],[]).
s([a,a,b,a,b,b,b,b],[]).
s([a,a,b,a,b,b,b,a,b],[]).
s([a,a,b,a,b,b,b,a,a],[]).
s([a,a,b,a,b,b,b,a],[]).
s([a,a,b,a,b,b,b],[]).
s([a,a,b,a,b,b,a,b,b],[]).
s([a,a,b,a,b,b,a,b,a],[]).
s([a,a,b,a,b,b,a,b],[]).
s([a,a,b,a,b,b,a,a,b],[]).
s([a,a,b,a,b,b,a,a,a],[]).
s([a,a,b,a,b,b,a,a],[]).
s([a,a,b,a,b,b,a],[]).
s([a,a,b,a,b,b],[]).
s([a,a,b,a,b,a,b,b,b],[]).
s([a,a,b,a,b,a,b,b,a],[]).
s([a,a,b,a,b,a,b,b],[]).
s([a,a,b,a,b,a,b,a,b],[]).
s([a,a,b,a,b,a,b,a,a],[]).
s([a,a,b,a,b,a,b,a],[]).
s([a,a,b,a,b,a,b],[]).
s([a,a,b,a,b,a,a,b,b],[]).
s([a,a,b,a,b,a,a,b,a],[]).
s([a,a,b,a,b,a,a,b],[]).
s([a,a,b,a,b,a,a,a,b],[]).
s([a,a,b,a,b,a,a,a,a],[]).
s([a,a,b,a,b,a,a,a],[]).
s([a,a,b,a,b,a,a],[]).
s([a,a,b,a,b,a],[]).
s([a,a,b,a,b],[]).
s([a,a,b,a,a,b,b,b,b],[]).
s([a,a,b,a,a,b,b,b,a],[]).
s([a,a,b,a,a,b,b,b],[]).
s([a,a,b,a,a,b,b,a,b],[]).
s([a,a,b,a,a,b,b,a,a],[]).
s([a,a,b,a,a,b,b,a],[]).
s([a,a,b,a,a,b,b],[]).
s([a,a,b,a,a,b,a,b,b],[]).
s([a,a,b,a,a,b,a,b,a],[]).
s([a,a,b,a,a,b,a,b],[]).
s([a,a,b,a,a,b,a,a,b],[]).
s([a,a,b,a,a,b,a,a,a],[]).
s([a,a,b,a,a,b,a,a],[]).
s([a,a,b,a,a,b,a],[]).
s([a,a,b,a,a,b],[]).
s([a,a,b,a,a,a,b,b,b],[]).
s([a,a,b,a,a,a,b,b,a],[]).
s([a,a,b,a,a,a,b,b],[]).
s([a,a,b,a,a,a,b,a,b],[]).
s([a,a,b,a,a,a,b,a,a],[]).
s([a,a,b,a,a,a,b,a],[]).
s([a,a,b,a,a,a,b],[]).
s([a,a,b,a,a,a,a,b,b],[]).
s([a,a,b,a,a,a,a,b,a],[]).
s([a,a,b,a,a,a,a,b],[]).
s([a,a,b,a,a,a,a,a,b],[]).
s([a,a,b,a,a,a,a,a,a],[]).
s([a,a,b,a,a,a,a,a],[]).
s([a,a,b,a,a,a,a],[]).
s([a,a,b,a,a,a],[]).
s([a,a,b,a,a],[]).
s([a,a,b,a],[]).
s([a,a,b],[]).
s([a,a,a,b,b,b,b,b,b],[]).
s([a,a,a,b,b,b,b,b,a],[]).
s([a,a,a,b,b,b,b,b],[]).
s([a,a,a,b,b,b,b,a,b],[]).
s([a,a,a,b,b,b,b,a,a],[]).
s([a,a,a,b,b,b,b,a],[]).
s([a,a,a,b,b,b,b],[]).
s([a,a,a,b,b,b,a,b,b],[]).
s([a,a,a,b,b,b,a,b,a],[]).
s([a,a,a,b,b,b,a,b],[]).
s([a,a,a,b,b,b,a,a,b],[]).
s([a,a,a,b,b,b,a,a,a],[]).
s([a,a,a,b,b,b,a,a],[]).
s([a,a,a,b,b,b,a],[]).
s([a,a,a,b,b,a,b,b,b],[]).
s([a,a,a,b,b,a,b,b,a],[]).
s([a,a,a,b,b,a,b,b],[]).
s([a,a,a,b,b,a,b,a,b],[]).
s([a,a,a,b,b,a,b,a,a],[]).
s([a,a,a,b,b,a,b,a],[]).
s([a,a,a,b,b,a,b],[]).
s([a,a,a,b,b,a,a,b,b],[]).
s([a,a,a,b,b,a,a,b,a],[]).
s([a,a,a,b,b,a,a,b],[]).
s([a,a,a,b,b,a,a,a,b],[]).
s([a,a,a,b,b,a,a,a,a],[]).
s([a,a,a,b,b,a,a,a],[]).
s([a,a,a,b,b,a,a],[]).
s([a,a,a,b,b,a],[]).
s([a,a,a,b,b],[]).
s([a,a,a,b,a,b,b,b,b],[]).
s([a,a,a,b,a,b,b,b,a],[]).
s([a,a,a,b,a,b,b,b],[]).
s([a,a,a,b,a,b,b,a,b],[]).
s([a,a,a,b,a,b,b,a,a],[]).
s([a,a,a,b,a,b,b,a],[]).
s([a,a,a,b,a,b,b],[]).
s([a,a,a,b,a,b,a,b,b],[]).
s([a,a,a,b,a,b,a,b,a],[]).
s([a,a,a,b,a,b,a,b],[]).
s([a,a,a,b,a,b,a,a,b],[]).
s([a,a,a,b,a,b,a,a,a],[]).
s([a,a,a,b,a,b,a,a],[]).
s([a,a,a,b,a,b,a],[]).
s([a,a,a,b,a,b],[]).
s([a,a,a,b,a,a,b,b,b],[]).
s([a,a,a,b,a,a,b,b,a],[]).
s([a,a,a,b,a,a,b,b],[]).
s([a,a,a,b,a,a,b,a,b],[]).
s([a,a,a,b,a,a,b,a,a],[]).
s([a,a,a,b,a,a,b,a],[]).
s([a,a,a,b,a,a,b],[]).
s([a,a,a,b,a,a,a,b,b],[]).
s([a,a,a,b,a,a,a,b,a],[]).
s([a,a,a,b,a,a,a,b],[]).
s([a,a,a,b,a,a,a,a,b],[]).
s([a,a,a,b,a,a,a,a,a],[]).
s([a,a,a,b,a,a,a,a],[]).
s([a,a,a,b,a,a,a],[]).
s([a,a,a,b,a,a],[]).
s([a,a,a,b,a],[]).
s([a,a,a,b],[]).
s([a,a,a,a,b,b,b,b,b],[]).
s([a,a,a,a,b,b,b,b,a],[]).
s([a,a,a,a,b,b,b,a,b],[]).
s([a,a,a,a,b,b,b,a,a],[]).
s([a,a,a,a,b,b,b,a],[]).
s([a,a,a,a,b,b,b],[]).
s([a,a,a,a,b,b,a,b,b],[]).
s([a,a,a,a,b,b,a,b,a],[]).
s([a,a,a,a,b,b,a,b],[]).
s([a,a,a,a,b,b,a,a,b],[]).
s([a,a,a,a,b,b,a,a,a],[]).
s([a,a,a,a,b,b,a,a],[]).
s([a,a,a,a,b,b,a],[]).
s([a,a,a,a,b,b],[]).
s([a,a,a,a,b,a,b,b,b],[]).
s([a,a,a,a,b,a,b,b,a],[]).
s([a,a,a,a,b,a,b,b],[]).
s([a,a,a,a,b,a,b,a,b],[]).
s([a,a,a,a,b,a,b,a,a],[]).
s([a,a,a,a,b,a,b,a],[]).
s([a,a,a,a,b,a,b],[]).
s([a,a,a,a,b,a,a,b,b],[]).
s([a,a,a,a,b,a,a,b,a],[]).
s([a,a,a,a,b,a,a,b],[]).
s([a,a,a,a,b,a,a,a,b],[]).
s([a,a,a,a,b,a,a,a,a],[]).
s([a,a,a,a,b,a,a,a],[]).
s([a,a,a,a,b,a,a],[]).
s([a,a,a,a,b,a],[]).
s([a,a,a,a,b],[]).
s([a,a,a,a,a,b,b,b,b],[]).
s([a,a,a,a,a,b,b,b,a],[]).
s([a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,b,b,a,b],[]).
s([a,a,a,a,a,b,b,a,a],[]).
s([a,a,a,a,a,b,b,a],[]).
s([a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,b,a,b,b],[]).
s([a,a,a,a,a,b,a,b,a],[]).
s([a,a,a,a,a,b,a,b],[]).
s([a,a,a,a,a,b,a,a,b],[]).
s([a,a,a,a,a,b,a,a,a],[]).
s([a,a,a,a,a,b,a,a],[]).
s([a,a,a,a,a,b,a],[]).
s([a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,b,b,b],[]).
s([a,a,a,a,a,a,b,b,a],[]).
s([a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,a,b,a,b],[]).
s([a,a,a,a,a,a,b,a,a],[]).
s([a,a,a,a,a,a,b,a],[]).
s([a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,b,b],[]).
s([a,a,a,a,a,a,a,b,a],[]).
s([a,a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,a,b],[]).
s([a,a,a,a,a,a,a,a,a],[]).
s([a,a,a,a,a,a,a,a],[]).
s([a,a,a,a,a,a,a],[]).
s([a,a,a,a,a,a],[]).
s([a,a,a,a,a],[]).
s([a,a,a,a],[]).
s([a,a,a],[]).
s([a,a],[]).
% 40,414,673 inferences, 61.984 CPU in 177.829 seconds (35% CPU, 652014 Lips)
true.

% Learn again but separate sub-programs with reduction(subhypotheses):

?- poker_configuration:reduction(R).
R = subhypotheses.

?- time( poker:learn(s([a,a,a,b,b,b],[]),_Pos,_Neg,_Ps) ), auxiliaries:print_clauses(_Ps).
% Generated 1018 new atoms:
% 40,405,196 inferences, 61.406 CPU in 184.281 seconds (33% CPU, 657998 Lips)
inv_1_7(A,B):-a(A,C),s(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-inv_1_7(A,C),b(C,B).
true ;
% 15,497 inferences, 0.000 CPU in 0.003 seconds (0% CPU, Infinite Lips)
inv_1_8(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),inv_1_8(C,B).
true.
```

Wew, you made it all the way down here! As a reward for your patience here is
some testing of the Top Program learned by Poker:

```
% Assert the learned program and BK at the Prolog top-level.
?- [user].
|: :-table(s/2). % Avoids generating the same strings multiple times.
|: inv_1_7(A,B):-a(A,C),s(C,B).
|: inv_1_8(A,B):-s(A,C),b(C,B).
|: s(A,B):-a(A,C),b(C,B).
|: s(A,B):-a(A,C),inv_1_8(C,B).
|: s(A,B):-inv_1_7(A,C),b(C,B).
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
