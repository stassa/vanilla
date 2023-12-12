Poker - True one-shot learning of logic programs
================================================

This README file is a Work-In-Progress.

Poker is a simple implementation of MIL using Vanilla, that learns from a
single positive example at a time, so it's a strict one-shot learner. Negative
examples may be given to specialise a learned program. So maybe not so strict
after all, eh?

Despite its one-shot-ness, Poker can learn complex programs with recursive
clauses, possibly mutually so, and with invented predicates that can also be
recursive. Because it's based on Vanilla, Poker is _complete_ with respect to
its single example i.e. it will return, on backtracking, _all_ logic programs
that entail its single example with respect to its first- and second-order
background knowledge.

Poker is named not after the card game but after [Wittgenstein's
Poker](https://en.wikipedia.org/wiki/Wittgenstein%27s_Poker).

An example of learning the `ancestor` relation, using the training data in
`data/examples/hello_world.pl` follows:

```prolog
% Don't forget to start up Vanilla first!
?- [load_project].
Global stack limit 1,073,741,824
Table space 2,147,483,648
true.

% Load Poker into memory:
?- use_module(lib(poker/poker)).
true.

% The hello_world.pl file should be loaded by default.
?- learn(ancestor/2).
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-parent(A,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true ;
[]
true.
```

In the above example, Poker is returning each program that it can learn from
each of the examples of `ancestor` it's given. Many examples only declare a
one-step ancestry relation, that is, the ancestry relation between a parent and
child, as opposed to the multi-step ancestry relation between a grand-parent, or
great-grandparent. Accordingly the definitions of the logic program `ancestor/2`
learned by Poker can be over-special, like the first few ones listed in the
output of `learn/1` above.

To avoid this over-generation we can hand-pick a "good" example and give only
that example to Poker. We show how to do this below:

```prolog
?- poker:experiment_data(ancestor/2,_,_Neg,_BK,_MS), learn([ancestor(stathis,stassa)],_Neg,_BK,_MS,_Ps), auxiliaries:print_clauses(_Ps).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true ;
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true ;
[]
true.
```

What is a "good example", you ask? Ooh boy, that's a can of worms...
