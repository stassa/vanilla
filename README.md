Vanilla - a Second-Order Logic engine for learning and reasoning
================================================================

This README is a work in progress
=================================

Vanilla is an inductive, second-order Prolog meta-interpreter for
Meta-Interpretive Learning (MIL). Like an ordinary Prolog meta-interpreter is
used for deduction or reasoning, so an inductive Prolog meta-interpreter is used
for induction or learning. In the second order of logic where Vanilla operates,
deduction and induction, reasoning and learning, are one.

Vanilla is an engine for machine learning and machine reasoning. As an engine,
Vanilla is not meant to be used as a stand-alone application but instead used to
implement new MIL systems.

To illustrate the use of Vanilla as a learning and reasoning engine, two example
systems are included with Vanilla: Metagol and Poker.

Metagol is the original MIL algorithm. It uses an iterative deepening search of
the space of logic programs, and a set of order constraints to control recursion
and ensure termination. An implementation of Metagol is included in the
following directory:

[https://github.com/stassa/vanilla/blob/master/lib/metagol](vanilla/lib/metagol)

Poker is a simple implementation of MIL that learns without the computationally
expensive iterative deepening search in Metagol and from a single example at a
time (it is strictly a one-shot learner). It uses tabling (a.k.a.
SLG-Resolution) to control recursion and ensure termination. Poker is included
in the following directory:

[https://github.com/stassa/vanilla/blob/master/lib/poker](vanilla/lib/poker)

Vanilla was originally developed as the learning engine in Louise. It is now
made available as a separate project to encourage experimentation with MIL. The
two example implementations of Metagol and Poker listed above can be used as a
template to create new MIL algorithms and learning systems.

Meta-Interpretive Learning as Second-Order SLD-Resolution
=========================================================

The hairy, formal terms in the preceding paragraphs are now explained in the
following sections. Deeep breath. Let's go.

First- and Second-Order Logic
-----------------------------

A First-Order Logic (FOL) language consists of formulae relating terms composed
of symbols in an alphabet.

A FOL alphabet consists of a set of predicate symbols, `Π = {P, Q, R ... }`, a
set of function symbols, `Φ = {f, g, h, ... }`, a set of constants `C = {a, b,
c, ...}` subset of `Φ`, a set of variables `V = {x, y, z, ...}`, the logical
connectives `¬` (negation), `∧` (conjunction), `∨` (disjunction), `→`
(implication) and `≡` (equivalence) and, finally, two quantifier symbols, the
existential quantifier, `∃` ("exists") and the universal quantifier `∀`. Each
predicate and function symbol is associated with a number, called the symbol's
arity. Constants have arity 0.

Terms are defined inductively as follows: a variable is a term; a constant is a
term; a function symbol `f` of arity `n`, followed by `n` comma-separated terms
in parentheses, is a term.

Formulae are defined inductively as follows: An atomic formula, or atom, is a
predicate symbol, `P`, of arity `m`, followed by `m` comma-separated terms in
parentheses. If `φ` is a formula, then `¬φ` is a formula. If `φ, χ`, are
formulae, then `φ ∧ χ, φ ∨ χ, φ → χ, φ ≡ χ`, are all formulae.

The variables in a formula may be quantified. If `φ` is a formula then `∃ x,y,z:
φ` is a formula where variables `x, y` and `z` are existentially quantified.
Accordingly, if `φ` is a formula then `∀ x,y,z: φ` is a formula where `x, y` and
`z` are universally quantified. Variables in a formula are quantified over the
se of terms, including constants. We say that a quantifier "ranges over" the set
of terms, including constants.

A FOL language can be used to form arbitrary formulae, but we are most
interested in formulae in clausal form, or clauses which are disjunctions of
literals, and in particular, Horn clauses, which are disjunction of at most one
positive literal.

A literal is an atom, or the negation of an atom. If `A` is an atom, then `A` is
a positive literal and `¬A` is a negative literal. A clause is a disjunction of
literals: `A ∨ B ∨ ¬C ∨ D ∨ ...` . 

A clause is Horn if it has at most one positive literal: `A ∨ ¬B ∨ ¬C ∨ ... . A`
Horn clause is definite if it has one positive literal, otherwise it is a Horn
goal. A definite clause is sometimes called a definite program clause. A
definite program, or logic program, is a conjunction of definite program
clauses. Definite clauses are always, and only, universally quantified.

Logic programming is the research discipline that studies logic programs, their
syntax, semantics and interpretation. By logic programming convention a definite
clause `A ∨ ¬B ∨ ¬C` is written as the equivalent implication `B ∧ C → A` (this
works because a disjunction `A ∨ ¬B` is equivalent to the implication `B → A`).
For convenience, the consequent of the implication is written first, then the
invernted, left-facing implication arrow and then each of the precednets
separated by commas substituted for the conjunction symbols: `A ← B, C`.

The following is an example of the definite clause `∀P,Q,R,x,y,z; P(x,y) ∨
¬Q(x,z) ∨ R(x,y)`, written according to logic programming conventions: 

```
∀P,Q,R,x,y,z: P(x,y) ← Q(x,z), R(z,y)
```

A Second-Order Logic (SOL) language is defined in the same way as a FOL
language, with two exceptions: in SOL, a) definite clauses can be universally or
existentially quantified, and b) the two quantifiers, `∃` and `∀`, range over
not only the set of terms, including constants, but also the sets of predicate
and function _symbols_. Thus, the following definite clause is a second-order:

```
∃P,Q,R, ∀x,y,z: P(x,y) ← Q(x,z), R(z,y)
```
Prolog and SLD-Resolution
-------------------------

Prolog is a programming language where programs are logic programs consisting of
sets of definite clauses, and executed by a "query" that is a Horn goal.

Prolog notation is slightly different to FOL notation. Opposite to FOL,
lower-case letters are used for predicate and function symbols, including
constants, whereas upper-case letters are used for variables. Second, since
definite clauses are universally quantified Prolog clauses are written without
quantifiers (they are implicitly universally quantified). Third, the symbol ":-"
stands in for the left-facing implication arrow. Finally, Prolog clauses are
terminated with a full-stop ".".

For example, the FOL definite clause in our earlier example is written in Prolog
as follows:

```
p(X,Y):- q(X,Z), r(Z,Y).
```

Prolog programs are executed by carrying out a proof by SLD-Resolution, an
automated theorem proving algorithm.

A Second-Order Prolog meta-interpreter
--------------------------------------

A Prolog meta-interpreter is an interpreter for Prolog, written in Prolog.

Prolog is a logic programming language where programs are theories in
First-Order Logic (FOL) given as sets of definite clauses. Prolog programs are
executed by proving them with SLD-Resolution, an automated theorem-proving
algorithm for FOL restricted to definite clauses. So a Prolog meta-interpreter
is an implementation of SLD-Resolution in Prolog.

Vanilla, the meta-interpreter included in this project, is a Prolog
meta-interpreter for SLD-Resolution with second-order definite clauses, so a
second-order Prolog meta-interpreter.


Meta-Interpretive Learning
--------------------------

Meta-Interpretive Learning (MIL) is a new form of Inductive Logic Programming
(ILP). ILP is the field that studies the machine-learning of logic programs from
examples and background knowledge. MIL is a form of ILP that learns by
SLD-Resolution proofs of training examples. 

MIL is learned after the technique of Prolog meta-interpretation 

