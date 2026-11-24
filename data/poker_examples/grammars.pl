:-module(grammars, [even/2
                   ,odd/2
                   ,palindrome/2
                   ,not_palindrome/2
                   ,anbn/2
                   ,not_anbn/2
                   ,anbm/2
                   ,not_anbm/2
                   ,anbn_uo/2
                   ,not_anbn_uo/2
                   ,parens/2
                   ,unbalanced_parens/2
                   ,arith/2
                   ,prop/2
                   ,bit_string/2
                   ,bit_string_p/2
                   ,even_bin/2
                   ,odd_bin/2
                   ,anbn_bin/2
                   ,not_anbn_bin/2
                   ,anbm_bin/2
                   ,not_anbm_bin/2
                   ,anbn_uo_bin/2
                   ,not_anbn_uo_bin/2
                   ,palindrome_bin/2
                   ,not_palindrome_bin/2
                   ,parens_bin/2
                   ,unbalanced_parens_bin/2
                   ]).

/** <module> Context Free and regular language grammars.

*/


%!      even(+Input,-Output) is nondet.
%
%       A grammar accepting bit strings with an even number of 1s.
%
%       Equivalent to the following grammar in DCG notation:
%       ==
%       q0 --> [].
%       q0 --> zero, q0.
%       q0 --> one, q1.
%       q1 --> zero --> q1.
%       q1 --> one --> q0.
%       ==
%
%       Input is a list of 1s and 0s, with an even number of 1s and any
%       number of 0s, including none, representing even parity. The
%       empty string is even.
%
%       Output is the remainder of the string in Input once all
%       characters are consumed by the grammar. Output should be [] when
%       the Input string has an even number of 1s.
%
even(X,Y):- empty(X,Y).
even(X,Y):- zero(X,Z), even(Z,Y).
even(X,Y):- one(X,Z), odd(Z,Y).
odd(X,Y):- zero(X,Z), odd(Z,Y).
odd(X,Y):- one(X,Z), even(Z,Y).

zero --> [0].
one --> [1].
empty --> [].


%!      palindrome is nondet.
%
%       A grammar for the language of palindromic bit-strings.
%
palindrome --> empty.
palindrome --> one.
palindrome --> zero.
palindrome --> one, palindrome, one.
palindrome --> zero, palindrome, zero.


%!      not_palindrome is nondet.
%
%       A grammar for the language of non-palindromic bit-strings.
%
not_palindrome --> bit_string(Ss), { \+ phrase(palindrome, Ss) }.

bit_string([B]) --> bit(B).
bit_string([B|Bs]) --> bit(B), bit_string(Bs).

bit(1) --> [1].
bit(0) --> [0].


%!      s0 is nondet.
%
%       Grammar for the a^nb^n context-free language.
%
anbn --> a,b.
anbn --> a,anbn,b.

a --> [a].
b --> [b].


%!      not_anbn is nondet.
%
%       A grammar for the language of non-a^nb^n a-b strings.
%
not_anbn --> ab_string(Ss), { \+ phrase(anbn,Ss) }.

ab_string([S]) --> ab(S).
ab_string([S|Ss]) --> ab(S), ab_string(Ss).

ab(a) --> a.
ab(b) --> b.


% Grammar for the language {a^nb^m|n >= m >= 0}
anbm --> empty.
anbm --> a,anbm.
anbm --> a,anbm,b.

%!      not_anbn is nondet.
%
%       A grammar for the language of non-a^nb^n a-b strings.
%
not_anbm --> ab_string(Ss), { \+ phrase(anbm,Ss) }.


% "anbn_uo" stands for "anbn unordered"
% Grammar for the language of equal numbers of as and bs in any order.
% L = {w in {a,b}* | n_a(w) = n_b(w)} (n_a is n with underscore a, so
% "the n for a").
:- table anbn_uo/2. % Because left-recursive.

anbn_uo --> empty.
anbn_uo --> a,anbn_uo,b.
anbn_uo --> b,anbn_uo,a.
anbn_uo --> anbn_uo, anbn_uo.

not_anbn_uo --> ab_string(Ss), { \+ phrase(anbn_uo,Ss) }.


% Language of balanced parentheses.

:- table parens/2.

parens --> empty.
parens --> lp, parens, rp.
parens --> parens, parens.

lp --> ['('].
rp --> [')'].


unbalanced_parens --> paren_string(Ss), { \+ phrase(parens,Ss) }.

paren_string([S]) --> paren(S).
paren_string([S|Ss]) --> paren(S), paren_string(Ss).

paren('(') --> ['('].
paren(')') --> [')'].


%!      not is nondet.
%
%       Bit string inverter grammar
%
not([1]) --> zero.
not([0]) --> one.
not([1|Bs]) --> zero, not(Bs).
not([0|Bs]) --> one, not(Bs).


%!      not_not is nondet.
%
%       Bit string inverter inverter grammar
%
not_not([0]) --> zero.
not_not([1]) --> one.
not_not([0|Bs]) --> zero, not_not(Bs).
not_not([1|Bs]) --> one, not_not(Bs).


:- table(arith/2).

arith --> int.
arith --> arith, arith_op, arith.
arith --> lp, arith, rp.

int --> [0] | [1] | [2] | [3] | [4] | [5] | [6] | [7] | [8] | [9].

arith_op --> [+].
arith_op --> [-].
arith_op --> [*].
arith_op --> [/].


:- table(prop/2).

prop --> bool.
prop --> neg, prop.
prop --> prop, conj, prop.
prop --> prop, disj, prop.
prop --> prop, impl, prop.

bool --> [1] | [0].

neg --> [~].
conj --> [/\].
disj --> [\/].
impl --> [->].


                /*******************************
                *     BIT-STRING GRAMMARS      *
                *******************************/

bit_string --> [].
bit_string --> one, bit_string.
bit_string --> zero, bit_string.

bit_string_p --> [].
bit_string_p --> one, bit_string_p.
bit_string_p --> zero, bit_string_p.

even_bin(X,Y):- empty(X,Y).
even_bin(X,Y):- zero(X,Z), even_bin(Z,Y).
even_bin(X,Y):- one(X,Z), odd_bin(Z,Y).
odd_bin(X,Y):- zero(X,Z), odd_bin(Z,Y).
odd_bin(X,Y):- one(X,Z), even_bin(Z,Y).

anbn_bin --> one,zero.
anbn_bin --> one,anbn_bin,zero.

not_anbn_bin --> [].
not_anbn_bin --> bit_string(Ss), { \+ phrase(anbn_bin,Ss) }.

anbm_bin --> empty.
anbm_bin --> one,anbm_bin.
anbm_bin --> one,anbm_bin,zero.

not_anbm_bin --> bit_string(Ss), { \+ phrase(anbm_bin,Ss) }.

:- table anbn_uo_bin/2. % Because left-recursive.

anbn_uo_bin --> empty.
anbn_uo_bin --> one, anbn_uo_bin, zero.
anbn_uo_bin --> zero, anbn_uo_bin, one.
anbn_uo_bin --> anbn_uo_bin, anbn_uo_bin.

not_anbn_uo_bin --> bit_string(Ss), { \+ phrase(anbn_uo_bin,Ss) }.

:- table parens_bin/2.

parens_bin --> empty.
parens_bin --> one, parens_bin, zero.
parens_bin --> parens_bin, parens_bin.

unbalanced_parens_bin --> bit_string(Ss), { \+ phrase(parens_bin,Ss) }.

palindrome_bin --> empty.
palindrome_bin --> one.
palindrome_bin --> zero.
palindrome_bin --> one, palindrome_bin, one.
palindrome_bin --> zero, palindrome_bin, zero.

not_palindrome_bin --> bit_string(Ss), { \+ phrase(palindrome_bin, Ss) }.
