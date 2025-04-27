:-module(binary_cgnf, [binary_parity/0
                      ,binary_anbn/0
                      ,binary_anbn_uo/0
                      ,binary_anbm/0
                      ,binary_parens/0
                      ,binary_palindrome/0
                      ,binary_anbn_uo/1
                      ,binary_anbn_range/3
                      ,binary_anbn_uo_range/3
                      ,binary_anbm_range/3
                      ,binary_anbm_range_unlabelled/3
                      ,binary_parens_range/3
                      ,binary_parens_range_unlabelled/3
                      ,binary_palindrome_range/3
                      ,cleanup_safe_example/0
                      ,setup_safe_example/1
                      ,background_knowledge/2
                      ,set_configs/1
                      ,metarules/2
                      ,labelled_example/2
                      ,unlabelled_example/2
                      ,zero/2
                      ,one/2
                      ,empty/2
                      ]).

:-use_module(project_root(configuration)).
:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(lib(poker/poker_configuration)).
:-use_module(data(poker_examples/test_harness)).
:-use_module(data(poker_examples/experiment_output)).
:-use_module(lib(poker/normal_forms/weak_chomsky_greibach_nf)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Unified script for experiments learning bit-string CFGs with Weak C-GNF.

Predicates in this module are used to setup and run experiments learning
Context-Free Grammars for languages of bit-strings, i.e. where the
terminals are in {0,1,epsilon} (where epsilon is the empty string).
Grammars are learned in Definite Clause Grammars form (unsugared).

There are six grammars used in experiments:

1. Language of bit-strings with an even number of "1"s.

Abbreviated as "even". Regular language. Accepts the empty string.

2. a^nb^n where n > 0.

Abbreviated as "anbn". Context Free. Characters "a" and "b" are replaced
by 1 and 0, respectively. The language does not accept the empty string.


3. {w in {a,b}* | count(a) = count(b)}

Accepts strings of equal numbers of as and bs in any oreder. Abbreviated
as "anbn_uo" (for "unordered"). Characters "a" and "b" are replaced by
"1" and "0" respsectively. The language does accept the empty string.

4. a^nb^m where n >= m >= 0.

Abbreviated as "anbm". Context Free. Characters "a" and "b" are replaced
by "1" and "0", respectively. The language does accept the empty string.


5. Language of balanced parentheses

Abbraviated as "parens". Context Free. Characters "(" and ")" are
replaced by "1" and "0" respectively. Accepts the empty string.

6. Language of palindromic bit-strings

Abbreviated as "palindrome". Context Free. Accepts the empty string.


Replacing characters "a", "b", "(", and ")" with "1" and "0" allows a
single first-order background theory to be used for all experiments.
This unified theory is the following Definite Clause Grammar:

==
one --> [1].
zero --> [0].
empty --> [].
==

All languages are Context Free except for the even parity language which
is regular, so they can all be represented as Context Free Grammars
(CFGs; a CFG can express a Regular language, but not the other way
around). Therefore the Weak Chomsky-Greibach Normal Form is used as a
Second Order background theory for all six languages.

*/

:-debug(generalise).
:-debug(generate).
:-debug(experiments).
:-debug(experiment).
:-debug(experiment_initial).
:-debug(experiment_time).
:-debug(experiment_examples).
:-debug(experiment_learned).
:-debug(experiment_learned_full).
:-debug(test_program).
:-debug(test_labelling).
:-debug(generate_examples).
:-debug(generate_initial).
:-debug(filter_by_language).


                /*******************************
                *      SINGLE EXPERIMENTS      *
                *******************************/

% Simple set of single-run experiments. Use for quick eyballing of
% results while working out the right configs.

%!      binary_parity is det.
%
%       Learn a CFG of the even-parity Regular language.
%
binary_parity:-
        Lang = even_bin
        ,T = s/2
        ,Sl = even_bin(all,0,4) % 16
        ,Su = []
        ,TPos = even_bin(all,4,8) % 248 ok duh
        ,TNeg = odd_bin(all,0,4)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true)).


%!      binary_anbn is det.
%
%       Learn a CFG of the a^nb^n: n > 0 Context-Free Language.
%
binary_anbn:-
        Lang = anbn_bin
        ,T = s/2
        ,Sl = anbn_bin(all,0,6) % 3
        ,Su = []
        ,TPos = anbn_bin(all,8,60) % 27
        ,TNeg = not_anbn_bin(all,0,5) % 60
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true)).


%!      binary_anbn_uo is det.
%
%       Learn a CFG of the unordered a^nb^n Context-Free Language.
%
binary_anbn_uo:-
        Lang = anbn_uo_bin
        ,T = s/2
        ,Sl = anbn_uo_bin(15,0,6) % 29
        ,Su = []
        ,TPos = anbn_uo_bin(all,5,10) % 342
        ,TNeg = not_anbn_uo_bin(all,0,8) % 412
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).


%!      binary_anbm is det.
%
%       Learn a CFG of the a^nb^m: n >= m >= 0 Context-Free Language.
%
binary_anbm:-
        Lang = anbm_bin
        ,T = s/2
        ,Sl = anbm_bin(all,0,4)
        ,Su = []
	,TPos = anbm_bin(all,5,8)
        ,TNeg = not_anbm_bin(all,0,5)
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true)).


%!      binary_parens is det.
%
%       Learn a CFG of the CFL of balanced parentheses.
%
binary_parens:-
        Lang = parens_bin
	,T = s/2
	,Sl = parens_bin(all,0,6) % 6
	,Su = []
	,TPos = parens_bin(1500,7,20)
	,TNeg = unbalanced_parens_bin(all,0,10) % 1982
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).


%!      binary_palindrome is det.
%
%       Learn a CFG of the CFL of palindromic bit-strings.
%
binary_palindrome:-
	Lang = palindrome_bin
	,T = p/2
	,Sl = palindrome_bin(all,0,4) % 13
	,Su = []
	,TPos = palindrome_bin(all,5,15) % 752
	,TNeg = not_palindrome_bin(all,0,10) % 1922
        ,setup_and_run_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(false)).


                /*******************************
                *      MULTI-EXPERIMENTS       *
                *******************************/

% Experiments with repetitions. Use to observe change of results over
% random samples of labelled, unlabelled examples and generated negative
% examples.

%!      binary_anbn_uo is det.
%
%       Run N experiments and print the aggregate evaluation results.
%
binary_anbn_uo(N):-
        Lang = anbn_uo_bin
        ,T = s/2
        ,Sl = anbn_uo_bin(15,0,6) % 29
        ,Su = []
        ,TPos = anbn_uo_bin(all,5,10) % 342
        ,TNeg = not_anbn_uo_bin(all,0,8) % 412
        ,setup_and_run_experiments(Lang,T,N,Sl,Su,TPos,TNeg).



                /*******************************
                *      RANGE EXPERIMENTS       *
                *******************************/

% Experiments with repetitions, varying the number of labelled and
% unlabelled and generated negative examples. Use to investigate the
% relation between labelled, unlabelled, and generated examples.
% Results can be optionally plotted with matplotlib.


%!      binary_anbn_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
%       Results are output to the given output Stream.
%
%       Plot is a boolean that denotes whether to plot results with
%       matplotlib.
%
%       If Plot is "true" then Stream must be a file stream where the
%       results' CSV file will be saved, and read from, to create the
%       plot.
%
binary_anbn_range(N,S,P):-
        Lang = anbn_bin
        ,T = s/2
        ,Gs = 0:25/5
        ,Sl = anbn_bin(1:21/5,0,45) % all is 22
        ,Su = []
        ,TPos = anbn_bin(all,46,80)
        ,TNeg = not_anbn_bin(all,0,12)
        ,(   P == true
         ->  Pl = plot('a^nb^n',@(false),@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      binary_anbn_uo_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
binary_anbn_uo_range(N,S,P):-
        Lang = anbn_uo_bin
        ,T = s/2
        ,Gs = 0:25/5
        ,Sl = anbn_uo_bin(1:21/5,0,6) % all is 29
        ,Su = []
        ,TPos = anbn_uo_bin(all,5,10)
        ,TNeg = not_anbn_uo_bin(all,0,8)
        ,(   P == true
         ->  Pl = plot('a^nb^n',@(false),@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      binary_anbm_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
binary_anbm_range(N,S,P):-
        Lang = anbm_bin
        ,T = s/2
        ,Gs = 0:25/5
        %,Sl = anbm_bin(1:61/10,0,9) % all is 88
        ,Sl = anbm_bin(1:21/5,0,9) % all is 88
        ,Su = []
        ,TPos = anbm_bin(all,9,18)
        ,TNeg = not_anbm_bin(all,0,13)
        ,(   P == true
         ->  Pl = plot('a^nb^m (n >= m >= 0)',@(false),@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      binary_anbm_range_unlabelled(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
binary_anbm_range_unlabelled(N,S,P):-
        Lang = anbm_bin
        ,T = s/2
        ,Gs = 0:25/5
        ,Sl = anbm_bin(10:10/10,0,9)
        ,Su = [anbn_bin(1:21/5,0,45)
              ,anbm_bin(1:21/5,10,12)
              ]
        ,TPos = anbm_bin(1500,12,18)
        ,TNeg = not_anbm_bin(1500,0,13)
        ,(   P == true
         ->  Pl = plot('a^nb^m (n >= m >= 0)',@(false),@(true))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      binary_parens_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
binary_parens_range(N,S,P):-
        Lang = parens_bin
        ,T = s/2
        ,Gs = 0:100/20
        %,Sl = parens_bin(1:61/10,0,10) % all is 65
        ,Sl = parens_bin(1:21/5,0,10) % all is 65
        ,Su = []
        ,TPos = parens_bin(1500,11,21) % all is 23649
        ,TNeg = unbalanced_parens_bin(1500,0,15) % all is 64909
        ,(   P == true
         ->  Pl = plot('Balanced Parentheses',@(false),@(false))
         ;   Pl = false
         )
        % Needs more tabling RAM
        ,Set = set_table_space(4_294_967_296,TS)
        ,G = setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl)
        % Table space reset to previous setting.
        ,Cl = set_table_space(TS,_)
        ,setup_call_cleanup(Set,G,Cl).


%!      set_table_space(+New,-Current) is det.
%
%       Set the RAM limit for tabling.
%
%       New is the number of bytes, in base-two, in which to set the
%       tabling RAM limit. This is done by modifying the value of
%       the Prolog flag table_space.
%
%       Current is the current value of the table_space flag. Used to
%       reset the table RAM to its previous setting later.
%
set_table_space(S,TS):-
        current_prolog_flag(table_space, TS)
        ,format('Current table space ~D~n',[TS])
        ,set_prolog_flag(table_space,S)
        ,current_prolog_flag(table_space, NS)
        ,format('New table space ~D~n',[NS]).


%!      binary_parens_range_unlabelled(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
binary_parens_range_unlabelled(N,S,P):-
        Lang = parens_bin
        ,T = s/2
        ,Gs = 0:100/20
        ,Sl = parens_bin(10:10/10,0,10)
        ,Su = [anbn_bin(1:21/4,0,45)
              ,anbm_bin(1:21/4,10,12)
              ,palindrome(1:21/4,0,5)
              ,unbalanced_parens_bin(1:21/4,0,10)
              ]
        ,TPos = parens_bin(all,11,21)
        ,TNeg = unbalanced_parens_bin(all,0,15)
        ,(   P == true
         ->  Pl = plot('Balanced Parentheses',@(false),@(true))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).


%!      binary_palindrome_range(+N,+Stream,+Plot) is det.
%
%       Run N experiments varying inputs and print evaluation results.
%
binary_palindrome_range(N,S,P):-
        Lang = palindrome_bin
        ,T = p/2
        ,Gs = 0:250/50
        ,Sl = palindrome_bin(1:21/4,0,5) % all is 54
        ,Su = []
        ,TPos = palindrome_bin(all,6,16)
        ,TNeg = not_palindrome_bin(all,0,10)
        ,(   P == true
         ->  Pl = plot('Palindrome',@(false),@(false))
         ;   Pl = false
         )
        ,setup_and_run_range_experiments(S,Lang,T,N,Gs,Sl,Su,TPos,TNeg,Pl).



                /*******************************
                *        CONFIGURATION         *
                *******************************/


%!      set_configs(+Language) is det.
%
%       Set configuration options for a target Language.
%
set_configs(even_bin):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[true])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[none])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[6])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(anbn_bin):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[3])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[false])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[20])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(anbn_uo_bin):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[false])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[all])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[1500])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(anbm_bin):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[1])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[20])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(parens_bin):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[4])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[generalised])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[100])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(palindrome_bin):-
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[0])
        ,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        ,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[true])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[false])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[learned])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[250])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).


                /*******************************
                *       EXPERIMENT DATA        *
                *******************************/


% Language alphabet for the constraints defeined
% in grammar_constraints.pl
%
weak_cgnf:target(s).
weak_cgnf:target(p).
weak_cgnf:invented(inv_1).
weak_cgnf:invented(inv_2).
weak_cgnf:invented(inv_3).
weak_cgnf:invented(inv_4).
weak_cgnf:preterminal(one).
weak_cgnf:preterminal(zero).
weak_cgnf:preterminal(empty).


%!      cleanup_safe_example is det.
%
%       Remove all clauses of safe_example/1 from the dynamic db.
%
cleanup_safe_example:-
        clause(safe_example(_),B)
        ,poker_configuration:retract(safe_example(_):-B)
        ,!.
cleanup_safe_example.


%!      setup_safe_example(+Language) is det.
%
%       Assert a clause of safe_example/1 for a Language.
%
%       @tbd These appear to be carefully hand-tuned to give best
%       results. In truth, they ain't. It's probably possible to
%       optimise them for efficiency x accuracy.
%
setup_safe_example(even_bin):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Ls,[])):-
             between(0,4,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(anbn_bin):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Ls,[])):-
             between(0,9,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(anbn_uo_bin):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Ls,[])):-
             between(0,9,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(anbm_bin):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Ls,[])):-
             between(0,9,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(parens_bin):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Ls,[])):-
             between(0,10,L)
             ,length(Ls,L))
        ,assert(G).
setup_safe_example(palindrome_bin):-
        !
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(p,Ls,[])):-
             between(0,8,L)
             ,length(Ls,L))
        ,assert(G).

% Palindrome
background_knowledge(p/2,[one/2,zero/2,empty/2]).
% Everything else
background_knowledge(s/2,[one/2,zero/2,empty/2]).

% Palindrome is special
metarules(p/2,[identity,chain,tri_chain]).
metarules(s/2,[identity,chain]).

labelled_example(_,_):- fail.

unlabelled_example(_,_):- fail.

one --> [1].
zero --> [0].
empty --> [].
