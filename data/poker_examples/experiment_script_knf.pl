:-module(exp_knf,[anbncn/0
                 ,copy_word/0
                 ,set_configs/1
                 ,background_knowledge/2
                 ,metarules/2
                 ,labelled_example/2
                 ,unlabelled_example/2
                 ,setup_safe_example/1
                 ,cleanup_safe_example/0
                 ,a/2
                 ,b/2
                 ,c/2
                 ,list_configs/1
                 ]).

:-use_module(lib(poker/normal_forms/kuroda_normal_form)).
:-use_module(data(poker_examples/experiment_output)).

% Identify thine self.
:-poker_configuration:experiment_file(P,M)
  ,format('Loading experiment file module ~w from ~w.~n',[P,M]).

/** <module> Learn CSGs in Second-Order Kuroda Normal Form.

Early stage in the development of Second-Order Definite Kuroda Normal
Form for CSG learning.

*/

:-dynamic safe_example/1.

%/*
% Logging, mostly unused.
:-debug(examples_initial).
:-debug(examples>'count.log').
%:-debug(label).
%:-debug(label_full).
:-debug(label_count>'count.log').
:-debug(label_examples>'count.log').
:-debug(label_full_final).
%:-debug(label_results).
:-debug(generalise).
%:-debug(signature).
:-debug(generalise_greedy).
:-debug(respecialise).
:-debug(generate).
%:-debug(generate_full).
%:-debug(prove_all).
:-debug(generate_examples).
:-debug(generate_initial).
%*/
/*
% Too much logging.
:-debug(prove_steps).
:-debug(prove_metasubs).
:-debug(fetch).
*/
%/* Experiment harness specific.
:-debug(experiments).
:-debug(experiment).
:-debug(experiment_initial).
:-debug(experiment_time).
:-debug(experiment_examples).
:-debug(experiment_learned).
:-debug(experiment_learned_full).
:-debug(test_program).
:-debug(test_labelling).
:-debug(filter_by_language).
%*/
% Debug statments in metasubstitution_constraints/2.
%:- debug(constraints).


                /*******************************
                *      SINGLE EXPERIMENTS      *
                *******************************/

%!      anbncn is det.
%
%       Learn and test a DCG of the aⁿbⁿcⁿ Context-Sensitive language.
%
%       @tbd Currently learns three correct, novel grammars after about
%       20 minutes. It seems this is only reliably possible when the
%       base case "abc" string is in the set of labelled positive
%       examples.
%
anbncn:-
        Lang = anbncn
        ,T = s/2
        %,Sl = anbncn(3,3,100) % 8
        ,Sl = anbncn(all,3,9) % 8
        ,Su = []
        ,TPos = anbncn(all,25,200) % 25 to 100, 58 to 200
        ,TNeg = not_anbncn(5000,1,12) % 100 of 797156; higher lengths blow up the stack/tables.
        ,Set = set_table_space(17_179_869_184,TS)
        ,G = setup_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true))
        ,Cl = set_table_space(TS,_)
        ,list_configs(anbncn)
        ,setup_call_cleanup(Set,G,Cl).

%!      copy_word is det.
%
%       Learn and test a DCG of the copy-word CSL.
%
%       The copy-word language is more formally defined as follows:
%
%       {ww|w in {a,b}* and |w| > 0}
%
%       In other words it is the language of two-character strings
%       concatenated to themselves.
%
%       @tbd currently doesn't learn anything and runs unproductively
%       for a very long time (I think about a couple of hours).  Clearly
%       needs more work.
%
copy_word:-
        Lang = copy_word
        ,T = p/2
        ,Sl = ww(all,1,4) % 6
        ,Su = []
        ,TPos = ww(all,7,15) % 240
        %,TNeg = not_ww(all,1,12) % 81798
        ,TNeg = not_ww(all,1,6) % 502
        ,Set = set_table_space(17_179_869_184,TS)
        ,G = setup_experiment(Lang,T,Sl,Su,TPos,TNeg,print_examples(true))
        ,Cl = set_table_space(TS,_)
        ,list_configs(copy_word)
        ,setup_call_cleanup(Set,G,Cl).


                /*******************************
                *      EXPERIMENT HELPERS      *
                *******************************/


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



                /*******************************
                *        CONFIGURATION         *
                *******************************/

%!      set_configs(+Language) is det.
%
%       Set configuration options for a target Language.
%
%       The following configuration options represent the best options
%       found by trial-and-error for their respective grammars.
%
set_configs(anbncn):-
        !
        ,poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
        %,poker_auxiliaries:set_configuration_option(table_meta_interpreter,[false])
        ,poker_auxiliaries:set_poker_configuration_option(clause_limit,[5])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[2])
        %,poker_auxiliaries:set_poker_configuration_option(proof_samples,[0.5])
        ,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[generalised])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[20_000])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).

set_configs(copy_word):-
        %poker_auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]])
        %,poker_auxiliaries:set_configuration_option(table_meta_interpreter,[false])
        poker_auxiliaries:set_poker_configuration_option(clause_limit,[6])
        ,poker_auxiliaries:set_poker_configuration_option(max_invented,[4])
        %,poker_auxiliaries:set_poker_configuration_option(gestalt,[false])
        %,poker_auxiliaries:set_poker_configuration_option(flatten_prove_all,[false])
        %,poker_auxiliaries:set_poker_configuration_option(proof_samples,[0.5])
        %,poker_auxiliaries:set_poker_configuration_option(respecialise,[true])
        ,poker_auxiliaries:set_poker_configuration_option(unfold_invented,[generalised])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples,[10_000])
        ,poker_auxiliaries:set_poker_configuration_option(unlabelled_examples_order
                                                         ,[random]).


set_configs(Lang):-
        \+ memberchk(Lang,[anbncn
                          ,copy_word
                          ])
        ,throw('set_configs/1: Unknown Language':Lang).


                /*******************************
                *       EXPERIMENT DATA        *
                *******************************/

knf:target(s).
knf:target(p).
knf:invented(inv_1).
knf:invented(inv_2).
knf:invented(inv_3).
knf:invented(inv_4).
knf:invented(inv_5).
knf:invented(inv_6).
knf:invented(inv_7).
knf:preterminal(a).
knf:preterminal(b).
knf:preterminal(c).


%!      setup_safe_example(+Language) is det.
%
%       Assert a clause of safe_example/1 for a Language.
%
%       @tbd These appear to be carefully hand-tuned to give best
%       results. In truth, they ain't. It's probably possible to
%       optimise them for efficiency x accuracy.
%
setup_safe_example(Lang):-
        memberchk(Lang,[copy_word
                        ]
                  )
        ,!
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(p,Is,[])):-
              between(0,10,I)
             ,length(Is,I))
        ,assert(G).
setup_safe_example(Lang):-
        !
        ,must_be(oneof([anbncn
                       ]
                      )
                ,Lang
                )
        ,cleanup_safe_example
        ,G = (poker_configuration:safe_example(m(s,Is,[])):-
              between(0,24,I)
             ,length(Is,I))
        ,assert(G).


%!      cleanup_safe_example is det.
%
%       Remove all clauses of safe_example/1 from the dynamic db.
%
cleanup_safe_example:-
        clause(safe_example(_),B)
        ,poker_configuration:retract(safe_example(_):-B)
        ,!.
cleanup_safe_example.


% Drops tables if it's expensive or useless to recompute them.
% If the flag value is N and the cost to compute a table is C, then
% a table is kept only if recomputing it would cost N * C more than C
% where the cost is measured in inferences.
:- set_prolog_flag(table_drop_intermediate, 10).
% If the trie representation of a table has fewer nodes than this flag's
% value answers are enumerated from the trie instead of compiling the
% trie into a clause. Default value is 32. Higher values avoid compiling
% tables that aren't called again while lower values reduce the memory
% used for tables with long answers.
:- set_prolog_flag(table_compile_threshold, 10).
%:- set_prolog_flag(table_subsumptive, true).


background_knowledge(s/2,[a/2,b/2,c/2]).
background_knowledge(p/2,[a/2,b/2]).

metarules(s/2,[knf_AB_CD,identity,chain,tri_chain]).
metarules(p/2,[knf_AB_CD,identity,chain,tri_chain]).

labelled_example(_,_):- fail.

unlabelled_example(_,_):- fail.

a --> [a].
b --> [b].
c --> [c].



                /*******************************
                *    LISTING CONFIGURATION     *
                *******************************/


%!      list_configs(+Language) is det.
%
%       List assorted configuration options for a Language.
%
list_configs(Lang):-
        writeln('Setting configs')
        ,listing(set_configs(Lang))
        ,set_configs(Lang)
        ,current_prolog_flag(stack_limit, X)
        ,format('Global stack limit ~D~n',[X])
        ,nl
        ,current_prolog_flag(table_space, V)
        ,format('Table space ~D~n',[V])
        ,nl
        ,auxiliaries:list_config
        ,nl
        ,poker_auxiliaries:list_poker_config
        ,nl
        ,poker_auxiliaries:list_mil_problem(s/2)
        ,nl
        ,listing(Lang)
        ,listing(setup_safe_example)
        ,listing(cleanup_safe_example).



                /*******************************
                *     LANGUAGE GENERATION      *
                *******************************/
% For now we can keep those here but later roll them up in the main
% grammars.pl file.


:-table(language_generation:anbncn//0).
:-table(s1//0).
:-table(s2//0).
:-table(a1//0).
:-table(a2//0).
:-table(a3//0).
:-table(c1//0).

% Target theory in KNF adapated from grammar proposed by Gemini.
%
% 10 clauses.
% 6 invented symbols: s1, s2, a1, a2, a3, c1.
language_generation:anbncn --> [a], s1.
language_generation:anbncn --> [a], s2.
s1 --> [b, c].
s2 --> a1, s1.

a1 --> [a], a2.
a1 --> [a], a3.
a2 --> [b], c1.
a3 --> a1, a2.

c1, [b] --> [b], c1.
c1, [c] --> [c, c].

:-table(abc//1).

language_generation:not_anbncn --> abc(Ps), { \+ phrase(language_generation:anbncn,Ps) }.

% I have no idea what this looks like in KNF.
%
abc([a|Ss]) --> [a], abc(Ss).
abc([b|Ss]) --> [b], abc(Ss).
abc([c|Ss]) --> [c], abc(Ss).
abc([a]) --> [a].
abc([b]) --> [b].
abc([c]) --> [c].

/* Actually makes generation slower.
:-table(language_generation:ww//0).
:-table(ab//1).
:-table(a//1).
:-table(b//1).
*/

language_generation:ww --> ab(Ss), ab(Ss).

ab([S|Ss]) --> a(S), ab(Ss).
ab([S|Ss]) --> b(S), ab(Ss).
ab([S]) --> a(S).
ab([S]) --> b(S).

a(a) --> [a].
b(b) --> [b].

language_generation:not_ww --> ab(Ps), ab(Rs), { Ps \== Rs }.
