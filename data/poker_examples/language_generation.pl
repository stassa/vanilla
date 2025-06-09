:-module(language_generation,[generate_initial/3
                             ,generate_initial/6
                             ,count_initial/3
                             ,count_initial/6
                             ]).

:-use_module(lib(poker/sampling/sampling)).
:-use_module(data(poker_examples/l_systems)).
:-use_module(data(poker_examples/grammars)).

/** <module> Predicates to generate examples of languages.

Predicates in this module generate examples of grammars of Context-Free
and L-System languages used in Poker experiments.

*/

%!      generate_initial(+Targets,+Symbol,-Examples) is det.
%
%       Generate atoms used as examples for a set of targets.
%
%       Similar to generate_initial/5 but allows atoms to be generated
%       from a compisition of two or more target languages.
%
%       Targets is either a list of language generation specification
%       terms S(N,J,K), or a single such term. Targets may also be a
%       pair Targets/Filter.
%
%       In each term S(N,J,K), S is the start symbol of a target
%       language that must be defined as a Definite Clause Grammar in
%       test_harness.
%
%       Symbol is a predicate indicator, Symbol/Arity, of the atoms in
%       Examples. Symbol is a single term, therefore all atoms in
%       Examples will have the same symbol, regardless of the target
%       language.
%
%       N, J, K, are the numbers of atoms to generate and the minimum
%       (J) and maximum (K) length of strings represented by those
%       atoms. N can be the atom "all", in which case, you guessed it,
%       _all_ examples of strings of S of length between J and K will be
%       generated.
%
%       If Targets is a term Targets/Filter, Filter is an atom, the
%       symbol, but not arity, of a language by which to filter
%       Examples.
%
%       Examples is the list of atoms generated that way. Those are
%       atoms of each language in Targets, therefore they are atoms in
%       Definite Clause Grammars notation i.e. they are of the form
%       S(Xs,Ys,...) where Xs, Ys, etc. are lists of characters
%       representing strings in a language, the arity of each atom is
%       the arity in Symbol and and S is the functor in Symbol.
%
%       If a Filter option is given all examples of the filtering
%       language are removed from Examples before returning.
%
%       Example:
%       ==
%       ?- generate_initial(anbn(all,0,12),S/2,_Es), maplist(writeln,_Es).
%       s([a,b],[])
%       s([a,a,b,b],[])
%       s([a,a,a,b,b,b],[])
%       s([a,a,a,a,b,b,b,b],[])
%       s([a,a,a,a,a,b,b,b,b,b],[])
%       s([a,a,a,a,a,a,b,b,b,b,b,b],[])
%       true.
%
%       ?- generate_initial([anbn(all,0,6),anbm(all,0,3)],S/2,_Es)
%       ,maplist(writeln,_Es).
%
%       s([a,b],[])
%       s([a,a,b,b],[])
%       s([a,a,a,b,b,b],[])
%       s([],[])
%       s([a],[])
%       s([a,a],[])
%       s([a,b],[])
%       s([a,a,a],[])
%       s([a,a,b],[])
%       s([a,a,b],[])
%       true.
%       ==
%
%       This predicate calls generate_initial/6 and passes it the name
%       of each tagret language, the Symbol of each atom, and the
%       corresponding number, and min and max length of strings to
%       generate.
%
%       Also see filter_by_language/3 for examples of calling
%       generate_initial/3 with a filtering term.
%
generate_initial(T/F,S,Es_f):-
        !
        ,generate_initial(T,S,Es)
        ,filter_by_language(F,S,Es,Es_f).
generate_initial(T,S,Es):-
        \+ is_list(T)
        ,compound(T)
        ,T =.. [L,N,J,K]
        ,generate_initial(L,S,N,J,K,Es)
        ,!.
generate_initial(Ts,S,Es):-
        is_list(Ts)
        ,findall(Es_s
               ,(member(T,Ts)
                ,generate_initial(T,S,Es_s)
                )
               ,Es_)
        ,flatten(Es_,Es).



%!      filter_by_language(+Filter,+Symbol,+Examples,-Filtered) is det.
%
%       Filter, or not, a set of Examples by a target language.
%
%       Filter is the symobl, but not arity, of a grammar used in
%       experiments. This predicate will remove from Examples all atoms
%       that are also examples of Filter.
%
%       Symbol is a predicate indicator, Functor/Arity, of all atoms in
%       Examples.
%
%       Examples is a list of example atoms of a grammar including zero
%       or more atoms of Filter.
%
%       Filtered is the list Examples with all examples of Filter
%       removed.
%
%       Example use:
%       ==
%       % No filtering
%       ?- test_harness:generate_initial(anbm(all,0,4),s/2,_Es)
%       , maplist(writeln,_Es), length(_Es,N).
%       s([],[])
%       s([a],[])
%       s([a,a],[])
%       s([a,b],[])
%       s([a,a,a],[])
%       s([a,a,b],[])
%       s([a,a,b],[])
%       s([a,a,a,a],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       s([a,a,b,b],[])
%       N = 12.
%
%       % Filtering by anbn; note ab and aabb are gone:
%
%       ?- test_harness:generate_initial(anbm(all,0,4)/anbn,s/2,_Es)
%       , maplist(writeln,_Es), length(_Es,N).
%       s([],[])
%       s([a],[])
%       s([a,a],[])
%       s([a,a,a],[])
%       s([a,a,b],[])
%       s([a,a,b],[])
%       s([a,a,a,a],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       s([a,a,a,b],[])
%       N = 10.
%
%       %Filter by anbm itself: everything's gone:
%
%       ?- test_harness:generate_initial(anbm(all,0,4)/anbm,s/2,_Es)
%       , maplist(writeln,_Es), length(_Es,N).
%       N = 0.
%       ==
%
filter_by_language(L,S/A,Es,Es_f):-
        debug(filter_by_language,'Filtering examples by ~w',[L])
        ,length(Args,A)
        % Atom of filtering language
        ,E =.. [L|Args]
        ,findall(U
                ,(member(U,Es)
                 % Atom of Examples to be filtered
                 ,U =.. [S|Args]
                 % Call the filtering language
                 ,\+ call(E)
                 )
                ,Es_f)
        % Cuts backtracking over more call(E)results.
        ,!.



%!      generate_initial(+Language,+Symbol,+N,+Min,+Max,-Atoms) is det.
%
%       Generate a set of atoms to use as initial examples of Language.
%
%       Language is the symbol, but not arity, of the target theory used
%       to generate atoms.
%
%       Symbol is the predicate indicator, Functor/Arity, of the Atoms
%       to generate.
%
%       N denotes the number of atoms to generate, expressed in one of
%       three ways:
%       * The atom 'all': all atoms with strings of length between Min
%       and Max will be generated.
%       * An integer: exactly that many strings of length between Min
%       and Max will be generated. Raises error if there are not enough
%       strings of that length.
%       * A float: examples will be sampled with probability N using
%       Bernoulli Sampling. There is no guarantee on the number of
%       examples generated this way.
%
%       Min and Max are the upper and lower bounds on the length of
%       strings, represented as definite clause grammars input lists, in
%       the generated atoms.
%
%       Atoms is the list of generated atoms.
%
%       Sampling trade-offs
%       -------------------
%
%       When N is an integer, this predicate first generates _all_ atoms
%       with input lists of length between Min and Max, and then samples
%       N of those atoms, with a call to k_list_samples/3. If N atoms
%       cannot be generated this way, k_list_samples/3 raises a type
%       error. Sampled atoms are added to the output list, Atoms.
%
%       When N is a float, a Bernoulli trial with probability N is
%       carried out for each generated atom and only atoms that pass the
%       trial are added to the output list, Atoms.
%
%       The tradeoff is that k_list_samples/3 guarantees that exactly N
%       Atoms will be returned, if N atoms can be generated, but must
%       first generate the entire set and then sample from it, thus
%       having to keep _two_ large lists in memory simultaneously.
%       Very large lists need a large stack limit and even larger ones
%       can easily blow past your puny student laptop RAM (or mine).
%
%       By contrast, Bernoulli sampling only needs to build the list of
%       Atoms to return, so it can handle larger lists with less memory.
%       On the other hand, because atoms are sampled from a generating
%       program, rather than a fixed-length list, the number of atoms
%       that can be generated cannot be known in advance and therefore
%       the number of Atoms returned can also not be know with
%       certainty. In other words, Bernoulli sampling does not guarantee
%       the number of examples returned in Atoms.
%
generate_initial(L,S,N,J,K,Es):-
        (   integer(N)
        ;   N == all
        )
        ,!
        ,debug(generate_initial,'Generating ~w ~w examples of length in [~w,~w].'
             ,[N,L,J,K])
        ,findall(E
               ,(between(J,K,I)
                ,generate_example(L,S,I,E)
                )
               ,Es_)
        ,(   number(N)
         ->  k_list_samples(N,Es_,Es)
         ;   N == all
         ->  Es = Es_
         ).
generate_initial(L,S,N,J,K,Es):-
        float(N)
        ,debug(generate_initial
              ,'Sampling ~w examples of length in [~w,~w] with probability ~w.'
             ,[L,J,K,N])
        ,findall(E
                ,(between(J,K,I)
                 ,G = generate_example(L,S,I,E)
                 ,goal_partition(N,language_generation,G,true)
                 )
                ,Es).


%!      generate_example(+Target,+Symbol,+Length,-Atom) is nondet.
%
%       Generate an Atom with a list of characters of the given Length.
%
%       Business end of generate_initial/6,
%
generate_example(L,S/2,N,E_):-
        !
        ,abolish_all_tables
        ,debug(generate_example,'Generating ~w length example of ~w:',[N,L])
        ,length(Xs,N)
        ,E =.. [L,Xs,[]]
        ,call(E)
        ,E_ =.. [S,Xs,[]].
generate_example(L,S/3,N,E_):-
        abolish_all_tables
        ,length(Is,N)
        ,E =.. [L,Is,Os,[]]
        ,call(E)
        ,E_ =.. [S,Is,Os,[]].


%!      count_initial(+Spec,+Symbol,-Count) is det.
%
%       Count the number of examples generated by a Specification.
%
%       Helper to determine the number of examples used in experiments.
%       This version calls generate_initial/2 and passes it Spec.
%
%       Spec is a language generation specification term, or a list
%       thereof.
%
%       Symbol is a predicate indicator, Functor/Arity, of the generated
%       examples.
%
%       Count is an integer the number of examples generated by
%       generate_initial/3 when given Spec.
%
count_initial(L,S,N):-
        generate_initial(L,S,Es)
        ,length(Es,N).


%!      count_initial(+Lang,+Symbol,+N,+Min,+Max,-Examples) is det.
%
%       Count the number of examples generated given the arguments.
%
%       Helper to determine the number of examples used in experiments.
%       This version generate_initial/5 and passes it its arguments.
%
%       Language is the symbol, but not arity, of the target theory used
%       to generate atoms.
%
%       Symbol is a predicate indicator, Functor/Arity, of the generated
%       examples.
%
%       N is either a number or the atom all. If N is a number, it's the
%       number of atoms to generate. If it is "all", then all atoms with
%       strings of length between Min and Max will be generated.
%
%       Min and Max are the upper and lower bounds on the length of
%       strings, represented as definite clause grammars input lists, in
%       the generated atoms.
%
%       Examples is the list of generated atoms.
%
count_initial(L,S,N,J,K,M):-
        generate_initial(L,S,N,J,K,Es)
        ,length(Es,M).
