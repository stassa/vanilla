:-module(l_systems,[l_star/3
                   ,algae/3
                   ,not_algae/3
                   ,dragon_curve/3
                   ,not_dragon_curve/3
                   ,koch_curve/3
                   ,not_koch_curve/3
                   ,hilbert_curve/3
                   ,hilbert_curve_with_vars/3
                   ,not_hilbert_curve/3
                   ,sierpinski_triangle/3
                   ,not_sierpinski_triangle/3
                   ,sierpinski_arrowhead/3
                   ,fractal_plant/3
                   ,test_draw/11
                   ,l_system/5
                   ]).

:-use_module(lib(poker/poker_auxiliaries)).
:-use_module(src(auxiliaries)).
:-reexport(data(poker_examples/l_systems_long)).

/** <module> L-System grammars


*/

%!      algae// is nondet.
%
%       Algae L-System as a context-free grammar.
%
%       Lindemayer's book ("The algorithmic beauty of plans") claims
%       that L-Systems are not context-free grammars, because at each
%       iteration all new tokens placed on the grammar stack are
%       processed "simultaneously".
%
%       That seems to be right, after all. Treating the algae grammar as
%       a Context-Free Grammar, here implemented in Definite Clause
%       Grammars form, results in a grammar that can only perform one
%       iteration of generation. On the other hand, this grammar can
%       start at any point in the generation, taking as input the algae
%       string at iteration k, and outputing the string at iteration
%       k+1.
%
%       Example query:
%       ==
%       ?- phrase(test_harness:algae(Ss),[a,b,a]).
%       Ss = [a,b,a,a,b] ;
%       false.
%
%       % You can also run the grammar backwards to go one iteration back:
%       ?- phrase(test_harness:algae([a,b,a]),Ss).
%       Ss = [a,b] ;
%       false.
%       ==
%
%       __Implementation Notes__
%
%       The DCG below is indeed a direct translation of the rules:
%       ==
%       A -> AB
%       B -> A
%       ==
%
%       And so it's missing the iteration counter that controls the
%       number of iterations. Parsing ends when the entire input string
%       (in the second variable of phrase/2) is consumed. It is possible
%       to add a counter if desired, but that's left as an exercise to
%       the reader.
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_1:_algae
%
algae([a,b|Ss]) --> a, algae(Ss).
algae([a|Ss]) --> b, algae(Ss).
algae([]) --> [].


a --> [a].
b --> [b].


%!      not_algae(Ss) is nondet.
%
%       Not an algage grammar.
%
%       Not great though.
%
not_algae(Ss) -->
        ab_string(Ss)
        ,{  \+ phrase(algae(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(ab_string(Xs),_)
         }
        ,Xs.


ab_string([S]) --> ab(S).
ab_string([S|Ss]) --> ab(S), ab_string(Ss).

ab(a) --> a.
ab(b) --> b.


%!      fractal_tree(?String) is semidet.
%
%       Calculate a fractal tree String.
%
%       Mnyeah, dunno. This one's not very clear.
%
%       Axiom: [0]
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_2:_fractal_(binary)_tree
%
fractal_tree(['['|Ss]) --> lsb,  fractal_tree(Ss).
fractal_tree([']'|Ss]) --> rsb,  fractal_tree(Ss).
fractal_tree([1,1|Ss]) --> one,  fractal_tree(Ss).
fractal_tree([1,'[',0,']',0|Ss]) --> zero,  fractal_tree(Ss).
fractal_tree([]) --> [].

zero --> [0].
one --> [1].



                /*******************************
                *      L-SYSTEM FRACTALS       *
                *******************************/


% Vocabulary, used to command a turtle.
% Note: b//0 is not declared here because it is already declared earlier
% but when used in an L-System grammar it is interpreted as "move
% backwards"


%!      lsb// is semidet.
%
%       Push current position and angle on the stack.
%
lsb --> ['['].

%!      rsb// is semidet.
%
%       Pop position and angle from the stack.
%
rsb --> [']'].

%!      f// is semidet.
%
%       Move the turtle forward in the current heading.
%
%       Synonymous to g//0.
%
f --> [f].

%!      g// is semidet.
%
%       Move the turtle forward in the current heading.
%
%       Synonymous to f//0.
%
g --> [g].


%!      plus// is semidet.
%
%       Turn left by the current angle setting.
%
plus --> [+].


%!      minus// is semidet.
%
%       Turn right by the current angle setting.
%
minus --> [-].

%!      x// is semidet.
%
%       Ignored, used to control evolution of shape.
%
x --> [x].


%!      y// is semidet.
%
%       Ignored, used to control evolution of shape.
%
y --> [y].

:- table l_star/3.

%!      l_star  is nondet.
%
%       Maximally general L-System grammar.
%
l_star(X,Y,Z):- plus(Y,U), plus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- minus(Y,U), minus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- f(Y,U), f(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- g(Y,U), g(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- x(Y,U), x(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- x(Y,U), x(X,V), l_star(V,U,Z).

l_star(X,Y,Z):- f(Y,U), plus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- f(Y,U), minus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- f(Y,U), f(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- f(Y,U), g(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- f(Y,U), x(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- f(Y,U), y(X,V), l_star(V,U,Z).

l_star(X,Y,Z):- g(Y,U), plus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- g(Y,U), minus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- g(Y,U), f(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- g(Y,U), g(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- g(Y,U), x(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- g(Y,U), y(X,V), l_star(V,U,Z).

l_star(X,Y,Z):- x(Y,U), plus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- x(Y,U), minus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- x(Y,U), f(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- x(Y,U), g(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- x(Y,U), x(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- x(Y,U), y(X,V), l_star(V,U,Z).

l_star(X,Y,Z):- y(Y,U), plus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- y(Y,U), minus(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- y(Y,U), f(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- y(Y,U), g(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- y(Y,U), x(X,V), l_star(V,U,Z).
l_star(X,Y,Z):- y(Y,U), y(X,V), l_star(V,U,Z).

l_star(X,Y,Y):- empty(X,Y).

empty --> [].


%!      dragon_curve(?String) is semidet.
%
%       Calculate a Dragon Curve string.
%
%       Axiom: [f]
%       Angle: 90
%
%       Draw with:
%       _S = dragon_curve, _K = 15, _A = [f], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ),atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,90,90,2,'center').
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_6:_dragon_curve
%
dragon_curve([+|Ss])--> plus, dragon_curve(Ss).
dragon_curve([-|Ss])--> minus, dragon_curve(Ss).
dragon_curve([f,+,g|Ss])--> f, dragon_curve(Ss).
dragon_curve([f,-,g|Ss])--> g, dragon_curve(Ss).
dragon_curve([])--> [].


%!      not_dragon_curve(?String) is semidet.
%
%       Not a dragon curve. Honest.
%
not_dragon_curve(Ss) -->
        dragon_string(Ss)
        ,{  \+ phrase(dragon_curve(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(dragon_string(Xs),_)
         }
        ,Xs.

dragon_string([C]) --> dragon_char(C).
dragon_string([C|Ss]) --> dragon_char(C), dragon_string(Ss).

dragon_char(+) --> plus.
dragon_char(-) --> minus.
dragon_char(f) --> f.
dragon_char(g) --> g.



%!      koch_curve(?String) is semidet.
%
%       Calculate a Koch Curve String.
%
%       Axiom: [f,-,-,f,-,-,f]
%       Alt:   [f,+,+,f,+,+,f]
%       Angle: 90
%
%       Draw with:
%       _S = koch_curve, _K = 6, _A = [f,-,-,f,-,-,f], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,60,60,1,'bottom_center').
%
%       Sources:
%       https://en.wikipedia.org/wiki/L-system#Example_4:_Koch_curve
%       https://en.wikipedia.org/wiki/Koch_snowflake#Representation_as_Lindenmayer_system
%
koch_curve([+|Ss])--> plus, koch_curve(Ss).
koch_curve([-|Ss])--> minus, koch_curve(Ss).
koch_curve([f,+,f,-,-,f,+,f|Ss]) --> f, koch_curve(Ss).
% Variants - which ones?
% Square:
%koch_curve([f,+,f,-,f,-,f,+,f|Ss]) --> f, koch_curve(Ss).
%koch_curve([f,-,f,+,+,f,-,f|Ss]) --> f, koch_curve(Ss).
koch_curve([]) --> [].


%!      not_koch_curve(?String) is semidet.
%
%       Not a Koch curve String.
%
not_koch_curve(Ss) -->
        koch_string(Ss)
        ,{  \+ phrase(koch_curve(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(koch_string(Xs),_)
         }
        ,Xs.

koch_string([C]) --> koch_char(C).
koch_string([C|Ss]) --> koch_char(C), koch_string(Ss).

koch_char(+) --> plus.
koch_char(-) --> minus.
koch_char(f) --> f.


%!      hilbert_curve(?String) is semidet.
%
%       Calculate a Hilbert Curve String.
%
%       Axiom: [x]
%       Angle: 90
%
%       Draw with:
%       _S = hilbert_curve, _K = 7, _A = [x], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,90,90,8,'bottom_left').
%
%       Source:
%       https://en.wikipedia.org/wiki/Hilbert_curve#Representation_as_Lindenmayer_system
%
hilbert_curve([+|Ss])--> plus, hilbert_curve(Ss).
hilbert_curve([-|Ss])--> minus, hilbert_curve(Ss).
hilbert_curve([f|Ss])--> f, hilbert_curve(Ss).
hilbert_curve([+,y,f,-,x,f,x,-,f,y,+|Ss]) --> x, hilbert_curve(Ss).
hilbert_curve([-,x,f,+,y,f,y,+,f,x,-|Ss]) --> y, hilbert_curve(Ss).
hilbert_curve([]) --> [].

%!      not_hilbert_curve(?String) is semidet.
%
%       Not a Hilbert curve String.
%
not_hilbert_curve(Ss) -->
        hilbert_string(Ss)
        ,{  \+ phrase(hilbert_curve(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(hilbert_string(Xs),_)
         }
        ,Xs.

hilbert_string([C]) --> hilbert_char(C).
hilbert_string([C|Ss]) --> hilbert_char(C), hilbert_string(Ss).

hilbert_char(+) --> plus.
hilbert_char(-) --> minus.
hilbert_char(f) --> f.
hilbert_char(x) --> x.
hilbert_char(y) --> y.



%!      sierpinski_triangle(?String) is semidet.
%
%       Calculate a Sierpinski Triangle string.
%
%       Axiom: [f,-,g,-,g]
%       Angle: 120
%
%       Draw with:
%       ?- _M = l_systems, _S = sierpinski_triangle, _I = 6, _A = [f,-,g,-,g], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw(_Is,120,120,6,-(-480,65),400,350,'output/sierpinski_triangle.eps'), !.
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_5:_Sierpinski_triangle
%
%       Also see:
%       https://en.wikipedia.org/wiki/Sierpi%C5%84ski_curve#Representation_as_Lindenmayer_system
%
sierpinski_triangle([+|Ss])--> plus, sierpinski_triangle(Ss).
sierpinski_triangle([-|Ss])--> minus, sierpinski_triangle(Ss).
sierpinski_triangle([f,-,g,+,f,+,g,-,f|Ss]) --> f, sierpinski_triangle(Ss).
sierpinski_triangle([g,g|Ss]) --> g, sierpinski_triangle(Ss).
sierpinski_triangle([]) --> [].

not_sierpinski_triangle(Ss) -->
        sierpinski_string(Ss)
        ,{  \+ phrase(sierpinski_triangle(Ss),_,[])
            ,maplist(length,[Ss,Xs],[N,N])
            ,phrase(sierpinski_string(Xs),_)
         }
        ,Xs.

sierpinski_string([C]) --> sierpinski_char(C).
sierpinski_string([C|Ss]) --> sierpinski_char(C), sierpinski_string(Ss).

sierpinski_char(+) --> plus.
sierpinski_char(-) --> minus.
sierpinski_char(f) --> f.
sierpinski_char(g) --> g.



%!      sierpinski_arrowhead(?String) is semidet.
%
%       Calculate a Sierpinski Arrowhead String.
%
%       Axiom: [x,f]
%       Angle: 60
%
%       Draw with:
%       _S = sierpinski_arrowhead, _K = 8, _A = [x,f], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,60,60,4,'bottom_center').
%
%       Source:
%       https://en.wikipedia.org/wiki/Sierpi%C5%84ski_curve#Representation_as_Lindenmayer_system_2
%
sierpinski_arrowhead([+|Ss])--> plus, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([-|Ss])--> minus, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([f|Ss])--> f, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([y,f,+,x,f,+,y|Ss])--> x, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([x,f,-,y,f,-,x|Ss])--> y, sierpinski_arrowhead(Ss).
sierpinski_arrowhead([]) --> [].



                /*******************************
                *       L-SYSTEMS PLANTS       *
                *******************************/


%!      fractal_plant(?String) is semidet.
%
%       Calculate a fractal plant String.
%
%       This is the classic L-System that draws a grass-like shape.
%
%       Axiom: [-,x]
%       Angle: 25
%
%       Draw with:
%       _S = fractal_plant, _K = 8, _A = [-,x], test_harness:( peano(_K,_I), l_system(_S,_A,_I,_Ss) ), atomic_list_concat(_Ss,'',_Is), writeln(_Is), test_harness:draw(_Is,25,25,1.5,'bottom_left').
%
%       Source:
%       https://en.wikipedia.org/wiki/L-system#Example_7:_fractal_plant
%
fractal_plant([+|Ss]) --> plus, fractal_plant(Ss).
fractal_plant([-|Ss]) --> minus, fractal_plant(Ss).
fractal_plant(['['|Ss]) --> lsb,  fractal_plant(Ss).
fractal_plant([']'|Ss]) --> rsb,  fractal_plant(Ss).
fractal_plant([f,f|Ss]) --> f,  fractal_plant(Ss).
fractal_plant([f,+,'[','[',x,']',-,x,']',-,f,'[',-,f,x,']',+,x|Ss]) -->
        x,
        fractal_plant(Ss).
fractal_plant([]) --> [].



                /*******************************
                *L-SYSTEMS PARSING AND DRAWING *
                *******************************/


%!      draw(+String,+LeftAngle,+RightAngle,+Distance,+Start,+Width,+Height,+File)
%!      is det.
%
%       Draw an L-System String to screen with Turtle graphics.
%
%       This predicate calls a Python script turtle_mapping.py in the
%       same directory as this file, test_harness.pl, to draw an
%       L-system as an image, using the Python turtle library, via
%       Janus.
%
%       String is an atom, an L-system string in atomic form.
%
%       LeftAngle is the angle for left turns when changing the
%       turtle's heading.
%
%       RightAngle is the angle for right turns.
%
%       Distance is the amount (of pixels, I guess) the turtle moves
%       across the screen in its current heading.
%
%       Start is an atom denoting the initial position of the turtle:
%       * center: start at dead center on screen
%       * bottom_right: duh
%       * bottom_center: duh
%       * bottom_left: also
%       * top_left: etc
%       * top_right: not implemented!
%
%       Alternatively, Start can be a term -(X,Y), where X and Y are the
%       coordinates of the point on the drawing screen where the turtle
%       will begin drawing the shape in String. Don't forget the "-"
%       operator: it identifies the term as a Python tuple when it is
%       passed to the Python turtle interpreter.
%
%       Width and Height are the widht and height of the drawing screen
%       on which the turtle will draw.
%
%       File is the name of an output file to save the generated image.
%       The file format is eps (Encapsulated Post Script). Once saved, a
%       file can be converted to a image format and enjoyed with a
%       suitable program, e.g. with epstopdf an outfile can be converted
%       to pdf like this (assuming the outfile name is
%       'hilbert_curve.eps'):
%       ==
%       epstopdf .\hilbert_curve.eps --outfile=hilbert_curve.pdf
%       ==
%
%       Width, Height and File, or only File can be the atom "nil",
%       indicating that the default options in the turtle_mapping.py
%       script should be kept. Width and Height must either both be
%       numbers, or nil; it's not possible to leave one as a number and
%       the other as "nil". Or, well, it's possible, but there will be
%       an error raised.
%
%       If File is "nil" the generated image is saved in a file named
%       "turtle.eps" in the current working directory of the SWI-Prolog
%       process.
%
draw(Is,S,A,D,P,nil,nil,nil):-
        py_call(turtle_mapping:draw(Is,S,A,D,P)).
draw(Is,S,A,D,P,W,H,nil):-
        py_call(turtle_mapping:draw(Is,S,A,D,P,W,H)).
draw(Is,S,A,D,P,W,H,F):-
        py_call(turtle_mapping:draw(Is,S,A,D,P,W,H,F)).



%!      test_draw(+Tgt,+H,+I,+Axiom,+Right,+Left,+Dist,+Start,+Width,+Height,+File)
%!      is det.
%
%       Draw an L-System string generated by a learned hypothesis.
%
%       Tgt is a predicate indicator, F/A, of a learning target.
%
%       H is the hypothesis, a learned logic program definition of Tgt.
%       H should be a learned grammar of an L-System, in DCG form
%       (unsugared).
%
%       I is an integer, the maximum iteration for L-System iteration.
%
%       Axiom is a list of characters, the input string of the L-System
%       grammar defined in H.
%
%       Right and Left are the angles for left and right turns of the
%       cursor drawing the L-System (a turtle-graphics cursor).
%
%       Distance is the distance covered by the cursor when moving in
%       a straight line (in pixels).
%
%       Start is an atom, or a term, denoting the initial position of
%       the cursor. See draw/5 for options.
%
%       Width and Height are the width and height of the drawing area on
%       the screen.
%
%       File is the name of an output file where the generated image
%       will be saved.
%
test_draw(S/Ar,Ps,I,Ax,RA,LA,D,St,W,H,F):-
        PM = l_systems
        ,experiment_data(S/Ar,_Ls,_Us,B,_MS)
        ,closure(B,experiment_file,Bs)
        ,flatten(Bs,Bs_f)
        ,Sup = maplist(assert_program(PM),[Ps,Bs_f],[Rs_1,Rs_2])
        ,G = once( l_system(S,PM,Ax,I,Ss) )
        ,C = maplist(erase_program_clauses,[Rs_1,Rs_2])
        ,setup_call_cleanup(Sup,G,C)
        ,atomic_list_concat(Ss,'',Ss_)
        ,draw(Ss_,LA,RA,D,St,W,H,F).



%!      l_system(+Symbol,+Module,+Input,+Iteration,-Output) is det.
%
%       L-system interpreter.
%
%       Symbol is the starting symbol of an L-System grammar.
%
%       Module is the module where the L-System grammar is defined.
%
%       Input is a list of terminals accepted by an L-system grammar,
%       given as an initial state.
%
%       Iteration is an integer, the final iteration of generation
%       following from the initial state given as Input.
%
%       Output is the output string of the L-System at the given
%       Iteration.
%
%       An L-System grammar must be loaded in memory as a set of DCG
%       rules with the start symbol Symbol//1 and accessible from the
%       named Module.
%
%       Examples:
%       ==
%       ?- test_harness:l_system(algae,test_harness,[a],3,Ss).
%       Ss = [a,b,a,a,b] ;
%       false.
%
%       ?- test_harness:l_system(algae,test_harness,[a,b,a,a,b],1,Ss).
%       Ss = [a,b,a,a,b,a,b,a] ;
%       false.
%
%       ?- test_harness:l_system(algae,test_harness,[a],4,Ss).
%       Ss = [a,b,a,a,b,a,b,a] ;
%       false.
%       ==
%
l_system(_S,_M,Os,0,Os):-
        !.
l_system(S,M,Is,N,Os):-
        S_ =.. [S,Ss]
	,phrase(M:S_, Is)
        ,N_ is N - 1
        ,l_system(S,M,Ss,N_,Os).


                /*******************************
                *  I JUST LIKE CTRL+C-CTRL+H   *
                *******************************/
