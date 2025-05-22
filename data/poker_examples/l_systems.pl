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
                   ,abop_plant_a/3
                   ,abop_plant_b/3
                   ,abop_plant_c/3
                   ,abop_plant_d/3
                   ,abop_plant_e/3
                   ,abop_plant_f/3
                   ,test_draw/5
                   ,draw/1
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
%       ==
%       ?- _M = l_systems, _S = dragon_curve, _I = 16, _A = [f], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(90),rangle(90),distance(2),tilt(-10),start(-(-280,-50)),width(850),height(550)]), !.
%       ==
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
%       ==
%       ?- _M = l_systems, _S = koch_curve, _I = 6, _A = [f,-,-,f,-,-,f], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(60),rangle(60),distance(1),start(-(-450,-250))]), !.
%       ==
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
%       ==
%       ?- _M = l_systems, _S = hilbert_curve, _I = 7, _A = [x], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(90),rangle(90),distance(8),start('top_left')]), !.
%       ==
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
%       ==
%       ?- _M = l_systems, _S = sierpinski_triangle, _I = 7, _A = [f,-,g,-,g], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(120),rangle(120),distance(6),start('bottom_left'),width(400),height(350)]), !.
%       ==
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
%       ==
%       _M = l_systems, _S = sierpinski_arrowhead, _I = 8, _A = [x,f], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(60),rangle(60),distance(4),start('bottom_left')]), !.
%       ==
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
%       Example 7 from Wikipedia page on L-Systems. Same as
%       Node-rewriting L-System plant in figure 1.24 (f) of ABOP,
%       implemented below as abop_plant_f, but with left and right
%       turns flipped (and different axiom).
%
%       Axiom: [-,x]
%       Angle: 25
%
%       Draw with:
%       ==
%       ?- _M = l_systems, _S = fractal_plant, _I = 8, _A = [-,x], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(25),rangle(-25),distance(2),tilt(75),start('bottom_left'),width(960),height(1800)]), !.
%       ==
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



%!      abop_plant_a(?String) is semidet.
%
%       Plant from ABOP figure 1.24 (a)
%
%       Axiom: [f]
%       Angle: 27.5
%
%       Draw with:
%       ==
%       ?- _M = l_systems, _S = abop_plant_a, _I = 5, _A = [f], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(25.7),rangle(-25.7),distance(4),tilt(90),start('bottom_center'),width(960),height(1200)]), !.
%       ==
%
abop_plant_a([+|Ss]) --> plus, abop_plant_a(Ss).
abop_plant_a([-|Ss]) --> minus, abop_plant_a(Ss).
abop_plant_a(['['|Ss]) --> lsb,  abop_plant_a(Ss).
abop_plant_a([']'|Ss]) --> rsb,  abop_plant_a(Ss).
abop_plant_a([f,'[',+,f,']',f,'[',-,f,']',f|Ss]) --> f,  abop_plant_a(Ss).
abop_plant_a([]) --> [].



%!      abop_plant_b(?String) is semidet.
%
%       Plant from ABOP figure 1.24 (b)
%
%       One of three edge-rewriting bracketed L-Systems in ABOP figure
%       1.24.
%
%       Axiom: [f]
%       Angle: 20
%
%       Draw with:
%       ==- _M = l_systems, _S = abop_plant_b, _I = 5, _A = [f], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(20),rangle(-20),distance(6),tilt(90),start('bottom_center'),width(960),height(1200)]), !.
%       ==
%
abop_plant_b([+|Ss]) --> plus, abop_plant_b(Ss).
abop_plant_b([-|Ss]) --> minus, abop_plant_b(Ss).
abop_plant_b(['['|Ss]) --> lsb,  abop_plant_b(Ss).
abop_plant_b([']'|Ss]) --> rsb,  abop_plant_b(Ss).
abop_plant_b([f,'[',+,f,']',f,'[',-,f,']','[',f,']'|Ss]) --> f,abop_plant_b(Ss).
abop_plant_b([]) --> [].



%!      abop_plant_c(?String) is semidet.
%
%       Plant from ABOP figure 1.24 (c)
%
%       One of three edge-rewriting bracketed L-Systems in ABOP figure
%       1.24.
%
%       Axiom: [f]
%       Angle: 22.5
%
%       Draw with:
%       ==
%       ?- _M = l_systems, _S = abop_plant_c, _I = 4, _A = [f], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(22.5),rangle(-22.5),distance(6),tilt(90),start('bottom_center'),width(960),height(1200)]), !.
%       ==
%
abop_plant_c([+|Ss]) --> plus, abop_plant_c(Ss).
abop_plant_c([-|Ss]) --> minus, abop_plant_c(Ss).
abop_plant_c(['['|Ss]) --> lsb,  abop_plant_c(Ss).
abop_plant_c([']'|Ss]) --> rsb,  abop_plant_c(Ss).
abop_plant_c([f,f,-,'[',-,f,+,f,+,f,']',+,'[',+,f,-,f,-,f,']'|Ss]) --> f,  abop_plant_c(Ss).
abop_plant_c([]) --> [].



%!      abop_plant_d(?String) is semidet.
%
%       Plant from ABOP figure 1.24 (d)
%
%       One of three node-rewriting bracketed L-Systems in ABOP figure
%       1.24.
%
%       Axiom: [x]
%       Angle: 20
%
%       Draw with:
%       ==
%       ?- _M = l_systems, _S = abop_plant_d, _I = 7, _A = [x], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(20),rangle(-20),distance(2),tilt(90),start('bottom_center'),weight(960),height(1200)]), !.
%       ==
%
abop_plant_d([+|Ss]) --> plus, abop_plant_d(Ss).
abop_plant_d([-|Ss]) --> minus, abop_plant_d(Ss).
abop_plant_d(['['|Ss]) --> lsb,  abop_plant_d(Ss).
abop_plant_d([']'|Ss]) --> rsb,  abop_plant_d(Ss).
abop_plant_d([f,'[',+,x,']',f,'[',-,x,']',+,x|Ss]) --> x,  abop_plant_d(Ss).
abop_plant_d([f,f|Ss]) --> f,  abop_plant_d(Ss).
abop_plant_d([]) --> [].



%!      abop_plant_e(?String) is semidet.
%
%       Plant from ABOP figure 1.24 (e)
%
%       One of three node-rewriting bracketed L-Systems in ABOP figure
%       1.24.
%
%       Axiom: [x]
%       Angle: 25.7
%
%       Draw with:
%       ==
%       ?- _M = l_systems, _S = abop_plant_e, _I = 7, _A = [x], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(25.7),rangle(-25.7),distance(2),tilt(90),start('bottom_center'),width(960),height(1200)]), !.
%       ==
%
abop_plant_e([+|Ss]) --> plus, abop_plant_e(Ss).
abop_plant_e([-|Ss]) --> minus, abop_plant_e(Ss).
abop_plant_e(['['|Ss]) --> lsb,  abop_plant_e(Ss).
abop_plant_e([']'|Ss]) --> rsb,  abop_plant_e(Ss).
abop_plant_e([f,'[',+,x,']','[',-,x,']',f,x|Ss]) --> x,  abop_plant_e(Ss).
abop_plant_e([f,f|Ss]) --> f,  abop_plant_e(Ss).
abop_plant_e([]) --> [].



%!      abop_plant_f(?String) is semidet.
%
%       Plant from ABOP figure 1.24 (f)
%
%       One of three node-rewriting bracketed L-Systems in ABOP figure
%       1.24.
%
%       Axiom: [x]
%       Angle: 22.5
%
%       Draw with:
%       ==
%       ?- _M = l_systems, _S = abop_plant_f, _I = 5, _A = [x], _M:l_system(_S,_M,_A,_I,_Ss), atomic_list_concat(_Ss,'',_Is), writeln(_Is), _M:draw([instructions(_Is),langle(22.5),rangle(-22.5),distance(6),tilt(90),start('bottom_center'),width(960),height(1200)]), !.
%       ==
%
abop_plant_f([+|Ss]) --> plus, abop_plant_f(Ss).
abop_plant_f([-|Ss]) --> minus, abop_plant_f(Ss).
abop_plant_f(['['|Ss]) --> lsb,  abop_plant_f(Ss).
abop_plant_f([']'|Ss]) --> rsb,  abop_plant_f(Ss).
abop_plant_f([f,-,'[','[',x,']',+,x,']',+,f,'[',+,f,x,']',-,x|Ss]) --> x,  abop_plant_f(Ss).
abop_plant_f([f,f|Ss]) --> f,  abop_plant_f(Ss).
abop_plant_f([]) --> [].



                /*******************************
                *L-SYSTEMS PARSING AND DRAWING *
                *******************************/

%!      draw(+Args) is det.
%
%       Draw an L-System string to screen with Turtle graphics.
%
%       This predicate calls a Python script turtle_mapping.py in the
%       same directory as this file, l_systems.pl, to draw an L-system
%       as an image, using the Python turtle library, via Janus.
%
%       Args is a list of arity-1 compound terms listing the arguments
%       passed to the Python script. This list must respect the ordering
%       of arguments in the Python script arguments. The following is a
%       list of Args term functors in the correct order:
%
%       * instructions: atom, an L-System string.
%       * langle: float, degrees for a left-angle turn.
%       * rangle: float, degrees for a right-angle turn.
%       * distance: integer, distance of forward moves.
%       * tilt: float, left tilt angle. Default: 0.
%       * start: atom, or tuple of integers, starting position. Default:
%         'center'.
%       * width: float, width of screen drawing area. Default: 960.
%       * height: float, height of screen drawing area. Default: 810.
%       * file: atom, path to output file. Default: 'output/turtle.eps'
%
%       Arguments with defaults are optional in the Python script. See
%       default_args/2 for defaults defined in this file.
%
%       The argument of the start/1 term can be an atom denoting a
%       pre-determined starting position for the turtle, as follows:
%       * center: dead center on screen
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
%       The argument of file/1 is a path to an output file where the
%       generated image will be saved.
%
%       The file format of output files is eps (Encapsulated Post
%       Script). Once saved, a file can be converted to a image format
%       and enjoyed with a suitable program, e.g. with epstopdf an
%       outfile can be converted to pdf like this (assuming the outfile
%       name is 'hilbert_curve.eps'):
%       ==
%       epstopdf .\hilbert_curve.eps --outfile=hilbert_curve.pdf
%       ==
%
%       Example call, drawing fractal_plant/3 L-system:
%       ==
%       ?- _M = l_systems
%       ,_S = fractal_plant
%       ,_I = 4
%       ,_A = [-,x]
%       ,_M:l_system(_S,_M,_A,_I,_Ss)
%       ,atomic_list_concat(_Ss,'',_Is)
%       ,writeln(_Is)
%       ,_M:draw([instructions(_Is)
%               ,langle(25)
%               ,rangle(-25)
%               ,distance(2)
%               ,tilt(75)
%               ,start('bottom_left')
%               ,width(960)
%               ,height(1800)]),
%               !.
%       ==
%
draw(As):-
        draw_args(As,[Is,L,R,D,S,T,W,H,F])
        ,py_call(turtle_mapping:draw(Is,L,R,D,S,T,W,H,F)).


%!      draw_args(+Args,-Values) is det.
%
%       Extract L-System drawing args from an input list.
%
%       Args is a list of compound terms listing the arguments passed to
%       the Turtle drawing script. See draw/1 for details.
%
%       Names of known arguments are taken from default_args/2. Missing
%       arguments are replaced by defaults defined in that predicate.
%
%       Example with default filename and dummy instructions list:
%       ==
%       ?- l_systems:draw_args([instructions(none)
%       ,langle(25)
%       ,rangle(-25)
%       ,distance(2)
%       ,tilt(75)
%       ,start('bottom_left')
%       ,width(960),height(1800)], Os).
%
%       Os = [none,25,-25,2,75,bottom_left,960,1800,'output/turtle.eps'].
%       ==
%
draw_args(As,As_):-
        findall(A
                ,(default_args(Arg,Def)
                 ,A_ =.. [Arg,A]
                 ,(   memberchk(A_,As)
                  ->  true
                  ;   A = Def
                  )
                 )
                ,As_).


%!      default_args(?Arg,?Defaults) is semidet.
%
%       Drawing script arguments and Defaults.
%
%       Defaults defined in this predicate override Pythons cript
%       defaults. I mean obviously because they're actually passed to
%       the script.
%
%       Arguments without defaults in the Python script are mapped to
%       the atom 'No default Arg'. If an error is raised because the
%       wrong argument is passed to the wrong place, the error should
%       proooobably help determine the problem.
%
default_args(instructions,'No Default Arg').
default_args(langle,'No Default Arg').
default_args(rangle,'No Default Arg').
default_args(distance,'No Default Arg').
default_args(tilt,0).
default_args(start,'center').
default_args(width,960).
default_args(height,810).
default_args(file,'output/turtle.eps').



%!      test_draw(+Target,+Program,+Generations,+Axiom,+Args)
%!      is det.
%
%       Draw an L-System string generated by a learned hypothesis.
%
%       Target is a predicate indicator, F/A, of a learning target.
%
%       Progam is the learned hypothesis, a logic program definition of
%       Target learned by Poker. Program should be the grammar of an
%       L-System, in DCG form (unsugared).
%
%       Generations is an integer, the maximum iteration for L-System
%       iteration.
%
%       Axiom is a list of characters, the initialiseing string of the
%       L-System grammar defined in Program.
%
%       Args is a list of drawing script argumnts passed to draw/1
%       headed by an L-System string generated by a call to Program. See
%       draw/1 for drawing script arguments.
%
test_draw(_S,[],_I,_Ax,_Args):-
        !.
test_draw(S/Ar,Ps,I,Ax,Args):-
        PM = l_systems
        ,experiment_data(S/Ar,_Ls,_Us,B,_MS)
        ,closure(B,experiment_file,Bs)
        ,flatten(Bs,Bs_f)
        ,Sup = maplist(assert_program(PM),[Ps,Bs_f],[Rs_1,Rs_2])
        ,G = once( l_system(S,PM,Ax,I,Ss) )
        ,C = maplist(erase_program_clauses,[Rs_1,Rs_2])
        ,setup_call_cleanup(Sup,G,C)
        ,atomic_list_concat(Ss,'',Ss_)
        ,draw([instructions(Ss_)|Args]).



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
