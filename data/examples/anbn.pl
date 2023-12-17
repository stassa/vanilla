:-module(anbn, [program_signature/3
	       ,background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,a/2
	       ,b/2
	       ]).

/** <module> Learn an a^nb^n CFG with recursion and predicate invention.


*/

program_signature(s/2,[s,a,'inv_1',b],[]).
program_signature(inv_1/2,['inv_1',s,a,b],[]).

background_knowledge(s/2,[a/2,b/2]).

metarules(s/2,[chain]).

positive_example(s/2,E):-
% Uncomment extra examples to experiment with different combinations
% thereof.
	member(E, [%s([a,b],[])
		  s([a,a,b,b],[])
		  %,s([a,a,a,b,b,b],[])
		  %,s([a,a,a,a,b,b,b,b],[])
		  %,s([a,a,a,a,a,b,b,b,b,b],[])
		  %,s([a,a,a,a,a,a,a,b,b,b,b,b,b,b],[])
		  ]).

negative_example(s/2,E):-
	member(E,[s([a,a],[])
		 ,s([b,b],[])
		 ,s([a,a,b],[])
		 ,s([a,b,b],[])
	       ]).

a([a|T],T).
b([b|T],T).

