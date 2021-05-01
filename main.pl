/*  
**  File: projeto.pl
**  Author: Jeronimo Mendes 99086 LEIC-T 
**  Description: LP Project. Kakuro puzzle solver.
*/

% Imports commun code and puzzles
:-consult(codigo_comum).

/*
**
**  MAIN PREDICATES
**
*/

combinacoes_soma(N, Els, Soma, Combs):-
	findall(Combs,combinacao(N,Els,Combs),X),
	include(list_sum(Soma), X, Combs).


/*
**
**  AUXILIAR PREDICATES
**
*/

% Sum of all elements of a list.
list_sum(0,[]).
list_sum(Sum, [H|T]) :-
   sum_list(T, Rest),
   Sum is H + Rest.
