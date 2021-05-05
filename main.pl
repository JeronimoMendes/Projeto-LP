/*  
**  File: projeto.pl
**  Author: Jeronimo Mendes 99086 LEIC-T 
**  Description: LP Project. Kakuro puzzle solver.
*/

% Imports commun code and puzzles
:-consult(codigo_comum).


%  ###################
%  MAIN PREDICATES
%  ###################

combinacoes_soma(N, Els, Soma, Combs):-
	findall(Combs,combinacao(N,Els,Combs),X),
	include(list_sum_helper(Soma), X, Combs).


permutacoes_soma(N, Els, Soma, Perms):-
   combinacoes_soma(N, Els, Soma, CombinacoesSoma),
   findall(Y, maplist(permutation, CombinacoesSoma, Y), PermsNested),
   append(PermsNested, PermsDups), % unest each permutation
   list_to_set(PermsDups, Perms). % remove duplicates
      

% espaco_fila(Fila, Esp, H_V) :- 
   
%  ###################
%  AUXILIAR PREDICATES
%  ###################

% Helper predicates. They change the order of arguments so that they
% can be used in excluce/3, include/3...
list_sum_helper(Sum, List) :- sum_list(List, Sum).

lenght_helper(Length, List) :- length(List, Length).

append_helper(L3, L2, L1) :- append(L1, L2, L3).


