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
      

espacos_fila(H_V, Fila, Espacos) :- 
   getBlocks(Fila, Aux),
   maplist(substituteSpace, Fila, Aux2), % substitues free variables with "free"
   listOfIndices(Aux2, Aux,IndicesAux),
   sort(IndicesAux, Indices),
   getSpaces(Fila, Indices, Spaces) -> 
   exclude(nextIsBlock(Aux2), Aux, ImportantBlocks),
   (H_V == v -> maplist(getVerticalValue, ImportantBlocks, ValuesAux)
   ;
   maplist(getHorizontalValue, ImportantBlocks, ValuesAux) 
   ),
   exclude(==(0), ValuesAux, Values),
   maplist(createSpaceStruct, Values, Spaces, Espacos);
   Espacos = [].
   

   

espaco_fila(Fila, Esp, H_V) :-
   espacos_fila(H_V, Fila, Espacos),
   member(Esp, Espacos).


espacos_puzzle(Puzzle, Espacos) :-
   maplist(espacos_fila(h), Puzzle, EspacosH),
   mat_transposta(Puzzle, PuzzleTrans),
   maplist(espacos_fila(v), PuzzleTrans, EspacosV),
   append(EspacosH, EspacosV, EspacosAux),
   append(EspacosAux, Espacos).



%  ###################
%  AUXILIAR PREDICATES
%  ###################

% Helper predicates. They change the order of arguments so that they
% can be used in excluce/3, include/3...
list_sum_helper(Sum, List) :- sum_list(List, Sum).

lenght_helper(Length, List) :- length(List, Length).

append_helper(L3, L2, L1) :- append(L1, L2, L3).

% Determines if a given block is null ([0,0])
isNullBlock(Block) :- Block == [0,0].

% Checks if a list is composed of free variables.
isSpace(List) :- include(nonvar, List, List2), List2 == [].

% Returns the blocks in a file.
getBlocks(File, List) :- include(is_list, File, List).

% Returns spaces in a file.
getSpaces(List, Indices, Spaces) :-
   length(List, Length),
   Length1 is Length + 1,
   append(Indices, [Length1], ListAux),
   appendFirst(1, ListAux, ListAux2),
   bagof(X, listOf_(List, ListAux2, X), SpacesWithNulls),
   exclude(==([]), SpacesWithNulls, SpacesWithDups), % Removes null lists
   list_to_set(SpacesWithDups, Spaces).
   

% Returns a espaco struct given the square value (Index) and the list of spaces
createSpaceStruct(Index, Space, Output) :- Output = espaco(Index, Space).


% Returns the number of occurrences of a given element in a list
occurrences_of(List, X, Count) :- aggregate_all(count, member(X, List), Count).


% Swaps head with tail
swap([X, Y|T], [Y, X|T]).


% Replaces free variables in a list with string "free"
substituteSpace(X, "free") :- \+ nonvar(X),!.
substituteSpace(X,X).


% Returns index of element in the list
index(List, E, Is) :-
   findall(N, nth1(N, List, E), Is).


% Returns a list of indices of given list of elements of list
listOfIndices(List, List2, Indices) :-
   maplist(index(List), List2, Aux),
   append(Aux, Indices).


% Creates a sublist from a list, given the two indices to cut it from
sublist(L, M, N, S) :-
   MP is M + 1, % M plus
   NM is N - 1, % N minus
   bagof(E, I^(between(MP, NM, I), nth1(I, L, E)), S).


% Appends an element to the beggining of a list
appendFirst(Item, List, [Item|List]).


% Returns a list of all group of spaces in a Fila, given the indices of blocks
listOf_(List, Indices, Solution) :-
   member(X, Indices),
   nextto(X, Y, Indices),
   sublist(List, X, Y, Solution).
   

% Removes the Nth1(N1) element from a list (As) and returns the result(Bs)
removeNth1(As,N1,Bs) :-
   same_length(As,[_|Bs]),
   append(Prefix,[_|Suffix],As),
   length([_|Prefix],N1),
   append(Prefix,Suffix,Bs).


% Returns the horizontal value of a given block
getHorizontalValue([_|[H|_]], H).


% Returns the vertical value of a given block
getVerticalValue([V|_], V).


% True is the element next to a given element in a lista is a block
nextIsBlock(List, El) :-
   maplist(substituteSpace, List, AuxList),
   nextto(El, Y, AuxList),
   is_list(Y).
   