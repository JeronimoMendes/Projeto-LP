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
   setof(Perm, permutationsOfCombination(CombinacoesSoma, Perm), Perms).


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
   exclude(==(0), ValuesAux, Values), % has to be changed to pass test 6
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


espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
   positionsEspaco(Esp, PositionsEsp),
   bagof(Pos, X^PositionEsp^(member(PositionEsp, PositionsEsp) ,member(Pos, Espacos) ,positionsEspaco(Pos, X), isInList(X, PositionEsp)), Esps_comDups),
   removeElement(Esp, Esps_comDups, Esps_com).


permutacoes_soma_espacos(Espacos, Perms_soma) :-
   bagof(Perm, Perm^Espaco^(member(Espaco, Espacos), permutacoes_soma_espaco(Espaco, Perm)), Perms_soma).


permutacao_possivel_espaco(Perm, Esp, Espacos, _) :-
   permutacoes_soma_espaco(Esp, EspPerms),
   positionsEspaco(Esp, Positions),
   espacos_com_posicoes_comuns(Espacos, Esp, EspsCom),
   permutacoes_soma_espacos(EspsCom, EspsComPerm),
   permsPermsEspaco(EspPerms, PermsAux), append(PermsAux, Perms),

   member(Positions, Perms), % iterates each position in the permutations

   % Tests if the position is valid for each espaco in common
   forall(member(PossibleEspsPerm, EspsComPerm), positionIsPossibleInEspaco(PossibleEspsPerm)),

   Perm = Positions.


permutacoes_possiveis_espaco(Espacos, _, Esp, Perms_poss) :-
   findall(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_poss), Perms),
   positionsEspaco(Esp, VarList),
   append([VarList], [Perms], Perms_poss).
   



%  ###################
%  AUXILIAR PREDICATES
%  ###################

% Tests if a position (ex: [3, 2]) is valid in a list of permutations of a espaco 
% (ex: [espaco(5, [P1, P2]), [...,...], ...])
positionIsPossibleInEspaco(PossibleEspsPerm) :-
   variablesPermsEspaco(PossibleEspsPerm, PossiblePerm),

   permsPermsEspaco(PossibleEspsPerm, ComPermsAux), append(ComPermsAux, ComPerms),

   member(ComPerm, ComPerms),

   subsumes_term(PossiblePerm, ComPerm),!.


% Helper predicates. They change the order of arguments so that they
% can be used in excluce/3, include/3...
list_sum_helper(Sum, List) :- sum_list(List, Sum).

lenght_helper(Length, List) :- length(List, Length).

append_helper(L3, L2, L1) :- append(L1, L2, L3).


% Returns a permutation of a list of combinations.
permutationsOfCombination(Combinations, Permutation) :-
   member(Combination, Combinations),
   permutation(Combination, Permutation).


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
   

% Returns the positions of a espaco structure
positionsEspaco(espaco(_, Positions), Positions).


% Returns the value of a espaco structure
valueEspaco(espaco(Value, _), Value).


% Checks if list is sublist
isSublist([], _ ).
isSublist([X|XS], [X|XSS]) :- isSublist(XS, XSS).
isSublist([X|XS], [_|XSS]) :- isSublist([X|XS], XSS).


% Checks if element is in the list
isInList(List, El) :-
   member(X, List),
   El == X.


% Removes all occurrences of an element in a list
removeElement(_X, [], []).
removeElement(X, [X|Y], Z) :-
   removeElement(X, Y, Z).
removeElement(X, [F|Y], [F|Z]) :-
   dif(X, F),
   removeElement(X, Y, Z).


% Same thing as permutacoes_soma_espacos/2, but only for a espaco, rather than a list of espaco's
permutacoes_soma_espaco(Espaco, Perms_soma) :-
   valueEspaco(Espaco, Sum),
   positionsEspaco(Espaco, Positions),
   length(Positions, Length),
   permutacoes_soma(Length, [1, 2, 3, 4, 5, 6, 7, 8, 9], Sum, Perms),
   Perms_soma = [Espaco, Perms].


% Returns permutations from Perms_soma of a permutacoes_soma_espaco
permsPermsEspaco([_|Perms], Perms).


% Returns espaco from Perms_soma of a permutacoes_soma_espaco
espacoPermsEspaco([Espaco|_], Espaco).


% Returns variables of espaco from Perms_soma of a permutacoes_soma_espaco
variablesPermsEspaco([espaco(_, Var)|_], Var).