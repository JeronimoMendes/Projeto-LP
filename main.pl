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

% Returns combinations of N elements from a list Els that sum to Soma
combinacoes_soma(N, Els, Soma, Combs):-
	findall(Combs,combinacao(N,Els,Combs),X),
	include(list_sum_helper(Soma), X, Combs).


% Returns permutations of the combinations of N elements from a list Els that sum to Soma
permutacoes_soma(N, Els, Soma, Perms):-
   combinacoes_soma(N, Els, Soma, CombinacoesSoma),
   setof(Perm, permutationsOfCombination(CombinacoesSoma, Perm), Perms).


% Returns a list of espacos in a given file.
espacos_fila(H_V, Fila, Espacos) :- 
   getBlocks(Fila, Aux),
   maplist(substituteSpace, Fila, Aux2), % substitues free variables with "free"
   listOfIndices(Aux2, Aux,IndicesAux),
   sort(IndicesAux, Indices),
   getSpaces(Fila, Indices, Spaces) -> 
   exclude(nextIsBlock(Aux2), Aux, ImportantBlocks),
   (H_V == v -> maplist(getVerticalValue, ImportantBlocks, Values)
   ;
   maplist(getHorizontalValue, ImportantBlocks, Values) 
   ),
   %writeln(Values),
   %exclude(==(0), ValuesAux, Values), % has to be changed to pass test 6
   maplist(createSpaceStruct, Values, Spaces, Espacos);
   Espacos = [].
   

% Returns a espaco in a given File
espaco_fila(Fila, Esp, H_V) :-
   espacos_fila(H_V, Fila, Espacos),
   member(Esp, Espacos).


% Returns a list of espaco in a given puzzle
espacos_puzzle(Puzzle, Espacos) :-
   maplist(espacos_fila(h), Puzzle, EspacosH),
   mat_transposta(Puzzle, PuzzleTrans),
   maplist(espacos_fila(v), PuzzleTrans, EspacosV),
   append(EspacosH, EspacosV, EspacosAux),
   append(EspacosAux, Espacos).


% Returns espacos with positions in common
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
   positionsEspaco(Esp, PositionsEsp),
   bagof(Pos, X^PositionEsp^(member(PositionEsp, PositionsEsp) ,member(Pos, Espacos) ,positionsEspaco(Pos, X), isInList(X, PositionEsp)), Esps_comDups),
   removeElement(Esp, Esps_comDups, Esps_com).


% Returns list of permutations of espaco
permutacoes_soma_espacos(Espacos, Perms_soma) :-
   bagof(Perm, Perm^Espaco^(member(Espaco, Espacos), permutacoes_soma_espaco(Espaco, Perm)), Perms_soma).


% Returns possible permutation of a given espaco.
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

% Returns list of possible permutations of a given espacos.
permutacoes_possiveis_espaco(Espacos, _, Esp, Perms_poss) :-
   findall(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_poss), Perms),
   positionsEspaco(Esp, VarList),
   append([VarList], [Perms], Perms_poss).
   

% Returns a list of possible permutations for a given list of espacos
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
   bagof(Perms, Espaco^Perms^(member(Espaco, Espacos), permutacoes_possiveis_espaco(Espacos, _, Espaco, Perms)), Perms_poss_esps).


% Returns (index, number), number being a number that's on the same index through out a list of lists.
numeros_comuns(Lst_Perms, Numeros_comuns) :-
   mat_transposta(Lst_Perms, List), 
   findall(NumCom, (member(Sublist, List), nth1(Count, List, Sublist), numeros_comuns_aux(Sublist, NumCom, Count)), Numeros_comunsDups),
   list_to_set(Numeros_comunsDups, Numeros_comuns). % Remove any duplicates
   

% Applies numeros_comuns/2 to Espacos with permutations, returning espaco with variables unified 
atribui_comuns(Perms_Possiveis) :-
   maplist(unifyCommonNums, Perms_Possiveis).


% Filters impossible permutation from a given EspacoPerms, and returns a new one
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
   bagof([Espaco | [Perms]], possiveisEspPerm(Perms_Possiveis, [Espaco | [Perms]]), Novas_Perms_Possiveis).
   

% Filters a EspacoPerm until it no longer holds impossible permutations
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
   atribui_comuns(Perms_Possiveis),
   retira_impossiveis(Perms_Possiveis, Novas_Perms_PossiveisAux),
   (Novas_Perms_PossiveisAux == Perms_Possiveis -> Novas_Perms_Possiveis = Novas_Perms_PossiveisAux
      ;
      simplifica(Novas_Perms_PossiveisAux, Novas_Perms_Possiveis)
      ).


% It takes a puzzle and simplifies all it's EspacoPerms, removing impossible permutations
inicializa(Puzzle, Perms_Possiveis) :-
   espacos_puzzle(Puzzle, Espacos),
   permutacoes_possiveis_espacos(Espacos, Perms_PossiveisNotSimplified),
   simplifica(Perms_PossiveisNotSimplified, Perms_Possiveis).


% Takes an EspacoPerm from a list of possible permutation that has the lowest number of permutations
escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
   include(spaceWithVariables, Perms_Possiveis, Perms_PossiveisWithVars),
   smallestEspPerms(Perms_PossiveisWithVars, Escolha).


% Testado
experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
   permsEspaco(Escolha, Lst_Perms),
   espEspaco(Escolha, Esp),
   nth1(Index, Perms_Possiveis, Escolha),
   member(Perm, Lst_Perms),
   Esp = Perm,
   nth1(Index, Perms_Possiveis, Slot),
   select(Slot, Perms_Possiveis, [Esp, [Perm]], Novas_Perms_Possiveis).


resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
   length(Perms_Possiveis, LengthInitial),
   resolve_aux_recursive(Perms_Possiveis, Novas_Perms_Possiveis),
   length(Novas_Perms_Possiveis, Length),
   Length == LengthInitial.
   
    

resolve_aux_recursive(Perms_Possiveis, Novas_Perms_Possiveis) :-
   (escolhe_menos_alternativas(Perms_Possiveis, Escolha) -> experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_PossiveisAux),
   simplifica(Novas_Perms_PossiveisAux, Perms_PossiveisSimp), resolve_aux_recursive(Perms_PossiveisSimp, Novas_Perms_Possiveis)
   ;
   Novas_Perms_Possiveis = Perms_Possiveis   
   ).  


resolve(Puz) :-
   inicializa(Puz, Perms_Possiveis),
   resolve_aux(Perms_Possiveis, _).
      

%  ###################
%  AUXILIAR PREDICATES
%  ################### 


applySolution(Space, [Solution|_]) :-
   positionsEspaco(Space, Vars),
   Vars = Solution.


% True if the length of Perms is equal to Length
isLengthOfPerms(Length, Perms) :-
   forall(member([_|[Perm]], Perms), (length(Perm, LengthPerm), LengthPerm == Length)).


% Returns permutations from a EspacoPerms
permsEspaco([_|[Perms]], Perms).
espEspaco([Esp|_], Esp).


% Returns number of permutations of a EspacoPerms
lengthEspPerms([_|[Perms]], Length) :-
   length(Perms, Length).


% Returns the EspacoPerm with the lowest number of permutations
smallestEspPerms(List, Solution) :-
   maplist(lengthEspPerms, List, Lengths),
   min_member(Min, Lengths),
   nth1(Index, Lengths, Min),
   nth1(Index, List, Solution).


% True if the space has variables
spaceWithVariables([Espaco | _]) :- 
   include(var, Espaco, Result), 
   \+ (Result == []). 



possiveisEspPerm(Perms_Possiveis, Novas_Perms_Possiveis) :-
   member([Espaco | [Perms]], Perms_Possiveis),
   bagof(Perm, retira_impossiveis_EspPerm([Espaco | [Perms]], Perm), Perms_PossiveisAux),
   Novas_Perms_Possiveis = [Espaco, Perms_PossiveisAux].



retira_impossiveis_EspPerm([Espaco | [Perms]], Nova_Perm_Possivel) :-
   member(Perm, Perms),
   (subsumes_term(Espaco, Perm) -> Nova_Perm_Possivel = Perm % Perm eh possivel
      ;
      false% Perm nao eh possivel
   ).


% Unifies variables with common number
unifyCommonNums([Vars|[Perms]]) :-
   numeros_comuns(Perms, CommonNums),
   maplist(unifyCommonNum(Vars), CommonNums).

% Unifies a variable with a common number
unifyCommonNum(Vars, (Index, X)) :-
   nth1(Index, Vars, Var),
   Var = X.


% Gets common number from sublist
numeros_comuns_aux(Sublist, Output, Count) :-
   same(Sublist),
   nth1(1, Sublist, Element),
   Output = (Count, Element),!.


% Checks if the elements in a list are all the same
same([_]).
same([X,X|T]) :- same([X|T]).


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
   (last(El, List) -> true
   ;
   nextto(El, Y, AuxList), is_list(Y)).
   

% Checks last element of list
last(X,[X]).
 last(X,[_|Z]) :- last(X,Z).


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