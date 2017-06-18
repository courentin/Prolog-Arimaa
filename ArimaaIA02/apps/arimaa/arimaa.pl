:- module(bot,
      [  get_moves/3
      ]).

% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ])
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call
get_moves([[[1,0],[4,0]],[[0,0],[1,0]],[[0,1],[0,0]],[[0,0],[0,1]]], Gamestate, Board).

board([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[2,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% Retourne la piece à la position [X, Y]
% tableau vide si rien
% what_on([X, Y], Board, Résultat)

what_on(_, [], []).
what_on([X, Y], [[X, Y, Piece, Color]|_], [Piece, Color]) :- !.
what_on([X, Y], [_|Q], Res) :- what_on([X, Y], Q, Res).



% troisième arg : pièces adjacentes dans l'ordre H - D - B - G
%get_adjacentes(_, _, [_, _, _, _]).
get_adjacentes([X, Y], [U, R, D, L], Board) :-
  XL is X - 1, what_on([XL, Y],Board,U),
  YD is Y + 1, what_on([X, YD],Board,R),
  XR is X + 1, what_on([XR, Y],Board,D),
  YU is Y - 1, what_on([X, YU],Board,L).

% is_enemy([X, Y], Gamestate, Board)
is_ally([X, Y] , [Side|_], Board) :- what_on([X,Y], Board, [_,Side]).

is_enemy([X, Y] , [Side|_], Board) :- what_on([X,Y], Board, [_,Color]), Side \= Color.

associate_animal_num(elephant, 7).
associate_animal_num(camel, 6).
associate_animal_num(horse, 5).
associate_animal_num(dog, 4).
associate_animal_num(cat, 3).
associate_animal_num(rabbit, 2).
associate_animal_num([], 0).

is_stronger(_, []).
is_stronger([A1,_], [A2,_]) :- associate_animal_num(A1, N1), associate_animal_num(A2, N2), N1 > N2.

is_not_empty(Pos, Board) :- what_on(Pos, Board, [_, _]).
is_empty(Pos, Board) :- \+is_not_empty(Pos, Board).

coord_exist([X, Y]) :- X =< 7, X >= 0, Y =< 7, Y >= 0.

left([X,Y], [X, TY])   :- TY is Y - 1, coord_exist([X, TY]).
right([X,Y], [X, DY])  :- DY is Y + 1, coord_exist([X, DY]).
top([X,Y], [LX, Y])  :- LX is X - 1, coord_exist([LX, Y]).
down([X,Y], [RX, Y]) :- RX is X + 1, coord_exist([RX, Y]).

is_trap([2,2]).
is_trap([5,2]).
is_trap([2,5]).
is_trap([5,5]).

% Attention, il faudra peut être rajouter un is_ally dans ce predicat, si on n'est pas sur de tester uniquement sur des alliés
% can move if it has an ally around
can_move(Pos, Board, [Side|_], [[_,Side]|_]).
can_move(Pos, Board, [Side|_], [_, [_,Side]|_]).
can_move(Pos, Board, [Side|_], [_, _, [_,Side]|_]).
can_move(Pos, Board, [Side|_], [_, _, _, [_,Side]]).

can_move(Pos, Board, _, [U, R, D, L]) :-
  what_on(Pos, Board, P),
  \+blocking(U, P),
  \+blocking(R, P),
  \+blocking(D, P),
  \+blocking(L, P).

% Est ce que la pièce 1 bloque la 2 ?
  % param 1 : pièce à tester
  % param 2 : notre pièce
blocking([Piece1, Color1], [Piece2, Color2]) :- is_stronger([Piece1, Color1], [Piece2, Color2]), not(Color1 = Color2).

has_adjacent_ally(Pos, [Side|_], Board, ) :-
  get_adjacentes(Pos, [[_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, [_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, _, [_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, _, _, [_,Side]], Board).

% Une piece peut bouger à cette position ?
% Pas de vérification de force entre les pieces adjacentes
can_move_here(Pos, OtherPos, Board) :-
is_empty(OtherPos, Board),
  \+rabbit_who_go_back(Pos, OtherPos, Board).

% Yes when a rabbit try to going back
rabbit_who_go_back(Pos, OtherPos, Board) :-
  top(Pos, OtherPos),
  what_on(Pos, Board, [rabbit,silver]).
rabbit_who_go_back(Pos, OtherPos, Board) :-
  down(Pos, OtherPos),
  what_on(Pos, Board, [rabbit,gold]).

movement(Pos, Board, Gamestate, Mov) :- is_not_empty(Pos, Board), get_adjacentes(Pos, Adj, Board), movement(Pos, Board, Gamestate, Mov, Adj).
movement([X, Y], Board, Gamestate, [[X,Y], [TX, TY]], Adj) :- can_move([X, Y], Board, Gamestate, Adj),   top([X,Y], [TX, TY]), can_move_here([X, Y], [TX, TY], Board).
movement([X, Y], Board, Gamestate, [[X,Y], [RX, RY]], Adj) :- can_move([X, Y], Board, Gamestate, Adj), right([X,Y], [RX, RY]), can_move_here([X, Y], [RX, RY], Board).
movement([X, Y], Board, Gamestate, [[X,Y], [LX, LY]], Adj) :- can_move([X, Y], Board, Gamestate, Adj),  left([X,Y], [LX, LY]), can_move_here([X, Y], [LX, LY], Board).
movement([X, Y], Board, Gamestate, [[X,Y], [DX, DY]], Adj) :- can_move([X, Y], Board, Gamestate, Adj),  down([X,Y], [DX, DY]), can_move_here([X, Y], [DX, DY], Board).

get_allies(_, [], _, []).
get_allies(Board, [[X, Y, P, S]|QB], Gamestate, [[X, Y, P, S]|Res]) :- is_ally([X, Y], Gamestate, Board), get_allies(Board, QB, Gamestate, Res).
get_allies(Board, [[X, Y, P, S]|QB], Gamestate, Res) :- \+is_ally([X, Y], Gamestate, Board), get_allies(Board, QB, Gamestate, Res).



	
% ---------------------------
%    Notation des plateaux
% ---------------------------

% Renvoie le meilleur plateau entre les deux proposés
% Param 1 : Plateau 1
% Param 2 : Score du plateau 1
% Param 3 : Plateau 2
% Param 4 : meilleure des deux plateaux
% Param 5 : Score du meilleur plateau
best_board(_, N1, B2, B2, N2) :- note_board(B2, N2), N2 > N1, !.
best_board(B1, N1, _, B1, N1).

% Renvoie une note pour le plateau
note_board(Board, Gamestate, Res) :- get_allies(Board, Board, Gamestate, Allies), note_pieces_recursively(Board, Gamestate, Allies, Res).

% Note les pièces une à une et fait la somme de leurs notes
note_pieces_recursively(_, _, [], 0).
note_pieces_recursively(Board, Gamestate, [[X, Y, P, S] | Q], Res) :- 
	note_piece(Board, Gamestate, [X, Y, P, S], Res1), 
	note_pieces_recursively(Board, Gamestate, Q, Res2),
	Res is Res1 + Res2,
	print(Res),nl.

	
%Note une pièce du plateau
% param 1 : Board
% param 2 : Gamestate
% param 3 : pièce (posX, posY, type, couleur)
% param 4 : valeur de retour
% Format : note_piece(Board, [X, Y, P, S], Res).
% Un lapin dans le camp adverse = note énorme, le coup doit être joué
note_piece(_, _, [7, _, rabbit, _], 10000) :- !.
% Une pièce va sur un piège = on retire des points en fonction de l'importance de la pièce (lapin = 200, elephant = 700)
note_piece(Board, _, [X, Y, P, S], Res) :- is_trap([X, Y]), \+has_adjacent_ally([X, Y], [S], Board), associate_animal_num(P, N), Res is N * -100, !.
% PLus un lapin peut se rapprocher du fond au prochain coup, plus il vaut de points
note_piece(Board, _, [X, Y, rabbit, _], Res) :- X1 is X + 1, can_move_here([X, Y], [X1, Y], Board), Res is X * 100, !.
% Bloquer notre pièce retire des points
note_piece(Board, Gamestate, [X, Y, _, _], -100) :- get_adjacentes([X, Y], Adj, Board), \+can_move([X, Y], Board, Gamestate, Adj), !.

% Se placer à coté d'une pièce adverse plus faible fait gagner des points

% Si une pièce n'a pas été notée avant, elle renvoie 0 (indispensable pour eviter les plantages)
note_piece(_, _, _, 0).

% --------------------------------------------------------------------------------------------------------
%    Génération des mouvements par Anthony (non fonctionnel, probablement travail en double avec Corentin)
% --------------------------------------------------------------------------------------------------------

find_best_board() :- generate_movements(Board, Gamestate, Movements), find_best_board_recursively(Movements, Board, [], -10000).


% Trouve le meilleur plateau en parcourant la liste des mouvements possibles
% Param 1 : Liste des mouvements possibles
% Param 2 : Plateau d'origine (Board)
% Param 3 : Meilleur plateau
% Param 4 : Score du meilleur plateau
%find_best_board_recursively() :- 



generate_movements(Board, Gamestate, Res) :- 
	get_allies(Board, Board, Gamestate, Allies), 
	generate_piece_by_piece_movements(Allies, Board, Gamestate).


% génère les mouvements possible pièce par pièce
% Param 1 : liste des pièces alliées
% Param 2 : Board
% Param 3 : Retour (liste de mouvements)
generate_piece_by_piece_movements([], _, []).
generate_piece_by_piece_movements([[X, Y, P, S] | Q], Board, Ret) :- 
	movements([X, Y], Board, Res1), 
	generate_piece_by_piece_movements(Q, Board, Res2), 
	append(Res1, Res2, Ret).

