:- module(bot,
      [  get_moves/3
      ]).

% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ])

board([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[2,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[2,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% Retourne la piece à la position [X, Y], tableau vide si rien
  % param1 : position à chercher de la forme [X, Y]
  % param2 : le Board
  % param3 : le résultat de la forme [Pièce, Couleur]
what_on(Pos, Board, Res) :- coord_exist(Pos), !, f_what_on(Pos, Board, Res).
what_on(_, _, []).
f_what_on(_, [], []).
f_what_on([X, Y], [[X, Y, Piece, Color]|_], [Piece, Color]) :- !.
f_what_on([X, Y], [_|Q], Res) :- f_what_on([X, Y], Q, Res).

% Retourne les pièces adjacentes à la piece argument
  % param1 : Position dont on veut connaitre les adjecents
  % param2 : Pièces adjacentes sour la forme [Pièce, Couleur], dans l'odre Haut, Droite, Bas, Gauche
get_adjacentes([X, Y], [U, R, D, L], Board) :-
  XL is X - 1, what_on([XL, Y],Board,U),
  YD is Y + 1, what_on([X, YD],Board,R),
  XR is X + 1, what_on([XR, Y],Board,D),
  YU is Y - 1, what_on([X, YU],Board,L).

% En fonction du gamestate, vérifie si une pièce est alliée ou non
  % param1 : La position de la piece à tester
  % param2 : Le Gamestate
  % param3 : Le Board
is_ally([X, Y] , [Side|_], Board) :- what_on([X,Y], Board, [_,Side]).

% En fonction du gamestate, vérifie si une pièce est alliée ou non
  % param1 : La position de la piece à tester
  % param2 : Le Gamestate
  % param3 : Le Board
is_enemy([X, Y] , [Side|_], Board) :- what_on([X,Y], Board, [_,Color]), Side \= Color.

% Associe un numéro à un animal en fonction de sa find_best_board_recursively
  % param1 : animal dont on veut connaitre la force
  % param2 : force de l'animal
associate_animal_num(elephant, 7).
associate_animal_num(camel, 6).
associate_animal_num(horse, 5).
associate_animal_num(dog, 4).
associate_animal_num(cat, 3).
associate_animal_num(rabbit, 2).
associate_animal_num([], 0).

% Vérifie si une piece (param1) est plus forte qu'une autre (param2)
  % param1 : Piece à tester de la forme [Pièce, Couleur]
  % param2 : autre pièce à tester
is_stronger(_, []).
is_stronger([A1,_], [A2,_]) :- associate_animal_num(A1, N1), associate_animal_num(A2, N2), N1 > N2.

% Vérifie si la position argument n'est pas vide
  % param1 : Position à tester
  % param2 : le Board
is_not_empty(Pos, Board) :- what_on(Pos, Board, [_, _]).

% Vérifie si la position argument est vide
  % param1 : Position à tester
  % param2 : le Board
is_empty(Pos, Board) :- \+is_not_empty(Pos, Board).

% Vérifie si la position argument est valide
  % param1 : Position à tester
coord_exist([X, Y]) :- X =< 7, X >= 0, Y =< 7, Y >= 0.

% Retourne la position left, right, top ou down d'une position particulière, retourne false quand la position est invalide
  % param1 : Position de base
  % param2 : Position haute, droite, gauche ou basse
left( [X,Y], [X, TY]) :- TY is Y - 1, coord_exist([X, TY]).
right([X,Y], [X, DY]) :- DY is Y + 1, coord_exist([X, DY]).
top(  [X,Y], [LX, Y]) :- LX is X - 1, coord_exist([LX, Y]).
down( [X,Y], [RX, Y]) :- RX is X + 1, coord_exist([RX, Y]).

% Vraie si la position est un piège
  % param1 : position à tester
is_trap([2,2]).
is_trap([5,2]).
is_trap([2,5]).
is_trap([5,5]).

% TODO : find other implementation
commit_suicide(Pos, Gamestate, Board) :-
  is_trap(Pos),
  \+has_adjacent_ally(Pos, Gamestate, Board).


% Vérifie si une pièce peut bouger dans au moins une direction
  % param1 : Position de la pièce à tester
  % param2 : le Board
  % param3 : le Gamestate
  % param4 : la liste d'adjecence
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

% Une piece peut bouger à cette position ? NB : pas de vérification de force entre les pieces adjacentes
  % param1 : Position de la piece à tester
  % param2 : Position voulue
  % param3 : le Board
can_move_here(Pos, OtherPos, Board) :-
is_empty(OtherPos, Board),
  \+rabbit_who_go_back(Pos, OtherPos, Board).

% Vrai quand un lapin veut retourner en arrière
  % param1 : Position du lapin à tester
  % param2 : Position voulue à vérifier
  % param3 : le Board
rabbit_who_go_back(Pos, OtherPos, Board) :-
  top(Pos, OtherPos),
  what_on(Pos, Board, [rabbit,silver]).
rabbit_who_go_back(Pos, OtherPos, Board) :-
  down(Pos, OtherPos),
  what_on(Pos, Board, [rabbit,gold]).

% Créer un nouveau mouvement possible à partir du board pour une position donnéd
  % param1 : Position à bouger
  % param2 : le Board
  % param3 : le Gamestate
  % param4 : le mouvement resultat
movement(Pos, Board, Gamestate, Mov) :- is_not_empty(Pos, Board), get_adjacentes(Pos, Adj, Board), can_move(Pos, Board, Gamestate, Adj), !, movement(Pos, Board, Gamestate, Mov, Adj).
%movement(Pos, _, _, [Pos, Pos], _).
movement([X, Y], Board, Gamestate, [[X,Y], [TX, TY]], Adj) :-   top([X,Y], [TX, TY]), can_move_here([X, Y], [TX, TY], Board).
movement([X, Y], Board, Gamestate, [[X,Y], [RX, RY]], Adj) :- right([X,Y], [RX, RY]), can_move_here([X, Y], [RX, RY], Board).
movement([X, Y], Board, Gamestate, [[X,Y], [LX, LY]], Adj) :-  left([X,Y], [LX, LY]), can_move_here([X, Y], [LX, LY], Board).
movement([X, Y], Board, Gamestate, [[X,Y], [DX, DY]], Adj) :-  down([X,Y], [DX, DY]), can_move_here([X, Y], [DX, DY], Board).

%
get_allies(_, [], _, []).
get_allies(Board, [[X, Y, P, S]|QB], Gamestate, [[X, Y, P, S]|Res]) :- is_ally([X, Y], Gamestate, Board), get_allies(Board, QB, Gamestate, Res).
get_allies(Board, [[X, Y, P, S]|QB], Gamestate, Res) :- \+is_ally([X, Y], Gamestate, Board), get_allies(Board, QB, Gamestate, Res).

% Créer un nouvel etat du jeu possible à partir de celui de base avec 4 mouvements
  % param1 : Board
  % param2 : Gamestate
  % param3 : le Board resultat
  % param4 : les mouvements necessaire pour arriver au board resultat
get_state(4, Board, [Side|_], [NewBoard, [Mvt1, Mvt2, Mvt3, Mvt4]]) :-
  get_piece_side(Side, Board, [X1, Y1, _, _]),
  movement([X1, Y1], Board, [Side|_], Mvt1),
  apply_movement(Board, Mvt1, Board1),

  get_piece_side(Side, Board1, [X2, Y2, _, _]),
  movement([X2, Y2], Board1, [Side|_], Mvt2),
  apply_movement(Board1, Mvt2, Board2),

  get_piece_side(Side, Board2, [X3, Y3, _, _]),
  movement([X3, Y3], Board2, [Side|_], Mvt3),
  apply_movement(Board2, Mvt3, Board3),

  get_piece_side(Side, Board3, [X4, Y4, _, _]),
  movement([X4, Y4], Board3, [Side|_], Mvt4),
  apply_movement(Board3, Mvt4, NewBoard).

% Créer un nouvel etat du jeu possible à partir de celui de base avec 1 seul mouvement
  % param1 : Board
  % param2 : Gamestate
  % param3 : le Board resultat
  % param4 : les mouvements necessaire pour arriver au board resultat
get_state(1, Board, [Side|_], [NewBoard, [Mvt1]]) :-
  get_piece_side(Side, Board, [X1, Y1, _, _]),
  movement([X1, Y1], Board, [Side|_], Mvt1),
  apply_movement(Board, Mvt1, Board1),
  remove_suicide(Board1, NewBoard).

get_state(2, Board, [Side|_], [NewBoard, [Mvt1, Mvt2]]) :-
  get_piece_side(Side, Board, [X1, Y1, _, _]),
  movement([X1, Y1], Board, [Side|_], Mvt1),
  apply_movement(Board, Mvt1, Board1),
  remove_suicide(Board1, Board11),

  get_piece_side(Side, Board11, [X2, Y2, _, _]),
  movement([X2, Y2], Board11, [Side|_], Mvt2),
  apply_movement(Board11, Mvt2, Board2),
  remove_suicide(Board2, NewBoard).

get_state(3, Board, [Side|_], [NewBoard, [Mvt1, Mvt2, Mvt3]]) :-
  get_piece_side(Side, Board, [X1, Y1, _, _]),
  movement([X1, Y1], Board, [Side|_], Mvt1),
  apply_movement(Board, Mvt1, Board1),
  remove_suicide(Board1, Board11),

  get_piece_side(Side, Board11, [X2, Y2, _, _]),
  movement([X2, Y2], Board11, [Side|_], Mvt2),
  apply_movement(Board11, Mvt2, Board2),
  remove_suicide(Board2, Board22),

  get_piece_side(Side, Board22, [X3, Y3, _, _]),
  movement([X3, Y3], Board22, [Side|_], Mvt3),
  apply_movement(Board22, Mvt3, Board3),
  remove_suicide(Board3, NewBoard).

remove_suicide(Board, NewBoard) :- remove_suicide(Board, Board, NewBoard).
remove_suicide(B, [], B).
remove_suicide(Board, [[X, Y, Piece, Color]|Q], NewBoard) :-
  commit_suicide([X, Y], [Color|_], Board), !,
  delete(Board, [X, Y, Piece, Color], NewBoard).

remove_suicide(Board, [[X, Y, Piece, Color]|Q], NewBoard) :- remove_suicide(Board, Q, NewBoard).

% Applique un mouvement à une Board
  % param1 : board de départ
  % param3 : mouvement
  % param2 : board d'arrivé
apply_movement([], _, []).
apply_movement([[X, Y, Piece, Color]|Q], [[X, Y], [NX, NY]], [[NX, NY, Piece, Color]|NewQ]) :-
  apply_movement(Q, [[X, Y], [NX, NY]], NewQ), !.
apply_movement([T|Q], Mvt, [T|NewQ]) :-
  apply_movement(Q, Mvt, NewQ).

% Retourne une pièce dont correspondant au side argument
  % param1 : le Side voulu
  % param3 : le Board
  % param2 : la pièce de retour, de la forme [X, Y, Pièce, Side]
get_piece_side(Side, [[X, Y, Piece, Side]|_], [X, Y, Piece, Side]).
get_piece_side(Side, [_|Q], Piece) :- get_piece_side(Side, Q, Piece).

% ---------------------------
%    Notation des plateaux
% ---------------------------

% Renvoie le meilleur plateau entre les deux proposés
% Param 1 : Plateau 1
% Param 2 : Score du plateau 1
% Param 3 : Plateau 2
% Param 4 : meilleure des deux plateaux
% Param 5 : Score du meilleur plateau
best_board(_, N1, B2, B2, N2, Gamestate) :- note_board(B2, Gamestate, N2), N2 > N1, !.
best_board(B1, N1, _, B1, N1, Gamestate).

% Renvoie une note pour le plateau
  % param1 : un couple [Board, Mouvements]
  % param2 : le Gamestate
  % param3 : la note résultat
note_board([Board, _], Gamestate, Res) :- get_allies(Board, Board, Gamestate, Allies), note_pieces_recursively(Board, Gamestate, Allies, Res).

% Note les pièces une à une et fait la somme de leurs notes
  % param1 : le Board
  % param
note_pieces_recursively(_, _, [], 0).
note_pieces_recursively(Board, Gamestate, [[X, Y, P, S] | Q], Res) :-
	note_piece(Board, Gamestate, [X, Y, P, S], Res1),
	note_pieces_recursively(Board, Gamestate, Q, Res2),
	Res is Res1 + Res2.

% Vrai si la position argument à des pieces alliés adjecentes
% TODO : utiliser top, left, right et down
  % param1 : la positon à tester
  % param2 : le Gamestate
  % param3 : le Board
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [[_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, [_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, _, [_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, _, _, [_,Side]], Board).

% Note une pièce du plateau
  % param 1 : Board
  % param 2 : Gamestate
  % param 3 : pièce (posX, posY, type, couleur)
  % param 4 : valeur de retour
  % Format : note_piece(Board, [X, Y, P, S], Res).
  % Un lapin dans le camp adverse = note énorme, le coup doit être joué
note_piece(_, _, [7, _, rabbit, _], 10000) :- !.
% Une pièce va sur un piège = on retire des points en fonction de l'importance de la pièce (lapin = 200, elephant = 700)
note_piece(Board, _, [X, Y, P, S], Res) :- is_trap([X, Y]), \+has_adjacent_ally([X, Y], [S], Board), associate_animal_num(P, N), Res is N * -300, !.
% PLus un lapin peut se rapprocher du fond au prochain coup, plus il vaut de points
note_piece(Board, _, [X, Y, rabbit, _], Res) :- X1 is X + 1, can_move_here([X, Y], [X1, Y], Board), Res is X * 100, !.
% Bloquer notre pièce retire des points
note_piece(Board, Gamestate, [X, Y, _, _], -100) :- get_adjacentes([X, Y], Adj, Board), \+can_move([X, Y], Board, Gamestate, Adj), !.
% Faire avancer une piece rapporte 50
note_piece(Board, _, [X, Y, _, _], Res) :- X1 is X + 1, can_move_here([X, Y], [X1, Y], Board), Res is X * 30, !.
% Se placer à coté d'une pièce adverse plus faible fait gagner des points
note_piece(Board, [Side|_], [X, Y, Piece, Side], Res) :- get_adjacentes([X, Y], Adj, Board), blocking_enemy_adj([Side|_], [Piece, Side], Adj), Res is (7-X) * (7-X) * 10.
% Si une pièce n'a pas été notée avant, elle renvoie 0 (indispensable pour eviter les plantages)
note_piece(_, _, _, 0).

% Vrai si la piece bloque un enemy adjecent
  % param1 : Le Gamestate
  % param2 : le nom de la piece à tester
  % param3 : la liste d'adjecence de la piece
blocking_enemy_adj([Side|_], Piece, [[P, C], _, _, _]) :- Side \= C, is_stronger(Piece, P).
blocking_enemy_adj([Side|_], Piece, [_, [P, C], _, _]) :- Side \= C, is_stronger(Piece, P).
blocking_enemy_adj([Side|_], Piece, [_, _, [P, C], _]) :- Side \= C, is_stronger(Piece, P).
blocking_enemy_adj([Side|_], Piece, [_, _, _, [P, C]]) :- Side \= C, is_stronger(Piece, P).

% --------------------------------------------------------------------------------------------------------
%    Génération des mouvements par Anthony (non fonctionnel, probablement travail en double avec Corentin)
% --------------------------------------------------------------------------------------------------------

find_best_board_recursively(Gamestate, [], _, -10000).

find_best_board_recursively(Gamestate, [Solution|Q], BestSolution, BestNote) :-
  note_board(Solution, Gamestate, Note), !,
  find_best_board_recursively(Gamestate, Q, NewSolution, NewNote),
  best_board(NewSolution, NewNote, Solution, BestSolution, BestNote, Gamestate).

% Trouve le meilleur plateau en fonction des notes attribué à chaque mouvements
  % param1 : le nombre de mouvements à évaluer,
    % 1 -> Evalue le meilleur plateau pour chaque mouvement
    % 2 -> Evalue le meilleur plateau 2 mouvements par 2
    % 3 -> Evalue le meilleur plateau pour 3 mouvements puis 1
    % 4 -> Evalue le meilleur plateau pour les 4 mouvemnts => stack overflow
  % param2 : le Gamestate
  % param3 : le Board
  % param4 : le meilleur board trouvé
  % param5 : les mouvements
  % param6 : le note resultat du meilleur board
find_best_board(1, Gamestate, Board, BestBoard, [Mvt1, Mvt2, Mvt3, Mvt4], Res) :-
  f_find_best_board(1, Gamestate, Board, Board2, [Mvt1], _),
  f_find_best_board(1, Gamestate, Board2, Board3, [Mvt2], _),
  f_find_best_board(1, Gamestate, Board3, Board4, [Mvt3], _),
  f_find_best_board(1, Gamestate, Board4, BestBoard, [Mvt4], Res).

find_best_board(2, Gamestate, Board, BestBoard, [Mvt1, Mvt2, Mvt3, Mvt4], Res) :-
  f_find_best_board(2, Gamestate, Board, Board2, [Mvt1, Mvt2], _),
  f_find_best_board(2, Gamestate, Board2, BestBoard, [Mvt3, Mvt4], Res).

find_best_board(3, Gamestate, Board, BestBoard, [Mvt1, Mvt2, Mvt3, Mvt4], Res) :-
  f_find_best_board(3, Gamestate, Board, Board2, [Mvt1, Mvt2, Mvt3], _),
  f_find_best_board(1, Gamestate, Board2, BestBoard, [Mvt4], Res).

find_best_board(4, Gamestate, Board, BestBoard, Mvts, Res) :-
  f_find_best_board(4, Gamestate, Board, BestBoard, Mvts, Res).


f_find_best_board(NB, Gamestate, Board, NewBoard, Mvts, Res) :-
  setof(Solution, get_state(NB, Board, [silver|_], Solution), Solutions),
  find_best_board_recursively(Gamestate, Solutions, [NewBoard, Mvts], Res).


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

get_moves(Mvts, Gamestate, Board) :-
  find_best_board(2, Gamestate, Board, _, Mvts, _).
