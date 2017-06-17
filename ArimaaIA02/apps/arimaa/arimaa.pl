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
what_on([X, Y], [T|Q], Res) :- what_on([X, Y], Q, Res).

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
can_move(Pos, Board, Gamestate) :-
  has_adjacent_ally(Pos, Gamestate, Board).

can_move(Pos, Board, Gamestate) :-
  get_adjacentes(Pos, [U, R, D, L], Board),
  what_on(Pos, Board, P),
  \+blocking(U, P),
  \+blocking(R, P),
  \+blocking(D, P),
  \+blocking(L, P).

% Est ce que la pièce 1 bloque la 2 ?
  % param 1 : pièce à tester
  % param 2 : notre pièce
blocking([Piece1, Color1], [Piece2, Color2]) :- is_stronger([Piece1, Color1], [Piece2, Color2]), not(Color1 = Color2).

  what_on(Pos, Board, Piece),
  \+is_stronger(U, Piece),
  \+is_stronger(R, Piece),
  \+is_stronger(D, Piece),
  \+is_stronger(L, Piece).

has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [[_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, [_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, _, [_,Side]|_], Board).
has_adjacent_ally(Pos, [Side|_], Board) :-
  get_adjacentes(Pos, [_, _, _, [_,Side]], Board).

% can't move on an non-empty case
can_move_here(_, OtherPos, Board) :-
  is_not_empty(OtherPos, Board),
  fail, !.

% a Rabbit can't go back
can_move_here(Pos, OtherPos, Board) :-
  top(Pos, OtherPos),
  what_on(Pos, Board, [rabbit|silver]),
  fail, !.

can_move_here(_, _, _).

movement([X, Y], Board, [[X,Y], [TX, TY]]) :- can_move([X, Y], Board),   top([X,Y], [TX, TY]), is_empty([TX, TY], Board).
movement([X, Y], Board, [[X,Y], [RX, RY]]) :- can_move([X, Y], Board), right([X,Y], [RX, RY]), is_empty([RX, RY], Board).
movement([X, Y], Board, [[X,Y], [LX, LY]]) :- can_move([X, Y], Board),  left([X,Y], [LX, LY]), is_empty([LX, LY], Board).
movement([X, Y], Board, [[X,Y], [DX, DY]]) :- can_move([X, Y], Board),  down([X,Y], [DX, DY]), is_empty([DX, DY], Board).

get_allies([], _, []).
get_allies(Board, [[X, Y, P, S]|QB], Gamestate, [[X, Y]|Res]) :- is_ally([X, Y], Gamestate, Board), get_allies(Board, QB, Gamestate, Res).
get_allies(Board, [[X, Y, P, S]|QB], Gamestate, Res) :- \+is_ally([X, Y], Gamestate, Board), get_allies(Board, QB, Gamestate, Res).
