%%%-------------------------------------------------------------------
%%% File    : othello.erl
%%% Author  : Jose Castro <>
%%% Description : 
%%%
%%% Created : 10 Jul 2009 by Jose Castro <>
%%%-------------------------------------------------------------------
-module(othello).

%% Copyright (C) 2008, Jose Castro
%% File    : othello.erl
%% Author  : Jose Castro
%% Purpose : Othello
%% Usage   : display:start(). needs display.erl


-vsn(1).
-author('jose.r.castro@gmail.com').

-compile(export_all).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% Terminlogy
%
% Board  - tuple containing current position of board
% Border - tuple indicating with 1's valid board positions, 0's outside of board
% Line   - Sequence of pieces than correspond to a valid move
% Dir    - Offset from a given position that may indicate 
% Moves  - list of posible moves (offsets) from a given position to determine lines

% structure of tree info
% 
% {BoardValue, Player, [Move*]}
% 
% Move = { BoardPosition, {BoardValue, Player, [Move*]}}

% createTree(Board, Player, Level, Moves)
% indicador de tablero, los espacios en 0 son
% el borde ayuda a hacer la búsqueda que la búsqueda
% de movidas no necesite contemplar casos especiales

% Constants --------------------------
edge() -> 
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0}.

directions() -> [-11,-10,-9,-1,1,9,10,11].

board() ->
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 1,-1, 0, 0, 0, 0,
     0, 0, 0, 0,-1, 1, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0}.

empty() ->
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0}.    

border() -> [34,35,36,37,44,47,54,57,64,65,66,67].


% Main new_frontier function
new_frontier(Frontier, Pos, Board) ->
    new_frontier(subtract(Pos, Frontier), lists:map(fun(X) -> X + Pos end, directions()), Board, edge()).

% Auxiliary new_frontier function
new_frontier(Border, [], _, _) -> Border;
new_frontier(Border, [Pos|Positions], Board, Edge) ->
    case not(lists:member(Pos, Border)) and (element(Pos, Board) == 0) and (element(Pos, Edge) == 1) of
      true  -> new_frontier([Pos | Border], Positions, Board, Edge);
      false -> new_frontier(       Border,  Positions, Board, Edge)
    end.

subtract(Pos, [Pos | Rest]) -> Rest;
subtract(Pos, [H   | Rest]) -> [H | subtract(Pos, Rest)];
subtract(_, []) -> [].

% check_move checks whether a given Pos is a valid move for Player = 1|-1
check_move(Pos, Board, [Dir|Directions], Player) ->
    case is_move(Pos, Board, Dir, Player) of 
       true  -> true;
       false -> check_move(Pos, Board, Directions, Player)
    end;
check_move(_, _, [], _) -> false.

% makeMove makes a move at Pos for the Player
make_move(Pos, Board, [Dir|Directions], Player) ->
    case is_move(Pos, Board, Dir, Player) of
       true  -> make_move(Pos, 
                   new_board(Pos+Dir, Dir, setelement(Pos, Board, Player), Player), 
                   Directions, Player);
       false -> make_move(Pos, Board, Directions, Player)
    end;

make_move(_, Board, [], _) -> Board.

new_board(Pos, Dir, Board, Player) ->
    case element(Pos, Board) of
        Player -> Board;
        _      -> new_board(Pos+Dir, Dir, setelement(Pos, Board, Player), Player)
    end.

% first entry of move line has to be of the oponents color
is_move(Pos, Board, Direction, Player) ->
   case element(Pos+Direction, Board) of
        0      -> false;
        Player -> false;
        _      -> check_line(Pos+Direction+Direction, Board, Direction, Player)
   end.

% line is made up of the oponents pieces and eventualy has to end with a piece from Player
check_line(Pos, Board, Direction, Player) ->
   case element(Pos, Board) of
         0      -> false; % ends with nothing, cannot play here
         Player -> true; % ends with my piece, good, it's a play line
         _      -> check_line(Pos+Direction, Board, Direction, Player) % its an oponents piece
   end.


%% debug printing functions

print_board([_,A,B,C,D,E,F,G,H,_|Rest], Pos, Frontier)->
     print_val(A, Pos+1, Frontier), print_val(B, Pos+2, Frontier), 
     print_val(C, Pos+3, Frontier), print_val(D, Pos+4, Frontier),
     print_val(E, Pos+5, Frontier), print_val(F, Pos+6, Frontier), 
     print_val(G, Pos+7, Frontier), print_val(H, Pos+8, Frontier),
     io:format("~n", []),
     print_board(Rest, Pos+10, Frontier);
print_board([], _, _) -> true.

print_val(0, Pos, Frontier)  -> 
   case lists:member(Pos, Frontier) of
       true  -> io:format(":", []);
       false -> io:format(".", [])
   end;
print_val( 1, _, _) -> io:format("X", []);
print_val(-1, _, _) -> io:format("O", []).



