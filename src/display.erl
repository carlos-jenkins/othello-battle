%%%-------------------------------------------------------------------
%%% File    : display.erl
%%% Author  : Jose Castro <>
%%% Description :
%%%
%%% Created :  9 Jul 2009 by Jose Castro <>
%%%-------------------------------------------------------------------
%% Copyright (C) 2008, Jose Castro
%% File    : display.erl
%% Author  : Jose Castro
%% Usage   : Othello display utility functions

-module(display).
-vsn(1).
-author('jose.r.castro@gmail.com').

%% API
-export([pos/2, change/3, draw_lines/1, print_board/1]).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

pos(A,B) ->
    C = A div 25,
    F = B div 25,
    (F + 1) * 10 + C + 2.

change(Canvas, Board, NewBoard) ->
    foreach(fun(Y) ->
            foreach(fun(X) ->
                    Pos = Y * 10 + X + 1,
                    change(Canvas, X, Y, element(Pos, Board), element(Pos, NewBoard))
                end, seq(1, 8))
        end, seq(1, 8)).

seq(1, 8) -> [1, 2, 3, 4, 5, 6, 7, 8].

foreach(_F, []) -> ok;
foreach( F, [Head|Tail]) ->
    F(Head),
    foreach(F, Tail).

change(     _, _, _, X, X) -> true;
change(Canvas, X, Y, 0, 1) -> draw_circle(Canvas, X, Y, white);
change(Canvas, X, Y, 0,-1) -> draw_circle(Canvas, X, Y, black);
change(     _, X, Y,-1, 1) -> gs:config(get({X, Y}), [{fill, white}]);
change(     _, X, Y, 1,-1) -> gs:config(get({X, Y}), [{fill, black}]);
change(     _, X, Y,-1, 0) -> gs:destroy(get({X, Y}));
change(     _, X, Y, 1, 0) -> gs:destroy(get({X, Y})).

draw_lines(Canvas) -> draw_lines(Canvas, 25, 25, 225).

draw_lines(_, Pos, _, Pos) -> true;
draw_lines(Canvas, Pos, Increment, Last) ->
    gs:create(line, Canvas, [{coords, [{0, Pos}, {200, Pos}]}, {width, 1}]),
    gs:create(line, Canvas, [{coords, [{Pos, 0}, {Pos, 200}]}, {width, 1}]),
    draw_lines(Canvas, Pos + Increment, Increment, Last).

draw_circle(Canvas, X, Y, Color) ->
    XX = (X - 1) * 25 + 2,
    YY = (Y - 1) * 25 + 2,
    put({X,Y}, gs:create(oval, Canvas, [{coords, [{XX, YY}, {XX + 21, YY + 21}]}, {fill, Color}])).

print_board([]) -> io:format("\n");
print_board([A, B, C, D, E, F, G, H, I, J|Rest]) ->
    io:format("~w ~w ~w ~w ~w ~w ~w ~w ~w ~w\n", [A, B, C, D, E, F, G, H, I, J]),
    print_board(Rest).
