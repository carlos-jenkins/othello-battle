-module(player).
-compile(export_all).

-import(server, []).

-include("othello.hrl").

-define(WAIT, 500).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Map time to depth
depth(Time) when Time == 0 -> 7;
depth(Time) when Time >= 1, Time < 3 -> 3;
depth(Time) when Time >= 3, Time < 120 -> 5;
depth(Time) when Time >= 120 -> 7.

% Select the value of each player
color(white) ->  1;
color(black) -> -1.

% Change of player
swap(white) -> black;
swap(black) -> white;
swap( 1) -> -1;
swap(-1) ->  1.

%% Wait function
wait(Mill) ->
    receive
    after Mill -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PLAYER PROCESS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Player's start
start(Color) ->
    spawn(player, connect, [Color]).


%% Player's connect
connect(Color) ->
    if
    Color == white; Color == black ->
        oserver ! {connect, self(), Color},
        receive
            {ok, State} -> State
        end,
        Current = State#game.current,
        case Current of
            Color ->
                self() ! {your_turn, State},
                wait_turn(Color);
            _-> wait_turn(Color)
        end;
    true ->
        io:format("[Player] Unknown player, please select 'white' or 'black'.~n")
    end.

wait_turn(Color) ->
    receive
            {your_turn, State} -> State
        end,
        Current = State#game.current,
        case Current of
            Color ->
                io:format("[Player] My turn.~n"),
                Board = State#game.board,
                Border = State#game.border,
                Depth = depth(State#game.seconds),
                {Play, _} = alpha_beta(Depth, Border, Board, -110000, 110000, Color),
                if
                    Play == -1 ->
                        White = pieces(white,Board),
                        Black = pieces(black,Board),
                        io:format("Blancas: ~w~n", [White]),
                        io:format("Negras: ~w~n", [Black]);
                    true ->
                        LetsSee= othello:check_move(Play, Board, othello:directions(), color(Color)),
                        if
                            LetsSee->
                                oserver ! {move, self(), {Color, Play}};
                            true ->
                                io:format("Dead Lock~n"),
                                oserver ! {change_player}
                        end
                end,
                %io:format("~w~n", [Play]),
                wait_turn(Color);
            _-> wait_turn(Color)
        end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Minimax with Alpha Beta Prunning
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alpha_beta(0, _, Board, _, _, Color) ->
    {-1, evaluate(Color, Board)};

alpha_beta(Depth, Border, Board, Alpha, Beta, Color) ->
    case possible_moves(Border, Board, othello:directions(), Color, []) of
    [] ->
        ColorOp = swap(Color),
        case possible_moves(Border, Board, othello:directions(), ColorOp, []) of
        [] ->
            dead_lock(Board, Color);
        PosDraws1 ->
            choose(PosDraws1, Board, Border, Depth - 1, -Beta, -Alpha, -1, ColorOp)
        end;
    PosDraws ->
        choose(PosDraws, Board, Border, Depth - 1, -Beta, -Alpha, -1, Color)
    end.

choose([Draw|Draws], Board, Border, Depth, Alpha, Beta, Record, Player) ->
    Player1 = swap(Player),
    {Border1, Board1} = update_board(Draw, Player, Board, Border),
    {_, Value} = alpha_beta(Depth, Border1, Board1, Alpha, Beta, Player1),
    Value1 = -Value,
    cutoff(Draw, Value1, Depth, Alpha, Beta, Draws, Board, Border, Record, Player);
choose([], _, _, _, Alpha, _, Draw, _) ->
    {Draw, Alpha}.

cutoff(Draw, Value, _, _, Beta, _, _, _, _, _) when Value >= Beta ->
    {Draw, Value};
cutoff(Draw, Value, Depth, Alpha, Beta, Draws, Board, Border, _, Player)
  when Alpha < Value, Value < Beta ->
    choose(Draws, Board, Border, Depth, Value, Beta, Draw, Player);
cutoff(_Draw, Value, Depth, Alpha, Beta, Draws, Board, Border, Record, Player)
  when Value =< Alpha ->
    choose(Draws, Board, Border, Depth, Alpha, Beta, Record, Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dead Lock checks who wins.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dead_lock(Board,Color) ->
    case win_or_lose(Board, Color) of
    0                    -> {-1,      0};
    Value when Value > 0 -> {-1,  10000};
    _                    -> {-1, -10000}
    end.

win_or_lose(Board, Color) ->
    Color1 = swap(Color),
    pieces(Color, Board) - pieces(Color1, Board).

pieces(Color, Board) ->
    Play = color(Color),
    pieces(Play, Board, 1, 0).

pieces(Color, Board, Pos, Count) when Pos < 101 ->
    case element(Pos, Board) of
    Color ->
        pieces(Color, Board, Pos + 1, Count + 1);
    _ ->
        pieces(Color, Board, Pos + 1, Count)
    end;
pieces(_, _, _, Count) ->
    Count.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Possibles Moves for a Player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possible_moves([F|T], Board, Directions, Color, Moves) ->
    LetsSee= othello:check_move(F, Board, Directions, color(Color)),
    if
        LetsSee-> possible_moves(T, Board, Directions, Color, lists:append(Moves, [F]));
        true -> possible_moves(T, Board, Directions, Color, Moves)
    end;
possible_moves([], _, _, _, Moves) -> Moves.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluate the board
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate(Color, Board) ->
    Play = color(Color),
    Other = color(swap(Color)),
    Score = eval(Play, Other, Board, Board, 1),
    Score1 = eval_corner(Play, Other, Score, Board),
    count(Score1,1).


eval(MyCol, OtCol, Score, Board, Pos) when Pos < 101 ->
    case element(Pos, Board) of
    MyCol ->
        Score1 = setelement(Pos, Score, score(Pos)),
        eval(MyCol, OtCol, Score1, Board, Pos + 1);
    OtCol ->
        Score1 = setelement(Pos, Score, -(score(Pos))),
        eval(MyCol, OtCol, Score1, Board, Pos + 1);
    _ ->
        eval(MyCol, OtCol, Score, Board, Pos + 1)
    end;
eval(_,_,Score,_,_) ->
    Score.

eval_corner(MyCol, OtCol, Score, Board) ->
    case element(12, Board) of
    MyCol ->
        Score1 = verify_line(12, MyCol, Score , Board,  50,  1, 19),
        Score2 = verify_line(12, MyCol, Score1, Board,  50, 11, 89),
        Score3 = verify_line(12, MyCol, Score2, Board,  50, 10, 82),
        eval_corner_2(MyCol, OtCol, Score3, Board);
    OtCol ->
        Score1 = verify_line(12, OtCol, Score , Board, -50,  1, 19),
        Score2 = verify_line(12, OtCol, Score1, Board, -50, 11, 89),
        Score3 = verify_line(12, OtCol, Score2, Board, -50, 10, 82),
        eval_corner_2(MyCol, OtCol, Score3, Board);
    _ ->
        eval_corner_2(MyCol, OtCol, Score, Board)
    end.

eval_corner_2(MyCol, OtCol, Score, Board) ->
    case element(19, Board) of
    MyCol ->
        Score1 = verify_line(19, MyCol, Score , Board,  50,  -1, 12),
        Score2 = verify_line(19, MyCol, Score1, Board,  50,   9, 82),
        Score3 = verify_line(19, MyCol, Score2, Board,  50,  10, 89),
        eval_corner_3(MyCol, OtCol, Score3, Board);
    OtCol ->
        Score1 = verify_line(19, OtCol, Score , Board, -50,  -1, 12),
        Score2 = verify_line(19, OtCol, Score1, Board, -50,   9, 82),
        Score3 = verify_line(19, OtCol, Score2, Board, -50,  10, 89),
        eval_corner_3(MyCol, OtCol, Score3, Board);
    _ ->
        eval_corner_3(MyCol, OtCol, Score, Board)
    end.

eval_corner_3(MyCol, OtCol, Score, Board) ->
    case element(82, Board) of
    MyCol ->
        Score1 = verify_line(82, MyCol, Score,  Board,  50,   1, 89),
        Score2 = verify_line(82, MyCol, Score1, Board,  50,  -9, 19),
        Score3 = verify_line(82, MyCol, Score2, Board,  50, -10, 12),
        eval_corner_4(MyCol, OtCol, Score3, Board);
    OtCol ->
        Score1 = verify_line(82, OtCol, Score,  Board, -50,   1, 89),
        Score2 = verify_line(82, OtCol, Score1, Board, -50,  -9, 19),
        Score3 = verify_line(82, OtCol, Score2, Board, -50, -10, 12),
        eval_corner_4(MyCol, OtCol, Score3, Board);
    _ ->
        eval_corner_4(MyCol, OtCol, Score, Board)
    end.

eval_corner_4(MyCol, OtCol, Score, Board) ->
    case element(89, Board) of
    MyCol ->
        Score1 = verify_line(89, MyCol, Score,  Board,  50,  -1, 82),
        Score2 = verify_line(89, MyCol, Score1, Board,  50, -11, 12),
        verify_line(89, MyCol, Score2, Board,  50, -10, 19);
    OtCol ->
        Score1 = verify_line(89, OtCol, Score,  Board, -50,  -1, 82),
        Score2 = verify_line(89, OtCol, Score1, Board, -50, -11, 12),
        verify_line(89, OtCol, Score2, Board, -50, -10, 19);
    _ ->
        Score
    end.

verify_line(Pos, Color, Score, Board, Value, Dir, Limit) ->
    Val = Pos + Dir,
    if
        Val /= Limit ->
            case element(Val, Board) of
            Color ->
                Score1 = setelement(Val, Score, Value),
                verify_line(Val, Color, Score1, Board, Value, Dir, Limit);
            _ ->
                Score
            end;
        true ->
            Score
    end.

count(Score, Pos) when Pos < 101 ->
    element(Pos, Score) + count(Score, Pos + 1);
count(_, _) ->
    0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set board with new draw.
%% Devuelve {Border,Board}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_board(Pos, Color, Board, Border) ->
    %%io:format("~w~n", [Color]),
    case lists:member(Pos, Border) of
    true ->
        NewBoard = setelement(Pos, Board, color(Color)),
        Empty = empty_neighbour(Pos, NewBoard, othello:directions(), []),
        NewBorder = lists:append(lists:delete(Pos, Border), Empty),
        turn(color(Color), Pos, NewBorder, NewBoard);
    _ ->
        {error, invalid_position}
    end.

empty_neighbour(_, _, [], Vec) -> Vec;
empty_neighbour(Pos, Board, [Dir|Directions], Vec) ->
    Neighbour = valid(Pos + Dir),
    if
        Neighbour == -1 -> empty_neighbour(Pos, Board, Directions, Vec);
        true ->
            case element(Neighbour, Board) of
                0 -> empty_neighbour(Pos, Board, Directions, lists:append(Vec, [Neighbour]));
                _ -> empty_neighbour(Pos, Board, Directions, Vec)
            end
    end.

valid(Num) when Num <  12 -> -1;
valid(Num) when Num == 20 -> -1;
valid(Num) when Num == 21 -> -1;
valid(Num) when Num == 30 -> -1;
valid(Num) when Num == 31 -> -1;
valid(Num) when Num == 40 -> -1;
valid(Num) when Num == 41 -> -1;
valid(Num) when Num == 50 -> -1;
valid(Num) when Num == 51 -> -1;
valid(Num) when Num == 60 -> -1;
valid(Num) when Num == 61 -> -1;
valid(Num) when Num == 70 -> -1;
valid(Num) when Num == 71 -> -1;
valid(Num) when Num == 80 -> -1;
valid(Num) when Num == 81 -> -1;
valid(Num) when Num >  89 -> -1;
valid(Num) -> Num.

turn(Color,Pos,Border,Board) ->
    Board1 = view_line(Color, Pos, Board,    1),
    Board2 = view_line(Color, Pos, Board1,  11),
    Board3 = view_line(Color, Pos, Board2,  10),
    Board4 = view_line(Color, Pos, Board3,   9),
    Board5 = view_line(Color, Pos, Board4,  -1),
    Board6 = view_line(Color, Pos, Board5, -11),
    Board7 = view_line(Color, Pos, Board6, -10),
    Board8 = view_line(Color, Pos, Board7,  -9),
    %%io:format("~w~n", [{Border, Board8}]),
    {Border, Board8}.

view_line(Color, Pos, Board, Dir) ->
    ColorOp = swap(Color),
    Val = Pos + Dir,
    case element(Val,Board) of
        ColorOp -> view_line(Color, Val, Board, Dir);
        Color -> change_line(Color, Val, Board, -1 * Dir);
        _ -> Board
    end.

change_line(Color, Pos, Board, Dir) ->
    Val = Pos + Dir,
    case element(Val, Board) of
        Color -> Board;
        _ ->
            Board1 = setelement(Val, Board, Color),
            change_line(Color, Val, Board1, Dir)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Score of positions on the board
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score(12) -> 120;
score(19) -> 120;
score(82) -> 120;
score(89) -> 120;

score(13) -> -40;
score(18) -> -40;
score(22) -> -40;
score(29) -> -40;
score(72) -> -40;
score(79) -> -40;
score(83) -> -40;
score(88) -> -40;

score(23) -> -60;
score(28) -> -60;
score(73) -> -60;
score(78) -> -60;

score(14) ->  20;
score(15) ->   5;
score(16) ->   5;
score(17) ->  20;

score(84) ->  20;
score(85) ->   5;
score(86) ->   5;
score(87) ->  20;

score(Pos) when 23 < Pos, Pos < 28 -> -5;
score(Pos) when 73 < Pos, Pos < 78 -> -5;

score(32) ->  20;
score(42) ->   5;
score(52) ->   5;
score(62) ->  20;

score(39) ->  20;
score(49) ->   5;
score(59) ->   5;
score(69) ->  20;

score(33) ->  -5;
score(43) ->  -5;
score(53) ->  -5;
score(63) ->  -5;

score(38) ->  -5;
score(48) ->  -5;
score(58) ->  -5;
score(68) ->  -5;

score(34) ->  15;
score(37) ->  15;
score(64) ->  15;
score(67) ->  15;

score(_) ->    3.
