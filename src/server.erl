%%%-------------------------------------------------------------------
%%% File    : server.erl
%%% Author  : Jose Castro <jose.r.castro@gmail.com>
%%% Description : server for othello game application
%%%
%%% Created :  8 Jul 2009 by Jose Castro <jose.r.castro@gmail.com>
%%%-------------------------------------------------------------------
-module(server).

%% API
-vsn(1).
-author('jose.r.castro@gmail.com').

% Server management
-export([start/0, stop/0]).

% User API
-export([init/0, connect/1, disconnect/0, get_status/0, make_move/2, proxy/0, call_proxy/3]).

-include("othello.hrl").

init()                   -> #game{board=othello:board(), border=othello:border()}.
connect(Color)           -> call(connect, Color).
disconnect()             -> call(disconnect).
get_status ()            -> call(get_status).
make_move  (Color, Pos)  -> call(move,  {Color, Pos}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
stop() -> oserver ! stop.
start() ->
    register(
      oserver,
      spawn(fun() ->
            GS        = gs:start(),
            Win       = gs:create(window, GS, [{width, 200}, {height,280}, {title, "Othello Server"}, {map, true}]),
            Canvas    = gs:create(canvas, can1, Win, [{x,0},{y,0},{width,200},{height,200}, {buttonpress, true}]),
            NewGame   = gs:create(button, Win, [{x,0}, {y,235}, {label, {text, "New Game"}}]),
            Exit      = gs:create(button, Win, [{x,100},{y,235},{label,{text, "Exit"}}]),
            OldBoard  = othello:empty(),
            GameState = init(),

            gs:radiobutton(negras,  Win, [{label, {text, "negras" }}, {value, negras }, {x,  0}, {y,205}]),
            gs:radiobutton(blancas, Win, [{label, {text, "blancas"}}, {value, blancas}, {x,100}, {y,205}]),

            #game{board = Board} = GameState,

            display:draw_lines(Canvas),
            display:change(Canvas, OldBoard, Board),

            event_loop(NewGame, Exit, Canvas, GameState)
        end)).


event_loop(NewGame, Exit, Canvas, #game{current=Player}=State) ->
    receive
    stop ->
        gs:stop(),
        finish;

    {gs, can1, buttonpress, [], [1,A,B|_]} ->
        {Reply, NewState} = move(Player, display:pos(A,B), State),
        io:format("server: reply = ~w~n", [Reply]),
        #game{board=Board} = State,
        case Reply of
        {ok, #game{board=NewBoard}} ->
            display:change(Canvas, Board, NewBoard);
        _ -> do_nothing
        end,
        event_loop(NewGame, Exit, Canvas, NewState);

    {gs, Exit, click, _, _} ->
        gs:stop(),
        finish;

    {gs, NewGame, click, _, _} ->
        io:format("server: New Game\n"),
        #game{white=W, black=B, board=Board} = State,
        NewBoard = othello:board(),
        NewState = #game{white=W, black=B, board=NewBoard, border=othello:border()},
        notify_players(black, NewState),
        display:change(Canvas, Board, NewBoard),
        event_loop(NewGame, Exit, Canvas, NewState);

    {connect, Client, Color} ->
        case Color of
        white ->
            Client ! {ok, State},
            event_loop(NewGame, Exit, Canvas, State#game{white=Client});
        black ->
            Client ! {ok, State},
            event_loop(NewGame, Exit, Canvas, State#game{black=Client});
        _ ->
            Client ! {error, State},
            event_loop(NewGame, Exit, Canvas, State)
        end;

    {disconnect, Client} ->
        #game{black=Black, white=White} = State,
        case Client of
        Black ->
            Client ! {ok, State},
            event_loop(NewGame, Exit, Canvas, State#game{black=none});
        White ->
            Client ! {ok, State},
            event_loop(NewGame, Exit, Canvas, State#game{white=none});
        _ ->
            Client ! {error, State},
            event_loop(NewGame, Exit, Canvas, State)
        end;

    {get_status, Client} ->
        Client ! {ok, State},
        event_loop(NewGame, Exit, Canvas, State);

    {change_player} ->
        NewState = State#game{current=other(Player)},
        Turn = NewState#game.current,
        if
            Turn == white ->
                White = NewState#game.white,
                notify(White, {your_turn, NewState});
            true ->
                Black = NewState#game.black,
                notify(Black, {your_turn, NewState})
        end,
        event_loop(NewGame, Exit, Canvas, NewState);

    {move, Client, {Player, Pos}} ->
        io:format("server: gona make a move~n", []),
        {Reply, NewState} = move(Player, Pos, State),
        io:format("reply = ~w~n", [Reply]),
        #game{board=Board} = State,
        Client ! Reply,
        case Reply of
        {ok, #game{board=NewBoard}} ->
            display:change(Canvas, Board, NewBoard);
        _ -> do_nothing
        end,
        event_loop(NewGame, Exit, Canvas, NewState)
    end.

% Utility functions for calling the othello server
call(Id, Data) ->
    oserver ! {Id, self(), Data},
    receive
    {ok, Result} ->
        Result;
    Error -> Error
    end.


call(Id) ->
    oserver ! {Id, self()},
    receive
    {ok, Result} ->
        Result;
    Error -> Error
    end.


%%--------------------------------------------------------------------


other(white) -> black;
other(black) -> white.

color(white) ->  1;
color(black) -> -1.

%% MAKE_MOVE
move( Player, _Pos, State=#game{current=Other}) when Player /= Other ->
    io:format("server: not your turn player = ~w, other = ~w, state = ~w...\n", [Player, Other, State]),
    {{not_your_turn, Player}, State};

move( Player, Pos, State=#game{current=Player, board=Board, border=Border}) ->
    io:format("~w, ~w~n", [Pos, Border]),
    case lists:member(Pos, Border) of
    true  -> case othello:check_move(Pos, Board, othello:directions(), color(Player)) of
                     true  -> NewBorder = othello:new_frontier(Border, Pos, Board),
                              NewBoard  = othello:make_move(Pos, Board, othello:directions(), color(Player)),
                              NewState  = State#game{current=other(Player), board=NewBoard, border=NewBorder},
                  notify_players(other(Player), NewState),
                  {{ok, NewState}, NewState};
             false ->
             io:format("oops~n", []),
             {{invalid_move, Pos}, State}
         end;
    false -> {{invalid_move, Pos}, State}
    end.

notify_players(Color, #game{white=White, black=Black}=State) ->
    if
    Color =:= white ->
            io:format("~n-----------SERVER DICE: Turno del blanco-----------~n"),
            io:format("Blanco: ~w~n", [White]),
        notify(White, {your_turn, State}),
        notify(Black, {ok, State});
    Color =:= black ->
            io:format("~n-----------SERVER DICE: Turno del negro-----------~n"),
            io:format("Negro: ~w~n", [Black]),
        notify(Black, {your_turn, State}),
        notify(White, {ok, State})
    end.

notify(none, _Message) -> ok;
notify(Proc,  Message) -> Proc ! Message.

proxy_loop() ->
    receive
    stop ->
        io:format("proxy ~w: good bye~n", [self()]),
        no_problem;
    {Fun, Args} ->
        RET = erlang:apply(server, Fun, Args),
        io:format("proxy ~w: ~w~n", [self(), RET]),
        proxy_loop()
    end.

proxy() ->
    spawn(fun() -> proxy_loop() end).
call_proxy(Proxy, Fun, Args) ->
    Proxy ! {Fun, Args}.
