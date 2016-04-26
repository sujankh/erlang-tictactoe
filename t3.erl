-module(t3).
-export([newgame/0, playwith/1, wait_opponent/0, connect_opponent/1, wait_msg/5, tell/1, placetoken/1]).
-export([update_board/3, create_empty_board/0, check/1]).

%Wait to receive the message from the opponent
wait_msg(YourSym, HisSym, Board, OpponentPID, []) ->
    io:format("Game ended ~n"), stop();

wait_msg(YourSym, HisSym, Board, OpponentPID, Turn) ->
    receive

%This is called by the owner node
%The process then forwards the message to the opponent
	{sendmsg, Msg} ->
            UpdatedBoard = Board,
            OpponentPID ! {message, Msg};

%A new message has arrived from the opponent
	{message, Msg} ->
	    UpdatedBoard = Board,
	    io:format("Msg: ~w~n", [Msg]);

%The owner node sends a move request
	{sendmove, Move} ->
	    CurrentProc = self(), io:format("self = ~p turn = ~p ~n", [CurrentProc, Turn]),
	    if
		Turn == CurrentProc ->
		    %update your board
		    UpdatedBoard = update_board(YourSym, Board, Move),
		    io:format("~w~n", [UpdatedBoard]),
						%send the message to opponent
		    OpponentPID ! {newmove, Move},
		    {Status, Winner} = check(UpdatedBoard),
		    if
			Status == victory ->
			    io:format("~p won!!~n", [Winner]), Turn = [];
			Status == draw ->
			    io:format("The result was a~p~n", [Status]), Turn = [];
			true ->
			    wait_msg(YourSym, HisSym, UpdatedBoard, OpponentPID, OpponentPID)
		    end;
		true ->
		    io:format("Not your turn~n"),
		    UpdatedBoard = Board,
		    wait_msg(YourSym, HisSym, UpdatedBoard, OpponentPID, Turn)
	    end;

	%The opponent sends a new move
	{newmove, Move} ->
	    io:format("Move received: ~w~n", [Move]),
	    %the opponent updates its board with his opponent's symbol
	    UpdatedBoard = update_board(HisSym, Board, Move),
	    io:format("~w~n", [UpdatedBoard]),
	    {Status, Winner} = check(UpdatedBoard),
	    if
		Status == victory ->
		    io:format("~p won!!~n", [Winner]), Turn = [];
		Status == draw ->
		    io:format("The result was a~p~n", [Winner]), Turn = [];
		true ->
		    SelfPID = self(),
		    wait_msg(YourSym, HisSym, UpdatedBoard, OpponentPID, SelfPID)
	    end
    end,
    wait_msg(YourSym, HisSym, UpdatedBoard, OpponentPID, Turn).

%Update the Board at Index position with the player's symbol
update_board(Who, Board, Index) ->
    Element = erlang:element(Index, Board),
    if 
	Element == '_' ->
	    setelement(Index, Board, Who);
  	true->
	    io:format("Invalid Move")
    end.

check(Board) ->
    case Board of
	{x, x, x,
	 _, _, _,
	 _, _, _} -> {victory, x};

	{_, _, _,
	 x, x, x,
	 _, _, _} -> {victory, x};

	{_, _, _,
	 _, _, _,
	 x, x, x} -> {victory, x};

	{x, _, _,
	 x, _, _,
	 x, _, _} -> {victory, x};

	{_, x, _,
	 _, x, _,
	 _, x, _} -> {victory, x};

	{_, _, x,
	 _, _, x,
	 _, _, x} -> {victory, x};

	{x, _, _,
	 _, x, _,
	 _, _, x} -> {victory, x};

	{_, _, x,
	 _, x, _,
	 x, _, _} -> {victory, x};

	{o, o, o,
	 _, _, _,
	 _, _, _} -> {victory, o};

	{_, _, _,
	 o, o, o,
	 _, _, _} -> {victory, o};

	{_, _, _,
	 _, _, _,
	 o, o, o} -> {victory, o};

	{o, _, _,
	 o, _, _,
	 o, _, _} -> {victory, o};

	{_, o, _,
	 _, o, _,
	 _, o, _} -> {victory, o};

	{_, _, o,
	 _, _, o,
	 _, _, o} -> {victory, o};

	{o, _, _,
	 _, o, _,
	 _, _, o} -> {victory, o};

	{_, _, o,
	 _, o, _,
	 o, _, _} -> {victory, o};

	{A, B, C,
	 D, E, F,
	 G, H, I} when A =/= '_', B =/= '_', C =/= '_',
		       D =/= '_', E =/= '_', F =/= '_',
		       G =/= '_', H =/= '_', I =/= '_' ->
	    {draw, undefined};

	_ -> {ok, ok}
    end.

create_empty_board()->
    {'_', '_', '_',
     '_', '_', '_',
     '_', '_', '_'}.

						%PlayerX waiting for PlayerY
wait_opponent() ->
    receive
	{connect, PlayerY_PID} ->
	    io:format("Another player joined.~n", []),
	    PlayerY_PID ! {gamestart, self()}, random:seed(now()),
	    R = random:uniform(),  %better to have a seed for random number
	    io:format("Random = ~w~n", [R]),
	    Board = create_empty_board(),

	    if 
		R  > 0.5 -> 
						%current player starts
		    io:format("You will start first~n", []), Turn = self();
		true -> 
						%the other player starts
		    PlayerY_PID ! {message, 'You will start first.~n'}, Turn = PlayerY_PID
	    end,
	    wait_msg(x, o, Board, PlayerY_PID, Turn)

    end.

						%PlayerY trying to connect to X
connect_opponent(XNode) ->
    {player, XNode} ! {connect, self()},
    receive
	{gamestart, PlayerX_PID} ->
	    io:format("Connection successful.~n", []),
	    Board = create_empty_board(),
	    wait_msg(o, x, Board, PlayerX_PID, self())
    end.

						%validates that Coordinate is a valid move and places a new token at Coordinate
						%Coordinate = a1, a2, a3...b1.. . . ., c3
placetoken(Coordinate) ->
    Positions = [a1, a2, a3, b1, b2, b3, c1, c2, c3],
						%Find the Index of the Board to be updated
    Index = string:str(Positions, [Coordinate]),    

						%First communicate the move to own process
						%The process will then forward the move to opponent
    {player, node()} ! {sendmove, Index}.

						%sends the message to the same node: wait_msg
						%The node will then forward the message to the opponent
tell(Message) ->
    {player, node()} ! {sendmsg, Message}.

						% starts a new game node and waits for an opponent
newgame()->
    register(player, spawn(t3, wait_opponent, [])).  

						%connects to another Erlang node identified by Opponent and starts a new game.
playwith(XNode)->
    register(player, spawn(t3, connect_opponent, [XNode])).

stop() ->
    player!{self(), reqstop},
    unregister(player).

