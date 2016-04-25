-module(t3).
-export([newgame/0, playwith/1, wait_opponent/0, connect_opponent/1, wait_msg/5, tell/1, placetoken/1]).
-export([update_board/3, create_empty_board/0]).

%Wait to receive the message from the opponent
wait_msg(YourSym, HisSym, Board, OpponentPID, undefined) ->
  io:format("Game ended ~n");

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
         if
           Turn == self() ->
             %update your board
             UpdatedBoard = update_board(YourSym, Board, Move),
             io:format("~w~n", [UpdatedBoard]),
             Winner = check(Board),
             if

               true ->
                 Turn = OpponentPID, OpponentPID ! {newmove, Move},
                 wait_msg(YourSym, HisSym, Board, OpponentPID, Turn)
             end;
             %send the message to opponent
             %OpponentPID ! {newmove, Move};
           true ->
             wait_msg(YourSym, HisSym, Board, OpponentPID, Turn)
         end;

       %The opponent sends a new move
	    {newmove, Move} ->
	        io:format("Move received: ~w~n", [Move]),
          %the opponent updates its board with his opponent's symbol
         UpdatedBoard = update_board(HisSym, Board, Move),
         io:format("~w~n", [UpdatedBoard]),
          Winner = check(Board),
        if
          Winner == {victory, _} or Winner == draw ->
            io:format("~p~n", [Winner]), Turn = undefined;
          true ->
            Turn = self()
        end
   end,
   wait_msg(YourSym, HisSym, Board, OpponentPID, Turn).

%Update the Board at Index position with the player's symbol
update_board(Who, Board, Index) ->
    setelement(Index, Board, Who).

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
			G, H, I} when A =/= undefined, B =/= undefined, C =/= undefined,
			D =/= undefined, E =/= undefined, F =/= undefined,
			G =/= undefined, H =/= undefined, I =/= undefined ->
			draw;

		_ -> ok
	end.

create_empty_board()->
    {'-', '-', '-',
     '-', '-', '-',
     '-', '-', '-'}.

%PlayerX waiting for PlayerY
wait_opponent() ->
   receive
       {connect, PlayerY_PID} ->
	   io:format("Another player joined.~n", []),
	   PlayerY_PID ! {gamestart, self()},
	   R = rand:uniform(),  %better to have a seed for random number
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
         wait_msg(x, o, Board, Board, PlayerY_PID, Turn)

   end.

%PlayerY trying to connect to X
connect_opponent(XNode) ->
   {player, XNode} ! {connect, self()},
   receive
	{gamestart, PlayerX_PID} ->
	   io:format("Connection successful.~n", []),
    Board = create_empty_board(),
	   wait_msg(o, x, Board, Board, PlayerX_PID)
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
   
