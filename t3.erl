-module(t3).
-export([newgame/0, playwith/1, wait_opponent/0, connect_opponent/1, wait_msg/4, tell/1, placetoken/1]).
-export([update_board/3, create_empty_board/0]).

%Wait to receive the message from the opponent
wait_msg(YourSym, HisSym, Board, OpponentPID) ->
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
	   %update your board
	   UpdatedBoard = update_board(YourSym, Board, Move),
	   io:format("~w~n", [UpdatedBoard]),
	   %send the message to opponent
	   OpponentPID ! {newmove, Move};

       %The opponent sends a new move
	{newmove, Move} ->
	   io:format("Move received: ~w~n", [Move]),
	    %the opponent updates its board with his opponent's symbol
	   UpdatedBoard = update_board(HisSym, Board, Move),
	   io:format("~w~n", [UpdatedBoard])
   end,
   wait_msg(YourSym, HisSym, UpdatedBoard, OpponentPID).

%Update the Board at Index position with the player's symbol
update_board(Who, Board, Index) ->
    setelement(Index, Board, Who).

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
	   R = random:uniform(),  %better to have a seed for random number
	   io:format("Random = ~w~n", [R]),

	   if 
	      R  > 0.5 -> 
		   %current player starts
		   io:format("You will start first~n", []);	   
	       true -> 
		   %the other player starts
		   PlayerY_PID ! {message, 'You will start first.~n'}
	   end,
	   wait_msg(x, o, create_empty_board(), PlayerY_PID)
   end.

%PlayerY trying to connect to X
connect_opponent(XNode) ->
   {player, XNode} ! {connect, self()},
   receive
	{gamestart, PlayerX_PID} ->
	   io:format("Connection successful.~n", []),
	   wait_msg(o, x, create_empty_board(), PlayerX_PID)
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
   
