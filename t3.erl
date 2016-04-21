-module(t3).
-export([newgame/0, playwith/1, wait_opponent/0, connect_opponent/1, wait_msg/1, tell/1]).


wait_msg(OpponentPID) ->
   receive
      {sendmsg, Msg} ->
          OpponentPID ! {message, Msg};
      {message, Msg} ->
          io:format("Message received: ~w~n", [Msg])
   end,
   wait_msg(OpponentPID).

%PlayerX waiting for PlayerY
wait_opponent() ->
   io:format("Hello~n", []),
   receive
       {connect, PlayerY_PID} ->
          PlayerY_PID ! {gamestart, self()},
	   io:format("Player Y joined~n", []),
	   wait_msg(PlayerY_PID)
   end.

%PlayerY trying to connect to X
connect_opponent(XNode) ->
   {player, XNode} ! {connect, self()},
   receive
	{gamestart, PlayerX_PID} ->
	   io:format("Player X says to start the game~n", []),
	   wait_msg(PlayerX_PID)
   end.

tell(Message) ->
  {player, node()} ! {sendmsg, Message}.

% starts a new game node and waits for an opponent
newgame()->
   register(player, spawn(t3, wait_opponent, [])).  

%connects to another Erlang node identified by Opponent and starts a new game.
playwith(XNode)->
   register(player, spawn(t3, connect_opponent, [XNode])).
   
