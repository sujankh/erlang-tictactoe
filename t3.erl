-module(t3).
-export([newgame/0, playwith/1, wait_opponent/0, connect_opponent/1, wait_msg/1, tell/1]).


wait_msg(OpponentPID) ->
   receive
      {send_message, Msg} ->
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
          io:format("Player Y joined~n", [])
   end,
   wait_msg(PlayerY_PID).

%PlayerY trying to connect to X
connect_opponent(XNode) ->
   {playerX, XNode} ! {connect, self()},
   receive
	{gamestart, PlayerX_PID} ->
	  io:format("Player X says to start the game~n", [])
   end,
   wait_msg(PlayerX_PID).

tell(Message) ->
  {self()} ! {send_message, Message}.

% starts a new game node and waits for an opponent
newgame()->
   register(playerX, spawn(t3, wait_opponent, [])).  

%connects to another Erlang node identified by Opponent and starts a new game.
playwith(XNode)->
   spawn(t3, connect_opponent, [XNode]).
   