-module(t3).
-export([newgame/0, playwith/1, wait_opponent/0, connect_opponent/1]).

wait_opponent() ->
   io:format("Hello~n", []),
   receive
       {connect, PlayerY_PID} ->
          PlayerY_PID ! gamestart,
          io:format("Player Y joined~n", [])
   end.

connect_opponent(XNode) ->
   {playerX, XNode} ! {connect, self()},
   receive
	gamestart ->
	  io:format("Player X says to start the game~n", [])
   end.

% starts a new game node and waits for an opponent
newgame()->
   register(playerX, spawn(t3, wait_opponent, [])).  

%connects to another Erlang node identified by Opponent and starts a new game.
playwith(XNode)->
   spawn(t3, connect_opponent, [XNode]).