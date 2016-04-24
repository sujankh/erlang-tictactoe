%%%-------------------------------------------------------------------
%%% @author Anand Abishek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2016 6:09 PM
%%%-------------------------------------------------------------------
-module(pi).
-author("Anand Abishek").

%% API
-export([montecarlo/2, start/2, createActor/3, randomgen/3, stop/0, listen/3]).

%%%
% spawns and registers a local listener that will act as master node and another process that will just create actors.
%%%
start(N, X) ->
  PIDListen = spawn(pi, listen, [N, 0, X]),
  Pid = spawn(pi, createActor, [N div X, X, PIDListen]),
  register(listen, PIDListen).
%%%
% spawns slave actors for generating random numbers, once all slaves have been formed
% then listens for points matching x^2 + y^2 < 1 and adds them up. Once all slave actors
% have sent their values then calculates the value of pi.
%%%
createActor(Num, 0, PID)->
  io:format("Finished spawning actors~n");
createActor(Num, X, PID)->
  Pid = spawn(pi, randomgen, [Num, PID, 0]),
  createActor(Num, X-1, PID).
%%%
% listens to incoming messages from all the actors
%%%
listen(N, Val, 0) ->
  io:format("The value of pi is ~p ~n", [4*Val/N]),
  stop();
listen(N, Val, X) ->
  %io:format("Listening...."),
  receive
    {M1} ->
      %io:format("Got message ~p~n", [M1]),
      listen(N, Val+M1, X-1)
  end.


%%%
% sends a request to stop the communication, and unregisters alistener
%%%
stop() ->
  listen!{self(), reqstop},
  unregister(listen).

montecarlo(N, 0) ->
  0;
montecarlo(0, X) ->
  0;
montecarlo(N, X) ->
  start(N, X).

%%%
% recursively generates random numbers and process them according to monte carlo simulation
%%%
randomgen(0, PIDmaster, M) ->
  %io:format("Exiting slave method~n"),
  PIDmaster ! {M};
randomgen(N, PIDmaster, M) ->
  %X = random:uniform(), Y = random:uniform(),
  X = rand:uniform(), Y = rand:uniform(),
  randomgen(N-1, PIDmaster, if X*X + Y*Y < 1 -> M+1; true -> M end).




