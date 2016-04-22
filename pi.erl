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
-export([montecarlo/2, start/2, createActor/6, randomgen/3, stop/0]).

%%%
% spawns and registers a local listener that will act as master node
%%%
start(N, X) ->
  PIDmaster = spawn(pi, createActor, [N, N/X, X, self(), 0, X]),
  register(alistener, PIDmaster).
%%%
% spawns slave actors for generating random numbers, once all slaves have been formed
% then listens for points matching x^2 + y^2 < 1 and adds them up. Once all slave actors
% have sent their values then calculates the value of pi.
%%%
createActor(N, Num, 0, PIDmaster, Val, 0) ->
  io:format("The value of pi is ~p ~n", [4*Val/N]),
  stop();
createActor(N, Num, 0, PIDmaster, Val, X) ->
  receive
    {M1} ->
      createActor(N, Num, 0, PIDmaster, Val+M1, X-1)
  end;
createActor(N, Num, X, PIDmaster, Val, 0)->
  spawn(pi, randomgen, [Num, PIDmaster, 0]),
  createActor(N, Num, X-1, PIDmaster, Val, 0).

%%%
% sends a request to stop the communication, and unregisters alistener
%%%
stop() ->
  alistener!{self(), reqstop},
  unregister(alistener).

montecarlo(N, 0) ->
  0;
montecarlo(0, X) ->
  0;
montecarlo(N, X) ->
  start(N, X).

randomgen(0, PIDmaster, M) ->
  PIDmaster ! M;
randomgen(N, PIDmaster, M) ->
  X = random:uniform(), Y = random:uniform(),
  randomgen(N-1, PIDmaster, if X*X + Y*Y < 1 -> M+1; true -> M end).




