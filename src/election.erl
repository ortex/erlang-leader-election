-module(election).

-export([start/1, state/0, kill_king/0, stop/0]).

start(N) ->
  monitor:start(),
  Pids = create_nodes(N),
  lists:foreach(fun(Pid) -> Pid ! {pids, Pids} end, Pids),
  monitor:state().

create_nodes(0) -> [];
create_nodes(N) ->
  [spawn(fsm, init, []) | create_nodes(N - 1)].

state() ->
  Data = monitor:state(),
  io:format("~p~n", [Data]).

stop() ->
  Data = monitor:state(),
  Pids = [P || {P, _} <- Data],
  io:format("~w~n", [Pids]),
  lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
  monitor:stop().

kill_king() ->
  Data = monitor:state(),
  KingPid = find_king(Data),
  exit(KingPid, kill_king),
  monitor:delete(KingPid),
  state().

find_king([]) -> error(unavalaible);
find_king([{Pid, Data} | Rest]) ->
  case lists:keyfind(state, 1, Data) of
    {_, king} ->
      Pid;
    _ ->
      find_king(Rest)
  end.