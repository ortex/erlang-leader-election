-module(monitor).

-export([start/0, state/0, write/1, delete/1, stop/0]).
-export([init/0]).

start() ->
  register(monitor, spawn(monitor, init, [])).

stop() ->
  monitor ! {stop, self()}.

init() ->
  loop([]).

loop(Db) ->
  receive
    {write, Pid, Value} ->
      loop(lists:keystore(Pid, 1, Db, {Pid, Value}));
    {show, Pid} ->
      Pid ! {data, Db},
      loop(Db);
    {delete, Pid} ->
      loop(lists:keydelete(Pid, 1, Db));
    {stop, Pid} ->
      Pid ! ok
  end.

write(Value) ->
  monitor ! {write, self(), Value}.

delete(Pid) ->
  monitor ! {delete, Pid}.

state() ->
  monitor ! {show, self()},
  receive
    {data, Data} -> Data
  end.


