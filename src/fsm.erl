-module(fsm).
-define(T, 2000).

-export([init/0]).

init() ->
  receive
    {pids, Pids} ->
      start_election([P || P <- Pids, P /= self()])
  end.

start_election(Pids) ->
  monitor:write([{state, start_election}]),
  case is_highest(Pids) of
    true ->
      log("I am the highest!"),
      coronation(Pids);
    false ->
      receive
        {i_am_the_king, KingPid} ->
          king_recognition(Pids, KingPid)
      after 0 ->
        send_to_highers(Pids, you_are_alive),
        election(Pids, ?T)
      end
  end.

election(Pids, T) ->
  monitor:write([{state, election}]),
  Start = get_timestamp(),
  receive
    {i_am_the_king, KingPid} ->
      king_recognition(Pids, KingPid)
  after 0 ->
    receive
      fine_thanks ->
        await_coronation(Pids);
      {i_am_the_king, KingPid} ->
        king_recognition(Pids, KingPid);
      {you_are_alive, Pid} ->
        Pid ! fine_thanks,
        NewT = normalize(T - get_timestamp() + Start),
        election(Pids, NewT)
    after T ->
      coronation(Pids)
    end
  end.

await_coronation(Pids) ->
  monitor:write([{state, await_coronation}]),
  receive
    {i_am_the_king, KingPid} ->
      king_recognition(Pids, KingPid)
  after ?T ->
    start_election(Pids)
  end.

coronation(Pids) ->
  log("coronation by self"),
  send_all(Pids, i_am_the_king),
  king(Pids).

king(Pids) ->
  monitor:write([{state, king}]),
  receive
    {ping, Pid} ->
      timer:sleep(rand:uniform(?T div 11)),
      Pid ! pong,
      king(Pids)
  end.

passive(Pids, KingPid) ->
  monitor:write([{state, passive}, {king_pid, KingPid}]),
  KingPid ! {ping, self()},
  receive
    pong ->
      passive(Pids, KingPid);
    {i_am_the_king, NewKingPid} ->
      king_recognition(Pids, NewKingPid);
    {you_are_alive, Pid} ->
      Pid ! fine_thanks,
      start_election(Pids)
  after 4 * ?T ->
    log("king dead. start election"),
    start_election(Pids)
  end.

king_recognition(Pids, KingPid) ->
  log("recognition king"),
  flush(),
  passive(Pids, KingPid).


is_highest(Pids) ->
  length([P || P <- Pids, P > self()]) == 0.

send_to_highers(Pids, Msg) ->
  HighPids = [P || P <- Pids, P > self()],
  send_all(HighPids, Msg).

send_all(Pids, Msg) ->
%%  io:format("~w: send [~p] to [~p]~n", [self(), Msg, Pids]),
  send_msg(Pids, Msg).

send_msg([], _) -> ok;
send_msg([Pid | RestPids], Msg) ->
  Pid ! {Msg, self()},
  send_msg(RestPids, Msg).

log(Msg) ->
  io:format("~w [~s]: ~p~n", [self(), str_time(), Msg]).

flush() ->
  receive
    _ -> flush()
  after
    0 -> ok
  end.

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

str_time() ->
  {H, M, S} = time(),
  {_, _, Millis} = os:timestamp(),
  [integer_to_list(H) ++ ":" ++
    integer_to_list(M) ++ ":" ++
    integer_to_list(S) ++ ":" ++
    integer_to_list(Millis)].


normalize(T) when T < 0 -> 0;
normalize(T) -> T.