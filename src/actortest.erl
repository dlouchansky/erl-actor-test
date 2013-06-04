%% Copyright
-module(actortest).
-author("dloutchansky").

%% API
-export([multiple/2, one/1]).

one(Messagecnt) ->
  Start = now(),
  outStats("Started", helpers:diff(Start, Start)),

  PID = erlang:spawn(fun() -> looper(self(), 0) end),
  sendMessagesToOne(Messagecnt, PID),
  Sent = now(),
  outStats("Messages sent", helpers:diff(Start, Sent)),

  PID ! "exit",
  io:format("Test took ~p seconds~n", [helpers:diff(Start, Sent)]),
  io:format("Throughput = ~p messages per second~n",[helpers:perSec(Messagecnt, Start, Sent)]).

multiple(Actorcnt, Messagecnt) ->
  Start = now(),
  outStats("Started", helpers:diff(Start, Start)),

  RandomActorIds = helpers:generateRandomNumList(Actorcnt, Messagecnt),
  RandomDone = now(),
  outStats("Random list generated", helpers:diff(Start, RandomDone)),

  Actors = createActors(Actorcnt, self()),
  Created = now(),
  outStats("Actors started", helpers:diff(Start, Created)),

  sendMessages(RandomActorIds, Actors),
  Sent = now(),
  outStats("Messages sent", helpers:diff(Start, Sent)),

  killActors(Actors),
  waitCompletion(Actorcnt, Start, RandomDone, Created, Sent, Messagecnt).

looper(ParentPID, MsgCnt) ->
  receive
    {msg, Msg} ->
      looper(ParentPID, MsgCnt + 1);
    _ ->
      ParentPID ! {finished, MsgCnt}
  end.

outStats(Action, Time) ->
  io:format("~p at ~p. Memory usage: total - ~p KB, processes - ~p KB~n", [Action, Time, round(erlang:memory(total) / 1024), round(erlang:memory(processes) / 1024)]).

sendMessages([], _) ->
  "ok";
sendMessages([Head|Tail], Actors) ->
  dict:fetch(Head, Actors) ! {msg, "Test"},
  sendMessages(Tail,Actors).

sendMessagesToOne(0, PID) ->
  "ok";
sendMessagesToOne(Cnt, PID) ->
  PID ! {msg, "Test"},
  sendMessagesToOne(Cnt-1, PID).

createActors(Actorcnt, PID) ->
  createActors(dict:new(), Actorcnt, PID).

createActors(Dict, 0, _) ->
  Dict;
createActors(Dict, Actorcnt, ParentPID) ->
  PID = erlang:spawn(fun() -> looper(ParentPID, 0) end),
  createActors(dict:store(Actorcnt, PID, Dict), Actorcnt - 1, ParentPID).

killActors(ActorDict) ->
    dict:map(fun(_, Val)-> Val ! "exit", "ok" end, ActorDict).

waitCompletion(Actorcnt, Start, RandomDone, Created, Sent, Messagecnt) ->
  lists:foreach(fun (_) ->
    receive {finished, MsgCnt} -> ok end
  end, lists:seq(1, Actorcnt)),
  Finish = now(),
  io:format("Test took ~p seconds~n", [helpers:diff(Start, Finish)]),
  io:format("Without random id generation - ~p seconds~n", [helpers:diff(RandomDone, Finish)]),
  io:format("Throughput = ~p messages per second~n",[helpers:perSec(Messagecnt, Created, Sent)]).
