%% Copyright
-module(actortest).
-author("dloutchansky").

%% API
-export([multiple/2, one/1]).

one(Messagecnt) ->
  Start = now(),
  helpers:outStats("Started", Start, Start),

  PID = erlang:spawn(fun() -> counter(self(), 0) end),
  sendMessagesToOne(Messagecnt, PID),
  Sent = now(),
  helpers:outStats("Messages sent", Start, Sent),

  PID ! "exit",
  helpers:testTime("Overall", Start, Sent),
  helpers:outThroughput(Messagecnt, Start, Sent).


multiple(Actorcnt, Messagecnt) ->
  Start = now(),
  helpers:outStats("Started", Start, Start),

  RandomActorIds = helpers:generateRandomNumList(Actorcnt, Messagecnt),
  RandomDone = now(),
  helpers:outStats("Random list generated", Start, RandomDone),

  Actors = createActors(Actorcnt, self()),
  Created = now(),
  helpers:outStats("Actors started", Start, Created),

  sendMessages(RandomActorIds, Actors),
  Sent = now(),
  helpers:outStats("Messages sent", Start, Sent),

  killActors(Actors),
  waitCompletion(Actorcnt, Start, RandomDone, Created, Sent, Messagecnt).


counter(ParentPID, MsgCnt) ->
  receive
    add ->
      counter(ParentPID, MsgCnt + 1);
    getcount ->
      io:format("~p actor counted ~p.~n", [self(), MsgCnt]),
      counter(ParentPID, MsgCnt);
    _ ->
      ParentPID ! {finished, MsgCnt}
  end.


sendMessages([], _) ->
  "ok";
sendMessages([Head|Tail], Actors) ->
  dict:fetch(Head, Actors) ! add,
  sendMessages(Tail,Actors).


sendMessagesToOne(0, PID) ->
  "ok";
sendMessagesToOne(Cnt, PID) ->
  PID ! add,
  sendMessagesToOne(Cnt - 1, PID).


createActors(Actorcnt, PID) ->
  createActors(dict:new(), Actorcnt, PID).

createActors(Dict, 0, _) ->
  Dict;
createActors(Dict, Actorcnt, ParentPID) ->
  PID = erlang:spawn(fun() -> counter(ParentPID, 0) end),
  createActors(dict:store(Actorcnt, PID, Dict), Actorcnt - 1, ParentPID).


killActors(ActorDict) ->
    dict:map(fun(_, Val) -> Val ! "exit" end, ActorDict).


waitCompletion(Actorcnt, Start, RandomDone, Created, Sent, Messagecnt) ->
  lists:foreach(fun (_) ->
    receive {finished, _} -> ok end
  end, lists:seq(1, Actorcnt)),
  Finish = now(),
  helpers:testTime("Overall", Start, Finish),
  helpers:testTime("Without random id generation", RandomDone, Finish),
  helpers:outThroughput(Messagecnt, Created, Sent).
