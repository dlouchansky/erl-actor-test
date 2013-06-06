%% Copyright
-module(actortest).
-author("dloutchansky").

%% API
-export([multiple/2, one/1]).

one(Messagecnt) ->
  Start = now(),
  helpers:outStats("Started", Start, Start),

  PID = createActor(self()),
  sendMessagesToOne(Messagecnt, PID),
  Sent = now(),
  helpers:outStats("Messages sent", Start, Sent),

  killActor(PID),
  waitCompletion(1, Start, Start, Sent, Messagecnt).


multiple(Actorcnt, Messagecnt) ->
  Start = now(),
  helpers:outStats("Started", Start, Start),

  % generating list of size Messagecnt, where each elem is random (1, Actorcnt)
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
  waitCompletion(Actorcnt, Start, Created, Sent, Messagecnt).


createActors(Actorcnt, PID) ->
  createActors(dict:new(), Actorcnt, PID).

createActors(Dict, 0, _) ->
  Dict;
createActors(Dict, Actorcnt, ParentPID) ->
  PID = createActor(ParentPID),
  createActors(dict:store(Actorcnt, PID, Dict), Actorcnt - 1, ParentPID).

createActor(ParentPID) ->
  erlang:spawn(fun() -> count(ParentPID, 0) end).


count(ParentPID, MsgCnt) ->
  receive
    add ->
      count(ParentPID, MsgCnt + 1);
    getcount ->
      helpers:outCounted(self(), MsgCnt),
      count(ParentPID, MsgCnt);
    exit ->
      ParentPID ! {finished, MsgCnt};
    _ ->
      count(ParentPID, MsgCnt)
  end.


sendMessages([], _) ->
  ok;
sendMessages([Head|Tail], Actors) ->
  dict:fetch(Head, Actors) ! add,
  sendMessages(Tail,Actors).


sendMessagesToOne(0, PID) ->
  ok;
sendMessagesToOne(Cnt, PID) ->
  PID ! add,
  sendMessagesToOne(Cnt - 1, PID).


killActors(ActorDict) ->
  dict:map(fun(_, Val) -> killActor(Val) end, ActorDict).

killActor(PID) ->
  PID ! exit.


waitCompletion(Actorcnt, Start, Created, Sent, Messagecnt) ->
  waitMurder(Actorcnt),
  Finish = now(),
  helpers:testTime("Overall", Start, Finish),
  helpers:outThroughput(Messagecnt, Created, Sent).


waitMurder(0) ->
  ok;
waitMurder(Cnt) ->
  receive
    {finished, _} ->
      waitMurder(Cnt - 1)
  after 5000 ->
    exit(string:concat(integer_to_list(Cnt), " actor(s) haven't been killed!"))
  end.