%% Main module
-module(actortest).
-author("dloutchansky").

%% API
-export([multiple/3, multiple/2, one/2, one/1]).

one(MessageCnt) ->
  one(MessageCnt, 1).

one(Messagecnt, MessageTimeout) ->
  Start = now(),
  helpers:outStats("Started", Start, Start),

  PID = createActor(self(), MessageTimeout),
  sendMessagesToOne(Messagecnt, PID),
  Sent = now(),
  helpers:outStats("Messages sent", Start, Sent),

  killActor(PID),
  waitCompletion(1, Start, Start, Messagecnt).


multiple(Actorcnt, Messagecnt) ->
  multiple(Actorcnt, Messagecnt, 1).

multiple(Actorcnt, Messagecnt, MessageTimeout) ->
  Start = now(),
  helpers:outStats("Started", Start, Start),

  % generating list of size Messagecnt, where each elem is random (1, Actorcnt)
  RandomActorIds = helpers:generateRandomNumList(Actorcnt, Messagecnt),
  RandomDone = now(),
  helpers:outStats("Random list generated", Start, RandomDone),

  Actors = createActors(Actorcnt, self(), MessageTimeout),
  Created = now(),
  helpers:outStats("Actors started", Start, Created),

  sendMessages(RandomActorIds, Actors),
  Sent = now(),
  helpers:outStats("Messages sent", Start, Sent),

  killActors(Actors),
  waitCompletion(Actorcnt, Start, Created, Messagecnt).


createActors(Actorcnt, PID, MessageTimeout) ->
  createActors(dict:new(), Actorcnt, PID, MessageTimeout).

createActors(Dict, 0, _, _) ->
  Dict;
createActors(Dict, Actorcnt, ParentPID, MessageTimeout) ->
  PID = createActor(ParentPID, MessageTimeout),
  createActors(dict:store(Actorcnt, PID, Dict), Actorcnt - 1, ParentPID, MessageTimeout).

createActor(ParentPID, MessageTimeout) ->
  erlang:spawn(fun() -> count(ParentPID, MessageTimeout, 0) end).


count(ParentPID, MessageTimeout, MsgCnt) ->
  receive
    add ->
      % making it a relatively long-running task, 1 millisecond per msg
      case MessageTimeout of
        0 ->
          ok;
        _ ->
          timer:sleep(MessageTimeout)
      end,
      count(ParentPID, MessageTimeout, MsgCnt + 1);
    getcount ->
      helpers:outCounted(self(), MsgCnt),
      count(ParentPID, MessageTimeout, MsgCnt);
    exit ->
      ParentPID ! {finished, MsgCnt};
    _ ->
      count(ParentPID, MessageTimeout, MsgCnt)
  end.


sendMessages([], _) ->
  ok;
sendMessages([Head|Tail], Actors) ->
  dict:fetch(Head, Actors) ! add,
  sendMessages(Tail,Actors).


sendMessagesToOne(0, _) ->
  ok;
sendMessagesToOne(Cnt, PID) ->
  PID ! add,
  sendMessagesToOne(Cnt - 1, PID).


killActors(ActorDict) ->
  dict:map(fun(_, Val) -> killActor(Val) end, ActorDict).

killActor(PID) ->
  PID ! exit.


waitCompletion(Actorcnt, Start, Created, Messagecnt) ->
  waitMurder(Actorcnt),
  Finish = now(),
  helpers:testTime("Overall", Start, Finish),
  helpers:outThroughput(Messagecnt, Created, Finish).


waitMurder(0) ->
  ok;
waitMurder(Cnt) ->
  receive
    {finished, _} ->
      waitMurder(Cnt - 1)
  end.