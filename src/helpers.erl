%% Time Formatting
-module(helpers).
-author("dloutchansky").

%% API
-export([perSec/3, diff/2, toSec/1, toMicroSeconds/1, generateRandomNumList/2, outStats/3, outThroughput/3, testTime/3]).

perSec(Size, Start, Finish) -> Size / diff(Start, Finish).

diff(Start, Finish) ->
  (toMicroSeconds(Finish) - toMicroSeconds(Start)) / 1000000.

toSec(Time) ->
  toMicroSeconds(Time) / 1000000.

toMicroSeconds({MegaSec, Sec, MicroSec}) ->
  (MegaSec + Sec) * 1000000 + MicroSec.


generateRandomNumList(UpperLimit, ListSize) ->
  generateRandom([], UpperLimit, ListSize).

generateRandom(RandomList, _, 0) ->
  RandomList;
generateRandom(Randomlist, UpperLimit, ListSize) ->
  generateRandom([random:uniform(UpperLimit)|Randomlist], UpperLimit, ListSize - 1).


outStats(Action, Start, End) ->
  io:format("~s at ~p. Memory usage: total - ~p KB, processes - ~p KB~n", [Action, diff(Start, End), round(erlang:memory(total) / 1024), round(erlang:memory(processes) / 1024)]).

outThroughput(Messagecnt, Start, Sent) ->
  io:format("Throughput = ~p messages per second~n",[helpers:perSec(Messagecnt, Start, Sent)]).

testTime(Desc, Start, Finish) ->
  io:format("~p took ~p seconds~n", [Desc, helpers:diff(Start, Finish)]).