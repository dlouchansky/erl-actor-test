%% Time Formatting
-module(helpers).
-author("dloutchansky").

%% API
-export([perSec/3, diff/2, toSec/1, toMicroSeconds/1, generateRandomNumList/2]).

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
