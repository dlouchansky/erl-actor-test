erl-actor-test
==============

Erlang actor performance test.

Two testing functions available:

actortest:one(M). - sends M messages to one actor

actortest:multiple(L, M). – randomly sends M messages to L actors.
Actors are stored in one dict and that's why the more actors are 
created, the less overall throughtput the system has.  


Install, compile && run:
1. Install Erlang
2. Open Erlang shell
3. Compile modules
c(actortest.erl).
c(helpers.erl).
4. Run tests
actortest:one(1000000).
actortest:multiple(100, 1000000).
