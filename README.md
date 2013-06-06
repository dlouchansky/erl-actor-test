Erlang actor benchmark
==============

Erlang actor performance test aka benchmark.

Two functions available:

+   actortest:one(M). – sends M messages to one actor
+   actortest:multiple(A, M). – randomly sends M messages to A actors.
    All actors are stored in one dict.

Install, compile && run:

1.  Install Erlang
2.  Open Erlang shell
3.  Compile modules
    ```erlang
    c(actortest.erl).
    c(helpers.erl).
    ```
4.  Run tests
    ```erlang
    actortest:one(1000000).
    actortest:multiple(100, 1000000).
    ```
