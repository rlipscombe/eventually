-module(message_probe_tests).
-include_lib("eunit/include/eunit.hrl").

message_probe_test() ->
    self() ! continue,

    eventually:assert(
        eventually:probe(
            fun() ->
                receive
                    continue -> true
                after 0 -> false
                end
            end,
            received_continue
        )
    ),
    ok.
