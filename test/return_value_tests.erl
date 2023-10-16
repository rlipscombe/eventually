-module(return_value_tests).
-include_lib("eunit/include/eunit.hrl").

simple_probe_returns_value_test() ->
    ?assertEqual(moo, eventually:assert(fun() -> {true, moo} end)).

matcher_returns_value_test() ->
    ?assertEqual(moo, eventually:assert(fun() -> moo end, fun(Value) -> {true, Value} end)).
