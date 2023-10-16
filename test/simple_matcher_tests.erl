-module(simple_matcher_tests).
-include_lib("eunit/include/eunit.hrl").

simple_matcher_test() ->
    CRef = counters:new(1, []),
    spawn_link(fun() -> incrementer(CRef) end),
    eventually:assert(
        fun() ->
            counters:get(CRef, 1)
        end,
        fun(Value) ->
            Value >= 30
        end
    ).

incrementer(CRef) ->
    timer:sleep(10),
    counters:add(CRef, 1, 1),
    incrementer(CRef).
