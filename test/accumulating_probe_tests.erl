-module(accumulating_probe_tests).
-include_lib("eunit/include/eunit.hrl").

% TODO: without_accumulating_probe_test() ->
    % Demonstrate the alternative.

accumulating_probe_test() ->
    % On each call to the probe, the previous return value is given. This allows the probe to collect data.
    % We _could_ write this explicitly, but the test is (arguably) more readable like this.
    Self = self(),
    spawn_link(fun() ->
        Self ! one,
        Self ! two,
        Self ! three,
        Self ! four,
        Self ! five
    end),
    eventually:assert(messages_received(), contains_message(five)).

messages_received() ->
    {
        fun(Acc) ->
            receive
                M ->
                    [M | Acc]
            end
        end,
        []
    }.

contains_message(Expected) ->
    fun(Acc) ->
        lists:member(Expected, Acc)
    end.
