-module(simple_probe_tests).
-include_lib("eunit/include/eunit.hrl").

passes_first_attempt_test() ->
    eventually:assert(fun() -> true end).

passes_last_attempt_test() ->
    % Because simple probes don't have any way to maintain state (that's coming later), we have to do that via the
    % process dictionary.
    put(?FUNCTION_NAME, 0),
    Probe = fun() ->
        put(?FUNCTION_NAME, get(?FUNCTION_NAME) + 1),
        case get(?FUNCTION_NAME) of
            5 -> true;
            _ -> false
        end
    end,
    eventually:assert(Probe).

fails_all_attempts_test() ->
    ?assertError(eventually_assert, eventually:assert(fun() -> false end)).

throws_all_attempts_test() ->
    NotOk = fun() -> not_ok end,
    ?assertError({badmatch, not_ok}, eventually:assert(fun() -> ok = NotOk() end)).
