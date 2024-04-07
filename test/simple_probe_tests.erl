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
    ?assertError(eventually_assert_failed, eventually:assert(fun() -> false end)).

error_all_attempts_test() ->
    % If we throw an error, do we get retried?
    put(?FUNCTION_NAME, 0),
    ?assertError(
        computer_says_no,
        eventually:assert(fun() ->
            put(?FUNCTION_NAME, get(?FUNCTION_NAME) + 1),
            error(computer_says_no)
        end)
    ),
    ?assertEqual(5, get(?FUNCTION_NAME)).

badmatch_error_all_attempts_test() ->
    NotOk = fun() -> not_ok end,
    ?assertError({badmatch, not_ok}, eventually:assert(fun() -> ok = NotOk() end)).

configurable_attempts_test() ->
    % If we throw an error, do we get retried?
    put(?FUNCTION_NAME, 0),
    ?assertError(
        eventually_assert_failed,
        eventually:assert(fun() ->
            put(?FUNCTION_NAME, get(?FUNCTION_NAME) + 1),
            false
        end, #{max_attempts => 7})
    ),
    ?assertEqual(7, get(?FUNCTION_NAME)).
