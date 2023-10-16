-module(eventually).
-export([assert/1]).

-define(DEFAULT_MAX_ATTEMPTS, 5).
-define(DEFAULT_SLEEP_MS, 250).

assert(Probe) when is_function(Probe, 0) ->
    assert(Probe, 1, ?DEFAULT_MAX_ATTEMPTS, ?DEFAULT_SLEEP_MS).

assert(Probe, Attempt, MaxAttempts, SleepMs) when
    is_function(Probe, 0), is_integer(Attempt), is_integer(MaxAttempts), is_integer(SleepMs)
->
    do_assert(Probe, Attempt, MaxAttempts, SleepMs).

do_assert(Probe, Attempt, MaxAttempts, SleepMs) when Attempt < MaxAttempts ->
    % Because this is not the final attempt, we'll catch any exceptions and retry.
    case catch Probe() of
        true ->
            true;
        _ ->
            timer:sleep(SleepMs),
            do_assert(Probe, Attempt + 1, MaxAttempts, SleepMs)
    end;
do_assert(Probe, _Attempt, _MaxAttempts, _SleepMs) ->
    % On the final attempt, don't catch the exception; it's better if the test sees it.
    case Probe() of
        true ->
            true;
        _ ->
            % TODO: Can we report something more useful here?
            error(eventually_assert)
    end.
