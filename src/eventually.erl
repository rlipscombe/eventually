-module(eventually).
-export([
    assert/1,
    assert/2
]).

-define(DEFAULT_MAX_ATTEMPTS, 5).
-define(DEFAULT_SLEEP_MS, 250).

assert(Probe) when is_function(Probe, 0) ->
    Probe1 = fun(_) -> Probe() end,
    Matcher = fun(Value) -> Value end,
    do_assert(Probe1, undefined, Matcher, 1, ?DEFAULT_MAX_ATTEMPTS, ?DEFAULT_SLEEP_MS).

assert(Probe, Matcher) when is_function(Probe, 0), is_function(Matcher, 1) ->
    Probe1 = fun(_) -> Probe() end,
    do_assert(Probe1, undefined, Matcher, 1, ?DEFAULT_MAX_ATTEMPTS, ?DEFAULT_SLEEP_MS);
assert({Probe, Init}, Matcher) when is_function(Probe, 1), is_function(Matcher, 1) ->
    do_assert(Probe, Init, Matcher, 1, ?DEFAULT_MAX_ATTEMPTS, ?DEFAULT_MAX_ATTEMPTS).

do_assert(Probe, Acc, Matcher, Attempt, MaxAttempts, SleepMs) when Attempt < MaxAttempts ->
    % Because this is not the final attempt, we'll catch any exceptions and retry.
    case catch Probe(Acc) of
        {'EXIT', _} ->
            do_retry(Probe, Acc, Matcher, Attempt, MaxAttempts, SleepMs);
        NextAcc ->
            case Matcher(NextAcc) of
                true ->
                    true;
                _ ->
                    do_retry(Probe, NextAcc, Matcher, Attempt, MaxAttempts, SleepMs)
            end
    end;
do_assert(Probe, Acc, Matcher, _Attempt, _MaxAttempts, _SleepMs) ->
    % On the final attempt, don't catch the exception; it's better if the test sees it.
    case Matcher(Probe(Acc)) of
        true ->
            true;
        _ ->
            % TODO: Can we report something more useful here?
            error(eventually_assert)
    end.

do_retry(Probe, Acc, Matcher, Attempt, MaxAttempts, SleepMs) ->
    timer:sleep(SleepMs),
    do_assert(Probe, Acc, Matcher, Attempt + 1, MaxAttempts, SleepMs).
