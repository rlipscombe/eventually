-module(eventually).
-export([
    assert/1,
    assert/2,
    assert/3
]).

-define(DEFAULT_MAX_ATTEMPTS, 5).
-define(DEFAULT_SLEEP_MS, 250).

assert(Probe) when is_function(Probe, 0) ->
    Probe1 = fun(_) -> Probe() end,
    Matcher = fun(Value) -> Value end,
    do_assert(Probe1, undefined, Matcher, 1, default_options()).

assert(Probe, Matcher) when is_function(Probe, 0), is_function(Matcher, 1) ->
    Probe1 = fun(_) -> Probe() end,
    do_assert(Probe1, undefined, Matcher, 1, default_options());
assert(Probe, Options) when is_function(Probe, 0), is_map(Options) ->
    Probe1 = fun(_) -> Probe() end,
    Matcher = fun(Value) -> Value end,
    do_assert(Probe1, undefined, Matcher, 1, default_options(Options));
assert({Probe, Init}, Matcher) when is_function(Probe, 1), is_function(Matcher, 1) ->
    do_assert(Probe, Init, Matcher, 1, default_options()).

assert({Probe, Init}, Matcher, Options) when is_function(Probe, 1), is_function(Matcher, 1), is_map(Options) ->
    do_assert(Probe, Init, Matcher, 1, default_options(Options)).

default_options() ->
    #{max_attempts => ?DEFAULT_MAX_ATTEMPTS, attempt_interval_ms => ?DEFAULT_SLEEP_MS}.

default_options(Options) ->
    maps:merge(default_options(), Options).

do_assert(Probe, Acc, Matcher, Attempt, Options = #{max_attempts := MaxAttempts}) when Attempt < MaxAttempts ->
    % Because this is not the final attempt, we'll catch any exceptions and retry.
    case catch Probe(Acc) of
        {'EXIT', _} ->
            do_retry(Probe, Acc, Matcher, Attempt, Options);
        NextAcc ->
            case Matcher(NextAcc) of
                ok ->
                    true;
                true ->
                    true;
                {true, Result} ->
                    Result;
                false ->
                    do_retry(Probe, NextAcc, Matcher, Attempt, Options)
            end
    end;
do_assert(Probe, Acc, Matcher, _Attempt, _Options) ->
    % On the final attempt, don't catch the exception; it's better if the test sees it.
    NextAcc = Probe(Acc),
    case Matcher(NextAcc) of
        ok ->
            true;
        true ->
            true;
        {true, Result} ->
            Result;
        false ->
            % TODO: Can we report something more useful here?
            error(eventually_assert)
    end.

do_retry(Probe, Acc, Matcher, Attempt, Options = #{attempt_interval_ms := SleepMs}) ->
    timer:sleep(SleepMs),
    do_assert(Probe, Acc, Matcher, Attempt + 1, Options).
