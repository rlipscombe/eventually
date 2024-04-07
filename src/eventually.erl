-module(eventually).
-export([
    assert/1,
    assert/2,
    assert/3
]).
-export([format_error/2]).

-define(DEFAULT_MAX_ATTEMPTS, 5).
-define(DEFAULT_SLEEP_MS, 250).

-record(probe, {
    description,
    probe,
    state
}).

-record(matcher, {
    description,
    matcher
}).

assert(Probe) when is_function(Probe, 0) ->
    Probe1 = fun(_) -> Probe() end,
    Matcher = fun(Value) -> Value end,
    do_assert(
        #probe{probe = Probe1, state = undefined}, #matcher{matcher = Matcher}, 1, default_options()
    );
assert(#{probe := Probe, description := Description}) when is_function(Probe, 0) ->
    Probe1 = fun(_) -> Probe() end,
    Matcher = fun(Value) -> Value end,
    do_assert(
        #probe{probe = Probe1, state = undefined, description = Description},
        #matcher{matcher = Matcher},
        1,
        default_options()
    ).

assert(Probe, Matcher) when is_function(Probe, 0), is_function(Matcher, 1) ->
    Probe1 = fun(_) -> Probe() end,
    do_assert(
        #probe{probe = Probe1, state = undefined}, #matcher{matcher = Matcher}, 1, default_options()
    );
assert(Probe, Options) when is_function(Probe, 0), is_map(Options) ->
    Probe1 = fun(_) -> Probe() end,
    Matcher = fun(Value) -> Value end,
    do_assert(
        #probe{probe = Probe1, state = undefined},
        #matcher{matcher = Matcher},
        1,
        default_options(Options)
    );
assert({Probe, Init}, Matcher) when is_function(Probe, 1), is_function(Matcher, 1) ->
    do_assert(
        #probe{probe = Probe, state = Init}, #matcher{matcher = Matcher}, 1, default_options()
    );
assert(#{probe := Probe, description := ProbeDescription}, #{
    matcher := Matcher, description := MatcherDescription
}) when
    is_function(Probe, 0), is_function(Matcher, 1)
->
    Probe1 = fun(_) -> Probe() end,
    do_assert(
        #probe{probe = Probe1, state = undefined, description = ProbeDescription},
        #matcher{matcher = Matcher, description = MatcherDescription},
        1,
        default_options()
    );
assert(#{probe := Probe, state := Init, description := ProbeDescription}, #{
    matcher := Matcher, description := MatcherDescription
}) when
    is_function(Probe, 1), is_function(Matcher, 1)
->
    do_assert(
        #probe{probe = Probe, state = Init, description = ProbeDescription},
        #matcher{matcher = Matcher, description = MatcherDescription},
        1,
        default_options()
    ).

assert({Probe, Init}, Matcher, Options) when
    is_function(Probe, 1), is_function(Matcher, 1), is_map(Options)
->
    do_assert(
        #probe{probe = Probe, state = Init},
        #matcher{matcher = Matcher},
        1,
        default_options(Options)
    ).

default_options() ->
    #{max_attempts => ?DEFAULT_MAX_ATTEMPTS, attempt_interval_ms => ?DEFAULT_SLEEP_MS}.

default_options(Options) ->
    maps:merge(default_options(), Options).

do_assert(
    Probe = #probe{probe = ProbeFun, state = ProbeState},
    Matcher = #matcher{matcher = MatcherFun},
    Attempt,
    Options = #{max_attempts := MaxAttempts}
) when
    Attempt < MaxAttempts
->
    % Because this is not the final attempt, we'll catch any exceptions and retry.
    case catch ProbeFun(ProbeState) of
        {'EXIT', _} ->
            do_retry(Probe, Matcher, Attempt, Options);
        NextState ->
            case MatcherFun(NextState) of
                ok ->
                    true;
                true ->
                    true;
                {true, Result} ->
                    Result;
                false ->
                    NextProbe = Probe#probe{state = NextState},
                    do_retry(
                        NextProbe,
                        Matcher,
                        Attempt,
                        Options
                    )
            end
    end;
do_assert(
    Probe = #probe{probe = ProbeFun, state = ProbeState},
    Matcher = #matcher{matcher = MatcherFun},
    _Attempt,
    _Options
) ->
    % On the final attempt, don't catch the exception; it's better if the test sees it.
    NextState = ProbeFun(ProbeState),
    case MatcherFun(NextState) of
        ok ->
            true;
        true ->
            true;
        {true, Result} ->
            Result;
        false ->
            assert_error(Probe, Matcher)
    end.

do_retry(
    Probe,
    Matcher,
    Attempt,
    Options = #{attempt_interval_ms := SleepMs}
) ->
    timer:sleep(SleepMs),
    do_assert(Probe, Matcher, Attempt + 1, Options).

assert_error(Probe, Matcher) ->
    erlang:error(eventually_assert_failed, [Probe, Matcher], [{error_info, #{module => ?MODULE}}]).

format_error(_Reason, [{_M, _F, _Args = [Probe, Matcher], Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorMap = maps:get(cause, ErrorInfo, #{}),
    ErrorMap#{
        general => format_error_message(Probe, Matcher)
    }.

format_error_message(
    #probe{description = ProbeDescription},
    #matcher{description = MatcherDescription}
) when ProbeDescription /= undefined, MatcherDescription /= undefined ->
    lists:flatten(
        io_lib:format("probe ~p, matching ~p, eventually failed", [
            ProbeDescription, MatcherDescription
        ])
    );
format_error_message(
    #probe{description = ProbeDescription},
    #matcher{}
) when ProbeDescription /= undefined ->
    lists:flatten(
        io_lib:format("probe ~p eventually failed", [
            ProbeDescription
        ])
    );
format_error_message(
    #probe{},
    #matcher{}
) ->
    "probe eventually failed".
