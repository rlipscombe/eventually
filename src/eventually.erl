-module(eventually).
-export([
    probe/1,
    probe/2,
    probe/3,

    match/1,
    match/2,

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

probe(Fun) when is_function(Fun, 0) ->
    Fun1 = fun(_) -> Fun() end,
    Description = get_fun_description(Fun),
    probe(Fun1, undefined, Description).

probe(Fun, Init) when is_function(Fun, 1) ->
    Description = get_fun_description(Fun),
    probe(Fun, Init, Description);
probe(Fun, Description) when is_function(Fun, 0) ->
    Fun1 = fun(_) -> Fun() end,
    probe(Fun1, undefined, Description).

probe(Fun, Init, Description) when is_function(Fun, 1) ->
    #probe{
        description = Description,
        probe = Fun,
        state = Init
    }.

match(Fun) when is_function(Fun, 1) ->
    Description = get_fun_description(Fun),
    match(Fun, Description).

match(Fun, Description) when is_function(Fun, 1) ->
    #matcher{
        description = Description,
        matcher = Fun
    }.

default_matcher() ->
    match(fun(Value) -> Value end, default_matcher).

get_fun_description(Fun) ->
    Info = erlang:fun_info(Fun),
    Module = proplists:get_value(module, Info),
    Name = proplists:get_value(name, Info),
    Arity = proplists:get_value(arity, Info),
    lists:flatten(io_lib:format("~s:~s/~B", [Module, Name, Arity])).

assert(Probe) when is_function(Probe, 0) ->
    assert(probe(Probe));
assert(Probe = #probe{}) ->
    assert(Probe, default_matcher()).

assert(Probe, Matcher) when is_function(Probe, 0), is_function(Matcher, 1) ->
    assert(probe(Probe), match(Matcher));
assert(Probe, Options) when is_function(Probe, 0), is_map(Options) ->
    assert(probe(Probe), default_matcher(), Options);
assert(Probe = #probe{}, Matcher = #matcher{}) ->
    assert(Probe, Matcher, #{}).

assert(Probe = #probe{}, Matcher = #matcher{}, Options) when is_map(Options) ->
    do_assert(
        Probe,
        Matcher,
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
            NextProbe = Probe#probe{state = NextState},
            case catch MatcherFun(NextState) of
                ok ->
                    true;
                true ->
                    true;
                {true, Result} ->
                    Result;
                false ->
                    do_retry(NextProbe, Matcher, Attempt, Options);
                {'EXIT', _} ->
                    do_retry(NextProbe, Matcher, Attempt, Options);
                _ ->
                    error(bad_return)
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
            assert_error(Probe, Matcher);
        _ ->
            error(bad_return)
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
