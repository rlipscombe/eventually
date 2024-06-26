-module(description_tests).
-include_lib("eunit/include/eunit.hrl").

probe_description_test() ->
    assert_format_error(
        "probe always_fail, matching default_matcher, eventually failed",
        fun() ->
            eventually:assert(
                eventually:probe(fun() -> false end, always_fail)
            )
        end
    ).

both_description_test() ->
    assert_format_error(
        "probe always_fail, matching never_match, eventually failed",
        fun() ->
            eventually:assert(
                eventually:probe(fun() -> false end, always_fail),
                eventually:match(fun(_) -> false end, never_match)
            )
        end
    ),
    ok.

assert_format_error(Expected, Fun) ->
    case catch Fun() of
        {'EXIT', {Reason = eventually_assert_failed, Stack}} ->
            [{_M, _F, _As, Info} | _] = Stack,
            #{module := Module} = proplists:get_value(error_info, Info),
            ?assertEqual(
                #{general => Expected},
                Module:format_error(Reason, Stack)
            )
    end,
    ok.
