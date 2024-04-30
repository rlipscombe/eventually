-module(auto_description_tests).
-include_lib("eunit/include/eunit.hrl").

auto_probe_description_test() ->
    assert_format_error(
        "probe \"auto_description_tests:-auto_probe_description_test/0-fun-0-/0\", "
        "matching default_matcher, eventually failed",
        fun() ->
            eventually:assert(
                eventually:probe(fun() -> false end)
            )
        end
    ).

auto_match_description_test() ->
    assert_format_error(
        "probe \"auto_description_tests:-auto_match_description_test/0-fun-1-/0\", "
        "matching \"auto_description_tests:-auto_match_description_test/0-fun-0-/1\", eventually failed",
        fun() ->
            eventually:assert(
                eventually:probe(fun() -> true end),
                eventually:match(fun(_) -> false end)
            )
        end
    ).

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
