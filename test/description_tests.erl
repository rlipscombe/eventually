-module(description_tests).
-include_lib("eunit/include/eunit.hrl").

probe_description_test() ->
    assert_format_error(
        "probe always_fail eventually failed",
        fun() ->
            eventually:assert(#{probe => fun() -> false end, description => always_fail})
        end
    ).

both_description_test() ->
    assert_format_error(
        "probe always_fail, matching never_match, eventually failed",
        fun() ->
            eventually:assert(#{probe => fun() -> false end, description => always_fail}, #{
                matcher => fun(_) -> false end, description => never_match
            })
        end
    ),
    ok.

assert_format_error(Expected, Fun) ->
    Catch = catch Fun(),
    {'EXIT', {Reason, Stack}} = Catch,
    [{_M, _F, _As, Info} | _] = Stack,
    #{module := Module} = proplists:get_value(error_info, Info),
    ?assertEqual(
        #{general => Expected},
        Module:format_error(Reason, Stack)
    ),
    ok.
