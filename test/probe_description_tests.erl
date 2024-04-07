-module(probe_description_tests).
-include_lib("eunit/include/eunit.hrl").

probe_description_test() ->
    ?assertError(
        eventually_assert_failed,
        eventually:assert(#{probe => fun() -> false end, description => always_fail})
    ).
