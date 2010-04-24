-module(tests).

-include_lib("eunit/include/eunit.hrl").

-define(TESTS, [
        menu_tests
    ]).

all_test() ->
    lists:foreach(fun eunit:test/1, ?TESTS).
