-module(basic_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0
]).

-export([
    ok_test/1,
    assertion_error_test/1,
    match_error_test/1
]).

all() ->
    [
        ok_test,
        assertion_error_test,
        match_error_test
    ].

ok_test(Config) ->
    ok.

assertion_error_test(Config) ->
    ?assertEqual(2, 3),
    ok.

match_error_test(Config) ->
    Data = 123,
    ok = Data,
    ok.