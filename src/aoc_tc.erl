%%-------------------------------------------------------------------
%% @doc Run all days and measure
%%-------------------------------------------------------------------

-module(aoc_tc).
-export([run/0]).

%%-------------------------------------------------------------------

run() ->
    L = [
        aoc_d01, aoc_d02, aoc_d03, aoc_d04, aoc_d05_2,
        aoc_d06, aoc_d07, aoc_d08, aoc_d09, aoc_d10,
        aoc_d11, aoc_d12, aoc_d13, aoc_d14, aoc_d15,
        aoc_d16, aoc_d17, aoc_d18, aoc_d19, aoc_d20,
        aoc_d21, aoc_d22, aoc_d23, aoc_d24, aoc_d25
    ],
    io:format("25 Erlang days:~n"),
    Mcs = [element(1, timer:tc(Mod, p12_test, [])) || Mod <- L],
    [io:format("    day ~p: ~.1pms~n", [No, Mc / 1000.0])
        || {No, Mc} <- lists:zip(lists:seq(1, 25), Mcs)],
    io:format("Average: ~.1pms~n", [lists:sum(Mcs) / 25000.0]).


%%-------------------------------------------------------------------
