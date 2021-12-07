%%-------------------------------------------------------------------
%% @doc Day 06: https://adventofcode.com/2021/day/06
%%-------------------------------------------------------------------

-module(aoc_d06).
-export([p1/0, p2/0, do_p1/2]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_one_line_ints("day06/input.txt"), 80).

p2() ->
    do_p1(aoc:read_one_line_ints("day06/input.txt"), 256).

%%-------------------------------------------------------------------

do_p1(L, Days) ->
    T = lists:foldl(fun (I, Acc) ->
        p1_inc(I + 1, Acc, 1)
    end, erlang:make_tuple(9, 0), L),
    p1_loop(T, Days).

p1_inc(Pos, T, Inc) ->
    setelement(Pos, T, Inc + element(Pos, T)).

p1_loop(T, Days) when Days > 0 ->
    Zero = element(1, T),
    T2 = erlang:append_element(erlang:delete_element(1, T), Zero),
    p1_loop(p1_inc(7, T2, Zero), Days - 1);

p1_loop(T1, 0) ->
    lists:sum(tuple_to_list(T1)).

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(26, do_p1(aoc:read_one_line_ints("day06/sample.txt"), 18)),
    ?assertEqual(5934, do_p1(aoc:read_one_line_ints("day06/sample.txt"), 80)).

p2_test() ->
    ?assertEqual(26984457539, do_p1(aoc:read_one_line_ints("day06/sample.txt"), 256)).

p12_test() ->
    ?assertEqual(390_011, p1()),
    ?assertEqual(1_746_710_169_834, p2()).

%%-------------------------------------------------------------------
