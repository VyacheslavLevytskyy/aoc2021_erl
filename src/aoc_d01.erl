%%-------------------------------------------------------------------
%% @doc Day 01: https://adventofcode.com/2021/day/01
%%-------------------------------------------------------------------

-module(aoc_d01).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_ints("day01/input.txt")).

p2() ->
    do_p2(aoc:read_ints("day01/input.txt")).

%%-------------------------------------------------------------------

do_p1([H | T]) ->
    do_p1(T, H, 0);

do_p1([]) ->
    0.

do_p1([H | T], Prev, Acc) when H > Prev ->
    do_p1(T, H, 1 + Acc);

do_p1([H | T], _, Acc) ->
    do_p1(T, H, Acc);

do_p1([], _, Acc) ->
    Acc.

%%-------------------------------------------------------------------

do_p2(L = [H1, H2, H3, _ | _]) ->
    do_p2(tl(L), H1, H1 + H2 + H3, 0);

do_p2(_) ->
    0.

do_p2(L = [H2, _, H4 | _], H1, Sum1, Acc) ->
    Sum2 = Sum1 - H1 + H4,
    Acc2 = Acc + if
        Sum2 > Sum1 ->
            1;
        true ->
            0
    end,
    do_p2(tl(L), H2, Sum2, Acc2);

do_p2(_, _, _, Acc) ->
    Acc.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(7, do_p1(p1_sample())).

p2_test() ->
    ?assertEqual(5, do_p2(p1_sample())).

p12_test() ->
    ?assertEqual(1759, p1()),
    ?assertEqual(1805, p2()).

p1_sample() ->
    [199, 200, 208, 210, 200, 207, 240, 269, 260, 263].

%%-------------------------------------------------------------------
