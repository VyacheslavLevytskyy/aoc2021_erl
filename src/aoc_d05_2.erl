%%-------------------------------------------------------------------
%% @doc Day 05, part 2: https://adventofcode.com/2021/day/5
%%-------------------------------------------------------------------

-module(aoc_d05_2).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_xy("day05/input.txt")).

p2() ->
    do_p2(aoc:read_xy("day05/input.txt")).

%%-------------------------------------------------------------------

do_p1(L0) ->
    L = lists:filter(fun ({X1, Y1, X2, Y2}) ->
        X1 == X2 orelse Y1 == Y2
    end, L0),
    do_p2(L).

do_p2(L) ->
    {_, R} = lists:foldl(fun p2_mark/2, {#{}, #{}}, L),
    maps:size(R).

p2_mark({X, Y1, X, Y2}, Acc) ->
    lists:foldl(fun (Y, InAcc) ->
        p2_mark_point({X, Y}, InAcc)
    end, Acc, lists:seq(Y1, Y2));

p2_mark({X1, Y, X2, Y}, Acc) ->
    lists:foldl(fun (X, InAcc) ->
        p2_mark_point({X, Y}, InAcc)
    end, Acc, lists:seq(X1, X2));

p2_mark({X1, Y1, X2, Y2}, Acc) ->
    Xs = if
        X2 >= X1 ->
            lists:seq(X1, X2);
        true ->
            lists:seq(X1, X2, -1)
    end,
    Ys = if
        Y2 >= Y1 ->
            lists:seq(Y1, Y2);
        true ->
            lists:seq(Y1, Y2, -1)
    end,
    lists:foldl(fun (Pt, InAcc) ->
        p2_mark_point(Pt, InAcc)
    end, Acc, lists:zip(Xs, Ys)).

p2_mark_point(Pt, {Points, Ready}) ->
    case maps:get(Pt, Points, undefined) of
        undefined ->
            {Points#{Pt => []}, Ready};
        _ ->
            {Points, Ready#{Pt => []}}
    end.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(5, do_p1(aoc:read_xy("day05/sample.txt"))).

p2_test() ->
    ?assertEqual(12, do_p2(aoc:read_xy("day05/sample.txt"))).

p12_test() ->
    ?assertEqual(5585, p1()),
    ?assertEqual(17193, p2()).

%%-------------------------------------------------------------------
