%%-------------------------------------------------------------------
%% @doc Day 07: https://adventofcode.com/2021/day/07
%%-------------------------------------------------------------------

-module(aoc_d07).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_one_line_ints("day07/input.txt")).

p2() ->
    do_p2(aoc:read_one_line_ints("day07/input.txt")).

%%-------------------------------------------------------------------

do_p1(L) ->
    Value = lists:nth(length(L) div 2, lists:sort(L)),
    lists:foldl(fun (I, Sum) ->
        Sum + abs(I - Value)
    end, 0, L).

do_p2(L0) ->
    [Min | _] = L = lists:sort(L0),
    p2_loop(Min + 1, lists:last(L), L, pdist2(Min, L, 0)).

p2_loop(X, Max, L, W1) when X =< Max ->
    W2 = pdist2(X, L, 0),
    if
        W2 < W1 ->
            p2_loop(X + 1, Max, L, W2);
        true ->
            W1 div 2
    end;

p2_loop(_, _, _, W) ->
    W div 2.

pdist2(X1, X2) ->
    D = abs(X1 - X2),
    D * (1 + D).

pdist2(X, [H | T], Cb) ->
    pdist2(X, T, pdist2(X, H) + Cb);

pdist2(_, [], Cb) ->
    Cb.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(37, do_p1(aoc:read_one_line_ints("day07/sample.txt"))).

p2_test() ->
    ?assertEqual(168, do_p2(aoc:read_one_line_ints("day07/sample.txt"))).

p12_test() ->
    ?assertEqual(337488, p1()),
    ?assertEqual(89647695, p2()).

%%-------------------------------------------------------------------
