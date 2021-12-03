%%-------------------------------------------------------------------
%% @doc Day 02
%%-------------------------------------------------------------------

-module(aoc_d02).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_cmds("day02/input.txt"), 0, 0).

p2() ->
    do_p2(aoc:read_cmds("day02/input.txt"), 0, 0, 0).

%%-------------------------------------------------------------------

do_p1([{1, X} | Cmds], Hor, Depth) ->
    do_p1(Cmds, Hor + X, Depth);

do_p1([{2, X} | Cmds], Hor, Depth) ->
    do_p1(Cmds, Hor, Depth + X);

do_p1([{3, X} | Cmds], Hor, Depth) ->
    do_p1(Cmds, Hor, Depth - X);

do_p1([], Hor, Depth) ->
    Hor * Depth.

%%-------------------------------------------------------------------

do_p2([{1, X} | Cmds], Hor, Depth, Aim) ->
    Hor2 = Hor + X,
    Depth2 = Depth + Aim * X,
    do_p2(Cmds, Hor2, Depth2, Aim);

do_p2([{2, X} | Cmds], Hor, Depth, Aim) ->
    do_p2(Cmds, Hor, Depth, Aim + X);

do_p2([{3, X} | Cmds], Hor, Depth, Aim) ->
    do_p2(Cmds, Hor, Depth, Aim - X);

do_p2([], Hor, Depth, _) ->
    Hor * Depth.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(150, do_p1(p1_sample(), 0, 0)).

p2_test() ->
    ?assertEqual(900, do_p2(p1_sample(), 0, 0, 0)).

p12_test() ->
    ?assertEqual(2322630, p1()),
    ?assertEqual(2105273490, p2()).

p1_sample() ->
    [
        {1, 5},
        {2, 5},
        {1, 8},
        {3, 3},
        {2, 8},
        {1, 2}
    ].

%%-------------------------------------------------------------------
