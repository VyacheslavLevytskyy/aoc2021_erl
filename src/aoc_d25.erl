%%-------------------------------------------------------------------
%% @doc Day 25: https://adventofcode.com/2521/day/25
%%-------------------------------------------------------------------

-module(aoc_d25).
-export([p1/0, sample/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    move(parse("day25/input.txt")).

sample() ->
    move(parse("day25/sample.txt")).

%%-------------------------------------------------------------------

move(Map) ->
    move(Map, 1).

move(Map, No) ->
    {Map1, Cb1} = move_right(Map),
    {Map2, Cb2} = move_down(Map1),
    if
        Cb1 == 0, Cb2 == 0 ->
            No;
        true ->
            move(Map2, No + 1)
    end.

move_down({Map, MaxX, MaxY}) ->
    {Map1, Cb} = maps:fold(fun
        (K = {X, Y}, 2, {Acc, AccCb}) ->
            Y2 = case Y of
                MaxY ->
                    0;
                _ ->
                    Y + 1
            end,
            K2 = {X, Y2},
            case maps:get(K2, Map, undefined) of
                undefined ->
                    {Acc#{K2 => 2}, AccCb + 1};
                _ ->
                    {Acc#{K => 2}, AccCb}
            end;
        (K, 1, {Acc, AccCb}) ->
            {Acc#{K => 1}, AccCb}
    end, {#{}, 0}, Map),
    {{Map1, MaxX, MaxY}, Cb}.

move_right({Map, MaxX, MaxY}) ->
    {Map1, Cb} = maps:fold(fun
        (K = {X, Y}, 1, {Acc, AccCb}) ->
            X2 = case X of
                MaxX ->
                    0;
                _ ->
                    X + 1
            end,
            K2 = {X2, Y},
            case maps:get(K2, Map, undefined) of
                undefined ->
                    {Acc#{K2 => 1}, AccCb + 1};
                _ ->
                    {Acc#{K => 1}, AccCb}
            end;
        (K, 2, {Acc, AccCb}) ->
            {Acc#{K => 2}, AccCb}
    end, {#{}, 0}, Map),
    {{Map1, MaxX, MaxY}, Cb}.

%%-------------------------------------------------------------------

parse(Fn) ->
    {Map, MaxX, MaxY1} = lists:foldl(fun (Line, {Acc, _, Y}) ->
        {Acc2, X2} = parse_to_map(Line, 0, Y, Acc),
        {Acc2, X2, Y + 1}
    end, {#{}, 0, 0}, aoc:read_lines(Fn, [<<10>>], [global, trim_all])),
    {Map, MaxX, MaxY1 - 1}.

parse_to_map(<<$v, R/binary>>, X, Y, Acc) ->
    parse_to_map(R, X + 1, Y, Acc#{{X, Y} => 2});

parse_to_map(<<$>, R/binary>>, X, Y, Acc) ->
    parse_to_map(R, X + 1, Y, Acc#{{X, Y} => 1});

parse_to_map(<<$., R/binary>>, X, Y, Acc) ->
    parse_to_map(R, X + 1, Y, Acc);

parse_to_map(<<>>, X, _, Acc) ->
    {Acc, X - 1}.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(58, move(parse("day25/sample.txt"))).

p12_test() ->
    ?assertEqual(414, p1()).

%%-------------------------------------------------------------------
