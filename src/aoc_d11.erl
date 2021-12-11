%%-------------------------------------------------------------------
%% @doc Day 11: https://adventofcode.com/2021/day/11
%%-------------------------------------------------------------------

-module(aoc_d11).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(EXPLODE, 10).
-define(MAX_STEPS, 100).
-define(SIZE, 100).

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_lines("day11/input.txt")).

p2() ->
    do_p2(aoc:read_lines("day11/input.txt")).

%%-------------------------------------------------------------------

do_p1(L) ->
    p1_steps(parse(L), 0, 1, ?MAX_STEPS).

do_p2(L) ->
    p2_steps(parse(L), 1).

p1_steps(M1, Cb1, No, Max) when No =< Max ->
    {M2, Cb2} = explode([], inc(M1), Cb1),
    p1_steps(M2, Cb2, No + 1, Max);

p1_steps(_, Cb, _, _) ->
    Cb.

p2_steps(M1, No) ->
    {M2, _} = explode([], inc(M1), 0),
    case length(maps:get(0, M2, [])) of
        ?SIZE ->
            No;
        _ ->
            p2_steps(M2, No + 1)
    end.

explode([Elem1 = {X1, Y1} | T], M, Cb) ->
    M2 = maps:fold(fun(Key, L, Acc) ->
        lists:foldl(fun (Elem2 = {X2, Y2}, InAcc) ->
            DX = abs(X2 - X1),
            DY = abs(Y2 - Y1),
            NewKey = if
                Key > 0, Key < ?EXPLODE, DX < 2, DY < 2 ->
                    Key + 1;
                true ->
                    Key
            end,
            add(NewKey, Elem2, InAcc)
        end, Acc, L)
    end, #{}, M),
    explode(T, add(0, Elem1, M2), 1 + Cb);

explode([], M, Cb) ->
    case maps:get(?EXPLODE, M, []) of
        [] ->
            {M, Cb};
        L ->
            explode(L, M#{?EXPLODE => []}, Cb)
    end.

add(Key, Elem, M) ->
    M#{Key => [Elem | maps:get(Key, M, [])]}.

inc(M) ->
    maps:fold(fun (Key, L, Acc) ->
        Acc#{Key + 1 => L}
    end, #{}, M).

parse(L) ->
    {_, M} = lists:foldl(fun (B, {Y, Acc}) ->
        parse_line(B, 1, Y, Acc)
    end, {1, #{}}, L),
    M.

parse_line(<<I, R/binary>>, X, Y, Acc) ->
    Key = I - $0,
    L = maps:get(Key, Acc, []),
    parse_line(R, X + 1, Y, Acc#{Key => [{X, Y} | L]});

parse_line(<<>>, _, Y, Acc) ->
    {Y + 1, Acc}.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(1656, do_p1(aoc:read_lines("day11/sample.txt"))).

p2_test() ->
    ?assertEqual(195, do_p2(aoc:read_lines("day11/sample.txt"))).

p12_test() ->
    ?assertEqual(1627, do_p1(aoc:read_lines("day11/input.txt"))),
    ?assertEqual(329, do_p2(aoc:read_lines("day11/input.txt"))).

%%-------------------------------------------------------------------
