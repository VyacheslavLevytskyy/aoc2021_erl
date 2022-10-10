%%-------------------------------------------------------------------
%% @doc Day 03: https://adventofcode.com/2021/day/3
%%-------------------------------------------------------------------

-module(aoc_d03).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_lines("day03/input.txt")).

p2() ->
    do_p2(aoc:read_lines("day03/input.txt")).

%%-------------------------------------------------------------------

do_p1(Lines) ->
    do_p1(Lines, {}, 0).

do_p1([Line | Lines], Acc, NLines) ->
    do_p1(Lines, update_acc1(Line, Acc, 1), 1 + NLines);

do_p1([], {}, _) ->
    0;

do_p1([], Counts, NLines) ->
    calc_power(Counts, NLines div 2, 1, size(Counts), 0, 0).

update_acc1(<<"1", R/binary>>, Acc, Pos) when size(Acc) < Pos ->
    update_acc1(R, erlang:append_element(Acc, 1), 1 + Pos);

update_acc1(<<"1", R/binary>>, Acc, Pos) ->
    update_acc1(R, setelement(Pos, Acc, 1 + element(Pos, Acc)), 1 + Pos);

update_acc1(<<"0", R/binary>>, Acc, Pos) ->
    update_acc1(R, Acc, 1 + Pos);

update_acc1(<<>>, Acc, _) ->
    Acc.

calc_power(Counts, Lim, Pos, MaxPos, Gamma, Eps) when Pos =< MaxPos ->
    Bit = if
        element(Pos, Counts) > Lim ->
            1;
        true ->
            0
    end,
    calc_power(Counts, Lim, 1 + Pos, MaxPos, Gamma bsl 1 + Bit, Eps bsl 1 + 1 - Bit);

calc_power(_, _, _, _, Gamma, Eps) ->
    Gamma * Eps.

%%-------------------------------------------------------------------

do_p2(Lines) ->
    {L, Sz} = lists:mapfoldl(fun (Line, Acc) ->
        {binary_to_integer(Line, 2), max(Acc, size(Line))}
    end, 0, Lines),
    O = p2_loop(L, 1 bsl (Sz - 1), true),
    CO2 = p2_loop(L, 1 bsl (Sz - 1), false),
    O * CO2.

p2_loop(L = [_ | _], Mask, Most) ->
    IsOne = p2_bit(L, Mask, Most),
    L2 = lists:filter(fun (I) -> (I band Mask == 0) xor IsOne end, L),
    case L2 of
        [Value] ->
            Value;
        _ ->
            p2_loop(L2, Mask bsr 1, Most)
    end;

p2_loop(_, 0, _) ->
    error(multiple_values);

p2_loop([], _, _) ->
    error(empty_list).

p2_bit(L, Mask, Most) ->
    {Sum, Len} = lists:foldl(fun (I, {AccSum, AccLen}) ->
        V = case I band Mask of
            0 ->
                0;
            _ ->
                1
        end,
        {V + AccSum, 1 + AccLen}
    end, {0, 0}, L),
    Lim = Len / 2,
    if
        Sum < Lim ->
            not Most;
        true ->
            Most
    end.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(198, do_p1(p1_sample())).

p2_test() ->
    ?assertEqual(230, do_p2(p1_sample())).

p12_test() ->
    ?assertEqual(3912944, p1()),
    ?assertEqual(4996233, p2()).

p1_sample() ->
    [
        <<"00100">>,
        <<"11110">>,
        <<"10110">>,
        <<"10111">>,
        <<"10101">>,
        <<"01111">>,
        <<"00111">>,
        <<"11100">>,
        <<"10000">>,
        <<"11001">>,
        <<"00010">>,
        <<"01010">>
    ].

%%-------------------------------------------------------------------
