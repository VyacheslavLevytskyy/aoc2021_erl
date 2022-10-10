%%-------------------------------------------------------------------
%% @doc Day 21: https://adventofcode.com/2121/day/21
%%-------------------------------------------------------------------

-module(aoc_d21).
-export([p1/0, p2/0, sample1/0, sample2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(CASES, [{3,1},{4,3},{5,6},{6,7},{7,6},{8,3},{9,1}]).

%%-------------------------------------------------------------------

p1() ->
    play1(1, 5, 10, 100, 3, 1000).

p2() ->
    play2(1, 5, 10, 21).

%%-------------------------------------------------------------------

sample1() ->
    play1(4, 8, 10, 100, 3, 1000).

sample2() ->
    play2(4, 8, 10, 21).

%%-------------------------------------------------------------------

play1(Pos1, Pos2, BoardMod, DiceMod, N, Win) ->
    do_play1({Pos1, 0}, {Pos2, 0}, {1, DiceMod}, 0, BoardMod, N, Win).

do_play1({A, VA}, {B, VB}, {Dice, DiceMod}, Move, BoardMod, N, Win) ->
    {D1, Dice1} = dice1(Dice, N, DiceMod),
    A2 = mod(A, D1, BoardMod),
    VA2 = VA + A2,
    if
        VA2 >= Win ->
            VB * (Move + N);
        true ->
            {D2, Dice2} = dice1(Dice1, N, DiceMod),
            B2 = mod(B, D2, BoardMod),
            VB2 = VB + B2,
            if
                VB2 >= Win ->
                    VA2 * (Move + 2 * N);
                true ->
                    do_play1({A2, VA2}, {B2, VB2}, {Dice2, DiceMod}, Move + 2 * N, BoardMod, N, Win)
            end
    end.

dice1(Dice, N, DiceMod) ->
    {lists:sum([mod(Dice, I, DiceMod) || I <- lists:seq(0, N - 1)]), mod(Dice, N, DiceMod)}.

%%-------------------------------------------------------------------

play2(Pos1, Pos2, BoardMod, Win) ->
    do_play2(#{{Pos1, 0, Pos2, 0} => 1}, {0, 0}, BoardMod, Win).

do_play2(States, Wins, BoardMod, WinSum) when map_size(States) > 0 ->
    {States1, Wins1} = lists:foldl(fun ({D, DN}, {S1, W1}) ->
        maps:fold(fun ({A, VA, B, VB}, SN, {S2, W2 = {WA2, WB2}}) ->
            Inc = DN * SN,
            A2 = mod(A, D, BoardMod),
            VA2 = VA + A2,
            if
                VA2 >= WinSum ->
                    {S2, {WA2 + Inc, WB2}};
                true ->
                    {inc({A2, VA2, B, VB}, Inc, S2), W2}
            end
        end, {S1, W1}, States)
    end, {#{}, Wins}, ?CASES),
    {States2, Wins2} = lists:foldl(fun ({D, DN}, {S1, W1}) ->
        maps:fold(fun ({A, VA, B, VB}, SN, {S2, W2 = {WA2, WB2}}) ->
            Inc = DN * SN,
            B2 = mod(B, D, BoardMod),
            VB2 = VB + B2,
            if
                VB2 >= WinSum ->
                    {S2, {WA2, WB2 + Inc}};
                true ->
                    {inc({A, VA, B2, VB2}, Inc, S2), W2}
            end
        end, {S1, W1}, States1)
    end, {#{}, Wins1}, ?CASES),
    %
    do_play2(States2, Wins2, BoardMod, WinSum);

do_play2(_, {A, B}, _, _) ->
    max(A, B).

%%-------------------------------------------------------------------

inc(Key, Inc, Acc) ->
    Acc#{Key => Inc + maps:get(Key, Acc, 0)}.

mod(X1, X2, Lim) ->
    (X1 + X2 - 1) rem Lim + 1.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(739785, sample1()),
    ?assertEqual(432450, p1()).

p2_test() ->
    ?assertEqual(444356092776315, sample2()),
    ?assertEqual(138508043837521, p2()).

p12_test() ->
    ?assertEqual(432450, p1()),
    ?assertEqual(138508043837521, p2()).

%%-------------------------------------------------------------------
