%%-------------------------------------------------------------------
%% @doc Day 17: https://adventofcode.com/2021/day/17
%%-------------------------------------------------------------------

-module(aoc_d17).
-export([p1/0, p2/0]).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_ints("day17/input.txt")).

p2() ->
    do_p2(aoc:read_ints("day17/input.txt")).

%%-------------------------------------------------------------------

do_p1([_, _, Sy1, Sy2]) when Sy2 >= Sy1 ->
    Ts = gen_pos(fun ty_pos/3, -Sy2, -Sy1, #{}),
    {_, Ys} = lists:max(maps:to_list(Ts)),
    {Y, _} = lists:max(Ys),
    dist(Y).

do_p2(L = [Sx1, Sx2, Sy1, Sy2]) when Sx2 >= Sx1, Sy2 >= Sy1 ->
    {_, Size} = p2_data(L),
    Size.

%%-------------------------------------------------------------------

p2_data([Sx1, Sx2, Sy1, Sy2]) when Sx2 >= Sx1, Sy2 >= Sy1 ->
    YTs = gen_pos(fun tx_pos/3, -Sy2, -Sy1, #{}),
    XTs = gen_pos(fun tx_pos/3, Sx1, Sx2, #{}),
    M2 = maps:fold(fun (T, Lx, Acc) ->
        case maps:get(T, YTs, undefined) of
            undefined ->
                Acc;
            Ly ->
                sets:union(Acc, sets:from_list([{X, -Y} || {X, _} <- Lx, {_, Y} <- Ly], [{version,2}]))
        end
    end, sets:new([{version, 2}]), XTs),
    %
    Ts = gen_pos(fun ty_pos/3, -Sy2, -Sy1, #{}),
    M1 = maps:fold(fun (T, Ly, Acc) ->
        case maps:get(T, XTs, undefined) of
            undefined ->
                maps:fold(fun
                    (Tx, Lx0, AccIn) when Tx < T ->
                        case [X || {X, 0} <- Lx0] of
                            [] ->
                                AccIn;
                            Lx ->
                                sets:union(AccIn, sets:from_list([{X, Y} || X <- Lx, {Y, _} <- Ly], [{version,2}]))
                        end;
                    (_, _, AccIn) ->
                        AccIn
                end, Acc, XTs);
            Lx ->
                sets:union(Acc, sets:from_list([{X, Y} || {X, _} <- Lx, {Y, _} <- Ly], [{version,2}]))
        end
    end, M2, Ts),
    %
    {M1, sets:size(M1)}.

%%-------------------------------------------------------------------

gen_pos(F, S1, Pos, Acc) when Pos >= S1 ->
    gen_pos(F, S1, Pos - 1, F(1, Pos, Acc));

gen_pos(_, _, _, Acc) ->
    maps:map(fun (_, M) -> maps:keys(M) end, Acc).

ty_pos(T, Pos, Acc) ->
    V = vy(T, Pos),
    if
        V > 0 ->
            VI = trunc(V),
            Acc2 = if
                V - VI == 0.0 ->
                    Key = 2 * VI + 1 + T,
                    D = maps:get(Key, Acc, #{}),
                    Acc#{Key => D#{{VI, VI + T} => []}};
                true ->
                    Acc
            end,
            ty_pos(T + 1, Pos, Acc2);
        true ->
            Acc
    end.

vy(T, Pos) ->
    Pos / T - (T + 1) / 2.

tx_pos(T, Pos, Acc) ->
    V = vx(T, Pos),
    if
        V >= T - 1 ->
            VI = trunc(V),
            Acc2 = if
                V - VI == 0.0 ->
                    D = maps:get(T, Acc, #{}),
                    Acc#{T => D#{{VI, VI - T + 1} => []}};
                true ->
                    Acc
            end,
            tx_pos(T + 1, Pos, Acc2);
        true ->
            Acc
    end.

vx(T, Pos) ->
    Pos / T + (T - 1) / 2.

dist(V) ->
    V * (V + 1) div 2.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(45, do_p1([20, 30, -10, -5])),
    ?assertEqual(9180, p1()).

p2_test() ->
    Lines = aoc:read_lines("day17/sample.txt", [<<10>>, <<32>>], [global, trim_all]),
    LPairs = [begin [X, Y] = binary:split(B, <<$,>>), {binary_to_integer(X), binary_to_integer(Y)} end || B <- Lines],
    Pairs = sets:from_list(LPairs, [{version,2}]),
    {Pairs, 112} = p2_data([20, 30, -10, -5]),
    ?assertEqual(3767, p2()).

%%-------------------------------------------------------------------
