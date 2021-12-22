%%-------------------------------------------------------------------
%% @doc Day 22: https://adventofcode.com/2021/day/22
%%-------------------------------------------------------------------

-module(aoc_d22).
-export([p1/0, p2/0, sample11/0, sample12/0, sample13/0, sample21/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    reboot(parse1(aoc:read_lines("day22/input.txt"))).

p2() ->
    reboot(parse2(aoc:read_lines("day22/input.txt"))).

sample11() ->
    reboot(parse1(aoc:read_lines("day22/sample.txt"))).

sample12() ->
    reboot(parse1(aoc:read_lines("day22/sample2.txt"))).

sample13() ->
    reboot(parse1(aoc:read_lines("day22/sample3.txt"))).

sample21() ->
    reboot(parse2(aoc:read_lines("day22/sample3.txt"))).

%%-------------------------------------------------------------------

reboot(Cmds) ->
    p1_loop(Cmds, [], 0).

%%-------------------------------------------------------------------

p1_loop([{_, []} | Cmds], Cubes, Sum) ->
    p1_loop(Cmds, Cubes, Sum);

p1_loop([{OnOff, P} | Cmds], Cubes, Sum) ->
    Init = case OnOff of
        1 ->
            {[{1, P}], sz(P) + Sum};
        0 ->
            {[], Sum}
    end,
    {NewCubes, Sum2} = lists:foldl(fun (Rec, Acc) ->
        account(P, Rec, Acc)
    end, Init, Cubes),
    p1_loop(Cmds, NewCubes ++ Cubes, Sum2);

p1_loop(_, _, Sum) ->
    Sum.

%%-------------------------------------------------------------------

account(P, {Sign, Q}, I = {AccL, AccSz}) ->
    case intersect(P, Q) of
        [] ->
            I;
        R ->
            %io:format("Add (~w): ~w~n", [- Sign * sz(R), {- Sign, R}]),
            {[{- Sign, R} | AccL], AccSz - Sign * sz(R)}
    end.

intersect([Px1, Px2, Py1, Py2, Pz1, Pz2], [Qx1, Qx2, Qy1, Qy2, Qz1, Qz2]) ->
    check_empty([max(Px1, Qx1), min(Px2, Qx2), max(Py1, Qy1), min(Py2, Qy2), max(Pz1, Qz1), min(Pz2, Qz2)]).

check_empty([Rx1, Rx2, Ry1, Ry2, Rz1, Rz2]) when Rx1 > Rx2; Ry1 > Ry2; Rz1 > Rz2 ->
    [];

check_empty(R) ->
    R.

sz([Px1, Px2, Py1, Py2, Pz1, Pz2]) ->
    (Px2 - Px1 + 1) * (Py2 - Py1 + 1) * (Pz2 - Pz1 + 1).

%%-------------------------------------------------------------------

parse1(Lines) ->
    parse(Lines, [-50, 50, -50, 50, -50, 50]).

parse2(Lines) ->
    parse(Lines, []).

parse(Lines, MinMax) ->
    lists:map(fun
        (<<"on ", B/binary>>) ->
            {1, parse_coords(B, MinMax)};
        (<<"off ", B/binary>>) ->
            {0, parse_coords(B, MinMax)}
    end, Lines).

parse_coords(B, MinMax) ->
    [<<"x=", XB/binary>>, <<"y=", YB/binary>>, <<"z=", ZB/binary>>] = binary:split(B, [<<$,>>], [global]),
    R = lists:append([parse_coord(XB), parse_coord(YB), parse_coord(ZB)]),
    case MinMax of
        [] ->
            R;
        _ ->
            intersect(MinMax, R)
    end.

parse_coord(B) ->
    [X1, X2] = binary:split(B, [<<"..">>]),
    [binary_to_integer(X1), binary_to_integer(X2)].

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(39, sample11()),
    ?assertEqual(590784, sample12()),
    ?assertEqual(474140, sample13()),
    ?assertEqual(2758514936282235, sample21()),
    ?assertEqual(580098, p1()),
    ?assertEqual(1134725012490723, p2()).

%%-------------------------------------------------------------------
