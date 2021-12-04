%%-------------------------------------------------------------------
%% @doc Day 05, part 1
%%-------------------------------------------------------------------

-module(aoc_d05_1).
-export([p1/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_xy("day05/input.txt")).

%%-------------------------------------------------------------------

do_p1(L0) ->
    L = lists:filter(fun ({X1, Y1, X2, Y2}) ->
        X1 == X2 orelse Y1 == Y2
    end, L0),
    R = p1_loop_out(L, sets:new([{version,2}])),
    sets:size(R).

p1_loop_out([H | T = [_ | _]], Acc) ->
    p1_loop_out(T, p1_loop_in(H, T, Acc));

p1_loop_out(_, Acc) ->
    Acc.

p1_loop_in(P, [Q | T], Acc) ->
    p1_loop_in(P, T, p1_intersect(P, Q, Acc));

p1_loop_in(_, _, Acc) ->
    Acc.

p1_intersect({Px1, Py1, Px2, Py2}, {Qx1, Qy1, Qx2, Qy2}) ->
    {max(Px1, Qx1), max(Py1, Qy1), min(Px2, Qx2), min(Py2, Qy2)}.

p1_intersect(P, Q, Acc) ->
    {Rx1, Ry1, Rx2, Ry2} = R = p1_intersect(P, Q),
    if
        Rx1 > Rx2; Ry1 > Ry2 ->
            Acc;
        true ->
            p1_union(R, Acc)
    end.

p1_union({X, Y1, X, Y2}, S1) ->
    S2 = sets:from_list([{X, Y} || Y <- lists:seq(Y1, Y2)], [{version,2}]),
    sets:union(S1, S2);

p1_union({X1, Y, X2, Y}, S1) ->
    S2 = sets:from_list([{X, Y} || X <- lists:seq(X1, X2)], [{version,2}]),
    sets:union(S1, S2);

p1_union({X1, Y1, X2, Y2}, S1) when (X2 - X1) == (Y2 - Y1) ->
    Points = lists:zip(lists:seq(X1, X2), lists:seq(Y1, Y2)),
    S2 = sets:from_list(Points, [{version,2}]),
    sets:union(S1, S2).

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(5585, do_p1(aoc:read_xy("day05/input.txt"))),
    ?assertEqual(5, do_p1(aoc:read_xy("day05/sample.txt"))),
    ?assertEqual(5, do_p1(aoc:read_xy("day05/sample2.txt"))).

%%-------------------------------------------------------------------
