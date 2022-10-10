%%-------------------------------------------------------------------
%% @doc Day 15: https://adventofcode.com/2021/day/15
%%-------------------------------------------------------------------

-module(aoc_d15).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(MOVES, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_lines("day15/input.txt")).

p2() ->
    do_p2(aoc:read_lines("day15/input.txt")).

%%-------------------------------------------------------------------

do_p1(L) ->
    {M, MaxX, MaxY} = parse(L),
    find(M, MaxX, MaxY).

do_p2(L) ->
    {M0, MaxX0, MaxY0} = parse(L),
    {M, MaxX, MaxY} = add_tiles(M0, MaxX0, MaxY0, 5, 5),
    find(M, MaxX, MaxY).

find(M, MaxX, MaxY) ->
    Key = {1, 1},
    find(M, {MaxX, MaxY}, #{}, gb_sets:singleton({0, Key}), #{Key => 0}, 0).

find(M, MaxKey = {MaxX, MaxY}, Shortest, Dist0, Costs0, Step) ->
    case gb_sets:take_smallest(Dist0) of
        {{Cost, MaxKey}, _} ->
            Cost;
        {{Cost, Key}, Dist1} ->
            Around = around(Key, MaxX, MaxY),
            {Dist2, Costs2} = lists:foldl(fun (NewKey, Acc = {AccD, AccC}) ->
                case maps:is_key(NewKey, Shortest) of
                    true ->
                        Acc;
                    false ->
                        UpdCost = Cost + maps:get(NewKey, M),
                        OldCost = maps:get(NewKey, AccC, undefined),
                        if
                            OldCost > UpdCost ->
                                {gb_sets:add_element({UpdCost, NewKey}, AccD), AccC#{NewKey => UpdCost}};
                            true ->
                                Acc
                        end
                end
            end, {Dist1, Costs0}, Around),
            find(M, MaxKey, Shortest#{Key => Cost}, Dist2, Costs2, Step + 1)
    end.

around({X, Y}, MaxX, MaxY) ->
    lists:foldl(fun ({Dx, Dy}, Acc) ->
        X2 = X + Dx,
        Y2 = Y + Dy,
        Key = {X2, Y2},
        if
            X2 >= 1, Y2 >= 1, X2 =< MaxX, Y2 =< MaxY ->
                [Key | Acc];
            true ->
                Acc
        end
    end, [], ?MOVES).

add_tiles(M, MaxX, MaxY, TilesX, TilesY) ->
    L = maps:to_list(M),
    L2 = [{{X + Tx * MaxX, Y + Ty * MaxY}, (V + Tx + Ty - 1) rem 9 + 1}
          || {{X, Y}, V} <- L, Tx <- lists:seq(0, TilesX - 1), Ty <- lists:seq(0, TilesY - 1)],
    {maps:from_list(L2), TilesX * MaxX, TilesY * MaxY}.

parse(L = [H | _]) ->
    {DY, M} = lists:foldl(fun (B, {Y, Acc}) ->
        parse_line(B, 1, Y, Acc)
    end, {1, #{}}, L),
    {M, size(H), DY - 1}.

parse_line(<<I, R/binary>>, X, Y, Acc) ->
    parse_line(R, X + 1, Y, Acc#{{X, Y} => I - $0});

parse_line(<<>>, _, Y, Acc) ->
    {Y + 1, Acc}.

%print(M, MaxX, MaxY) ->
%    io:format("~n"),
%    [begin
%        [begin
%            io:format("~p | ", [maps:get({X, Y}, M)])
%        end || X <- lists:seq(1, MaxX)],
%        io:format("~n")
%    end || Y <- lists:seq(1, MaxY)].

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(40, do_p1(aoc:read_lines("day15/sample.txt"))),
    ?assertEqual(315, do_p1(aoc:read_lines("day15/sample5x5.txt"))).

p2_test() ->
    ?assertEqual(315, do_p2(aoc:read_lines("day15/sample.txt"))).

p12_test() ->
    ?assertEqual(390, do_p1(aoc:read_lines("day15/input.txt"))),
    ?assertEqual(2814, do_p2(aoc:read_lines("day15/input.txt"))).

%%-------------------------------------------------------------------
