%%-------------------------------------------------------------------
%% @doc Day 13: https://adventofcode.com/2021/day/13
%%-------------------------------------------------------------------

-module(aoc_d13).
-export([p1/0, p2/0, print/1]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1("day13/input.txt").

p2() ->
    do_p2("day13/input.txt").

%%-------------------------------------------------------------------

do_p1(L) ->
    {M, [Cmd | _]} = parse(L),
    maps:size(fold(M, [Cmd])).

fold(M, [Cmd | Cmds]) ->
    fold(one_fold(M, Cmd), Cmds);

fold(M, []) ->
    M.

one_fold(M, {1, Line}) ->
    maps:fold(fun
        (Key = {X, _}, _, Acc) when X < Line ->
            Acc#{Key => []};
        ({X, Y}, _, Acc) ->
            Acc#{{Line - (X - Line), Y} => []}
    end, #{}, M);

one_fold(M, {2, Line}) ->
    maps:fold(fun
        (Key = {_, Y}, _, Acc) when Y < Line ->
            Acc#{Key => []};
        ({X, Y}, _, Acc) ->
            Acc#{{X, Line - (Y - Line)} => []}
    end, #{}, M).

do_p2(L) ->
    {M, Cmds} = parse(L),
    %%print(fold(M, Cmds)). % 0 for the sample, KJBKEUBG for the input
    maps:size(fold(M, Cmds)).

print(M) ->
    {Xs, Ys} = lists:unzip(maps:keys(M)),
    io:format("~n"),
    [begin
        [begin
            Ch = case maps:is_key({X, Y}, M) of
                true ->
                    $#;
                false ->
                    32
            end,
            io:format("~c", [Ch])
        end || X <- lists:seq(0, lists:max(Xs))],
        io:format("~n")
    end || Y <- lists:seq(0, lists:max(Ys))].

%%-------------------------------------------------------------------

parse(Fn) ->
    [H1, H2] = aoc:read_lines(Fn, [<<10, 10>>], [trim]),
    L1 = binary:split(H1, [<<10>>], [global, trim_all]),
    L2 = binary:split(H2, [<<10>>], [global, trim_all]),
    M = maps:from_list(lists:map(fun (I) ->
        [X, Y] = binary:split(I, [<<$,>>]),
        {{binary_to_integer(X), binary_to_integer(Y)}, []}
    end, L1)),
    Cmds = lists:map(fun
        (<<"fold along x=", R/binary>>) ->
            {1, binary_to_integer(R)};
        (<<"fold along y=", R/binary>>) ->
            {2, binary_to_integer(R)}
    end, L2),
    {M, Cmds}.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(17, do_p1("day13/sample.txt")).

p12_test() ->
    ?assertEqual(788, do_p1("day13/input.txt")),
    ?assertEqual(102, do_p2("day13/input.txt")).

%%-------------------------------------------------------------------
