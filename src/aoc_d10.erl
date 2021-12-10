%%-------------------------------------------------------------------
%% @doc Day 10: https://adventofcode.com/2021/day/10
%%-------------------------------------------------------------------

-module(aoc_d10).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_lines("day10/input.txt")).

p2() ->
    do_p2(aoc:read_lines("day10/input.txt")).

%%-------------------------------------------------------------------

do_p1(L) ->
    lists:foldl(fun (Line, Acc) ->
        case p1_score(Line, []) of
            {'p1', N} ->
                N + Acc;
            {'p2', _} ->
                Acc
        end
    end, 0, L).

do_p2(L) ->
    Scores = lists:foldl(fun (Line, Acc) ->
        case p1_score(Line, []) of
            {'p1', _} ->
                Acc;
            {'p2', N} ->
                [N | Acc]
        end
    end, [], L),
    lists:nth(length(Scores) div 2 + 1, lists:sort(Scores)).

p1_score(<<I, R/binary>>, Stack) when I == $(; I == $[; I == ${; I == $< ->
    p1_score(R, [I | Stack]);

p1_score(<<I1, R/binary>>, [I2 | Stack]) ->
    case match(I1) of
        I2 ->
            p1_score(R, Stack);
        Error ->
            {'p1', score(Error)}
    end;

p1_score(<<_, _/binary>>, []) ->
    error(unexpected_eol);

p1_score(<<>>, Stack) ->
    {'p2', auto_score(Stack, 0)}.

score($() ->
    3;
score($[) ->
    57;
score(${) ->
    1197;
score($<) ->
    25137.

match($)) ->
    $(;
match($]) ->
    $[;
match($}) ->
    ${;
match($>) ->
    $<.

auto_score([$( | L], Acc) ->
    auto_score(L, Acc * 5 + 1);
auto_score([$[ | L], Acc) ->
    auto_score(L, Acc * 5 + 2);
auto_score([${ | L], Acc) ->
    auto_score(L, Acc * 5 + 3);
auto_score([$< | L], Acc) ->
    auto_score(L, Acc * 5 + 4);
auto_score([], Acc) ->
    Acc.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(26397, do_p1(aoc:read_lines("day10/sample.txt"))).

p2_test() ->
    ?assertEqual(288957, do_p2(aoc:read_lines("day10/sample.txt"))).

p12_test() ->
    ?assertEqual(268845, do_p1(aoc:read_lines("day10/input.txt"))),
    ?assertEqual(4038824534, do_p2(aoc:read_lines("day10/input.txt"))).

%%-------------------------------------------------------------------
