%%-------------------------------------------------------------------
%% @doc Day 04: https://adventofcode.com/2021/day/04
%%-------------------------------------------------------------------

-module(aoc_d04).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(ROWS, 5).
-define(COLS, 5).

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_bingo("day04/input.txt", ?ROWS, ?COLS)).

p2() ->
    do_p2(aoc:read_bingo("day04/input.txt", ?ROWS, ?COLS)).

%%-------------------------------------------------------------------

do_p1({Input, Numbers, Cards, Wins}) ->
    p1_loop(Input, Numbers, Cards, Wins, #{}).

p1_loop([Draw | Input], Numbers, Cards, Wins, Past) ->
    case maps:is_key(Draw, Past) of
        true ->
            p1_loop(Input, Numbers, Cards, Wins, Past);
        false ->
            case maps:get(Draw, Numbers, undefined) of
                undefined ->
                    p1_loop(Input, Numbers, Cards, Wins, Past#{Draw => []});
                Coords ->
                    case draw_number1(Draw, Coords, Cards, Wins) of
                        {win, R} ->
                            R;
                        {cont, Cards2, Wins2} ->
                            p1_loop(Input, maps:remove(Draw, Numbers), Cards2, Wins2, Past#{Draw => []})
                    end
            end
    end;

p1_loop([], _, _, _, _) ->
    error(no_winner).

draw_number1(Draw, [{CardNo, Pos} | Coords], Cards, Wins) ->
    Unmarked2 = sets:del_element(Draw, maps:get(CardNo, Cards)),
    Cards2 = Cards#{CardNo => Unmarked2},
    KeyR = {r, CardNo, 1 + (Pos - 1) div ?COLS},
    case 1 + maps:get(KeyR, Wins, 0) of
        ?COLS ->
            {win, win_score(Draw, Unmarked2)};
        CbR ->
            KeyC = {c, CardNo, 1 + (Pos - 1) rem ?ROWS},
            case 1 + maps:get(KeyC, Wins, 0) of
                ?ROWS ->
                    {win, win_score(Draw, Unmarked2)};
                CbC ->
                    Wins2 = Wins#{KeyR => CbR, KeyC => CbC},
                    draw_number1(Draw, Coords, Cards2, Wins2)
            end
    end;

draw_number1(_, [], Cards, Wins) ->
    {cont, Cards, Wins}.

win_score(Draw, Unmarked) ->
    Draw * lists:sum(sets:to_list(Unmarked)).

%%-------------------------------------------------------------------

do_p2({Input, Numbers, Cards, Wins}) ->
    p2_loop(Input, Numbers, Cards, Wins, #{}).

p2_loop([Draw | Input], Numbers, Cards, Wins, Past) ->
    case maps:is_key(Draw, Past) of
        true ->
            p2_loop(Input, Numbers, Cards, Wins, Past);
        false ->
            Past2 = Past#{Draw => []},
            case maps:get(Draw, Numbers, undefined) of
                undefined ->
                    p2_loop(Input, Numbers, Cards, Wins, Past2);
                [] ->
                    p2_loop(Input, Numbers, Cards, Wins, Past2);
                Coords ->
                    case draw_number2(Draw, Coords, Numbers, Cards, Wins) of
                        {win, R} ->
                            R;
                        {cont, Numbers2, Cards2, Wins2} ->
                            p2_loop(Input, maps:remove(Draw, Numbers2), Cards2, Wins2, Past2)
                    end
            end
    end;

p2_loop([], _, _, _, _) ->
    error(no_winner).

%%-------------------------------------------------------------------

draw_number2(Draw, [{CardNo, Pos} | Coords], Numbers, Cards, Wins) ->
    Unmarked2 = sets:del_element(Draw, maps:get(CardNo, Cards)),
    Cards1 = Cards#{CardNo => Unmarked2},
    KeyR = {r, CardNo, 1 + (Pos - 1) div ?COLS},
    case 1 + maps:get(KeyR, Wins, 0) of
        ?COLS when map_size(Cards1) == 1 ->
            {win, win_score(Draw, Unmarked2)};
        ?COLS ->
            {Numbers2, Cards2, Wins2} = filter_out(CardNo, Numbers, Cards1, Wins),
            draw_number2(Draw, Coords, Numbers2, Cards2, Wins2);
        CbR ->
            KeyC = {c, CardNo, 1 + (Pos - 1) rem ?ROWS},
            case 1 + maps:get(KeyC, Wins, 0) of
                ?ROWS when map_size(Cards1) == 1 ->
                    {win, win_score(Draw, Unmarked2)};
                ?ROWS ->
                    {Numbers2, Cards2, Wins2} = filter_out(CardNo, Numbers, Cards1, Wins),
                    draw_number2(Draw, Coords, Numbers2, Cards2, Wins2);
                CbC ->
                    Wins2 = Wins#{KeyR => CbR, KeyC => CbC},
                    draw_number2(Draw, Coords, Numbers, Cards1, Wins2)
            end
    end;

draw_number2(_, [], Numbers, Cards, Wins) ->
    {cont, Numbers, Cards, Wins}.

filter_out(CardNo, Numbers, Cards, Wins) ->
    Cards2 = maps:remove(CardNo, Cards),
    Numbers2 = maps:map(fun (_, VCoords) ->
        lists:filter(fun ({No, _}) -> No /= CardNo end, VCoords)
    end, Numbers),
    Wins2 = maps:filter(fun ({_, No, _}, _) ->
        No /= CardNo
    end, Wins),
    {Numbers2, Cards2, Wins2}.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(4512, do_p1(aoc:read_bingo("day04/sample.txt", ?ROWS, ?COLS))).

p2_test() ->
    ?assertEqual(1924, do_p2(aoc:read_bingo("day04/sample.txt", ?ROWS, ?COLS))).

p12_test() ->
    ?assertEqual(38913, p1()),
    ?assertEqual(16836, p2()).

%%-------------------------------------------------------------------
