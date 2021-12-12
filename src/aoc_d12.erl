%%-------------------------------------------------------------------
%% @doc Day 12: https://adventofcode.com/2021/day/12
%%-------------------------------------------------------------------

-module(aoc_d12).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(START, 0).
-define(END, -1).

-record(dict, {
    d = #{
        <<"start">> => ?START,
        <<"end">> => ?END
    },
    hi = 1,
    lo = -2
}).

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_lines("day12/input.txt")).

p2() ->
    do_p2(aoc:read_lines("day12/input.txt")).

%%-------------------------------------------------------------------

do_p1(L) ->
    {Map, _} = create_map(L),
    {_, Cb} = search(Map, ?START, {one_elem(?START), ?START}, 1, undefined, 0),
    Cb.

do_p2(L) ->
    {Map, _} = create_map(L),
    {_, Cb} = search(Map, ?START, {one_elem(?START), undefined}, 1, undefined, 0),
    Cb.

search(Map, Current, Visited, Len, MinLen, Cb) ->
    Next = maps:get(Current, Map, empty()),
    aggregate(Map, sets:to_list(Next), Visited, Len, MinLen, Cb).

aggregate(Map, [?END | Next], Visited, Len, MinLen, Cb) ->
    aggregate(Map, Next, Visited, Len, min(MinLen, Len + 1), Cb + 1);

aggregate(Map, [H | Next], Visited, Len, MinLen1, Cb1) when H > 0 ->
    {MinLen2, Cb2} = search(Map, H, Visited, Len + 1, MinLen1, Cb1),
    aggregate(Map, Next, Visited, Len, min(MinLen1, MinLen2), Cb2);

aggregate(Map, [H | Next], Visited = {Once, Twice}, Len, MinLen1, Cb1) ->
    case sets:is_element(H, Once) of
        true when Twice == undefined, H /= ?START ->
            {MinLen2, Cb2} = search(Map, H, {Once, H}, Len + 1, MinLen1, Cb1),
            aggregate(Map, Next, Visited, Len, min(MinLen1, MinLen2), Cb2);
        true ->
            aggregate(Map, Next, Visited, Len, MinLen1, Cb1);
        false ->
            {MinLen2, Cb2} = search(Map, H, {sets:add_element(H, Once), Twice}, Len + 1, MinLen1, Cb1),
            aggregate(Map, Next, Visited, Len, min(MinLen1, MinLen2), Cb2)
    end;

aggregate(_, [], _, _, MinLen, Cb) ->
    {MinLen, Cb}.

create_map(L) ->
    lists:foldl(fun (B, {AccM, AccD}) ->
        [V1, V2] = binary:split(B, [<<$->>], [trim_all]),
        {I1, AccD1} = encode(V1, AccD),
        {I2, AccD2} = encode(V2, AccD1),
        AccM1 = add_to_map(I1, I2,  AccM),
        AccM2 = add_to_map(I2, I1, AccM1),
        {AccM2, AccD2}
    end, {#{}, #dict{}}, L).

add_to_map(Key, Elem, M) ->
    Val = case maps:get(Key, M, undefined) of
        undefined ->
            one_elem(Elem);
        S1 ->
            sets:add_element(Elem, S1)
    end,
    M#{Key => Val}.

empty() ->
    sets:new([{version, 2}]).

one_elem(Elem) ->
    sets:from_list([Elem], [{version,2}]).

encode(W = <<H, _/binary>>, Dict = #dict{d = D, hi = Up, lo = Lo}) ->
    case maps:get(W, D, undefined) of
        undefined when H >= $A, H =< $Z ->
            {Up, Dict#dict{d = D#{W => Up}, hi = Up + 1}};
        undefined when H >= $a, H =< $z ->
            {Lo, Dict#dict{d = D#{W => Lo}, lo = Lo - 1}};
        Value when is_integer(Value) ->
            {Value, Dict}
    end.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(10, do_p1(aoc:read_lines("day12/sample.txt"))),
    ?assertEqual(19, do_p1(aoc:read_lines("day12/sample1.txt"))),
    ?assertEqual(226, do_p1(aoc:read_lines("day12/sample2.txt"))).

p2_test() ->
    ?assertEqual(36, do_p2(aoc:read_lines("day12/sample.txt"))),
    ?assertEqual(103, do_p2(aoc:read_lines("day12/sample1.txt"))),
    ?assertEqual(3509, do_p2(aoc:read_lines("day12/sample2.txt"))).

p12_test() ->
    ?assertEqual(3450, do_p1(aoc:read_lines("day12/input.txt"))),
    ?assertEqual(96528, do_p2(aoc:read_lines("day12/input.txt"))).

%%-------------------------------------------------------------------
