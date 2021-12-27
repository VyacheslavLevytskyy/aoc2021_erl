%%-------------------------------------------------------------------
%% @doc Day 24: https://adventofcode.com/2021/day/24
%%-------------------------------------------------------------------

-module(aoc_d24).
-export([p1/0, p2/0,
         exec/1, exec/2]).

-include_lib("eunit/include/eunit.hrl").

-define(EMPTY, erlang:make_tuple(14, [])).

%%-------------------------------------------------------------------
%% Solution

p1() ->
    Rules = link_positions(1, params(), [], #{}),
    gen_minmax(fun mx/1, Rules, 14, ?EMPTY, 1).

p2() ->
    Rules = link_positions(1, params(), [], #{}),
    gen_minmax(fun mn/1, Rules, 14, ?EMPTY, 1).

gen_minmax(Choice, Rules, Pos, Acc, Cb) when Cb =< 7 ->
    case element(Pos, Acc) of
        [] ->
            {LinkedPos, Add} = maps:get(Pos, Rules),
            PosVal = Choice(-Add),
            Acc2 = setelement(LinkedPos, setelement(Pos, Acc, PosVal), PosVal - Add),
            gen_minmax(Choice, Rules, Pos - 1, Acc2, Cb + 1);
        _ ->
            gen_minmax(Choice, Rules, Pos - 1, Acc, Cb)
    end;

gen_minmax(_, _, _, Acc, 8) ->
    to_int(tuple_to_list(Acc), 0).

mx(V) when V < 0 -> 9;
mx(V) -> 9 - V.

mn(V) when V > 0 -> 1;
mn(V) -> 1 - V.

link_positions(Pos, [{1, _, V3} | T], Acc, Rules) ->
    link_positions(Pos + 1, T, [{Pos, V3} | Acc], Rules);

link_positions(Pos, [{26, V2, _} | T], [{Pos0, V30} | Acc], Rules) ->
    link_positions(Pos + 1, T, Acc, Rules#{Pos => {Pos0, V30 + V2}, Pos0 => {Pos, - V30 - V2}});

link_positions(_, [], [], Rules) ->
    Rules.

to_int([H | T], V) ->
    to_int(T, V * 10 + H);

to_int([], V) ->
    V.

%%-------------------------------------------------------------------
%% Research

exec(Inp) ->
    try
        #{<<"z">> := Z} = exec(code(), Inp),
        Z == 0
    catch
        Class:Reason:Stacktrace ->
            {error, Class, Reason, Stacktrace}
    end.

exec(Cmds, Inp) ->
    exec(Cmds, #{<<"w">> => 0, <<"x">> => 0, <<"y">> => 0, <<"z">> => 0}, Inp).

exec([{<<"inp">>, Name} | Cmds], Vars, [Value | In]) when is_integer(Value) ->
    exec(Cmds, Vars#{Name => Value}, In);

exec([{Op, Name, Op2} | Cmds], Vars, In) ->
    exec(Cmds, Vars#{Name => op(Op, maps:get(Name, Vars), val(Op2, Vars))}, In);

exec([], Vars, []) ->
    Vars.

op(<<"add">>, V1, V2) ->
    V1 + V2;
op(<<"mul">>, V1, V2) ->
    V1 * V2;
op(<<"div">>, V1, V2) ->
    V1 div V2;
op(<<"mod">>, V1, V2) ->
    V1 rem V2;
op(<<"eql">>, V, V) ->
    1;
op(<<"eql">>, _, _) ->
    0.

val(Num, _) when is_integer(Num) ->
    Num;

val(Id, Vars) ->
    maps:get(Id, Vars).

code() ->
    [begin
        case C of
            {Cmd, Var, {No}} ->
                {Cmd, Var, element(No, P)};
            _ ->
                C
        end
    end || P <- params(), C <- code0()].

code0() ->
[
    {<<"inp">>, <<"w">>},
    {<<"mul">>, <<"x">>, 0},
    {<<"add">>, <<"x">>, <<"z">>},
    {<<"mod">>, <<"x">>, 26},
    {<<"div">>, <<"z">>, {1}},
    {<<"add">>, <<"x">>, {2}},
    {<<"eql">>, <<"x">>, <<"w">>},
    {<<"eql">>, <<"x">>, 0},
    {<<"mul">>, <<"y">>, 0},
    {<<"add">>, <<"y">>, 25},
    {<<"mul">>, <<"y">>, <<"x">>},
    {<<"add">>, <<"y">>, 1},
    {<<"mul">>, <<"z">>, <<"y">>},
    {<<"mul">>, <<"y">>, 0},
    {<<"add">>, <<"y">>, <<"w">>},
    {<<"add">>, <<"y">>, {3}},
    {<<"mul">>, <<"y">>, <<"x">>},
    {<<"add">>, <<"z">>, <<"y">>}
].

params() ->
[
    {1, 10, 0},
    {1, 12, 6},
    {1, 13, 4},
    {1, 13, 2},
    {1, 14, 9},
    {26, -2, 1},
    {1, 11, 10},
    {26, -15, 6},
    {26, -10, 4},
    {1, 10, 6},
    {26, -10, 3},
    {26, -4, 9},
    {26, -1, 15},
    {26, -1, 5}
].

%%-------------------------------------------------------------------
%% Tests

p1_test() ->
    ?assertEqual(94992994195998, aoc_d24:p1()),
    ?assertEqual(21191861151161, aoc_d24:p2()),
    ?assertEqual(#{<<"w">> => 0,<<"x">> => 30,<<"y">> => 0,<<"z">> => 1}, exec([
        {<<"inp">>, <<"z">>},
        {<<"inp">>, <<"x">>},
        {<<"mul">>, <<"z">>, 3},
        {<<"eql">>, <<"z">>, <<"x">>}
    ], [10, 30])),
    ?assertEqual(#{<<"w">> => 1,<<"x">> => 1,<<"y">> => 1,<<"z">> => 1}, exec([
        {<<"inp">>, <<"w">>},
        {<<"add">>, <<"z">>, <<"w">>},
        {<<"mod">>, <<"z">>, 2},
        {<<"div">>, <<"w">>, 2},
        {<<"add">>, <<"y">>, <<"w">>},
        {<<"mod">>, <<"y">>, 2},
        {<<"div">>, <<"w">>, 2},
        {<<"add">>, <<"x">>, <<"w">>},
        {<<"mod">>, <<"x">>, 2},
        {<<"div">>, <<"w">>, 2},
        {<<"mod">>, <<"w">>, 2}
    ], [15])),
    ?assertEqual(#{<<"w">> => 0,<<"x">> => 1,<<"y">> => 0,<<"z">> => 1}, exec([
        {<<"inp">>, <<"w">>},
        {<<"add">>, <<"z">>, <<"w">>},
        {<<"mod">>, <<"z">>, 2},
        {<<"div">>, <<"w">>, 2},
        {<<"add">>, <<"y">>, <<"w">>},
        {<<"mod">>, <<"y">>, 2},
        {<<"div">>, <<"w">>, 2},
        {<<"add">>, <<"x">>, <<"w">>},
        {<<"mod">>, <<"x">>, 2},
        {<<"div">>, <<"w">>, 2},
        {<<"mod">>, <<"w">>, 2}
    ], [5])).

%%-------------------------------------------------------------------
