%%-------------------------------------------------------------------
%% @doc Day 18: https://adventofcode.com/2021/day/18
%%-------------------------------------------------------------------

-module(aoc_d18).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(THRESHOLD, 10).
-define(MAX_LVL, 4).

%%-------------------------------------------------------------------

p1() ->
    mag(fold(parse(aoc:read_lines("day18/input.txt")))).

p2() ->
    Nums = parse(aoc:read_lines("day18/input.txt")),
    Pairs = [{I1, I2} || I1 <- Nums, I2 <- Nums -- [I1]],
    Mags = [mag(eval_loop(Node)) || Node <- Pairs],
    lists:max(Mags).

%%-------------------------------------------------------------------

fold([H | T]) ->
    lists:foldl(fun (V2, V1) -> eval_loop({V1, V2}) end, H, T).

eval_loop(Node) ->
    case eval_explode(1, Node) of
        {_, undefined} ->
            case eval_split(1, Node) of
                {_, undefined} ->
                    Node;
                {Node1, _} ->
                    eval_loop(Node1)
            end;
        {Node2, _} ->
            eval_loop(Node2)
    end.

eval_explode(Lvl, {{N1, N2}, R}) when Lvl >= ?MAX_LVL, is_integer(N1), is_integer(N2) ->
    {{0, eval_add(R, {'r', N2})}, {'l', N1}};

eval_explode(Lvl, {L, {N1, N2}}) when Lvl >= ?MAX_LVL, is_integer(N1), is_integer(N2) ->
    {{eval_add(L, {'l', N1}), 0}, {'r', N2}};

eval_explode(_, N) when is_integer(N) ->
    {N, undefined};

eval_explode(Lvl, {L, R}) ->
    case eval_explode(Lvl + 1, L) of
        {L1, undefined} ->
            case eval_explode(Lvl + 1, R) of
                {R1, undefined} ->
                    {{L1, R1}, undefined};
                {R1, 'redo'} ->
                    {{L1, R1}, 'redo'};
                {R1, Cmd = {'r', _}} ->
                    {{L1, R1}, Cmd};
                {R1, Cmd = {'l', _}} ->
                    {{eval_add(L1, Cmd), R1}, 'redo'}
            end;
        {L1, 'redo'} ->
            {{L1, R}, 'redo'};
        {L1, Cmd = {'l', _}} ->
            {{L1, R}, Cmd};
        {L1, Cmd = {'r', _}} ->
            {{L1, eval_add(R, Cmd)}, 'redo'}
    end.

eval_split(_, N) when is_integer(N), N >= ?THRESHOLD ->
    {split(N), 'redo'};

eval_split(_, N) when is_integer(N) ->
    {N, undefined};

eval_split(Lvl, {L, R}) ->
    case eval_split(Lvl + 1, L) of
        {L1, undefined} ->
            case eval_split(Lvl + 1, R) of
                {R1, undefined} ->
                    {{L1, R1}, undefined};
                {R1, 'redo'} ->
                    {{L1, R1}, 'redo'}
            end;
        {L1, 'redo'} ->
            {{L1, R}, 'redo'}
    end.

eval_add(N, {_, Value}) when is_integer(N) ->
    N + Value;

eval_add({L, R}, Cmd = {'l', _}) ->
    {L, eval_add(R, Cmd)};

eval_add({L, R}, Cmd = {'r', _}) ->
    {eval_add(L, Cmd), R}.

split(Val) ->
    X = Val / 2,
    {floor(X), ceil(X)}.

mag({L, R}) ->
    3 * mag(L) + 2 * mag(R);

mag(N) when is_integer(N) ->
    N.

%%-------------------------------------------------------------------

parse(Lines) ->
    lists:map(fun (Line) ->
        {Tr, <<>>} = parse_one(Line),
        Tr
    end, Lines).

% we could just replace [] to {} and eval the Erlang term
parse_one(<<$[, R1/binary>>) ->
    {L, <<$,, R2/binary>>} = parse_one(R1),
    {R, <<$], R3/binary>>} = parse_one(R2),
    {{L, R}, R3};

parse_one(B) ->
    parse_int(B, 0).

parse_int(<<I, R/binary>>, V) when I >= $0, I =< $9 ->
    parse_int(R, V * 10 + I - $0);

parse_int(R, V) ->
    {V, R}.

%%-------------------------------------------------------------------

p12_test() ->
    ?assertEqual(3524, p1()),
    ?assertEqual(4656, p2()).

p0_test() ->
    R1 = fold(parse(aoc:read_lines("day18/sample.txt"))),
    ?assertEqual({{{{8,7},{7,7}},{{8,6},{7,7}}},{{{0,7},{6,6}},{8,7}}}, R1),
    ?assertEqual(hd(parse([<<"[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]">>])), fold(parse([
        <<"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]">>,
        <<"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]">>
    ]))),
    ?assertEqual({{{{0,7},4},{{7,8},{6,0}}},{8,1}}, fold(parse([
        <<"[[[[4,3],4],4],[7,[[8,4],9]]]">>,
        <<"[1,1]">>
    ]))),
    ?assertEqual(hd(parse([<<"[[[[3,0],[5,3]],[4,4]],[5,5]]">>])), fold(parse([
        <<"[1,1]">>,
        <<"[2,2]">>,
        <<"[3,3]">>,
        <<"[4,4]">>,
        <<"[5,5]">>
    ]))),
    ?assertEqual(hd(parse([<<"[[[[5,0],[7,4]],[5,5]],[6,6]]">>])), fold(parse([
        <<"[1,1]">>,
        <<"[2,2]">>,
        <<"[3,3]">>,
        <<"[4,4]">>,
        <<"[5,5]">>,
        <<"[6,6]">>
    ]))),
    %
    [{{1,{8,{5,8}}},{{4,4},{8,{8,8}}}}] = parse([<<"[[1,[8,[5,8]]],[[4,4],[8,[8,8]]]]">>]),

    ?assertEqual(1137, mag({{{{5,0},{7,4}},{5,5}},{6,6}})),
    ?assertEqual(3488, mag({
        {
            {
                {8,7},
                {7,7}
            },
            {
                {8,6},
                {7,7}
            }
        },
        {
            {
                {0,7},
                {6,6}
            },
            {8,7}
        }
    })).

%%-------------------------------------------------------------------
