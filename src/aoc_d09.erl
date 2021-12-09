%%-------------------------------------------------------------------
%% @doc Day 09: https://adventofcode.com/2021/day/9
%%-------------------------------------------------------------------

-module(aoc_d09).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(BIG, $9).
-define(AROUND, [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]).

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_lines("day09/input.txt")).

p2() ->
    do_p2(aoc:read_lines("day09/input.txt")).

%%-------------------------------------------------------------------

do_p1(L) ->
    {M, Rows, Cols} = transform(L),
    lists:foldl(fun (Row, Acc1) ->
        lists:foldl(fun (Col, Acc2) ->
            case mark(M, Row, Col) of
                0 ->
                    Acc2;
                X ->
                    X - $0 + 1 + Acc2
            end
        end, Acc1, Cols)
    end, 0, Rows).

do_p2(L) ->
    {M, Rows, Cols} = transform(L),
    LowPoints = lists:foldl(fun (Row, Acc1) ->
        lists:foldl(fun (Col, Acc2) ->
            case mark(M, Row, Col) of
                0 ->
                    Acc2;
                X ->
                    [{{Row, Col}, X} | Acc2]
            end
        end, Acc1, Cols)
    end, [], Rows),
    NegMarks = lists:map(fun ({Coord, X}) ->
        Lock = #{Coord => X},
        eval_basin(M, Lock, Lock)
    end, LowPoints),
    TopBasins = lists:sublist(lists:sort(NegMarks), 3),
    abs(lists:foldl(fun (Val, Product) ->
        Product * Val
    end, 1, TopBasins)).

%%-------------------------------------------------------------------

wrap(L1, NCols) ->
    Border = list_to_binary(lists:duplicate(NCols + 2, ?BIG)),
    L2 = [<<?BIG, Row/binary, ?BIG>> || Row <- L1],
    erlang:append_element(list_to_tuple([Border | L2]), Border).

at(M, Row, Col) ->
    binary:at(element(Row, M), Col).

mark(M, Row, Col) ->
    mark(M, at(M, Row, Col), Row, Col, ?AROUND).

mark(M, X, Row, Col, [{DR, DC} | T]) ->
    case at(M, Row + DR, Col + DC) of
        Elem when X < Elem ->
            mark(M, X, Row, Col, T);
        _ ->
            0
    end;

mark(_, X, _, _, []) ->
    X.

higher(M, X, Row, Col, [{DR, DC} | T], Acc) ->
    R = Row + DR,
    C = Col + DC,
    Elem = at(M, R, C),
    Acc2 = if
        Elem < $9, Elem > X ->
            [{{R, C}, Elem} | Acc];
        true ->
            Acc
    end,
    higher(M, X, Row, Col, T, Acc2);

higher(_, _, _, _, [], Acc) ->
    Acc.

transform(L = [H | _]) ->
    NCols = size(H),
    M = wrap(L, NCols),
    NRows = size(M) - 2,
    Rows = lists:seq(2, NRows + 1),
    Cols = lists:seq(1, NCols),
    {M, Rows, Cols}.

eval_basin(M, Check, Locked) when map_size(Check) > 0 ->
    Check1 = maps:fold(fun ({Row, Col}, X, Acc) ->
        Higher = higher(M, X, Row, Col, ?AROUND, []),
        lists:foldl(fun({HPos, Elem}, InAcc) ->
            case maps:is_key(HPos, Locked) of
                true ->
                    InAcc;
                false ->
                    InAcc#{HPos => Elem}
            end
        end, Acc, Higher)
    end, #{}, Check),
    NewLocked = maps:merge(Locked, Check1),
    NewCheck = maps:filter(fun (_, Elem) -> Elem /= $8 end, Check1),
    eval_basin(M, NewCheck, NewLocked);

eval_basin(_, _, Locked) ->
    - maps:size(Locked).

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(15, do_p1(aoc:read_lines("day09/sample.txt"))).

p2_test() ->
    ?assertEqual(1134, do_p2(aoc:read_lines("day09/sample.txt"))).

p12_test() ->
    ?assertEqual(452, do_p1(aoc:read_lines("day09/input.txt"))),
    ?assertEqual(1263735, do_p2(aoc:read_lines("day09/input.txt"))).

%%-------------------------------------------------------------------
