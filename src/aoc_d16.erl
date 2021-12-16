%%-------------------------------------------------------------------
%% @doc Day 16: https://adventofcode.com/2021/day/16
%%-------------------------------------------------------------------

-module(aoc_d16).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    [B] = aoc:read_lines("day16/input.txt"),
    ver_sum(B).

p2() ->
    [B] = aoc:read_lines("day16/input.txt"),
    eval_decoded(B).

%%-------------------------------------------------------------------

ver_sum(B) ->
    fold(fun ({Ver, _, _}, S) -> Ver + S end, 0, decode(B)).

eval_decoded(B) ->
    eval(decode(B)).

eval({_, 4, Int}) ->
    Int;

eval({_, 0, L}) when is_list(L) ->
    lists:foldl(fun (I, S) -> S + eval(I) end, 0, L);

eval({_, 1, L}) when is_list(L) ->
    lists:foldl(fun (I, P) -> P * eval(I) end, 1, L);

eval({_, 2, [H | T]}) ->
    lists:foldl(fun (I, Val) -> min(eval(I), Val) end, eval(H), T);

eval({_, 3, [H | T]}) ->
    lists:foldl(fun (I, Val) -> max(eval(I), Val) end, eval(H), T);

eval({_, 5, [P1, P2]}) ->
    V1 = eval(P1),
    V2 = eval(P2),
    if
        V1 > V2 ->
            1;
        true ->
            0
    end;

eval({_, 6, [P1, P2]}) ->
    V1 = eval(P1),
    V2 = eval(P2),
    if
        V1 < V2 ->
            1;
        true ->
            0
    end;

eval({_, 7, [P1, P2]}) ->
    V1 = eval(P1),
    V2 = eval(P2),
    if
        V1 == V2 ->
            1;
        true ->
            0
    end.

%%-------------------------------------------------------------------

fold(F, Acc, [H | T]) ->
    fold(F, fold(F, Acc, H), T);

fold(_, Acc, []) ->
    Acc;

fold(F, Acc, Elem = {_, _, L}) when is_list(L) ->
    fold(F, F(Elem, Acc), L);

fold(F, Acc, Elem = {_, _, N}) when is_integer(N) ->
    F(Elem, Acc).

%%-------------------------------------------------------------------

decode(B) ->
    {Packet, Rest, _} = decode_one(to_hex(B)),
    ok = zero_bits(Rest),
    Packet.

decode_one(<<Ver:3, Id:3, R/bitstring>>) ->
    {Value, Rest, Size} = decode_type(Id, R, 0),
    {{Ver, Id, Value}, Rest, Size + 6}.

decode_type(4, R, AccSz) ->
    decode_int(R, 0, AccSz);

decode_type(_, <<0:1, Size:15, R/bitstring>>, AccSz) ->
    decode_by_size(Size, R, [], AccSz + 16);

decode_type(_, <<1:1, Count:11, R/bitstring>>, AccSz) ->
    decode_by_number(Count, R, [], AccSz + 12).

decode_int(<<1:1, I:4, R/bitstring>>, Acc, AccSz) ->
    decode_int(R, Acc bsl 4 + I, AccSz + 5);

decode_int(<<0:1, I:4, R/bitstring>>, Acc, AccSz) ->
    {Acc bsl 4 + I, R, AccSz + 5}.

decode_by_size(Size, R, Acc, AccSz) when Size > 0 ->
    {Packet, Rest, Read} = decode_one(R),
    decode_by_size(Size - Read, Rest, [Packet | Acc], AccSz + Read);

decode_by_size(0, R, Acc, AccSz) ->
    {lists:reverse(Acc), R, AccSz}.

decode_by_number(Count, R, Acc, AccSz) when Count > 0 ->
    {Packet, Rest, Read} = decode_one(R),
    decode_by_number(Count - 1, Rest, [Packet | Acc], AccSz + Read);

decode_by_number(0, R, Acc, AccSz) ->
    {lists:reverse(Acc), R, AccSz}.

zero_bits(<<0:1, R/bitstring>>) ->
    zero_bits(R);

zero_bits(<<>>) ->
    ok.

%%-------------------------------------------------------------------

to_hex(B) ->
    << <<(if I < $A -> I - $0; true -> I - 55 end):4>> || <<I>> <= B >>.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual({6,4,2021}, decode(<<"D2FE28">>)),
    ?assertEqual(6, ver_sum(<<"D2FE28">>)),
    ?assertEqual({1,6,[{6,4,10},{2,4,20}]}, decode(<<"38006F45291200">>)),
    ?assertEqual(9, ver_sum(<<"38006F45291200">>)),
    ?assertEqual({7,3,[{2,4,1},{4,4,2},{1,4,3}]}, decode(<<"EE00D40C823060">>)),
    ?assertEqual(14, ver_sum(<<"EE00D40C823060">>)),
    L = [
        {<<"8A004A801A8002F478">>, 16},
        {<<"620080001611562C8802118E34">>, 12},
        {<<"C0015000016115A2E0802F182340">>, 23},
        {<<"A0016C880162017C3686B18A3D4780">>, 31}
    ],
    [?assertEqual(Sum, ver_sum(B)) || {B, Sum} <- L],
    ?assertEqual(1014, p1()),
    %
    ?assertEqual(1, eval_decoded(<<"9C0141080250320F1802104A08">>)),
    ?assertEqual(0, eval_decoded(<<"9C005AC2F8F0">>)),
    ?assertEqual(0, eval_decoded(<<"F600BC2D8F">>)),
    ?assertEqual(1, eval_decoded(<<"D8005AC2A8F0">>)),
    ?assertEqual(9, eval_decoded(<<"CE00C43D881120">>)),
    ?assertEqual(7, eval_decoded(<<"880086C3E88112">>)),
    ?assertEqual(54, eval_decoded(<<"04005AC33890">>)),
    ?assertEqual(3, eval_decoded(<<"C200B40A82">>)),
    ?assertEqual(1922490999789, p2()).

%%-------------------------------------------------------------------
