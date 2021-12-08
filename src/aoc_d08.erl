%%-------------------------------------------------------------------
%% @doc Day 08: https://adventofcode.com/2021/day/8
%%-------------------------------------------------------------------

-module(aoc_d08).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_7display("day08/input.txt")).

p2() ->
    do_p2(aoc:read_7display("day08/input.txt")).

%%-------------------------------------------------------------------

do_p1(L0) ->
    {_, L1} = lists:unzip(L0),
    lists:foldl(fun (L2, Acc) ->
        lists:foldl(fun (D, InAcc) ->
            case size(D) of
                2 ->
                    1;
                3 ->
                    1;
                4 ->
                    1;
                7 ->
                    1;
                _ ->
                    0
            end + InAcc
        end, Acc, L2)
    end, 0, L1).

do_p2(L) ->
    lists:foldl(fun ({In, Out}, Cb) ->
        [I1, I2, I3, I4] = decode(Out, wire(In)),
        Cb + (I4 + 10*(I3 + 10*(I2 + 10*I1)))
    end, 0, L).

%%-------------------------------------------------------------------

get_bit($a) ->
    2#1;
get_bit($b) ->
    2#10;
get_bit($c) ->
    2#100;
get_bit($d) ->
    2#1000;
get_bit($e) ->
    2#10000;
get_bit($f) ->
    2#100000;
get_bit($g) ->
    2#1000000.

n_bits(X) ->
    n_bits(X, 0).

n_bits(X, Bits) when X > 0 ->
    n_bits(X bsr 1, Bits + (X band 1));

n_bits(0, Bits) ->
    Bits.

digit(36) ->
    1;
digit(37) ->
    7;
digit(46) ->
    4;

digit(93) ->
    2;
digit(109) ->
    3;
digit(107) ->
    5;

digit(119) ->
    0;
digit(123) ->
    6;
digit(111) ->
    9;

digit(127) ->
    8;

digit(_) ->
    undefined.

to_bits(L) ->
    [to_bits(B, 0) || B <- L].

to_bits(<<I, R/binary>>, Mask) ->
    to_bits(R, Mask bor get_bit(I));

to_bits(<<>>, Mask) ->
    Mask.

wire(In) ->
    [One, Seven, Four, X5, Y5, Z5, X6, Y6, Z6, Eight] =
    to_bits(lists:sort(fun (A, B) ->
        size(A) =< size(B)
    end, In)),
    P0 = Seven bxor One,
    P13 = Four bxor One,
    [{2, X1}, {2, X2}, {4, P1245}] = lists:sort([{n_bits(I), I} || I <- [X5 bxor Y5, Y5 bxor Z5, X5 bxor Z5]]),
    {P12, P45} = case P13 band X1 of
        0 ->
            {X2, X1};
        _ ->
            {X1, X2}
    end,
    P1 = P13 band P12,
    P2 = P12 bxor P1,
    P3 = P13 bxor P1,
    P23 = P2 bor P3,
    Y1 = X6 bxor Y6,
    Y2 = Y6 bxor Z6,
    Y3 = X6 bxor Z6,
    P4 = if
        P23 == Y1 ->
            Y2 band Y3;
        P23 == Y2 ->
            Y1 band Y3;
        P23 == Y3 ->
            Y1 band Y2
    end,
    P5 = P45 bxor P4,
    P6 = Eight bxor (P0 bor P3 bor P1245),
    #{
        P0 => 2#1,
        P1 => 2#10,
        P2 => 2#100,
        P3 => 2#1000,
        P4 => 2#10000,
        P5 => 2#100000,
        P6 => 2#1000000
    }.

decode(Out, Wire) ->
    [decode1(B, Wire) || B <- Out].

decode1(B, _) when size(B) == 2 ->
    1;

decode1(B, _) when size(B) == 3 ->
    7;

decode1(B, _) when size(B) == 4 ->
    4;

decode1(B, _) when size(B) == 7 ->
    8;

decode1(B, Wire) ->
    decode_bits(B, 0, Wire).

decode_bits(<<I, R/binary>>, Mask, Wire) ->
    Bit = maps:get(get_bit(I), Wire),
    decode_bits(R, Mask bor Bit, Wire);

decode_bits(<<>>, Mask, _) ->
    digit(Mask).

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(26, do_p1(aoc:read_7display("day08/sample.txt"))).

p2_test() ->
    ?assertEqual(61229, do_p2(aoc:read_7display("day08/sample.txt"))).

p12_test() ->
    ?assertEqual(493, p1()),
    ?assertEqual(1010460, p2()).

%%-------------------------------------------------------------------
