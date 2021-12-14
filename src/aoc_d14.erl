%%-------------------------------------------------------------------
%% @doc Day 14: https://adventofcode.com/2021/day/14
%%-------------------------------------------------------------------

-module(aoc_d14).
-export([p1/0, p2/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(aoc:read_lines("day14/input.txt"), 10).

p2() ->
    do_p2(aoc:read_lines("day14/input.txt"), 40).

%%-------------------------------------------------------------------

do_p1(L, N) ->
    {<<Hd, Fml/binary>>, Rules} = parse(L),
    FinalFml = transform1(Hd, Fml, Rules, N),
    weight(to_freq1(FinalFml)).

transform1(Hd, Fml, Rules, N) when N > 1 ->
    <<Hd2, Fml2/binary>> = step1(Hd, Fml, Rules, <<>>),
    transform1(Hd2, Fml2, Rules, N - 1);

transform1(Hd, Fml, Rules, 1) ->
    step1(Hd, Fml, Rules, <<>>).

step1(I1, <<I2, R/binary>>, Rules, Acc) ->
    Ins = maps:get(<<I1, I2>>, Rules, <<>>),
    step1(I2, R, Rules, <<Acc/binary, I1, Ins>>);

step1(I, <<>>, _, Acc) ->
    <<Acc/binary, I>>.

to_freq1(Fml) ->
    to_freq1(Fml, #{}).

to_freq1(<<I, R/binary>>, Acc) ->
    to_freq1(R, inc(I, Acc, 1));

to_freq1(<<>>, Acc) ->
    Acc.

%%-------------------------------------------------------------------

do_p2(L, N) ->
    {<<Hd, Fml/binary>>, Rules} = parse(L),
    {Pairs, Freq} = to_pairs(Hd, Fml, #{}, #{}),
    weight(transform2(Pairs, Freq, Rules, N)).

to_pairs(I1, <<I2, R/binary>>, Acc, Freq) ->
    Key = <<I1, I2>>,
    to_pairs(I2, R, inc(Key, Acc, 1), inc(I1, Freq, 1));

to_pairs(I, <<>>, Acc, Freq) ->
    {Acc, inc(I, Freq, 1)}.

transform2(Pairs, Freq, Rules, N) when N > 0 ->
    {NewPairs, NewFreq} = maps:fold(fun (Pair = <<I1, I2>>, Cb, {Acc, AccFreq}) ->
        case maps:get(Pair, Rules, undefined) of
            undefined ->
                {inc(Pair, Acc, Cb), AccFreq};
            Ins ->
                {inc(<<I1, Ins>>, inc(<<Ins, I2>>, Acc, Cb), Cb), inc(Ins, AccFreq, Cb)}
        end
    end, {#{}, Freq}, Pairs),
    transform2(NewPairs, NewFreq, Rules, N - 1);

transform2(_, Freq, _, 0) ->
    Freq.

%%-------------------------------------------------------------------

inc(Key, Acc, Inc) ->
    Acc#{Key => Inc + maps:get(Key, Acc, 0)}.

weight(Freq) ->
    {Max, Min} = maps:fold(fun (_, V, {AccMax, AccMin}) ->
        {max(AccMax, V), min(AccMin, V)}
    end, {0, undefined}, Freq),
    Max - Min.

parse([Fml, _ | Rules]) ->
    {Fml, maps:from_list(lists:map(fun (B) ->
        [L, <<R>>] = binary:split(B, [<<" -> ">>]),
        {L, R}
    end, Rules))}.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(1588, do_p2(aoc:read_lines("day14/sample.txt"), 10)),
    ?assertEqual(1588, do_p1(aoc:read_lines("day14/sample.txt"), 10)).

p2_test() ->
    ?assertEqual(2188189693529, do_p2(aoc:read_lines("day14/sample.txt"), 40)).

p12_test() ->
    ?assertEqual(2112, do_p1(aoc:read_lines("day14/input.txt"), 10)),
    ?assertEqual(2112, do_p2(aoc:read_lines("day14/input.txt"), 10)),
    ?assertEqual(3243771149914, do_p2(aoc:read_lines("day14/input.txt"), 40)).

%%-------------------------------------------------------------------
