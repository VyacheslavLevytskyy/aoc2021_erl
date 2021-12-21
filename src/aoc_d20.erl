%%-------------------------------------------------------------------
%% @doc Day 20: https://adventofcode.com/2021/day/20
%%-------------------------------------------------------------------

-module(aoc_d20).
-export([p1/0, p2/0, sample/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    enhance_loop(parse("day20/input.txt"), 2).

p2() ->
    enhance_loop(parse("day20/input.txt"), 50).

sample() ->
    enhance_loop(parse("day20/sample.txt"), 2).

%%-------------------------------------------------------------------

enhance_loop({Mask, Image}, N) when N > 0 ->
    enhance_loop({Mask, enhance(Mask, Image, N)}, N - 1);

enhance_loop({_M, Image}, 0) ->
    lists:sum(maps:values(Image)).

enhance(Mask, Image, N) ->
    {KXs, KYs} = lists:unzip(maps:keys(Image)),
    Xs = lists:seq(lists:min(KXs) - 1, lists:max(KXs) + 1),
    Ys = lists:seq(lists:min(KYs) - 1, lists:max(KYs) + 1),
    lists:foldl(fun (X, Acc1) ->
        lists:foldl(fun (Y, Acc2) ->
            Default = case {Mask band 1, N rem 2} of
                {1, 1} ->
                    1;
                _ ->
                    0
            end,
            Idx = pt_to_int(X, Y, Image, Default),
            V = (Mask band (1 bsl Idx)),
            case V of
                0 ->
                    Acc2#{{X, Y} => 0};
                _ ->
                    Acc2#{{X, Y} => 1}
            end
        end, Acc1, Ys)
    end, #{}, Xs).

%%-------------------------------------------------------------------

pt_to_int(X, Y, Image, Default) ->
    pts_to_int([{I, J} || J <- lists:seq(Y - 1, Y + 1), I <- lists:seq(X - 1, X + 1)], Image, Default, 0).

pts_to_int([H | T], M, Default, Acc) ->
    pts_to_int(T, M, Default, (Acc bsl 1) + maps:get(H, M, Default));

pts_to_int([], _, _, Acc) ->
    Acc.

%%-------------------------------------------------------------------

parse(Fn) ->
    [H, Lines] = aoc:read_lines(Fn, [<<10, 10>>], [global, trim_all]),
    Mask = parse_to_int(reverse(H), 0),
    {Image, _} = lists:foldl(fun (Line, {Acc, Y}) ->
        {parse_to_map(Line, 0, Y, Acc), Y + 1}
    end, {#{}, 0}, binary:split(Lines, [<<10>>], [global, trim_all])),
    {Mask, Image}.

parse_to_int(<<$#, R/binary>>, Acc) ->
    parse_to_int(R, Acc bsl 1 + 1);

parse_to_int(<<$., R/binary>>, Acc) ->
    parse_to_int(R, Acc bsl 1);

parse_to_int(<<>>, Acc) ->
    Acc.

parse_to_map(<<$#, R/binary>>, X, Y, Acc) ->
    parse_to_map(R, X + 1, Y, Acc#{{X, Y} => 1});

parse_to_map(<<$., R/binary>>, X, Y, Acc) ->
    parse_to_map(R, X + 1, Y, Acc#{{X, Y} => 0});

parse_to_map(<<>>, _, _, Acc) ->
    Acc.

reverse(B) ->
    binary:encode_unsigned(binary:decode_unsigned(B, little)).

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(35, enhance_loop(parse("day20/sample.txt"), 2)),
    ?assertEqual(5583, p1()),
    ?assertEqual(19592, p2()).

%%-------------------------------------------------------------------
