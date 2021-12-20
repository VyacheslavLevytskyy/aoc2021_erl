%%-------------------------------------------------------------------
%% @doc Day 19: https://adventofcode.com/2021/day/19
%%-------------------------------------------------------------------

-module(aoc_d19).
-export([p1/0, p2/0, sample/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------

p1() ->
    do_p1(parse("day19/input.txt")).

p2() ->
    do_p2(parse("day19/input.txt")).

sample() ->
    do_p1(parse("day19/sample.txt")).

%%-------------------------------------------------------------------

do_p1(SMap) ->
    {Sz, _} = intersect(SMap),
    Sz.

do_p2(SMap) ->
    {_, Diffs} = intersect(SMap),
    lists:max([dist_m(P1, P2) || {P1, P2} <- pairs(Diffs, [])]).

intersect(SMap) ->
    intersect(SMap, to_dist(SMap, fun dist/2)).

%%-------------------------------------------------------------------

intersect(SMap, DMap) ->
    {Id0, Turns} = lists:foldr(fun ({{Id1, {_, Dist1}}, {Id2, {_, Dist2}}}, Acc = {AccId, AccT}) ->
        Common = map_inter(Dist1, Dist2),
        Sz = maps:size(Common),
        if
            Sz >= 66 ->
                CDist1 = maps:with(maps:keys(Common), Dist1),
                CDist2 = maps:with(maps:keys(Common), Dist2),
                case {get_translation(CDist1, CDist2), get_translation(CDist2, CDist1)} of
                    {{true, TransId1, Diff1}, {true, TransId2, Diff2}} ->
                        AccId2 = case AccId of
                            undefined ->
                                Id1;
                            _ ->
                                AccId
                        end,
                        AccT1 = add(Id1, {Id2, TransId1, Diff1}, AccT),
                        AccT2 = add(Id2, {Id1, TransId2, Diff2}, AccT1),
                        {AccId2, AccT2};
                    _ ->
                        Acc
                end;
            true ->
                Acc
        end
    end, {undefined, #{}}, pairs(lists:keysort(1, maps:to_list(DMap)), [])),
    Shifts = to_origin([{Id0, [{1, 1}], [0, 0, 0]}], SMap, Turns, #{}),
    AllPts = lists:foldl(fun ({_, _, PtsSet}, Acc) ->
        sets:union(Acc, PtsSet)
    end, to_set(maps:get(Id0, SMap)), Shifts),
    {sets:size(AllPts), [[0, 0, 0] | [Diff || {_, Diff, _} <- Shifts]]}.

to_origin([{Id, TransIds, Diff} | Cmds], SMap, Turns, Visited) ->
    case maps:is_key(Id, Visited) of
        true ->
            to_origin(Cmds, SMap, Turns, Visited);
        false ->
            {Ids, Visited2} = lists:foldl(fun ({IdTo, TransIdTo, DiffTo}, Acc = {AccI, AccV}) ->
                case maps:is_key(IdTo, Visited) of
                    true ->
                        Acc;
                    false ->
                        DiffTo2 = add_diff(Diff, trans_seq(TransIds, DiffTo)),
                        PtsTo = [add_diff(DiffTo2, trans_seq([TransIdTo | TransIds], Pos)) || Pos <- maps:get(IdTo, SMap)],
                        {[{IdTo, [TransIdTo | TransIds], DiffTo2} | AccI], add(Id, {IdTo, DiffTo2, to_set(PtsTo)}, AccV)}
                end
            end, {[], Visited}, maps:get(Id, Turns)),
            to_origin(Cmds ++ Ids, SMap, Turns, Visited2)
    end;

to_origin([], _, _, Visited) ->
    lists:append(maps:values(Visited)).

%%-------------------------------------------------------------------

get_translation(Dists1, Dists2) ->
    Candidates = maps:to_list(maps:fold(fun (D, {P1, P2}, Acc) ->
        {Q1, Q2} = maps:get(D, Dists2),
        Q = to_set([Q1, Q2]),
        add(P2, Q, add(P1, Q, Acc))
    end, #{}, Dists1)),
    case map_points(Candidates, []) of
        undefined ->
            false;
        [{P, Q} | _] = Map ->
            case check_match(Map) of
                {true, TransId} ->
                    Diff = diff(P, trans(TransId, Q)),
                    {true, TransId, Diff};
                false ->
                    false
            end
    end.

map_points([{P, QSets} | T], Acc) ->
    case sets:to_list(sets:intersection(QSets)) of
        [Q] ->
            map_points(T, [{P, Q} | Acc]);
        [_ | _] ->
            undefined
    end;

map_points([], Acc) ->
    Acc.

check_match([HPos | TPos]) ->
    case check_match(HPos, TPos, trans_ids()) of
        [] ->
            false;
        [TransId] ->
            {true, TransId}
    end.

check_match(_, _, []) ->
    [];

check_match(Pair1, [Pair2 | TPos], TransIds) ->
    check_match(Pair2, TPos, trans_to_match(Pair1, Pair2, TransIds));

check_match(_, [], TransIds) ->
    TransIds.

trans_to_match({P1, Q1}, {P2, Q2}, TransIds) ->
    lists:filter(fun (TransId) ->
        match({P1, trans(TransId, Q1)}, {P2, trans(TransId, Q2)})
    end, TransIds).

match({P1, Q1}, {P2, Q2}) ->
    D1 = diff(P1, Q1),
    case diff(P2, Q2) of
        D1 ->
            true;
        _ ->
            false
    end.

add_diff([X1, Y1, Z1], [X2, Y2, Z2]) ->
    [X1 + X2, Y1 + Y2, Z1 + Z2].

diff([X1, Y1, Z1], [X2, Y2, Z2]) ->
    [X1 - X2, Y1 - Y2, Z1 - Z2].

trans_ids() ->
    [{Sign, Perm} || Sign <- lists:seq(1, 8), Perm <- lists:seq(1, 6)].

trans_seq(TransIds, Pos) ->
    lists:foldl(fun (TransId, Acc) ->
        trans(TransId, Acc)
    end, Pos, TransIds).

trans({Sign, Perm}, Pos) ->
    flip(Sign, perm(Perm, Pos)).

flip(Sign, [X, Y, Z]) ->
    case Sign of
        1 ->
            [X, Y, Z];
        2 ->
            [X, Y, -Z];
        3 ->
            [X, -Y, Z];
        4 ->
            [X, -Y, -Z];
        5 ->
            [-X, Y, Z];
        6 ->
            [-X, Y, -Z];
        7 ->
            [-X, -Y, Z];
        8 ->
            [-X, -Y, -Z]
    end.

perm(No, [X, Y, Z]) ->
    case No of
        1 ->
            [X, Y, Z];
        2 ->
            [X, Z, Y];
        3 ->
            [Y, X, Z];
        4 ->
            [Y, Z, X];
        5 ->
            [Z, X, Y];
        6 ->
            [Z, Y, X]
    end.

%%-------------------------------------------------------------------

to_dist(M, DistF) ->
    maps:map(fun (_, Ps) ->
        D = maps:from_list([{DistF(P1, P2), {P1, P2}} || {P1, P2} <- pairs(Ps, [])]),
        {maps:size(D), D}
    end, M).

pairs([H | T], Acc) ->
    pairs(T, lists:foldl(fun (I, AccIn) -> [{H, I} | AccIn] end, Acc, T));

pairs([], Acc) ->
    Acc.

dist(P1, P2) ->
    dist_e(P1, P2).

dist_e(P1, P2) ->
    X = math:sqrt(lists:sum([(A - B) * (A - B) || {A, B} <- lists:zip(P1, P2)])),
    trunc(X * 1000000).

dist_m(P1, P2) ->
    lists:sum([abs(A - B) || {A, B} <- lists:zip(P1, P2)]).

to_set(L) ->
    sets:from_list(L, [{version, 2}]).

add(Key, Elem, M) ->
    M#{Key => [Elem | maps:get(Key, M, [])]}.

map_inter(M1, M2) when map_size(M1) < map_size(M2) ->
    maps:with(maps:keys(M1), M2);

map_inter(M1, M2) ->
    maps:with(maps:keys(M2), M1).

%%-------------------------------------------------------------------

parse(Fn) ->
    lists:foldl(fun (B, M) ->
        [Hd | Lines] = binary:split(B, [<<10>>], [global, trim_all]),
        [_, _, Id, _] = binary:split(Hd, <<32>>, [global, trim_all]),
        Ps = [binary:split(Ln, <<$,>>, [global, trim_all]) || Ln <- Lines],
        M#{binary_to_integer(Id) => [[binary_to_integer(I) || I <- Coords] || Coords <- Ps]}
    end, #{}, aoc:read_lines(Fn, [<<10, 10>>], [global, trim_all])).

%%-------------------------------------------------------------------

p12_test() ->
    ?assertEqual(79, aoc_d19:sample()),
    ?assertEqual(449, aoc_d19:p1()),
    ?assertEqual(13128, aoc_d19:p2()).

%%-------------------------------------------------------------------
