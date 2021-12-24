%%-------------------------------------------------------------------
%% @doc Day 23: https://adventofcode.com/2021/day/23
%%-------------------------------------------------------------------

-module(aoc_d23).
-export([p1/0, p2/0, sample1/0, sample2/0]).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%-------------------------------------------------------------------

p1() ->
    play(#{{a, 0} => c, {a, 1} => b, {b, 0} => b, {b, 1} => c,
           {c, 0} => a, {c, 1} => d, {d, 0} => d, {d, 1} => a}, fun map1/1, linear1(), slots1(), 1).

p2() ->
    play(#{{a, 0} => c, {a, 1} => d, {a, 2} => d, {a, 3} => b,
           {b, 0} => b, {b, 1} => c, {b, 2} => b, {b, 3} => c,
           {c, 0} => a, {c, 1} => b, {c, 2} => a, {c, 3} => d,
           {d, 0} => d, {d, 1} => a, {d, 2} => c, {d, 3} => a}, fun map2/1, linear2(), slots2(), 1).

sample1() ->
    play(#{{a, 0} => b, {a, 1} => a, {b, 0} => c, {b, 1} => d,
           {c, 0} => b, {c, 1} => c, {d, 0} => d, {d, 1} => a}, fun map1/1, linear1(), slots1(), 1).

sample2() ->
    play(#{{a, 0} => b, {a, 1} => d, {a, 2} => d, {a, 3} => a,
           {b, 0} => c, {b, 1} => c, {b, 2} => b, {b, 3} => d,
           {c, 0} => b, {c, 1} => b, {c, 2} => a, {c, 3} => c,
           {d, 0} => d, {d, 1} => a, {d, 2} => c, {d, 3} => a}, fun map2/1, linear2(), slots2(), 1).

%%-------------------------------------------------------------------
%% invariants

price(a) -> 1;
price(b) -> 10;
price(c) -> 100;
price(d) -> 1000.

out(a) -> 3;
out(b) -> 5;
out(c) -> 7;
out(d) -> 9.

is_out(3) -> true;
is_out(5) -> true;
is_out(7) -> true;
is_out(9) -> true;
is_out(_) -> false.

cost(Token, {A1, No1}, {A2, No2}) ->
    price(Token) * (abs(out(A1) - out(A2)) + 2 + No1 + No2);
cost(Token, {A, No}, Num) when is_integer(Num) ->
    price(Token) * (abs(Num - out(A)) + 1 + No);
cost(Token, Num, {A, No}) when is_integer(Num) ->
    price(Token) * (abs(Num - out(A)) + 1 + No).

tokens() -> [a, b, c, d].

%%-------------------------------------------------------------------
%% parameters

slots1() -> [0, 1].

slots2() -> [0, 1, 2, 3].

map1({a, 1}) -> [{a, 0}];
map1({a, 0}) -> [3, {a, 1}];
map1({b, 1}) -> [{b, 0}];
map1({b, 0}) -> [5, {b, 1}];
map1({c, 1}) -> [{c, 0}];
map1({c, 0}) -> [7, {c, 1}];
map1({d, 1}) -> [{d, 0}];
map1({d, 0}) -> [9, {d, 1}];
map1(1) -> [2];
map1(2) -> [1, 3];
map1(3) -> [2, 4, {a, 0}];
map1(4) -> [3, 5];
map1(5) -> [4, 6, {b, 0}];
map1(6) -> [5, 7];
map1(7) -> [6, 8, {c, 0}];
map1(8) -> [7, 9];
map1(9) -> [8, 10, {d, 0}];
map1(10) -> [9, 11];
map1(11) -> [10].

map2({a, 3}) -> [{a, 2}];
map2({a, 2}) -> [{a, 1}, {a, 3}];
map2({a, 1}) -> [{a, 0}, {a, 2}];
map2({a, 0}) -> [3, {a, 1}];

map2({b, 3}) -> [{b, 2}];
map2({b, 2}) -> [{b, 1}, {b, 3}];
map2({b, 1}) -> [{b, 0}, {b, 2}];
map2({b, 0}) -> [5, {b, 1}];

map2({c, 3}) -> [{c, 2}];
map2({c, 2}) -> [{c, 1}, {c, 3}];
map2({c, 1}) -> [{c, 0}, {c, 2}];
map2({c, 0}) -> [7, {c, 1}];

map2({d, 3}) -> [{d, 2}];
map2({d, 2}) -> [{d, 1}, {d, 3}];
map2({d, 1}) -> [{d, 0}, {d, 2}];
map2({d, 0}) -> [9, {d, 1}];

map2(1) -> [2];
map2(2) -> [1, 3];
map2(3) -> [2, 4, {a, 0}];
map2(4) -> [3, 5];
map2(5) -> [4, 6, {b, 0}];
map2(6) -> [5, 7];
map2(7) -> [6, 8, {c, 0}];
map2(8) -> [7, 9];
map2(9) -> [8, 10, {d, 0}];
map2(10) -> [9, 11];
map2(11) -> [10].

linear1() ->
    [1, 2, {a, 0}, {a, 1}, 4, {b, 0}, {b, 1}, 6, {c, 0}, {c, 1}, 8, {d, 0}, {d, 1}, 10, 11].

linear2() ->
    [1, 2, {a, 0}, {a, 1}, {a, 2}, {a, 3}, 4, {b, 0}, {b, 1}, {b, 2}, {b, 3},
        6, {c, 0}, {c, 1}, {c, 2}, {c, 3}, 8, {d, 0}, {d, 1}, {d, 2}, {d, 3}, 10, 11].

%%-------------------------------------------------------------------
%% utils

is_solution(S) ->
    lists:all(fun ({{A, _}, A}) -> true; (_) -> false end, maps:to_list(S)).

estimate(S, Slots) ->
    Goals = [{A, N} || A <- tokens(), N <- Slots, maps:get({A, N}, S, []) /= A],
    {C, _} = maps:fold(fun
        ({A, _}, A, Acc) ->
            Acc;
        (From, A, {Sum, AccG}) ->
            {value, To, AccG2} = lists:keytake(A, 1, AccG),
            {Sum + cost(A, From, To), AccG2}
    end, {0, Goals}, S),
    C.

is_deadlock(S, Linear) ->
    check_deadlock([{I, maps:get(I, S, [])} || I <- Linear], 0).

check_deadlock([{Num, []} | T], Balance) when is_integer(Num) ->
    check_deadlock(T, Balance + 1);

check_deadlock([{Num, _} | _], Balance) when is_integer(Num), Balance < 0 ->
    true;

check_deadlock([{Num, _} | T], _) when is_integer(Num) ->
    check_deadlock(T, 0);

check_deadlock([{{A1, _}, A2} | T], Balance) when A1 /= A2, A2 /= [] ->
    check_deadlock(T, Balance - 1);

check_deadlock([{{_, _}, _} | T], Balance) ->
    check_deadlock(T, Balance);

check_deadlock([], Balance) ->
    Balance < 0.

branches(_, _, _, _, A, {A, 1}) ->
    [];

branches(Map, Max, Cost, S, A, Pos0 = {A, No1}) ->
    case [V || {{T, No2}, V} <- maps:to_list(S), T == A, No2 > No1, V /= A] of
        [] ->
            [];
        [_ | _] ->
            branches1(Map, Max, Cost, S, A, Pos0)
    end;

branches(Map, Max, Cost, S, A, Pos0) ->
    branches1(Map, Max, Cost, S, A, Pos0).

branches1(Map, Max, Cost, S, A, Pos0) ->
    {Stops, _} = paths(Map, Max, S, A, Pos0, Pos0, #{}, []),
    [{Cost + cost(A, Pos0, Pos2), Pos0, Pos2, A} || Pos2 <- Stops].

paths(Map, Max, S, A, Pos0, Pos1, Visited, Stops) ->
    lists:foldl(fun (Pos2, Acc = {AccS, AccV}) ->
        case maps:is_key(Pos2, AccV) orelse maps:is_key(Pos2, S) of
            true ->
                Acc;
            false when is_integer(Pos0) ->
                case Pos2 of
                    _ when is_integer(Pos2) ->
                        paths(Map, Max, S, A, Pos0, Pos2, AccV#{Pos1 => []}, AccS);
                    {T, _} when T /= A ->
                        Acc;
                    {_, No1} when No1 == Max ->
                        paths(Map, Max, S, A, Pos0, Pos2, AccV#{Pos1 => []}, [Pos2 | AccS]);
                    {_, No1} when No1 < Max ->
                        case [Token || {{T, No2}, Token} <- maps:to_list(S), T == A, No2 > No1] of
                            [] ->
                                paths(Map, Max, S, A, Pos0, Pos2, AccV#{Pos1 => []}, AccS);
                            Below ->
                                case lists:all(fun (I) -> I == A end, Below) of
                                    true ->
                                        paths(Map, Max, S, A, Pos0, Pos2, AccV#{Pos1 => []}, [Pos2 | AccS]);
                                    false ->
                                        Acc
                                end
                        end
                end;
            false ->
                IsOut = is_out(Pos2),
                CanStop = case {Pos0, Pos2} of
                    {{T1, _}, {T2, _}} when T1 /= T2, A /= T2 ->
                        wrong;
                    _ when IsOut ->
                        false;
                    {{_, _}, Num} when is_integer(Num) ->
                        true;
                    {{T, _}, {T, _}} ->
                        false;
                    {{_, _}, {_, No1}} when No1 == Max ->
                        true;
                    {{_, _}, {T, No1}} when A == T, No1 < Max ->
                        maps:is_key({T, No1 + 1}, S);
                    {{_, _}, {T, No1}} when A /= T, No1 < Max ->
                        true
                end,
                case CanStop of
                    wrong ->
                        Acc;
                    _ ->
                        paths(Map, Max, S, A, Pos0, Pos2, AccV#{Pos1 => []}, if
                            CanStop ->
                                [Pos2 | AccS];
                            true ->
                                AccS
                        end)
                end
        end
    end, {Stops, Visited}, Map(Pos1)).

play(Init, Map, Linear, Slots, Max) ->
    Best = undefined,
    bnb(Map, Linear, Slots, Max, Best, {Best, []}, [{0, [], Init, #{}}], 0).

bnb(Map, Linear, Slots, Max, Best0, Sol = {Best, _}, [{Cost, Log, S, Visited} | Q], Cb) ->
    case Cb rem 100000 of
        0 when Cb > 0, Best < 16000 ->
            io:format("~p: ~p vs ~p~n~p~n~p~nvisited: ~p, q-len: ~p~n~n~n", [Cb, Best, Cost, Log, S, maps:size(Visited), length(Q)]);
        _ ->
            ok
    end,
    case is_solution(S) of
        true when Cost < Best ->
            io:format("best solution: ~p~n~p~nq-len is ~p~n~n", [Cost, Log, length(Q)]),
            bnb(Map, Linear, Slots, Max, Best0, {Cost, Log}, Q, Cb + 1);
        true ->
            %io:format("sub-optimal solution: ~p~n~p~nq-len is ~p~n~n", [Cost, Log, length(Q)]),
            bnb(Map, Linear, Slots, Max, Best0, Sol, Q, Cb + 1);
        _ ->
            Opts0 = lists:flatten([branches(Map, Max, Cost, S, A, Pos0) || {Pos0, A} <- maps:to_list(S)]),
            Opts1 = [{C, [I | Log], maps:put(P2, A, maps:remove(P1, S))} || I = {C, P1, P2, A} <- Opts0],
            Opts2 = [I || I = {C1, _, S1} <- Opts1,
                C1 + estimate(S1, Slots) =< Best],
                %not is_deadlock(S1, Linear)],
            Aux = lists:foldl(fun ({Cost1, Log1, S1}, Acc) ->
                case maps:is_key(S1, Visited) of
                    true ->
                        Acc;
                    false ->
                        [{Cost1, Log1, S1, Visited#{S1 => []}} | Acc]
                        %[{Cost1, Log1, S1, Visited} | Acc]
                end
            end, [], Opts2),
            Q2 = Aux ++ Q,
            bnb(Map, Linear, Slots, Max, Best0, Sol, Q2, Cb + 1)
    end;

bnb(_, _, _, _, _, {Cost, _Log}, [], _) ->
    Cost.
    %{Cost, Log}.

%%-------------------------------------------------------------------

p1_test() ->
    ?assertEqual(12521, sample1()).

%%-------------------------------------------------------------------
