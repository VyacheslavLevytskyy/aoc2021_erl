%%-------------------------------------------------------------------
%% @doc Utils
%%-------------------------------------------------------------------

-module(aoc).
-export([
    read_lines/1,
    read_ints/1,
    read_bits/1,
    read_cmds/1,
    read_bingo/3
]).

%%-------------------------------------------------------------------

-spec read_lines(Fn :: string()) -> [binary()].
read_lines(Fn0) ->
    read_lines(Fn0, [<<10>>], [global, trim]).

read_lines(Fn0, Sep, Opts) ->
    Fn = filename:join(code:priv_dir(aoc), Fn0),
    {ok, B} = file:read_file(Fn),
    binary:split(B, Sep, Opts).

-spec read_ints(Fn :: string()) -> [integer()].
read_ints(Fn) ->
    [binary_to_integer(I) || I <- read_lines(Fn)].

-spec read_cmds(Fn :: string()) -> [{1 | 2 | 3, integer()}].
read_cmds(Fn) ->
    read_cmds(read_lines(Fn), []).

-spec read_bits(Fn :: string()) -> [integer()].
read_bits(Fn) ->
    [binary_to_integer(I, 2) || I <- read_lines(Fn)].

-spec read_bingo(Fn :: string(), NRows :: integer(), NCols :: integer()) ->
            {Input :: [integer()], Numbers :: map(), Cards :: map(), Wins :: map()}.
read_bingo(Fn, NRows, NCols) ->
    Opts = [global, trim_all],
    [LHead | LCards] = read_lines(Fn, [<<10, 10>>], Opts),
    Input = [binary_to_integer(I) || I <- binary:split(LHead, [<<$,>>], Opts)],
    {Numbers, Cards, _} = lists:foldl(fun (LCard, {AccN, AccC, AccSz}) ->
        Nos = read_bingo_card(LCard, NRows, NCols),
        {AccN2, _} = lists:foldl(fun (No, {InAccN, InAccSz}) ->
            {InAccN#{No => [{AccSz, InAccSz} | maps:get(No, InAccN, [])]}, 1 + InAccSz}
        end, {AccN, 1}, Nos),
        AccC2 = AccC#{AccSz => sets:from_list(Nos, [{version,2}])},
        {AccN2, AccC2, 1 + AccSz}
    end, {#{}, #{}, 1}, LCards),
    {Input, Numbers, Cards, #{}}.

%%-------------------------------------------------------------------

read_bingo_card(LCard, NRows, NCols) ->
    Rows = binary:split(LCard, [<<10>>], [global, trim_all]),
    NRows = length(Rows),
    L = lists:map(fun (Row) ->
        Nos = binary:split(Row, [<<32>>], [global, trim_all]),
        NCols = length(Nos),
        [binary_to_integer(I) || I <- Nos]
    end, Rows),
    lists:append(L).

read_cmds([<<"forward ", R/binary>> | T], Acc) ->
    read_cmds(T, [{1, binary_to_integer(R)} | Acc]);

read_cmds([<<"down ", R/binary>> | T], Acc) ->
    read_cmds(T, [{2, binary_to_integer(R)} | Acc]);

read_cmds([<<"up ", R/binary>> | T], Acc) ->
    read_cmds(T, [{3, binary_to_integer(R)} | Acc]);

read_cmds([], Acc) ->
    lists:reverse(Acc).

%%-------------------------------------------------------------------
