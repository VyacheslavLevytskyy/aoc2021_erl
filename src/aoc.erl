%%-------------------------------------------------------------------
%% @doc Utils
%%-------------------------------------------------------------------

-module(aoc).
-export([read_lines/1, read_ints/1, read_cmds/1, read_bits/1]).

%%-------------------------------------------------------------------

-spec read_lines(Fn :: string()) -> [binary()].
read_lines(Fn0) ->
    Fn = filename:join(code:priv_dir(aoc), Fn0),
    {ok, B} = file:read_file(Fn),
    binary:split(B, [<<10>>], [global, trim]).

-spec read_ints(Fn :: string()) -> [integer()].
read_ints(Fn) ->
    [binary_to_integer(I) || I <- read_lines(Fn)].

-spec read_cmds(Fn :: string()) -> [{1 | 2 | 3, integer()}].
read_cmds(Fn) ->
    read_cmds(read_lines(Fn), []).

-spec read_bits(Fn :: string()) -> [binary()].
read_bits(Fn) ->
    [binary_to_integer(I, 2) || I <- read_lines(Fn)].

%%-------------------------------------------------------------------

read_cmds([<<"forward ", R/binary>> | T], Acc) ->
    read_cmds(T, [{1, binary_to_integer(R)} | Acc]);

read_cmds([<<"down ", R/binary>> | T], Acc) ->
    read_cmds(T, [{2, binary_to_integer(R)} | Acc]);

read_cmds([<<"up ", R/binary>> | T], Acc) ->
    read_cmds(T, [{3, binary_to_integer(R)} | Acc]);

read_cmds([], Acc) ->
    lists:reverse(Acc).

%%-------------------------------------------------------------------
