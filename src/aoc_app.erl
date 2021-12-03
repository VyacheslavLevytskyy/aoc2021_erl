%%%-------------------------------------------------------------------
%% @doc
%% @end
%%%-------------------------------------------------------------------

-module(aoc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
