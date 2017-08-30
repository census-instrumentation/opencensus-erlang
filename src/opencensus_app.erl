%%%-------------------------------------------------------------------
%% @doc opencensus public API
%% @end
%%%-------------------------------------------------------------------

-module(opencensus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    opencensus_sup:start_link().

stop(_State) ->
    ok.
