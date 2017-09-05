%%%-------------------------------------------------------------------
%% @doc opencensus functionality using the pdict to track trace context
%% @end
%%%-------------------------------------------------------------------
-module(ocp).

-export([start_span/1,
         child_span/2]).

-include("opencensus.hrl").

start_span(Name) ->
    opencensus:start_span(Name, undefined, undefined).

child_span(Name, Span) ->
    opencensus:child_span(Name, Span).
