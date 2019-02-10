-module(oc_reporter_stdout).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2]).

init(_) ->
    ok.

handle_call(_Msg, State) -> {ok, ok, State}.

handle_event({spans, Spans}, State) ->
    [io:format("~p~n", [Span]) || Span <- Spans],

    {ok, State}.
