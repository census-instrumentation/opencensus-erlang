-module(oc_reporter_stdout).

-export([init/1,
         report/2]).

init(_) ->
    ok.

report(Spans, _) ->
    [io:format("~p~n", [Span]) || Span <- Spans].
