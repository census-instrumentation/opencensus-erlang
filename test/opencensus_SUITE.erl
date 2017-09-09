%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(opencensus_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [start_finish, child_spans, noops].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(opencensus).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

start_finish(_Config) ->
    SpanName1 = <<"span-1">>,
    Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined),
    ?assertMatch({T, O} when is_integer(T)
                           andalso is_integer(O), Span1#span.start_time),

    Span2 = opencensus:finish_span(Span1),

    ?assertEqual(SpanName1, Span2#span.name),
    ?assert(Span2#span.end_time > Span2#span.start_time).

child_spans(_Config) ->
    SpanName1 = <<"span-1">>,
    Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpan1 = opencensus:child_span(ChildSpanName1, Span1),
    ?assertEqual(ChildSpanName1, ChildSpan1#span.name),
    ?assertEqual(Span1#span.span_id, ChildSpan1#span.parent_span_id),
    ChildSpan2 = opencensus:finish_span(ChildSpan1),
    ?assert(ChildSpan2#span.end_time > ChildSpan2#span.start_time),

    Span2 = opencensus:finish_span(Span1),
    ?assertEqual(SpanName1, Span2#span.name),
    ?assert(Span2#span.end_time > Span2#span.start_time).

noops(_Config) ->
    SpanName1 = <<"span-1">>,
    Span1 = opencensus:start_span(SpanName1, undefined, undefined),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpan1 = opencensus:child_span(ChildSpanName1, Span1),
    ChildSpan2 = opencensus:finish_span(ChildSpan1),
    Span2 = opencensus:finish_span(Span1),

    ?assertEqual(undefined, ChildSpan2),
    ?assertEqual(undefined, Span2).
