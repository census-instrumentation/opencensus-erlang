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
    [start_finish, child_spans, noops, attributes_test].

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
    TraceId = opencensus:generate_trace_id(),
    Span1 = opencensus:start_span(SpanName1, TraceId),
    ?assertMatch({T, O} when is_integer(T)
                           andalso is_integer(O), Span1#span.start_time),
    ?assertMatch(TraceId, Span1#span.trace_id),

    Span2 = opencensus:finish_span(Span1),

    ?assertEqual(SpanName1, Span2#span.name),
    ?assert(Span2#span.end_time > Span2#span.start_time).

child_spans(_Config) ->
    SpanName1 = <<"span-1">>,
    Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined, #{}),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1),
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
    ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1),
    ChildSpan2 = opencensus:finish_span(ChildSpan1),
    Span2 = opencensus:finish_span(Span1),

    ?assertEqual(undefined, ChildSpan2),
    ?assertEqual(undefined, Span2).

attributes_test(_Config) ->
    SpanName1 = <<"span-1">>,
    Attributes = #{<<"attr-1">> => <<"value-1">>,
                   <<"attr-2">> => 123,
                   <<"attr-3">> => true},
    Span0 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined, #{}),
    Span1 = opencensus:put_attribute(<<"attr-0">>, <<"value-0">>, Span0),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1),
    ?assertEqual(ChildSpanName1, ChildSpan1#span.name),
    ?assertEqual(Span1#span.span_id, ChildSpan1#span.parent_span_id),

    ChildSpan2 = opencensus:put_attribute(<<"attr-1">>, <<"value-1">>, ChildSpan1),
    ChildSpan3 = opencensus:put_attribute(<<"attr-2">>, 123, ChildSpan2),
    ChildSpan4 = opencensus:put_attribute(<<"attr-3">>, true, ChildSpan3),

    ChildSpan5 = opencensus:finish_span(ChildSpan4),
    ?assert(ChildSpan5#span.end_time > ChildSpan1#span.start_time),
    ?assertNot(maps:is_key(<<"attr-0">>, ChildSpan5#span.attributes)),
    ?assertEqual(<<"value-1">>, maps:get(<<"attr-1">>, ChildSpan5#span.attributes)),
    ?assertEqual(123, maps:get(<<"attr-2">>, ChildSpan5#span.attributes)),
    ?assertEqual(true, maps:get(<<"attr-3">>, ChildSpan5#span.attributes)),

    SpanA1 = opencensus:start_span(ChildSpanName1, Span1, Attributes),
    SpanA2 = opencensus:start_span(ChildSpanName1, Span1#span.trace_id, Span1#span.span_id, Attributes),
    ?assertEqual(ChildSpan4#span.attributes, SpanA1#span.attributes),
    ?assertEqual(ChildSpan4#span.attributes, SpanA2#span.attributes),

    Span2 = opencensus:finish_span(Span1),

    ?assertEqual(SpanName1, Span2#span.name),
    ?assert(Span2#span.end_time > Span2#span.start_time),
    ?assertEqual(<<"value-0">>, maps:get(<<"attr-0">>, Span2#span.attributes)).
