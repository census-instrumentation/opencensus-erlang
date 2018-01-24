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
    [start_finish, child_spans, noops, attributes_test,
     links, time_events, status].

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

links(_Config) ->
    SpanName1 = <<"span-1">>,
    Span0TraceId = opencensus:generate_trace_id(),
    Span0 = opencensus:start_span(SpanName1, Span0TraceId),

    SpanWithLinkName = <<"span-with-link-1">>,
    SpanWithLink0 = opencensus:start_span(SpanWithLinkName, opencensus:generate_trace_id()),

    LinkAttributes = #{<<"attr-1">> => <<"value-1">>},
    Link = opencensus:link(?LINK_TYPE_CHILD_LINKED_SPAN, Span0TraceId,
                           Span0#span.span_id, LinkAttributes),
    SpanWithLink1 = opencensus:add_link(Link, SpanWithLink0),
    SpanWithLink2 = opencensus:finish_span(SpanWithLink1),

    ?assertEqual([#link{type=?LINK_TYPE_CHILD_LINKED_SPAN,
                        trace_id=Span0TraceId,
                        span_id=Span0#span.span_id,
                        attributes=LinkAttributes}], SpanWithLink2#span.links),

    Span1 = opencensus:finish_span(Span0),
    ?assert(Span1#span.end_time > Span1#span.start_time),
    ok.


time_events(_Config) ->
    SpanName1 = <<"time-events-span-1">>,
    Span0TraceId = opencensus:generate_trace_id(),
    Span0 = opencensus:start_span(SpanName1, Span0TraceId),

    Description = <<"annotation description">>,
    Attributes = #{<<"attr-1">> => <<"value-1">>},
    Annotation = opencensus:annotation(Description, Attributes),
    Timestamp1 = wts:timestamp(),
    Span1 = opencensus:add_time_event(Timestamp1, Annotation, Span0),

    MessageEventType = ?MESSAGE_EVENT_TYPE_SENT,
    MessageEventId = 34,
    UncompressedSize = 100,
    CompressedSize = 64,
    MessageEvent = opencensus:message_event(MessageEventType, MessageEventId, UncompressedSize, CompressedSize),

    Span2 = opencensus:add_time_event(MessageEvent, Span1),

    Span3 = opencensus:finish_span(Span2),
    ?assert(Span3#span.end_time > Span3#span.start_time),
    ?assertMatch([{Timestamp2, #message_event{type=MessageEventType,
                                              id=MessageEventId,
                                              uncompressed_size=UncompressedSize,
                                              compressed_size=CompressedSize}},
                  {Timestamp1, #annotation{description=Description,
                                           attributes=Attributes}}]
                 when Timestamp2 > Timestamp1, Span3#span.time_events),
    ok.


status(_Config) ->
    SpanName1 = <<"status-span-1">>,
    Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id()),

    Code = 1,
    Message = <<"I am a status">>,
    Span2 = opencensus:set_status(Code, Message, Span1),

    Span3 = opencensus:finish_span(Span2),

    ?assertEqual(SpanName1, Span3#span.name),
    ?assert(Span3#span.end_time > Span3#span.start_time),
    ?assertMatch(#status{code=Code,
                         message=Message}, Span3#span.status).
