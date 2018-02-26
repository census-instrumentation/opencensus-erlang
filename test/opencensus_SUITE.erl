%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(opencensus_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").
-include("oc_test_utils.hrl").

all() ->
    [start_finish, child_spans, noops, attributes_test,
     links, time_events, status].

init_per_suite(Config) ->
    application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Tab = ets:new(reporter_tab, [public, {write_concurrency, true},
                                 {read_concurrency, true}, {keypos, #span.span_id}]),
    application:set_env(opencensus, send_interval_ms, 1),
    application:set_env(opencensus, reporter, {oc_tab_reporter, []}),
    application:set_env(opencensus, tab_reporter, #{tid => Tab}),
    {ok, _} = application:ensure_all_started(opencensus),
    [{tid, Tab} | Config].

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

start_finish(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"span-1">>,
    TraceId = opencensus:generate_trace_id(),
    SpanCtx = oc_trace:start_span(SpanName1, #span_ctx{trace_id=TraceId}),
    ?FINISH(Tab, SpanCtx),

    [SpanData] = ets:lookup(Tab, SpanCtx#span_ctx.span_id),
    ?assertEqual(SpanName1, SpanData#span.name),
    ?assert(SpanData#span.end_time > SpanData#span.start_time).

child_spans(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"span-1">>,
    SpanCtx = oc_trace:start_span(SpanName1, undefined),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = oc_trace:start_span(ChildSpanName1, SpanCtx),

    ?FINISH(Tab, ChildSpanCtx),
    ?FINISH(Tab, SpanCtx),

    [ChildSpanData] = ets:lookup(Tab, ChildSpanCtx#span_ctx.span_id),
    ?assertEqual(SpanCtx#span_ctx.span_id, ChildSpanData#span.parent_span_id),
    ?assert(ChildSpanData#span.end_time > ChildSpanData#span.start_time),

    [SpanData] = ets:lookup(Tab, SpanCtx#span_ctx.span_id),
    ?assertEqual(SpanName1, SpanData#span.name),
    ?assert(SpanData#span.end_time > SpanData#span.start_time).

noops(_Config) ->
    SpanCtx = #span_ctx{trace_options=0},

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = oc_trace:start_span(ChildSpanName1, SpanCtx),
    oc_trace:finish_span(ChildSpanCtx),
    oc_trace:finish_span(SpanCtx),

    ?assertEqual(0, ChildSpanCtx#span_ctx.trace_options),
    ?assertEqual(0, SpanCtx#span_ctx.trace_options).

attributes_test(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"span-1">>,
    SpanCtx0 = oc_trace:start_span(SpanName1, undefined),
    oc_trace:put_attribute(<<"attr-0">>, <<"value-0">>, SpanCtx0),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx1 = oc_trace:start_span(ChildSpanName1, SpanCtx0),

    [ChildSpan1Data] = ets:lookup(?SPAN_TAB, ChildSpanCtx1#span_ctx.span_id),
    ?assertEqual(ChildSpanName1, ChildSpan1Data#span.name),
    ?assertEqual(SpanCtx0#span_ctx.span_id, ChildSpan1Data#span.parent_span_id),

    oc_trace:put_attribute(<<"attr-1">>, <<"value-1">>, ChildSpanCtx1),
    oc_trace:put_attribute(<<"attr-2">>, 123, ChildSpanCtx1),
    oc_trace:put_attribute(<<"attr-3">>, true, ChildSpanCtx1),

    ?FINISH(Tab, ChildSpanCtx1),

    [ChildSpan1Data1] = ets:lookup(Tab, ChildSpanCtx1#span_ctx.span_id),
    ?assert(ChildSpan1Data1#span.end_time > ChildSpan1Data1#span.start_time),
    ?assertNot(maps:is_key(<<"attr-0">>, ChildSpan1Data1#span.attributes)),
    ?assertEqual(<<"value-1">>, maps:get(<<"attr-1">>, ChildSpan1Data1#span.attributes)),
    ?assertEqual(123, maps:get(<<"attr-2">>, ChildSpan1Data1#span.attributes)),
    ?assertEqual(true, maps:get(<<"attr-3">>, ChildSpan1Data1#span.attributes)),

    ?FINISH(Tab, SpanCtx0),

    [Span1Data] = ets:lookup(Tab, SpanCtx0#span_ctx.span_id),
    ?assertEqual(SpanName1, Span1Data#span.name),
    ?assert(Span1Data#span.end_time > Span1Data#span.start_time),
    ?assertEqual(<<"value-0">>, maps:get(<<"attr-0">>, Span1Data#span.attributes)).

links(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"span-1">>,
    Span0TraceId = opencensus:generate_trace_id(),
    SpanCtx0 = oc_trace:start_span(SpanName1, #span_ctx{trace_id=Span0TraceId}),

    SpanWithLinkName = <<"span-with-link-1">>,
    SpanWithLinkCtx0 = oc_trace:start_span(SpanWithLinkName, undefined),

    LinkAttributes = #{<<"attr-1">> => <<"value-1">>},
    Link = oc_trace:link(?LINK_TYPE_CHILD_LINKED_SPAN, Span0TraceId,
                         SpanCtx0#span_ctx.span_id, LinkAttributes),
    oc_trace:add_link(Link, SpanWithLinkCtx0),
    ?FINISH(Tab, SpanWithLinkCtx0),

    [SpanWithLinkData] = ets:lookup(Tab, SpanWithLinkCtx0#span_ctx.span_id),
    ?assertEqual([#link{type=?LINK_TYPE_CHILD_LINKED_SPAN,
                        trace_id=Span0TraceId,
                        span_id=SpanCtx0#span_ctx.span_id,
                        attributes=LinkAttributes}], SpanWithLinkData#span.links),

    ?FINISH(Tab, SpanCtx0),
    [SpanData] = ets:lookup(Tab, SpanCtx0#span_ctx.span_id),
    ?assert(SpanData#span.end_time > SpanData#span.start_time),
    ok.


time_events(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"time-events-span-1">>,
    Span0TraceId = opencensus:generate_trace_id(),
    SpanCtx = oc_trace:start_span(SpanName1, #span_ctx{trace_id=Span0TraceId}),

    Description = <<"annotation description">>,
    Attributes = #{<<"attr-1">> => <<"value-1">>},
    Annotation = oc_trace:annotation(Description, Attributes),
    Timestamp1 = wts:timestamp(),
    oc_trace:add_time_event(Timestamp1, Annotation, SpanCtx),

    MessageEventType = ?MESSAGE_EVENT_TYPE_SENT,
    MessageEventId = 34,
    UncompressedSize = 100,
    CompressedSize = 64,
    MessageEvent = oc_trace:message_event(MessageEventType, MessageEventId, UncompressedSize, CompressedSize),

    oc_trace:add_time_event(MessageEvent, SpanCtx),

    ?FINISH(Tab, SpanCtx),
    [SpanData] = ets:lookup(Tab, SpanCtx#span_ctx.span_id),
    ?assert(SpanData#span.end_time > SpanData#span.start_time),
    ?assertMatch([{Timestamp2, #message_event{type=MessageEventType,
                                              id=MessageEventId,
                                              uncompressed_size=UncompressedSize,
                                              compressed_size=CompressedSize}},
                  {Timestamp1, #annotation{description=Description,
                                           attributes=Attributes}}]
                 when Timestamp2 > Timestamp1, SpanData#span.time_events),
    ok.


status(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"status-span-1">>,
    SpanCtx = oc_trace:start_span(SpanName1, undefined),

    Code = 1,
    Message = <<"I am a status">>,
    oc_trace:set_status(Code, Message, SpanCtx),

    ?FINISH(Tab, SpanCtx),

    [SpanData] = ets:lookup(Tab, SpanCtx#span_ctx.span_id),
    ?assertEqual(SpanName1, SpanData#span.name),
    ?assert(SpanData#span.end_time > SpanData#span.start_time),
    ?assertMatch(#status{code=Code,
                         message=Message}, SpanData#span.status).
