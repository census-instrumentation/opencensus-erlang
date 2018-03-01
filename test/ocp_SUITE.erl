%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(ocp_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").
-include("oc_test_utils.hrl").

all() ->
    [with_span_tests, multiple_child_spans, attributes_test].

init_per_suite(Config) ->
    application:load(opencensus),
    application:set_env(opencensus, sampler, {oc_sampler_always, []}),
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

with_span_tests(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanName2 = <<"span-2">>,
    ocp:with_child_span(SpanName1, #{},
                        fun() ->
                          SpanCtx1 = ocp:current_span(),
                          TraceId = SpanCtx1#span_ctx.trace_id,
                          SpanId1 = SpanCtx1#span_ctx.span_id,
                          ocp:with_child_span(SpanName2, #{},
                                              fun() ->
                                                      ?assertMatch(#span_ctx{span_id=SpanId2,
                                                                             trace_id=TraceId}
                                                                   when SpanId2 =/= SpanId1, ocp:current_span())
                                              end)
                        end),
    ?assertMatch(undefined, ocp:current_span()),
    ok.

multiple_child_spans(Config) ->
    Tab = ?config(tid, Config),

    SpanName1 = <<"span-1">>,
    SpanName2 = <<"span-2">>,
    Attributes = #{key => value},

    ocp:with_child_span(SpanName1),
    ?OCP_FINISH(Tab),

    ocp:with_child_span(SpanName1),
    ocp:with_child_span(SpanName2, Attributes,
                        fun() ->
                                [ChildSpanData] = ets:lookup(?SPAN_TAB, (ocp:current_span())#span_ctx.span_id),
                                ?assertMatch(Attributes, ChildSpanData#span.attributes)
                        end),
    ?OCP_FINISH(Tab).

attributes_test(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"span-1">>,

    ocp:with_child_span(SpanName1),

    ocp:put_attribute(<<"attr-0">>, <<"value-0">>),
    ocp:put_attributes(#{<<"attr-0">> => true,
                         <<"attr-4">> => 5423,
                         <<"attr-5">> => <<"value-5">>}),

    ChildSpanName1 = <<"child-span-1">>,
    SpanCtx = ocp:with_child_span(ChildSpanName1),

    ocp:put_attribute(<<"attr-1">>, <<"value-1">>),
    ocp:put_attribute(<<"attr-2">>, 123),
    ocp:put_attribute(<<"attr-3">>, false),

    %% attribute keys must be binary strings and values must be binary strings, integers or booleans
    ?assertEqual({error, invalid_attribute}, ocp:put_attribute('attr-6', <<"value-6">>)),
    ?assertEqual({error, invalid_attribute}, ocp:put_attribute(<<"attr-7">>, 1.0)),

    ChildSpanCtx = ocp:current_span(),
    ?OCP_FINISH(Tab),
    ocp:with_span(SpanCtx),

    [FinishedChild] = ets:lookup(Tab, ChildSpanCtx#span_ctx.span_id),
    ?assertNot(maps:is_key(<<"attr-0">>, FinishedChild#span.attributes)),
    ?assertNot(maps:is_key(<<"attr-4">>, FinishedChild#span.attributes)),
    ?assertNot(maps:is_key(<<"attr-5">>, FinishedChild#span.attributes)),
    ?assertEqual(<<"value-1">>, maps:get(<<"attr-1">>, FinishedChild#span.attributes)),
    ?assertEqual(123, maps:get(<<"attr-2">>, FinishedChild#span.attributes)),
    ?assertEqual(false, maps:get(<<"attr-3">>, FinishedChild#span.attributes)),

    ?OCP_FINISH(Tab),

    [FinishedSpan] = ets:lookup(Tab, SpanCtx#span_ctx.span_id),
    ?assertEqual(SpanName1, FinishedSpan#span.name),
    ?assert(FinishedSpan#span.end_time > FinishedSpan#span.start_time),
    ?assertEqual(true, maps:get(<<"attr-0">>, FinishedSpan#span.attributes)),
    ?assertEqual(5423, maps:get(<<"attr-4">>, FinishedSpan#span.attributes)),
    ?assertEqual(<<"value-5">>, maps:get(<<"attr-5">>, FinishedSpan#span.attributes)).

