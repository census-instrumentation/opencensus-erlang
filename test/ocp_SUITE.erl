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
    [with_span_tests,
     multiple_child_spans,
     attributes_test,
     spawns_tests].

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
    application:set_env(opencensus, reporters, [{oc_tab_reporter, []}]),
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
                                SpanCtx1 = ocp:current_span_ctx(),
                                TraceId = SpanCtx1#span_ctx.trace_id,
                                SpanId1 = SpanCtx1#span_ctx.span_id,
                                ocp:with_child_span(SpanName2, #{},
                                                    fun() ->
                                                            ?assertMatch(#span_ctx{span_id=SpanId2,
                                                                                   trace_id=TraceId}
                                                                         when SpanId2 =/= SpanId1,
                                                                              ocp:current_span_ctx())
                                                    end)
                        end),
    ?assertMatch(undefined, ocp:current_span_ctx()),
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
                                [ChildSpanData] = ets:lookup(?SPAN_TAB, (ocp:current_span_ctx())#span_ctx.span_id),
                                ?assertMatch(Attributes, ChildSpanData#span.attributes)
                        end),
    ?OCP_FINISH(Tab),

    ocp:with_child_span(SpanName1),
    Ctx1 = ocp:current_span_ctx(),
    ocp:with_child_span(SpanName2),
    Ctx2 = ocp:current_span_ctx(),
    ?assertNotMatch(Ctx1, Ctx2),
    ocp:finish_span(),
    ?assertEqual(Ctx1, ocp:current_span_ctx()).

attributes_test(Config) ->
    Tab = ?config(tid, Config),
    SpanName1 = <<"span-1">>,

    ocp:with_child_span(SpanName1, #{<<"attr-4">> => 5423}),

    ocp:put_attribute(<<"attr-0">>, <<"value-0">>),
    ocp:put_attributes(#{<<"attr-0">> => true,
                         <<"attr-5">> => <<"value-5">>}),

    ChildSpanName1 = <<"child-span-1">>,
    SpanCtx = ocp:with_child_span(ChildSpanName1),

    ocp:put_attribute(<<"attr-1">>, <<"value-1">>),
    ocp:put_attribute(<<"attr-2">>, 123),
    ocp:put_attribute(<<"attr-3">>, false),

    %% attribute keys must be binary strings and values must be binary strings, integers or booleans
    ?assertEqual({error, invalid_attribute}, ocp:put_attribute('attr-6', <<"value-6">>)),
    ?assertEqual({error, invalid_attribute}, ocp:put_attribute(<<"attr-7">>, 1.0)),

    ChildSpanCtx = ocp:current_span_ctx(),
    ?OCP_FINISH(Tab),
    ocp:with_span_ctx(SpanCtx),

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

spawns_tests(_Config) ->
    erlang:process_flag(trap_exit, true),

    Self = self(),
    RootSpanCtx = oc_trace:start_span("Root", undefined),
    ocp:with_span_ctx(RootSpanCtx),

    Tags = oc_tags:new(#{type => "mpeg",
                         category => "category1"}),
    ocp:with_tags(Tags),


    ocp:spawn(spawn_fn()),
    receive_context(),

    ocp:spawn(node(), spawn_fn()),
    receive_context(),

    ocp:spawn(?MODULE, spawn_fn, [Self]),
    receive_context(),

    ocp:spawn(node(), ?MODULE, spawn_fn, [Self]),
    receive_context(),


    Pid1 = ocp:spawn_link(spawn_fn()),
    receive_context(),
    receive_exit(Pid1),

    Pid2 = ocp:spawn_link(node(), spawn_fn()),
    receive_context(),
    receive_exit(Pid2),

    Pid3 = ocp:spawn_link(?MODULE, spawn_fn, [Self]),
    receive_context(),
    receive_exit(Pid3),

    Pid4 = ocp:spawn_link(node(), ?MODULE, spawn_fn, [Self]),
    receive_context(),
    receive_exit(Pid4),


    PM1 = ocp:spawn_monitor(spawn_fn()),
    receive_context(),
    receive_down(PM1),

    PM2 = ocp:spawn_monitor(spawn_fn()),
    receive_context(),
    receive_down(PM2),


    PM3 = ocp:spawn_opt(spawn_fn(), [monitor]),
    receive_context(),
    receive_down(PM3),

    PM4 = ocp:spawn_opt(node(), spawn_fn(), [monitor]),
    receive_context(),
    receive_down(PM4),

    PM5 = ocp:spawn_opt(?MODULE, spawn_fn, [Self], [monitor]),
    receive_context(),
    receive_down(PM5),

    PM6 = ocp:spawn_opt(node(), ?MODULE, spawn_fn, [Self], [monitor]),
    receive_context(),
    receive_down(PM6).

spawn_fn() ->
    Self = self(),

    fun () ->
            spawn_fn(Self)
    end.

spawn_fn(Self) ->
    Self ! {context, ocp:current_span_ctx(), ocp:current_tags()}.

receive_context() ->
    RootSpanCtx = ocp:current_span_ctx(),
    Tags = ocp:current_tags(),

    receive
        {context, FCtx, FTags} ->
            ?assertMatch(RootSpanCtx, FCtx),
            ?assertEqual(Tags, FTags)
    after
        6000 ->
            ct:fail("Spawn* failed")
    end.

receive_exit(From) ->
    receive
        {'EXIT', From, normal} ->
            ok
    after
        6000 ->
            ct:fail("Exit receive failed")
    end.

receive_down({Pid, MRef}) ->
    receive
        {'DOWN', MRef, process, Pid, normal} ->
            ok
    after
        6000 ->
            ct:fail("Down receive failed")
    end.
