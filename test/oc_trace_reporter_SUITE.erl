%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_trace_reporter_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [pid_reporter,
     dynamically_register_reporter,
     sequential_reporter].

init_per_suite(Config) ->
    ok = application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok = application:unload(opencensus),
    ok.

init_per_testcase(pid_reporter, Config) ->
    Reporters = [{oc_reporter_pid, self()}],
    application:set_env(opencensus, trace, [{interval, 1},
                                            {handlers, Reporters}]),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(dynamically_register_reporter, Config) ->
    application:set_env(opencensus, trace, [{interval, 1}]),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(sequential_reporter, Config) ->
    Reporters = [{{oc_reporter_pid, 1}, self()},
                 {{oc_reporter_pid, 2}, self()}],
    application:set_env(opencensus, trace, [{interval, 1},
                                            {handlers, Reporters}]),
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

pid_reporter(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanCtx = oc_trace:start_span(SpanName1, undefined),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = oc_trace:start_span(ChildSpanName1, SpanCtx),

    [ChildSpanData] = ets:lookup(?SPAN_TAB, ChildSpanCtx#span_ctx.span_id),
    ?assertEqual(ChildSpanName1, ChildSpanData#span.name),
    ?assertEqual(SpanCtx#span_ctx.span_id, ChildSpanData#span.parent_span_id),

    ok = oc_trace:finish_span(ChildSpanCtx),
    ok = oc_trace:finish_span(SpanCtx),

    %% Order the spans are reported is undefined, so use a selective receive to make
    %% sure we get them all
    lists:foreach(fun(Name) ->
                          receive
                              {span, S=#span{name = Name}} ->
                                  %% Verify the end time and duration are set when the span was finished
                                  ?assertMatch({ST, O} when is_integer(ST)
                                                            andalso is_integer(O), S#span.start_time),
                                  ?assertMatch({ST, O} when is_integer(ST)
                                                            andalso is_integer(O), S#span.end_time)
                          after
                            5000 -> ct:fail("Do not received any message in requested time (5s)")
                          end
                  end, [SpanName1, ChildSpanName1]).

dynamically_register_reporter(_Config) ->
    oc_trace:add_handler(oc_reporter_pid, self()),

    SpanName1 = <<"span-1">>,
    SpanCtx = oc_trace:start_span(SpanName1, undefined),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = oc_trace:start_span(ChildSpanName1, SpanCtx),

    [ChildSpanData] = ets:lookup(?SPAN_TAB, ChildSpanCtx#span_ctx.span_id),
    ?assertEqual(ChildSpanName1, ChildSpanData#span.name),
    ?assertEqual(SpanCtx#span_ctx.span_id, ChildSpanData#span.parent_span_id),

    oc_trace:finish_span(ChildSpanCtx),
    oc_trace:finish_span(SpanCtx),

    %% Order the spans are reported is undefined, so use a selective receive to make
    %% sure we get them all
    lists:foreach(fun(Name) ->
                          receive
                              {span, S=#span{name = Name}} ->
                                  %% Verify the end time and duration are set when the span was finished
                                  ?assertMatch({ST, O} when is_integer(ST)
                                                            andalso is_integer(O), S#span.start_time),
                                  ?assertMatch({ST, O} when is_integer(ST)
                                                            andalso is_integer(O), S#span.end_time)
                          after
                            5000 -> ct:fail("Do not received any message in requested time (5s)")
                          end
                  end, [SpanName1, ChildSpanName1]).

sequential_reporter(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanCtx = oc_trace:start_span(SpanName1, undefined),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = oc_trace:start_span(ChildSpanName1, SpanCtx),

    oc_trace:finish_span(ChildSpanCtx),
    oc_trace:finish_span(SpanCtx),

    SortedNames = [ChildSpanName1, ChildSpanName1, SpanName1, SpanName1],

    Received = lists:map(fun(Name) ->
                                 receive
                                     {span, #span{name = Name}} ->
                                         Name
                                 after 1000 ->
                                         undefined
                                 end
                         end, SortedNames), %% receive order is undefined though

    ?assertMatch(SortedNames, lists:sort(Received)).
