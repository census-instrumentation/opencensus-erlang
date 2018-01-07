
%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_reporters_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [pid_reporter,
     sequential_reporter].

init_per_suite(Config) ->
    ok = application:load(opencensus),
    Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(pid_reporter, Config) ->
    application:set_env(opencensus, reporter, {oc_pid_reporter, []}),
    application:set_env(opencensus, pid_reporter, #{pid => self()}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(sequential_reporter, Config) ->
    application:set_env(opencensus, reporter, {oc_sequential_reporter, [{oc_pid_reporter, []},
                                                                        {oc_pid_reporter, []}]}),
    application:set_env(opencensus, pid_reporter, #{pid => self()}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

pid_reporter(_Config) ->
    SpanName1 = <<"span-1">>,
    Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined, #{}),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1),
    ?assertEqual(ChildSpanName1, ChildSpan1#span.name),
    ?assertEqual(Span1#span.span_id, ChildSpan1#span.parent_span_id),
    opencensus:finish_span(ChildSpan1),
    opencensus:finish_span(Span1),

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
                      end
                  end, [SpanName1, ChildSpanName1]).

sequential_reporter(_Config) ->
    SpanName1 = <<"span-1">>,
    Span1 = opencensus:start_span(SpanName1, opencensus:generate_trace_id(), undefined, #{}),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpan1 = opencensus:start_span(ChildSpanName1, Span1),
    opencensus:finish_span(ChildSpan1),
    opencensus:finish_span(Span1),

    SortedNames = [ChildSpanName1, ChildSpanName1, SpanName1, SpanName1],

    Received = lists:map(fun(Name) ->
                             receive
                                 {span, #span{name = Name}} ->
                                     Name
                             after 5000 ->
                                     undefined
                             end
                         end, SortedNames), %% receive order is undefined though

    ?assertMatch(SortedNames, lists:sort(Received)).
