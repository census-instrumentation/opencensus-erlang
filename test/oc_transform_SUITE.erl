%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_transform_SUITE).

-compile({parse_transform, oc_transform}).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [trace_transform].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    application:load(opencensus),
    application:set_env(opencensus, reporter, {oc_pid_reporter, []}),
    application:set_env(opencensus, pid_reporter, #{pid => self()}),

    {ok, _} = application:ensure_all_started(opencensus),

    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

trace_transform(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanName2 = <<"span-2">>,
    ocp:start_trace(),
    ocp:start_span(SpanName1),

    traced_function(),

    ocp:start_span(SpanName2),
    ?assertMatch(#span{name=SpanName2}, ocp:finish_span()),
    ?assertMatch(#span{name=SpanName1}, ocp:finish_span()),

    ?assertMatch(undefined, ocp:finish_span()),

    %% verify all spans, including spans for the transform using functions are reported
    lists:foreach(fun(Name) ->
                      receive
                          {span, S=#span{name = Name}} ->
                              %% Verify the end time and duration are set when the span was finished
                              ?assertMatch({ST, O} when is_integer(ST)
                                                      andalso is_integer(O), S#span.start_time),
                              ?assertMatch({ST, O} when is_integer(ST)
                                                      andalso is_integer(O), S#span.end_time)
                      after 1000 ->
                              error(timeout)
                      end
                  end, [SpanName1, <<"oc_transform_SUITE:traced_function/0">>, <<"my_name">>, SpanName2]).

-trace([]).
traced_function() ->
    another_traced_function().

-trace(<<"my_name">>).
another_traced_function() ->
    trace_this.
