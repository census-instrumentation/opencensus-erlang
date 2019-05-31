%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_transform_SUITE).

-compile({parse_transform, oc_span_transform}).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [trace_transform].

init_per_suite(Config) ->
    application:load(opencensus),
    application:set_env(opencensus, sampler, {oc_sampler_always, []}),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Reporters = [{oc_reporter_pid, self()}],
    application:set_env(opencensus, trace, [{interval, 1},
                                            {handlers, Reporters}]),
    {ok, _} = application:ensure_all_started(opencensus),

    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus).

trace_transform(_Config) ->
    SpanName1 = <<"span-1">>,

    ocp:with_child_span(SpanName1),
    traced_function(),
    ocp:finish_span(),

    %% verify all spans, including spans for the transform using functions are reported
    lists:foreach(fun(Name) ->
                      receive
                          {span, S=#span{name=Name}} ->
                              %% Verify the end time and duration are set when the span was finished
                              ?assertMatch({ST, O} when is_integer(ST)
                                                      andalso is_integer(O), S#span.start_time),
                              ?assertMatch({ST, O} when is_integer(ST)
                                                      andalso is_integer(O), S#span.end_time)
                      after 1000 ->
                              ct:fail("Did not receive any message in 1s")
                      end
                  end, [SpanName1, <<"oc_transform_SUITE:traced_function/0">>, <<"my_name">>]).

-span([]).
traced_function() ->
    another_traced_function().

-span(<<"my_name">>).
another_traced_function() ->
    trace_this.
