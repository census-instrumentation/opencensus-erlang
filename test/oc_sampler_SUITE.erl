%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_sampler_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [never_sample, always_sample, probability_sample, probability_zero_sample,
     probability_hundred_sample, deterministic_probability].

init_per_suite(Config) ->
    application:load(opencensus),
    [{limit, 10} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(never_sample, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_never, []}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(always_sample, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_always, []}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(probability_sample, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_probability, [{probability, 0.5}]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(probability_zero_sample, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_probability, [{probability, 0.0}]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(probability_hundred_sample, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_probability, [{probability, 1.0}]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(deterministic_probability, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_probability, [{probability, 0.5}]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

never_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         SpanContext = oc_trace:start_span(<<"span">>, undefined),
                         oc_trace:is_enabled(SpanContext)
                     end, lists:seq(1, Limit)),

    ?assertEqual(0, length(L)),
    %% no garbage in ETS
    ?assertEqual(0, ets:info(?SPAN_TAB, size)).

always_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         %% include a test where 'undefined' is passed as the TC
                         SpanContext = oc_trace:start_span(<<"span">>, undefined),
                         oc_trace:is_enabled(SpanContext)
                     end, lists:seq(1, Limit)),
    ?assertEqual(Limit, length(L)),
    %% all spans are in ETS
    ?assertEqual(Limit, ets:info(?SPAN_TAB, size)).

probability_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         SpanContext = oc_trace:start_span(<<"span">>, undefined),
                         oc_trace:is_enabled(SpanContext)
                     end, lists:seq(1, Limit)),
    Length = length(L),
    ?assert(Length < Limit andalso Length > 0).

probability_zero_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         SpanContext = oc_trace:start_span(<<"span">>, undefined),
                         oc_trace:is_enabled(SpanContext)
                     end, lists:seq(1, Limit)),
    ?assertEqual(0, length(L)).

probability_hundred_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         %% include a test where an already generated id is passed as the TraceId
                         SpanContext = oc_trace:start_span(<<"span">>, undefined),
                         oc_trace:is_enabled(SpanContext)
                     end, lists:seq(1, Limit)),
    ?assertEqual(Limit, length(L)).

deterministic_probability(Config) ->
    Limit = ?config(limit, Config),

    TraceIds = [opencensus:generate_trace_id() || _ <- lists:seq(1, Limit)],

    lists:foldl(fun(_, Acc) ->
                    L = lists:filter(fun(TraceId) ->
                                         %% include a test where a TC record is passed as the TraceId
                                         SpanContext = oc_trace:start_span(<<"span">>, #span_ctx{trace_id=TraceId}),
                                         oc_trace:is_enabled(SpanContext)
                                     end, TraceIds),
                    %% verify that every list of sampled is the same
                    ?assert(lists:all(fun(X) -> X =:= L end, Acc)),
                    [L | Acc]
                end, [], lists:seq(1, Limit)).
