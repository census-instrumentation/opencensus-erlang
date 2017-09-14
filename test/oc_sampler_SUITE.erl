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
    [never_sample, always_sample, probability_sample, probability_0_sample,
     probability_100_sample, deterministic_probability].

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
init_per_testcase(probability_0_sample, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_probability, [{probability, 0.0}]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(probability_100_sample, Config) ->
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
                         TraceContext = opencensus:start_trace(),
                         TraceContext#trace_context.enabled
                     end, lists:seq(1, Limit)),

    ?assertEqual(0, length(L)).

always_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         %% include a test where 'undefined' is passed as the TC
                         TraceContext = opencensus:start_trace(undefined),
                         TraceContext#trace_context.enabled
                     end, lists:seq(1, Limit)),
    ?assertEqual(Limit, length(L)).

probability_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         TraceContext = opencensus:start_trace(),
                         TraceContext#trace_context.enabled
                     end, lists:seq(1, Limit)),
    Length = length(L),
    ?assert(Length < Limit andalso Length > 0).

probability_0_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         TraceContext = opencensus:start_trace(),
                         TraceContext#trace_context.enabled
                     end, lists:seq(1, Limit)),
    ?assertEqual(0, length(L)).

probability_100_sample(Config) ->
    Limit = ?config(limit, Config),
    L = lists:filter(fun(_) ->
                         %% include a test where an already generated id is passed as the TraceId
                         TraceContext = opencensus:start_trace(opencensus:generate_trace_id()),
                         TraceContext#trace_context.enabled
                     end, lists:seq(1, Limit)),
    ?assertEqual(Limit, length(L)).

deterministic_probability(Config) ->
    Limit = ?config(limit, Config),

    TraceIds = [opencensus:generate_trace_id() || _ <- lists:seq(1, Limit)],

    lists:foldl(fun(_, Acc) ->
                    L = lists:filter(fun(TraceId) ->
                                         %% include a test where a TC record is passed as the TraceId
                                         TraceContext = opencensus:start_trace(#trace_context{trace_id=TraceId}),
                                         TraceContext#trace_context.enabled
                                     end, TraceIds),
                    %% verify that every list of sampled is the same
                    ?assert(lists:all(fun(X) -> X =:= L end, Acc)),
                    [L | Acc]
                end, [], lists:seq(1, Limit)).
