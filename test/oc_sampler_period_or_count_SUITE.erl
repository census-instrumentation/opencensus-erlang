%%%------------------------------------------------------------------------
%% Copyright 2019, OpenCensus Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% @end
%%%-----------------------------------------------------------------------
-module(oc_sampler_period_or_count_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     period_sample_1,
     period_sample_2,
     count_sample_1,
     count_sample_2,
     count_and_period_sample_1,
     count_and_period_sample_2,
     count_and_period_sample_3,
     count_and_period_sample_4
    ].

init_per_suite(Config) ->
    application:load(opencensus),

    %% limit is a count of traces to run
    %% delay is a time delay between traces, in milliseconds
    [{limit, 10000}, {delay, 5} | Config].

init_per_testcase(period_sample_1, Config) ->
    Config1 = [{period, 1}, {count, 0} | Config],

    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1;

init_per_testcase(period_sample_2, Config) ->
    Config1 = [{period, 10}, {count, 0} | Config],
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1;

init_per_testcase(count_sample_1, Config) ->
    Config1 = [{period, 0}, {count, 1} | Config],
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1;

init_per_testcase(count_sample_2, Config) ->
    Config1 = [{period, 0}, {count, 10} | Config],
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1;

init_per_testcase(count_and_period_sample_1, Config) ->
    Config1 = [{period, 5}, {count, 1} | Config],
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1;

init_per_testcase(count_and_period_sample_2, Config) ->
    Config1 = [{period, 5}, {count, 50} | Config],
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1;

init_per_testcase(count_and_period_sample_3, Config) ->
    Config1 = [{period, 2}, {count, 100} | Config],
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1;

init_per_testcase(count_and_period_sample_4, Config) ->
    Config1 = [{period, 2}, {count, 100} | Config],
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count, [
                                                      {period, ?config(period, Config1)},
                                                      {count, ?config(count, Config1)}
                                                     ]}),
    {ok, _} = application:ensure_all_started(opencensus),
    Config1.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

end_per_suite(_Config) ->
    ok.

period_sample_1(Config) ->

    %% expecting 1 trace after every 2 second
    %% count = 0 doesn't affect on result
    {Time, Result} = run_tracing(Config),
    DesiredResult = desired_result(Config, Time),

    ?assert(abs(DesiredResult - Result) =< 5).

period_sample_2(Config) ->
    %% expecting 1 trace after every 10 seconds
    %% count = 0 doesn't affect on result
    {Time, Result} = run_tracing(Config),
    DesiredResult = desired_result(Config, Time),

    ?assert(abs(DesiredResult - Result) =< 5).

count_sample_1(Config) ->
    %% all traces will be stored
    %% period = 0 doesn't affect on result
    {Time, Result} = run_tracing(Config),
    DesiredResult = desired_result(Config, Time),

    ?assertEqual(DesiredResult, Result).

count_sample_2(Config) ->
    %% expecting every 10th trace
    %% period = 0 doesn't affect on result
    {Time, Result} = run_tracing(Config),
    DesiredResult = desired_result(Config, Time),

    ?assertEqual(DesiredResult, Result).

count_and_period_sample_1(Config) ->
    %% all traces will be stored
    %% because count = 1 that means every first
    {Time, Result} = run_tracing(Config),
    DesiredResult = desired_result(Config, Time),

    ?assertEqual(DesiredResult, Result).

count_and_period_sample_2(Config) ->
    %% expecting every fifth trace
    %% or 1 trace each 5 seconds
    {Time, Result} = run_tracing(Config),
    DesiredResult = desired_result(Config, Time),

    ?assert(abs(DesiredResult - Result) =< 5).

count_and_period_sample_3(Config) ->
    %% expecting every 100th trace
    %% or 1 trace each 2 seconds
    {Time, Result} = run_tracing(Config),
    DesiredResult = desired_result(Config, Time),

    ?assert(abs(DesiredResult - Result) =< 5).

count_and_period_sample_4(Config) ->
    %% 1000/100 (0th, 99th, 199th, etc)
    ?assertMatch({_, 100}, run_tracing(Config)),

    timer:sleep(2500),

    ?assertMatch([{sampler, _, 100}], ets:lookup(sampler_period_or_count, sampler)),

    %% this enabled because of counter
    SpanContext = oc_trace:start_span(<<"span">>, undefined),
    ?assertEqual(true, oc_trace:is_enabled(SpanContext)),

    ?assertMatch([{sampler, _, 1}], ets:lookup(sampler_period_or_count, sampler)),

    timer:sleep(2500),

    %% this enabled because of period
    SpanContext1 = oc_trace:start_span(<<"span">>, undefined),
    ?assertEqual(true, oc_trace:is_enabled(SpanContext1)),

    ?assertMatch([{sampler, _, 2}], ets:lookup(sampler_period_or_count, sampler)),

    ?assertMatch({_, 100}, run_tracing(Config)).

desired_result(Config, Duration) ->
    Limit = ?config(limit, Config),
    _Delay = ?config(delay, Config),
    Period = ?config(period, Config),
    Count = ?config(count, Config),

    Res = if
              Count == 0 ->
                  Duration / Period;
              Period == 0 ->
                  Limit / Count;

              %% if period is too small, counter will not reach the trigger value
              %% and sampler will act according to period duration settings
              (Duration / Period) > (Limit / Count) ->
                  Duration / Period;
              true ->
                  Limit / Count
                  %% (Limit / Count) + ((Limit - (Limit / Count)) * (Delay / 1000) / Period)
          end,
    round(Res).

%%
%%

run_tracing(Config) ->
    Start = erlang:monotonic_time(microsecond),

    Limit = ?config(limit, Config),
    Delay = ?config(delay, Config),

    %% run traces counted by Limit, with pause specified by Delay,
    %% and filter only enabled ones

    L = lists:filter(fun(_) ->
                             SpanContext = oc_trace:start_span(<<"span">>, undefined),
                             timer:sleep(Delay),
                             oc_trace:is_enabled(SpanContext)
                     end, lists:seq(1, Limit)),

    End = erlang:monotonic_time(microsecond),

    {(End - Start) / 1000000, length(L)}.
