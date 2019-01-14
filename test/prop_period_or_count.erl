%%%------------------------------------------------------------------------
%% Copyright 2018, OpenCensus Authors
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
-module(prop_period_or_count).

-include_lib("proper/include/proper.hrl").

-define(CONFIG(Key, Config), proplists:get_value(Key, Config)).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_compare_desired_sampling_result_with_factual() ->
    ?FORALL({Period, Count, Limit, Delay},
            {pos_integer(), pos_integer(), pos_integer(), pos_integer()},
            begin
                Config = [{limit, Limit},
                          {delay, Delay},
                          {period, Period},
                          {count, Count}],
                start_apps(Config),

                {Time, Result} = run_tracing(Config),
                DesiredResult = desired_result(Config, Time),

                stop_apps(),

                abs(DesiredResult - Result) < 2
            end).

prop_validate_sampling_by_checking_counters() ->
    ?FORALL({Period, Count, Limit, Delay},
            {integer(1, 5), integer(1, 10), integer(10, 100), integer(5, 10)},
            begin
                Config = [{limit, Limit},
                          {delay, Delay},
                          {period, Period},
                          {count, Count}],
                start_apps(Config),

                %% 1000/100 (0th, 99th, 199th, etc)
                {Time, Result} = run_tracing(Config),
                DesiredResult = desired_result(Config, Time),
                true = abs(DesiredResult - Result) < 2,

                timer:sleep(Period * 1000 + 500),

                X = if
                        Limit rem Count == 0 -> Count;
                        true -> Limit rem Count
                    end,

                [{sampler, _, X}] = ets:lookup(sampler_period_or_count, sampler),

                %% increment counter to trigger value
                _ = lists:foreach(fun(_) ->
                                          oc_trace:start_span(<<"span">>, undefined)
                                  end, lists:seq(X, Count - 1)),

                %% this enabled because of counter
                Span1 = oc_trace:start_span(<<"span">>, undefined),
                true = oc_trace:is_enabled(Span1),

                [{sampler, _, 1}] = ets:lookup(sampler_period_or_count, sampler),

                timer:sleep(Period * 1000 + 500),

                %% this enabled because of period
                Span2 = oc_trace:start_span(<<"span">>, undefined),
                true = oc_trace:is_enabled(Span2),

                if
                    Count > 1 ->
                        [{sampler, _, 2}] = ets:lookup(sampler_period_or_count, sampler);
                    true ->
                        [{sampler, _, 1}] = ets:lookup(sampler_period_or_count, sampler)
                end,

                {Time1, Result1} = run_tracing(Config),
                DesiredResult1 = desired_result(Config, Time1),
                stop_apps(),

                true = abs(DesiredResult1 - Result1) < 2
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
%% boolean(_) -> true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
%% mytype() -> term().

desired_result(Config, Duration) ->
    Limit = ?CONFIG(limit, Config),
    Period = ?CONFIG(period, Config),
    Count = ?CONFIG(count, Config),

    round(calculate_desired_result(Limit, Duration, Period, Count)).

calculate_desired_result(_Limit, Duration, Period, Count)
  when Count == 0 ->
    Duration / Period;

calculate_desired_result(Limit, _Duration, Period, Count)
  when Period == 0 ->
    Limit / Count;

calculate_desired_result(Limit, Duration, Period, Count)
%% if period is too small, counter will not reach the trigger value
%% and sampler will act according to period duration settings
  when (Duration / Period) > (Limit / Count) ->
    Duration / Period;

calculate_desired_result(Limit, _Duration, _Period, Count) ->
    Limit / Count.


%%
%%

run_tracing(Config) ->
    Start = erlang:monotonic_time(microsecond),

    Limit = ?CONFIG(limit, Config),
    Delay = ?CONFIG(delay, Config),

    %% run traces counted by Limit, with pause specified by Delay,
    %% and filter only enabled ones
    L = lists:filter(fun(_) ->
                             SpanContext = oc_trace:start_span(<<"span">>, undefined),
                             timer:sleep(Delay),
                             oc_trace:is_enabled(SpanContext)
                     end, lists:seq(1, Limit)),

    End = erlang:monotonic_time(microsecond),

    {(End - Start) / 1000000, length(L)}.


start_apps(Config) ->
    application:set_env(opencensus, sampler,
                        {oc_sampler_period_or_count,
                         [
                          {period, ?CONFIG(period, Config)},
                          {count, ?CONFIG(count, Config)}
                         ]}),
    {ok, _} = application:ensure_all_started(opencensus).

stop_apps() ->
    _ = application:stop(opencensus).
