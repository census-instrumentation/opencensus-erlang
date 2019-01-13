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
%% This sampler makes sure you will have at least 1 sample during `period'
%% or `1/count' samples otherwise.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_sampler_period_or_count).

-behaviour(oc_sampler).

-export([init/1,
         should_sample/4]).

-define(DEFAULT_PERIOD, 10).
-define(DEFAULT_COUNT, 1000).
-define(ETS_TABLE, sampler_period_or_count).

%% public

init(Opts) ->
    _ = ets:new(?ETS_TABLE, [named_table, public,
                             {read_concurrency, true},
                             {write_concurrency, true}]),

    Period0 = proplists:get_value(period, Opts, ?DEFAULT_PERIOD),
    %% TODO: check Period0 is a non-negative integer
    Count = proplists:get_value(count, Opts, ?DEFAULT_COUNT),
    %% TODO: check Counter is a non-negative? integer

    CountThreshold = Count,
    Period = erlang:convert_time_unit(Period0, second, native),

    _ = ets:insert(?ETS_TABLE, {sampler, erlang:monotonic_time(), CountThreshold}),
    {Period, CountThreshold}.

should_sample(_TraceId, _, _, {Period, CountThreshold}) ->
    should_sample(Period, CountThreshold).


%% private

should_sample(0, 0) ->
    false;
should_sample(_, 1) ->
    true;
should_sample(0, CountThreshold) ->
    ets:update_counter(?ETS_TABLE, sampler, {3, 1, CountThreshold, 1}) =:= 1;
should_sample(Period, 0) ->
    Now = erlang:monotonic_time(),
    ets:select_replace(?ETS_TABLE,
                       [{{sampler, '$1', '$2'},
                         [{'>=', {'-', Now, '$1'}, Period}],
                         [{{sampler, Now, '$2'}}]}]) > 0;
should_sample(Period, CountThreshold) ->
    Now = erlang:monotonic_time(),
    Res = ets:select_replace(?ETS_TABLE,
                             [{{sampler, '$1', '$2'},
                               [{'>=', '$2', CountThreshold}],
                               [{{sampler, Now, 0}}]},
                              {{sampler, '$1', '$2'},
                               [{'>=', {'-', Now, '$1'}, Period}],
                               [{{sampler, Now, '$2'}}]}
                             ]) > 0,

    ets:update_counter(?ETS_TABLE, sampler, {3, 1}),
    Res.
