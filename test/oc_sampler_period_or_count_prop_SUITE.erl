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
-module(oc_sampler_period_or_count_prop_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [compare_desired_sampling_result_with_factual, validate_sampling_by_checking_counters].

init_per_suite(Config) ->
    [{property_test_tool, proper} | Config].

end_per_suite(_Config) ->
    ok.

compare_desired_sampling_result_with_factual(Config) ->
    ct_property_test:quickcheck(
      prop_period_or_count:prop_compare_desired_sampling_result_with_factual(),
      Config).

validate_sampling_by_checking_counters(Config) ->
    ct_property_test:quickcheck(
      prop_period_or_count:prop_validate_sampling_by_checking_counters(),
      Config).
