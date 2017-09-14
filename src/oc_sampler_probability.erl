%%%------------------------------------------------------------------------
%% Copyright 2017, OpenCensus Authors
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
%% @doc This sampler assumes the lower 64 bits of the trace id are
%% randomly distributed around the whole (long) range. The sampler creates
%% an upper bound id based on the configured probability and compares the
%% lower 64 bits of the trace id to for the sampling decision.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_sampler_probability).

-behaviour(oc_sampler).

-include_lib("syntax_tools/include/merl.hrl").

-export([init/1,
         should_sample/3]).

-define(MAX_VALUE, 9223372036854775807).

-define(DEFAULT_PROBABILITY, 0.5).

init(Opts) ->
    case proplists:get_value(probability, Opts, ?DEFAULT_PROBABILITY) of
        P when P =:= 0.0 ->
            IdUpperBound = 0;
        P when P =:= 1.0 ->
            IdUpperBound = ?MAX_VALUE;
        P when P >= 0.0
             , P =< 1.0 ->
            IdUpperBound = (P * ?MAX_VALUE)
    end,
    IdUpperBound,

    application:set_env(opencensus, sampler_id_upper_bound, IdUpperBound).

should_sample(TraceId, _, _) ->
    {ok, IdUpperBound} = application:get_env(opencensus, sampler_id_upper_bound),
    Lower64Bits = TraceId band ?MAX_VALUE,
    erlang:abs(Lower64Bits) < IdUpperBound.
