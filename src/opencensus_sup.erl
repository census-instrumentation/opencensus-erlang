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
%% @doc opencensus top level supervisor.
%% @end
%%%------------------------------------------------------------------------

-module(opencensus_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    ok = oc_sampler:init(application:get_env(opencensus, sampler, {oc_sampler_always, []})),

    StatOpts = application:get_env(opencensus, metric, []),
    StatSup = #{id => stat,
                start => {oc_stat_sup, start_link, [StatOpts]},
                type => supervisor},

    TraceOpts = application:get_env(opencensus, trace, []),
    TraceSup = #{id => traces,
                 start => {oc_trace_sup, start_link, [TraceOpts]},
                 type => supervisor},

    {ok, {#{strategy => one_for_one}, [TraceSup, StatSup]}}.
