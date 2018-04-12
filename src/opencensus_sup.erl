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
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ok = oc_sampler:init(application:get_env(opencensus, sampler, {oc_sampler_always, []})),

    Reporter = #{id => oc_reporter,
                 start => {oc_reporter, start_link, []},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [oc_reporter]},

    Exporter = #{id => oc_stat_exporter,
                 start => {oc_stat_exporter, start_link, []},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [oc_stat_exporter]},

    ViewServer = #{id => oc_stat,
                   start => {oc_stat, start_link, []},
                   restart => permanent,
                   shutdown => 1000,
                   type => worker,
                   modules => [oc_stat]},

    TraceServer = #{id => oc_server,
                    start => {oc_server, start_link, []},
                    restart => permanent,
                    shutdown => 1000,
                    type => worker,
                    modules => [oc_server]},

    {ok, {#{strategy => one_for_one,
            intensity => 1,
            period => 5}, [Reporter, Exporter, ViewServer, TraceServer]}}.
