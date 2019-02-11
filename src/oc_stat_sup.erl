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
%%%------------------------------------------------------------------------

-module(oc_stat_sup).

-export([start_link/1, init/1]).

start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

init(Opts) ->
    Interval = proplists:get_value(interval, Opts, 5000),
    Handlers = proplists:get_value(handlers, Opts, []),

    Reporter = #{id => reporter,
                 start => {oc_stat_reporter, start_link, [Handlers]}},
    ViewServer = #{id => view_server,
                   start => {oc_stat, start_link, []}},
    Timer = #{id => timer,
              start => {oc_internal_timer, start_link, [Interval,
                                                        oc_stat_reporter]}},

    {ok, {#{strategy => one_for_one}, [Reporter, Timer, ViewServer]}}.
