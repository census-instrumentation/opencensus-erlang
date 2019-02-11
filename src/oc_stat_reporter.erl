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

%% @doc
%% Exporter exports the collected records as view data.
%% @end
-module(oc_stat_reporter).

-behaviour(oc_internal_timer).

-export([start_link/1,
         ping/0]).

-include("opencensus.hrl").
-include("oc_logger.hrl").

start_link(Handlers) ->
    case gen_event:start_link({local, ?MODULE}, []) of
        {ok, Pid} ->
            [gen_event:add_handler(Pid, Handler, Opts)
             || {Handler, Opts} <- Handlers],

            {ok, Pid};
        Other -> Other
    end.

%% @private
ping() ->
    Measurements = oc_stat:export(),
    gen_event:sync_notify(?MODULE, {stats, Measurements}),

    ok.
