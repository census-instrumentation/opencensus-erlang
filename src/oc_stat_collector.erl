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
%% @doc Worker process for sending recorded stats.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_stat_collector).

-export([start_link/0,
         record/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record(MeasureName, Tags, Value) ->
    oc_stat_collector ! {record, MeasureName, Tags, Value}.

init([]) ->
    {ok, #{}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({record, MeasureName, Tags, Value}, State) ->
    Module = oc_stat_measure:measure_module(MeasureName),
    Module:record(MeasureName, Tags, Value),
    {noreply, State}.
