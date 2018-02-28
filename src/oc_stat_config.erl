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
%% @doc stats configuration
%% @end
%%%------------------------------------------------------------------------

-module(oc_stat_config).

-export([views/0,
         export_interval/0,
         exporters/0]).

-define(DEFAULT_VIEWS, []).
-define(DEFAULT_EXPORTERS, []).
-define(DEFAULT_EXPORT_INTERVAL, 5000).

views() ->
    proplists:get_value(views, stat_conf(), ?DEFAULT_VIEWS).

export_interval() ->
    proplists:get_value(export_interval, stat_conf(), ?DEFAULT_EXPORT_INTERVAL).

exporters() ->
    proplists:get_value(exporters, stat_conf(), ?DEFAULT_EXPORTERS).

stat_conf() ->
    application:get_env(opencensus, stat, []).
