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
%% Measure represents a type of metric to be tracked and recorded.
%% For example, latency, request Mb/s, and response Mb/s are measures
%% to collect from a server.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_stat_measure).

-export([new/3]).

-export_types([name/0,
               description/0,
               unit/0]).

-type name() :: atom() | binary() | string().
-type description() :: binary() | string().
-type unit() :: atom().


%% @doc
%% Creates and registers a measure. If a measure with the same name
%% already exists, old measure returned.
%% @end
-spec new(name(), description(), unit()) -> oc_stat_view:measure().
new(Name, Description, Unit) ->
    oc_stat_view:register_measure(Name, Description, Unit).
