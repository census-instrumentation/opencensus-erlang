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
%% @doc OpenCensus Stats package
%% @end
%%%-----------------------------------------------------------------------
-module(oc_stat).

-export([record/2,
         record/3,
         export/0]).

-include("opencensus.hrl").

%% @doc Records one or multiple measurements with the same tags at once.
%% If there are any tags in the context, measurements will be tagged with them.
-spec record(ctx:t() | oc_tags:tags(), measure_name(), number()) -> ok.
record(Tags, MeasureName, Value) when is_map(Tags) ->
    Module = oc_stat_view:measure_module(MeasureName),
    Module:record(Tags, Value),
    ok;
record(Ctx, MeasureName, Value)->
    Tags = oc_tags:from_ctx(Ctx),
    record(Tags, MeasureName, Value).

-spec record(ctx:t() | oc_tags:tags(), [{measure_name(), number()}]) -> ok.
record(Tags, Measures) when is_map(Tags) ->
    [record(Tags, MeasureName, Value)
     || {MeasureName, Value} <- Measures],
    ok;
record(Ctx, Measures) ->
    Tags = oc_tags:from_ctx(Ctx),
    record(Tags, Measures).


%% @doc Exports view_data of all subscribed views
-spec export() -> oc_stat_view:view_data().
export() ->
    [oc_stat_view:export(View) || View <- oc_stat_view:all_subscribed()].
