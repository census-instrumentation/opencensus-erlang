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
%% @doc opencensus application
%% @end
%%%------------------------------------------------------------------------

-module(opencensus_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("opencensus.hrl").

start(_StartType, _StartArgs) ->
    maybe_init_ets(),
    opencensus_sup:start_link().

stop(_State) ->
    ok.

maybe_init_ets() ->
    case ets:info(?SPAN_TAB, name) of
        undefined ->
            ets:new(?SPAN_TAB, [named_table, public, {write_concurrency, true}, {keypos, #span.span_id}]);
        _ ->
            ok
    end,

     ets:new(oc_producer_registry, [bag, named_table, public]),

    oc_producer_registry:add_producer(oc_self_producer).
