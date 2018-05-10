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
%% @doc Server with no logic, simply owns the span ets table.
%% @end
%%%-------------------------------------------------------------------------
-module(oc_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include("opencensus.hrl").

-record(state, {}).

start_link() ->
    maybe_init_ets(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

maybe_init_ets() ->
    case ets:info(?SPAN_TAB, name) of
        undefined ->
            ets:new(?SPAN_TAB, [named_table, public, {write_concurrency, true},
                                {read_concurrency, true}, {keypos, #span.span_id}]);
        _ ->
            ok
    end.
