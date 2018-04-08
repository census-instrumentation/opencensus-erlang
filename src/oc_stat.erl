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

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {}).

-include("opencensus.hrl").

-define(RECORD(Tags, MeasureName, Value),
        begin
            Module = oc_stat_view:measure_module(MeasureName),
            Module:record(Tags, Value),
            ok
        end).

%% @doc Records one or multiple measurements with the same tags at once.
%% If there are any tags in the context, measurements will be tagged with them.
-spec record(ctx:t() | oc_tags:tags(), oc_stat_measure:name(), number()) -> ok.
record(Tags, MeasureName, Value) when is_map(Tags) ->
    ?RECORD(Tags, MeasureName, Value);
record(Ctx, MeasureName, Value)->
    Tags = oc_tags:from_ctx(Ctx),
    ?RECORD(Tags, MeasureName, Value).

-spec record(ctx:t() | oc_tags:tags(), [{oc_stat_measure:name(), number()}]) -> ok.
record(Tags, Measures) when is_map(Tags) ->
    [?RECORD(Tags, MeasureName, Value) || {MeasureName, Value} <- Measures],
    ok;
record(Ctx, Measures) ->
    Tags = oc_tags:from_ctx(Ctx),
    record(Tags, Measures).

%% @doc Exports view_data of all subscribed views
-spec export() -> oc_stat_view:view_data().
export() ->
    [oc_stat_view:export(View) || View <- oc_stat_view:all_subscribed()].

%% gen_server implementation

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init(_Args) ->
    process_flag(trap_exit, true),
    ok = oc_stat_view:'__init_backend__'(),
    ok = oc_stat_measure:'__init_backend__'(),
    ok = oc_stat_view:preload(),
    {ok, #state{}}.

%% @private
handle_call({measure_register, Measure}, _From, State) ->
    {reply, oc_stat_measure:register_(Measure), State};
handle_call({view_register, View}, _From, State) ->
    {reply, oc_stat_view:register_(View), State};
handle_call({view_deregister, Name}, _From, State) ->
    oc_stat_view:deregister_(Name),
    {reply, ok, State};
handle_call({view_subscribe, Name}, _From,  State) ->
    {reply, oc_stat_view:subscribe_(Name), State};
handle_call({view_unsubscribe, Name}, _From, State) ->
    {reply, oc_stat_view:unsubscribe_(Name), State};
handle_call(_, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_, State) ->
    {noreply, State}.

%% @private
handle_info(_, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_, _) ->
    oc_stat_measure:terminate_(),
    ok.
