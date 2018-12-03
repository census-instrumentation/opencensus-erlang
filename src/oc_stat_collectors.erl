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
-module(oc_stat_collectors).

-behaviour(gen_server).

-export([start_link/0,
         record/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {workers :: [],
                num_workers :: integer()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record(MeasureName, Tags, Value) ->
    Workers = persistent_term:get(?MODULE),
    element(erlang:system_info(scheduler_id), Workers) ! {record, MeasureName, Tags, Value}.

init([]) ->
    erlang:process_flag(trap_exit, true),
    NumSchedulers = erlang:system_info(schedulers),
    Workers =
        lists:map(fun(_) ->
                          {ok, Pid} = oc_stat_collector:start_link(),
                          Pid
                  end, lists:seq(1, NumSchedulers)),
    persistent_term:put(?MODULE, list_to_tuple(Workers)),
    {ok, #state{workers=Workers,
                num_workers=NumSchedulers}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', FromPid, _Reason}, State=#state{workers=Workers}) ->
    %% hopefully this goes behind any other immediate EXITs in the mailbox
    %% so we only update once if more than 1 crash at the same time
    self() ! update_workers,
    {noreply, State#state{workers=lists:delete(FromPid, Workers)}};
handle_info(update_workers, State=#state{workers=Workers,
                                         num_workers=NumWorkers}) ->
    flush_update_workers(),
    case NumWorkers - length(Workers) of
        N when N > 0 ->
            Workers1 =
                lists:map(fun(_) ->
                                  {ok, Pid} = oc_stat_collector:start_link(),
                                  Pid
                          end, lists:seq(1, N)),
            persistent_term:put(?MODULE, list_to_tuple(Workers1++Workers)),
            {noreply, State#state{workers=Workers1}};
        _ ->
            {noreply, State}
    end.

flush_update_workers() ->
    receive
        update_workers ->
            flush_update_workers()
    after
        0 ->
            ok
    end.
