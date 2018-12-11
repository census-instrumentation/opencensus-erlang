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
%%%------------------------------------------------------------------------
-module(oc_span_sweeper).

-behaviour(gen_statem).

-export([start_link/0]).

-export([init/1,
         callback_mode/0,
         handle_event/4,
         code_change/4,
         terminate/3]).

-include("opencensus.hrl").
-include("oc_logger.hrl").

-define(EXPIRED_MS(Time, Return), [{#span{start_time={'$1', '_'}, _='_'},
                                    [{'<', '$1', Time}],
                                    [Return]}]).

-record(data, {sweep_timeout :: integer() | infinity,
               strategy :: drop | finish | failed_attribute_and_finish | fun((opencensus:span()) -> ok),
               ttl :: integer()}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    SweepTimeout = application:get_env(opencensus, sweep_timeout, timer:minutes(5)),
    Strategy = application:get_env(opencensus, sweep_strategy, drop),
    TTL = application:get_env(opencensus, span_ttl, timer:minutes(5)),
    {ok, ready, #data{sweep_timeout=SweepTimeout,
                      strategy=Strategy,
                      ttl=erlang:convert_time_unit(TTL, millisecond, native)},
     [hibernate, {state_timeout, SweepTimeout, sweep}]}.

callback_mode() ->
    handle_event_function.

handle_event(state_timeout, sweep, _, #data{sweep_timeout=SweepTimeout,
                                            strategy=drop,
                                            ttl=TTL}) ->
    TooOld = erlang:monotonic_time() - TTL,
    case ets:select_delete(?SPAN_TAB, ?EXPIRED_MS(TooOld, true)) of
        0 ->
            ok;
        NumDeleted ->
            ?LOG_INFO("sweep old spans: ttl=~p num_dropped=~p", [TTL, NumDeleted])
    end,
    {keep_state_and_data, [hibernate, {state_timeout, SweepTimeout, sweep}]};
handle_event(state_timeout, sweep, _, #data{sweep_timeout=SweepTimeout,
                                            strategy=finish,
                                            ttl=TTL}) ->
    Expired = select_expired(TTL),
    [finish_span(Span) || Span <- Expired],
    {keep_state_and_data, [hibernate, {state_timeout, SweepTimeout, sweep}]};
handle_event(state_timeout, sweep, _, #data{sweep_timeout=SweepTimeout,
                                            strategy=failed_attribute_and_finish,
                                            ttl=TTL}) ->
    Expired = select_expired(TTL),
    [finish_span(oc_span:put_attribute(<<"finished_by_sweeper">>, true, Span)) || Span <- Expired],
    {keep_state_and_data, [hibernate, {state_timeout, SweepTimeout, sweep}]};
handle_event(state_timeout, sweep, _, #data{sweep_timeout=SweepTimeout,
                                            strategy=Fun,
                                            ttl=TTL}) when is_function(Fun) ->
    Expired = select_expired(TTL),
    [Fun(Span) || Span <- Expired],
    {keep_state_and_data, [hibernate, {state_timeout, SweepTimeout, sweep}]};
handle_event(_, _, _, _Data) ->
    keep_state_and_data.

code_change(_, State, Data, _) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

%%

finish_span(S=#span{span_id=SpanId}) ->
    oc_span:finish_span(S),
    ets:delete(?SPAN_TAB, SpanId).

select_expired(TTL) ->
    TooOld = erlang:monotonic_time() - TTL,
    ets:select(?SPAN_TAB, ?EXPIRED_MS(TooOld, '$_')).
