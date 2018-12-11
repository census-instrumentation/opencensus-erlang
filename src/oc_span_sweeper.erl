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

-record(data, {interval :: integer() | infinity,
               strategy :: drop | finish | failed_attribute_and_finish | fun((opencensus:span()) -> ok),
               ttl :: integer()}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    SweeperConfig = application:get_env(opencensus, sweeper, #{}),

    Interval = maps:get(interval, SweeperConfig, timer:minutes(5)),
    Strategy = maps:get(strategy, SweeperConfig, drop),
    TTL = maps:get(span_ttl, SweeperConfig, timer:minutes(5)),
    {ok, ready, #data{interval=Interval,
                      strategy=Strategy,
                      ttl=erlang:convert_time_unit(TTL, millisecond, native)},
     [hibernate, {state_timeout, Interval, sweep}]}.

callback_mode() ->
    handle_event_function.

handle_event(state_timeout, sweep, _, #data{interval=Interval,
                                            strategy=drop,
                                            ttl=TTL}) ->
    TooOld = erlang:monotonic_time() - TTL,
    case ets:select_delete(?SPAN_TAB, expired_match_spec(TooOld, true)) of
        0 ->
            ok;
        NumDeleted ->
            ?LOG_INFO("sweep old spans: ttl=~p num_dropped=~p", [TTL, NumDeleted])
    end,
    {keep_state_and_data, [hibernate, {state_timeout, Interval, sweep}]};
handle_event(state_timeout, sweep, _, #data{interval=Interval,
                                            strategy=finish,
                                            ttl=TTL}) ->
    Expired = select_expired(TTL),
    [finish_span(Span) || Span <- Expired],
    {keep_state_and_data, [hibernate, {state_timeout, Interval, sweep}]};
handle_event(state_timeout, sweep, _, #data{interval=Interval,
                                            strategy=failed_attribute_and_finish,
                                            ttl=TTL}) ->
    Expired = select_expired(TTL),
    [finish_span(oc_span:put_attribute(<<"finished_by_sweeper">>, true, Span)) || Span <- Expired],
    {keep_state_and_data, [hibernate, {state_timeout, Interval, sweep}]};
handle_event(state_timeout, sweep, _, #data{interval=Interval,
                                            strategy=Fun,
                                            ttl=TTL}) when is_function(Fun) ->
    Expired = select_expired(TTL),
    [Fun(Span) || Span <- Expired],
    {keep_state_and_data, [hibernate, {state_timeout, Interval, sweep}]};
handle_event(_, _, _, _Data) ->
    keep_state_and_data.

code_change(_, State, Data, _) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

%%

%% ignore these functions because dialyzer doesn't like match spec use of '_'
-dialyzer({nowarn_function, expired_match_spec/2}).
-dialyzer({nowarn_function, finish_span/1}).
-dialyzer({nowarn_function, select_expired/1}).

expired_match_spec(Time, Return) ->
    [{#span{start_time={'$1', '_'}, _='_'},
      [{'<', '$1', Time}],
      [Return]}].

finish_span(S=#span{span_id=SpanId}) ->
    oc_span:finish_span(S),
    ets:delete(?SPAN_TAB, SpanId).

select_expired(TTL) ->
    TooOld = erlang:monotonic_time() - TTL,
    ets:select(?SPAN_TAB, expired_match_spec(TooOld, '$_')).
