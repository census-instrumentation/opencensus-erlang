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
%% @doc This module has the behaviour that each reporter must implement
%% and creates the buffer of trace spans to be reported.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_trace_reporter).

-behaviour(oc_internal_timer).

-export([start_link/1,
         store_span/1]).

-export([ping/0]).

-include("opencensus.hrl").
-include("oc_logger.hrl").

%% behaviour for reporters to implement
-type opts() :: term().

%% Do any initialization of the reporter here and return configuration
%% that will be passed along with a list of spans to the `report' function.
-callback init(term()) -> opts().

%% This function is called when the configured interval expires with any
%% spans that have been collected so far and the configuration returned in `init'.
%% Do whatever needs to be done to report each span here, the caller will block
%% until it returns.
-callback report(nonempty_list(opencensus:span()), opts()) -> ok.

-define(BUFFER_1, oc_report_buffer1).
-define(BUFFER_2, oc_report_buffer2).
-define(BUFFER_STATUS, oc_report_status).

start_link(Handlers) ->
    maybe_init_ets(),
    case gen_event:start_link({local, ?MODULE}, []) of
        {ok, Pid} ->
            [gen_event:add_handler(Pid, Handler, Opts)
             || {Handler, Opts} <- Handlers],

            {ok, Pid};
        Other -> Other
    end.

-spec store_span(opencensus:span()) -> true | {error, invalid_span} | {error, no_report_buffer}.
store_span(Span=#span{}) ->
    try
        [{_, Buffer}] = ets:lookup(?BUFFER_STATUS, current_buffer),
        ets:insert(Buffer, Span)
    catch
        error:badarg ->
            {error, no_report_buffer}
    end;
store_span(_) ->
    {error, invalid_span}.

ping() ->
    [{_, Buffer}] = ets:lookup(?BUFFER_STATUS, current_buffer),
    NewBuffer = case Buffer of
                    ?BUFFER_1 ->
                        ?BUFFER_2;
                    ?BUFFER_2 ->
                        ?BUFFER_1
                end,
    ets:insert(?BUFFER_STATUS, {current_buffer, NewBuffer}),
    case ets:tab2list(Buffer) of
        [] ->
            ok;
        Spans ->
            ets:delete_all_objects(Buffer),
            gen_event:sync_notify(?MODULE, {spans, Spans}),
            ok
    end.

maybe_init_ets() ->
    case ets:info(?BUFFER_STATUS, name) of
        undefined ->
            [ets:new(Tab, [named_table, public | TableProps]) ||
                {Tab, TableProps} <- [{?BUFFER_1, [{write_concurrency, true}, {keypos, #span.span_id}]},
                                      {?BUFFER_2, [{write_concurrency, true}, {keypos, #span.span_id}]},
                                      {?BUFFER_STATUS, [{read_concurrency, true}]}]],
            ets:insert(?BUFFER_STATUS, {current_buffer, ?BUFFER_1}),

            ok;
        _ ->
            ok
    end.
