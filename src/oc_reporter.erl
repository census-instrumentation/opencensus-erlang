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
-module(oc_reporter).

-behaviour(gen_server).

-compile({no_auto_import, [register/2]}).

-export([start_link/0,
         store_span/1,
         register/1,
         register/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

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

-record(state, {reporters :: [{module(), term()}],
                send_interval_ms :: integer(),
                timer_ref :: reference()}).

-define(BUFFER_1, oc_report_buffer1).
-define(BUFFER_2, oc_report_buffer2).
-define(BUFFER_STATUS, oc_report_status).

start_link() ->
    maybe_init_ets(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc
%% @equiv register(Reporter, []).
%% @end
register(Reporter) -> register(Reporter, []).

%% @doc
%% Register new traces reporter `Reporter' with `Config'.
%% @end
-spec register(module(), term()) -> ok.
register(Reporter, Options) ->
    gen_server:call(?MODULE, {register, init_reporter({Reporter, Options})}).

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

init(_Args) ->
    SendInterval = application:get_env(opencensus, send_interval_ms, 500),
    Reporters = [init_reporter(Config) || Config <- application:get_env(opencensus, reporters, [])],
    Ref = erlang:send_after(SendInterval, self(), report_spans),
    {ok, #state{reporters=Reporters,
                send_interval_ms=SendInterval,
                timer_ref=Ref}}.

handle_call({register, Reporter}, _From, #state{reporters=Reporters} = State) ->
    {reply, ok, State#state{reporters=[Reporter | Reporters]}};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(report_spans, State=#state{reporters=Reporters,
                                       send_interval_ms=SendInterval,
                                       timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    Ref1 = erlang:send_after(SendInterval, self(), report_spans),
    send_spans(Reporters),
    {noreply, State#state{timer_ref=Ref1}};
handle_info({ssl_closed, _}, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #state{timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    ok.

init_reporter({Reporter, Config}) ->
    {Reporter, Reporter:init(Config)};
init_reporter(Reporter) when is_atom(Reporter) ->
    {Reporter, Reporter:init([])}.

maybe_init_ets() ->
    case ets:info(?BUFFER_STATUS, name) of
        undefined ->
            [ets:new(Tab, [named_table, public | TableProps ]) ||
                {Tab, TableProps} <- [{?BUFFER_1, [{write_concurrency, true}, {keypos, #span.span_id}]},
                                      {?BUFFER_2, [{write_concurrency, true}, {keypos, #span.span_id}]},
                                      {?BUFFER_STATUS, [{read_concurrency, true}]}]],
            ets:insert(?BUFFER_STATUS, {current_buffer, ?BUFFER_1});
        _ ->
            ok
    end.

send_spans(Reporters) ->
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
            [report(Reporter, Spans, Config)
             || {Reporter, Config} <- Reporters],
            ok
    end.

report(undefined, _, _) ->
    ok;
report(Reporter, Spans, Config) ->
    %% don't let a reporter exception crash us
    try
        Reporter:report(Spans, Config)
    catch
        ?WITH_STACKTRACE(Class, Exception, StackTrace)
            ?LOG_INFO("reporter threw exception: reporter=~p ~p:~p stacktrace=~p",
                      [Reporter, Class, Exception, StackTrace])
    end.
