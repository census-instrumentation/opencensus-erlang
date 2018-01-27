-module(oc_server).

-export([start_link/0,

         add_span/1,
         finish_span/1,
         put_attribute/3,
         put_attributes/2,
         add_time_event/2,
         add_time_event/3,
         set_status/3,
         add_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include("opencensus.hrl").

-record(state, {}).

start_link() ->
    maybe_init_ets(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_span(Span) ->
    gen_server:cast(?MODULE, {add_span, Span}).

finish_span(SpanCtx) ->
    gen_server:cast(?MODULE, {finish_span, SpanCtx}).

put_attribute(Key, Value, SpanCtx) ->
    gen_server:cast(?MODULE, {put_attribute, Key, Value, SpanCtx}).

put_attributes(NewAttributes, SpanCtx) ->
    gen_server:cast(?MODULE, {put_attributes, NewAttributes, SpanCtx}).

add_time_event(TimeEvent, Span) ->
    add_time_event(wts:timestamp(), TimeEvent, Span).

add_time_event(Timestamp, TimeEvent, SpanCtx) ->
    gen_server:cast(?MODULE, {add_time_event, Timestamp, TimeEvent, SpanCtx}).

set_status(Code, Message, SpanCtx) ->
    gen_server:cast(?MODULE, {set_status, Code, Message, SpanCtx}).

add_link(Link, SpanCtx) ->
    gen_server:cast(?MODULE, {add_link, Link, SpanCtx}).

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast({add_span, Span}, State) ->
    ets:insert(?SPAN_TAB, Span),
    {noreply, State};
handle_cast({finish_span, SpanId}, State) ->
    ets:update_element(?SPAN_TAB, SpanId, [{#span.end_time, wts:timestamp()}]),
    {noreply, State}.

maybe_init_ets() ->
    case ets:info(?SPAN_TAB, name) of
        undefined ->
            ets:new(?SPAN_TAB, [named_table, public, {write_concurrency, true},
                                {read_concurrency, true}, {keypos, #span.span_id}]);
        _ ->
            ok
    end.
