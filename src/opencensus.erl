%%%-------------------------------------------------------------------
%% @doc opencensus
%% @end
%%%-------------------------------------------------------------------
-module(opencensus).

-export([start_span/3,
         finish_span/1,
         child_span/2,

         timestamp/0,
         generate_trace_id/0,
         generate_span_id/0]).

-include("opencensus.hrl").

-export_type([trace_id/0,
              span_id/0,
              trace_context/0,
              span/0]).

-type trace_id()      :: non_neg_integer().
-type span_id()       :: non_neg_integer().
-type trace_context() :: #trace_context{}.
-type span()          :: #span{}.

-type annotations()   :: maps:map(unicode:chardata(), attribute_value()).
-type attributes()    :: maps:map(unicode:chardata(), attribute_value()).
-type attribute_value() :: unicode:chardata() | boolean() | integer().

%% A link requires a type which describes the relationship with the linked span
%% and the identifiers of the linked span.
%% Can be used, for example, in batching operations, where a single batch handler
%% processes multiple requests from different traces.
-type link() :: #link{}.
-type link_type() :: child_linked_span | parent_linked_span | unspecified.
-type links() :: [link()].

-type time_event() :: #time_event{}.
-type time_events() :: #time_events{}.
-type annotation() :: {unicode:chardata(), opencensus:attributes()}.
-type network_event() :: #network_event{}.
-type network_event_type() :: recv | sent | unspecified.

-type maybe(T)        :: T | undefined.

%% timestamp in microseconds
-type time_us() :: non_neg_integer().

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given Trace ID and Parent ID.
%% @end
%%--------------------------------------------------------------------
-spec start_span(unicode:chardata(), maybe(integer()), maybe(integer())) -> maybe(span()).
start_span(_Name, undefined, undefined) ->
    undefined;
start_span(Name, TraceId, ParentId) when is_integer(TraceId)
                                       , (is_integer(ParentId)
                                         orelse ParentId =:= undefined) ->
    #span{start_time = timestamp(),
          trace_id = TraceId,
          span_id = generate_span_id(),
          parent_span_id = ParentId,
          name = Name}.

%%--------------------------------------------------------------------
%% @doc
%% Finish a span, setting the end_time.
%% @end
%%--------------------------------------------------------------------
-spec finish_span(maybe(span())) -> maybe(span()).
finish_span(undefined) ->
    undefined;
finish_span(Span) ->
    Span#span{end_time = timestamp()}.

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span as a child of a existing span, using the parents
%% Trace ID or a `{trace_id, parent_id}` tuple and setting the childs
%% parent to the parents Span ID
%% @end
%%--------------------------------------------------------------------
-spec child_span(unicode:chardata(), maybe(span())) -> maybe(span()).
child_span(_Name, undefined) ->
    undefined;
child_span(Name, #span{trace_id = TraceId, span_id = ParentId}) ->
    start_span(Name, TraceId, ParentId).

%%--------------------------------------------------------------------
%% @doc
%% Returns the current system time in microseconds.
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> time_us().
timestamp() ->
    erlang:system_time(microsecond).

%%--------------------------------------------------------------------
%% @doc
%% Generates a 128 bit random integer to use as a trace id.
%% @end
%%--------------------------------------------------------------------
-spec generate_trace_id() -> non_neg_integer().
generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    Id.

%%--------------------------------------------------------------------
%% @doc
%% Generates a 64 bit random integer to use as a span id.
%% @end
%%--------------------------------------------------------------------
-spec generate_span_id() -> non_neg_integer().
generate_span_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    Id.
