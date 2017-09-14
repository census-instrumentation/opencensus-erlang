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
%% @doc opencensus main module
%% @end
%%%-------------------------------------------------------------------------
-module(opencensus).

-export([start_trace/0,
         start_trace/1,
         start_trace/3,

         start_span/2,
         start_span/3,
         finish_span/1,

         context/1,

         put_attribute/3,
         put_attributes/2,

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

-type annotations()   :: maps:map(unicode:unicode_binary(), attribute_value()).
-type attributes()    :: #{unicode:unicode_binary() => attribute_value()}.
-type attribute_value() :: unicode:unicode_binary() | boolean() | integer().

%% A link requires a type which describes the relationship with the linked span
%% and the identifiers of the linked span.
%% Can be used, for example, in batching operations, where a single batch handler
%% processes multiple requests from different traces.
-type link() :: #link{}.
-type link_type() :: child_linked_span | parent_linked_span | unspecified.
-type links() :: [link()].

-type time_event() :: #time_event{}.
-type time_events() :: #time_events{}.
-type annotation() :: {unicode:unicode_binary(), opencensus:attributes()}.
-type network_event() :: #network_event{}.
-type network_event_type() :: recv | sent | unspecified.

-type maybe(T) :: T | undefined.

%% timestamp in microseconds
-type time_us() :: non_neg_integer().

%%--------------------------------------------------------------------
%% @doc
%% Creates a new trace context if `enabled` is true in the trace context
%% argument or the sampling function returns true. If the sampling returns
%% false then `undefined` is returned.
%% @end
%%--------------------------------------------------------------------
-spec start_trace() -> trace_context().
start_trace()  ->
    start_trace(generate_trace_id(), undefined, undefined).

start_trace(undefined)  ->
    start_trace(generate_trace_id(), undefined, undefined);
start_trace(#trace_context{trace_id=TraceId,
                           span_id=SpanId,
                           enabled=Enabled})  ->
    start_trace(TraceId, SpanId, Enabled);
start_trace(TraceId)  ->
    start_trace(TraceId, undefined, undefined).

start_trace(TraceId, SpanId, Enabled)  ->
    #trace_context{trace_id = TraceId,
                   span_id = SpanId,
                   enabled = Enabled}.

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given Trace ID and Parent ID.
%% @end
%%--------------------------------------------------------------------
-spec start_span(unicode:unicode_binary(), maybe(trace_context() | span())) -> maybe(span()).
start_span(_Name, undefined) ->
    undefined;
start_span(Name, #trace_context{trace_id=TraceId,
                                span_id=ParentId}) ->
    start_span(Name, TraceId, ParentId);
start_span(Name, #span{trace_id=TraceId,
                       span_id=ParentId}) ->
    start_span(Name, TraceId, ParentId).

-spec start_span(unicode:unicode_binary(), maybe(integer()), maybe(integer())) -> maybe(span()).
start_span(_Name, undefined, undefined) ->
    undefined;
start_span(Name, TraceId, ParentId) when is_integer(TraceId)
                                       , (is_integer(ParentId)
                                         orelse ParentId =:= undefined) ->
    #span{start_time = wts:timestamp(),
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
finish_span(Span=#span{start_time=StartTime}) ->
    EndTime = wts:timestamp(),
    Span1 = Span#span{end_time = EndTime,
                      duration = wts:duration(StartTime, EndTime)},
    _ = oc_report_buffer:store_span(Span1),
    Span1.

%%--------------------------------------------------------------------
%% @doc
%% Return the current trace context for a span, to be used for
%% propagation across process boundries.
%% @end
%%--------------------------------------------------------------------
context(undefined) ->
    undefined;
context(#span{trace_id=TraceId,
              span_id=SpanId}) ->
    #trace_context{trace_id=TraceId,
                   span_id=SpanId}.

%%--------------------------------------------------------------------
%% @doc
%% Put an attribute (a key/value pair) in the attribute map of a span.
%% If the attribute already exists it is overwritten with the new value.
%% @end
%%--------------------------------------------------------------------
-spec put_attribute(unicode:unicode_binary(), attribute_value(), maybe(span()))
                   -> maybe(span()) | {error, invalid_attribute}.
put_attribute(_Key, _Value, undefined) ->
    undefined;
put_attribute(Key, Value, Span=#span{attributes=Attributes})
  when is_binary(Key)
     , (is_binary(Value) orelse is_integer(Value) orelse is_boolean(Value)) ->
    Span#span{attributes=maps:put(Key, Value, Attributes)};
put_attribute(_Key, _Value, _Span) ->
    {error, invalid_attribute}.

%%--------------------------------------------------------------------
%% @doc
%% Merge a map of attributes with the current attributes of a span.
%% The new values overwrite the old if any keys are the same.
%% @end
%%--------------------------------------------------------------------
-spec put_attributes(#{unicode:unicode_binary() => attribute_value()}, maybe(span())) -> maybe(span()).
put_attributes(_NewAttributes, undefined) ->
    undefined;
put_attributes(NewAttributes, Span=#span{attributes=Attributes}) ->
    Span#span{attributes=maps:merge(Attributes, NewAttributes)}.

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
