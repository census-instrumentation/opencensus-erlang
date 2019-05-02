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
%% @doc Functions for functional manipulation of span data.
%% @end
%%%-------------------------------------------------------------------------
-module(oc_span).

-export([finish_span/2,

         put_attribute/3,
         put_attributes/2,

         add_time_event/2,
         add_time_event/3,

         add_link/2,
         link/4,

         annotation/2,

         message_event/4,

         set_status/3]).

-include("opencensus.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Finish a span, setting the end_time and sending to the reporter.
%% @end
%%--------------------------------------------------------------------
-spec finish_span(opencensus:span_ctx(), maybe(opencensus:span())) -> ok | {error, term()}.
finish_span(#span_ctx{tracestate=Tracestate}, Span=#span{}) ->
    EndTime = wts:timestamp(),
    %% update tracestate to what the context has when finished
    Span1 = Span#span{end_time=EndTime,
                      tracestate=Tracestate},
    oc_trace_reporter:store_span(Span1);
finish_span(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Put an attribute (a key/value pair) in the attribute map of a span.
%% If the attribute already exists it is overwritten with the new value.
%% @end
%%--------------------------------------------------------------------
-spec put_attribute(Key, Value, Span) -> Span when Key   :: unicode:unicode_binary(),
                                                   Value :: opencensus:attribute_value(),
                                                   Span  :: maybe(opencensus:span()).
put_attribute(Key, Value, Span=#span{attributes=Attributes}) ->
    Span#span{attributes=maps:put(Key, Value, Attributes)};
put_attribute(_, _, undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Merge a map of attributes with the current attributes of a span.
%% The new values overwrite the old if any keys are the same.
%% @end
%%--------------------------------------------------------------------
-spec put_attributes(Attributes, Span) -> Span when Attributes :: #{unicode:unicode_binary() =>
                                                                        opencensus:attribute_value()},
                                                    Span       :: maybe(opencensus:span()).
put_attributes(NewAttributes, Span=#span{attributes=Attributes}) ->
    Span#span{attributes=maps:merge(Attributes, NewAttributes)};
put_attributes(_, undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Add an Annotation or MessageEvent to the list of TimeEvents in a span.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_time_event(TimeEvent, Span) -> Span when TimeEvent :: opencensus:annotation() | opencensus:message_event(),
                                                   Span      :: maybe(opencensus:span()).
add_time_event(TimeEvent, Span) ->
    add_time_event(wts:timestamp(), TimeEvent, Span).

add_time_event(Timestamp, TimeEvent, Span=#span{time_events=TimeEvents}) ->
    Span#span{time_events=[{Timestamp, TimeEvent} | TimeEvents]};
add_time_event(_Timestamp, _TimeEvent, undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Create an Annotation.
%% @end
%%--------------------------------------------------------------------
-spec annotation(Description, Attributes) -> Annotation when Description :: unicode:unicode_binary(),
                                                             Attributes  :: opencensus:attributes(),
                                                             Annotation  :: opencensus:annotation().
annotation(Description, Attributes) ->
    #annotation{description=Description,
                attributes=Attributes}.

%%--------------------------------------------------------------------
%% @doc
%% Create a MessageEvent.
%% @end
%%--------------------------------------------------------------------
-spec message_event(MessageEventType, Id, UncompressedSize, CompressedSize) ->
                           MessageEvent when MessageEventType :: opencensus:message_event_type(),
                                             Id               :: integer(),
                                             UncompressedSize :: integer(),
                                             CompressedSize   :: integer(),
                                             MessageEvent     :: opencensus:message_event().
message_event(MessageEventType, Id, UncompressedSize, CompressedSize) ->
    #message_event{type=MessageEventType,
                   id=Id,
                   uncompressed_size=UncompressedSize,
                   compressed_size=CompressedSize}.

%%--------------------------------------------------------------------
%% @doc
%% Set Status.
%% @end
%%--------------------------------------------------------------------
-spec set_status(Code, Message, Span) -> Span when Code    :: integer(),
                                                   Message :: unicode:unicode_binary(),
                                                   Span    :: maybe(opencensus:span()).
set_status(Code, Message, Span=#span{}) ->
    Span#span{status=#status{code=Code,
                             message=Message}};
set_status(_, _, undefined) ->
    undefined.


%%--------------------------------------------------------------------
%% @doc
%% Add a Link to the list of Links in the span.
%% @end
%%--------------------------------------------------------------------
-spec add_link(Link, Span) -> Span when Link :: opencensus:link(),
                                        Span :: maybe(opencensus:span()).
add_link(Link, Span=#span{links=Links}) ->
    Span#span{links=[Link | Links]};
add_link(_, undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Create a Link which can be added to a Span.
%% @end
%%--------------------------------------------------------------------
-spec link(LinkType, TraceId, SpanId, Attributes) -> Link when LinkType   :: opencensus:link_type(),
                                                               TraceId    :: opencensus:trace_id(),
                                                               SpanId     :: opencensus:span_id(),
                                                               Attributes :: opencensus:attributes(),
                                                               Link       :: opencensus:link().
link(LinkType, TraceId, SpanId, Attributes) ->
    #link{type=LinkType,
          trace_id=TraceId,
          span_id=SpanId,
          attributes=Attributes}.
