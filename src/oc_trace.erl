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
%% @doc The main module for working with the span in a current context. This
%%      module provides functions for getting the current span context from
%%      a ctx variable, creating new spans, and manipulating the span data
%%      for the span in the current context.
%% @end
%%%-------------------------------------------------------------------------
-module(oc_trace).

-export([from_ctx/1,

         current_span_ctx/1,
         parent_span_ctx/1,

         with_span_ctx/2,
         with_child_span/2,
         with_child_span/3,

         start_span/2,
         start_span/3,

         finish_span/1,

         is_enabled/1,

         put_attribute/3,
         put_attributes/2,

         add_time_event/2,
         add_time_event/3,

         add_link/2,
         link/4,

         annotation/2,

         message_event/4,

         set_status/3]).

-dialyzer({nowarn_function, update_trace_options/2}).

-include("opencensus.hrl").
-include("oc_logger.hrl").

%% sampling bit is the first bit in 8-bit trace options
-define(IS_ENABLED(X), (X band 1) =/= 0).

%%--------------------------------------------------------------------
%% @doc
%% Return the span context, if it exists, from Ctx.
%% @end
%%--------------------------------------------------------------------
-spec from_ctx(ctx:t()) -> opencensus:span_ctx().
from_ctx(Ctx) ->
    ctx:get(Ctx, ?SPAN_CTX, #span_ctx{}).

%%--------------------------------------------------------------------
%% @doc
%% Return the current span context in a `Ctx' or `undefined'.
%% @end
%%--------------------------------------------------------------------
-spec current_span_ctx(ctx:t()) -> maybe(opencensus:span_ctx()).
current_span_ctx(Ctx) ->
    ctx:get(Ctx, ?SPAN_CTX, undefined).


-spec parent_span_ctx(maybe(opencensus:span_ctx() | opencensus:span())) ->
                             maybe(opencensus:span_ctx()).
parent_span_ctx(#span_ctx{span_id=SpanId}) ->
    parent_span_ctx_for_span_id(SpanId);
parent_span_ctx(#span{parent_span_id=undefined}) ->
    undefined;
parent_span_ctx(#span{parent_span_id=ParentId}) ->
    span_ctx_for_span_id(ParentId);
parent_span_ctx(undefined) ->
    undefined.


%%--------------------------------------------------------------------
%% @doc
%% Set the current span context in a context to `SpanCtx'. Or to a new
%% span context with name `Name` that is the child of the span context
%% in `Ctx', if it exists.
%% @end
%%--------------------------------------------------------------------
-spec with_span_ctx(Ctx, SpanCtx) -> Ctx when
      Ctx :: ctx:t(),
      SpanCtx :: opencensus:span_ctx().
with_span_ctx(Ctx, SpanCtx=#span_ctx{}) ->
    ctx:with_value(Ctx, ?SPAN_CTX, SpanCtx).

%%--------------------------------------------------------------------
%% @doc
%% Create a child span with parent from the current context `Ctx'. And
%% sets it as the current span context in `Ctx'.
%% @end
%%--------------------------------------------------------------------
-spec with_child_span(Ctx, Name) -> Ctx when
      Ctx :: ctx:t(),
      Name :: unicode:unicode_binary().
with_child_span(Ctx, Name) ->
    with_span_ctx(Ctx, new_span_(Name, current_span_ctx(Ctx), ?SPAN_KIND_UNSPECIFIED, false)).

-spec with_child_span(Ctx, Name, Options) -> Ctx when
      Ctx :: ctx:t(),
      Name :: unicode:unicode_binary(),
      Options :: #{remote_parent => boolean(),
                   sampler => module(),
                   attributes => opencensus:attributes()}.
with_child_span(Ctx, Name, Options) ->
    with_span_ctx(Ctx, start_span(Name, current_span_ctx(Ctx), Options)).

%%--------------------------------------------------------------------
%% @doc
%% Create a new span, detached from any context.
%% @end
%%--------------------------------------------------------------------

-spec start_span(Name, SpanCtx) -> SpanCtx when
      Name :: unicode:unicode_binary(),
      SpanCtx :: opencensus:span_ctx().
start_span(Name, SpanCtx) ->
    new_span_(Name, SpanCtx, ?SPAN_KIND_UNSPECIFIED, false).

-spec start_span(Name, SpanCtx, Options) -> SpanCtx when
      Name :: unicode:unicode_binary(),
      SpanCtx :: opencensus:span_ctx(),
      Options :: #{remote_parent => boolean(),
                   sampler => module(),
                   kind => opencensus:span_kind(),
                   attributes => opencensus:attributes()}.
start_span(Name, SpanCtx, Options) ->
    RemoteParent = maps:get(remote_parent, Options, false),
    Attributes = maps:get(attributes, Options, #{}),
    Kind = maps:get(kind, Options, ?SPAN_KIND_UNSPECIFIED),

    %% TODO: support overriding the sampler
    _Sampler = maps:get(sampler, Options, undefined),

    SpanCtx1 = new_span_(Name, SpanCtx, Kind, RemoteParent),
    put_attributes(Attributes, SpanCtx1),
    SpanCtx1.

%% if parent is undefined, first run sampler
new_span_(Name, undefined, Kind, _) ->
    TraceId = opencensus:generate_trace_id(),
    Span = #span_ctx{trace_id=TraceId,
                     trace_options=0},
    TraceOptions = update_trace_options(should_sample, Span),
    new_span_(Name, Span#span_ctx{trace_options=TraceOptions}, Kind, false);
%% if parent is remote, first run sampler
new_span_(Name, Span=#span_ctx{trace_id=TraceId}, Kind, RemoteParent) when RemoteParent =:= true ->
    TraceOptions = update_trace_options(should_sample, Span),
    new_span_(Name, #span_ctx{trace_id=TraceId,
                              trace_options=TraceOptions}, Kind, false);
new_span_(Name, Parent=#span_ctx{trace_id=TraceId,
                                 trace_options=TraceOptions,
                                 span_id=ParentSpanId}, Kind, _RemoteParent) when ?IS_ENABLED(TraceOptions) ->
    SpanId = opencensus:generate_span_id(),

    ?SET_LOG_METADATA(TraceId, SpanId),

    Span = #span{trace_id=TraceId,
                 span_id=SpanId,
                 start_time=wts:timestamp(),
                 parent_span_id=ParentSpanId,
                 kind=Kind,
                 name=Name,
                 attributes=#{}},

    ets:insert(?SPAN_TAB, Span),

    Parent#span_ctx{span_id=SpanId};
new_span_(_Name, Parent, _Kind, _RemoteParent) ->
    SpanId = opencensus:generate_span_id(),

    ?SET_LOG_METADATA(TraceId, SpanId),

    %% if discarded by sampler, create no span
    Parent#span_ctx{span_id=SpanId}.

%%
update_trace_options(should_sample, #span_ctx{trace_id=TraceId,
                                              span_id=ParentSpanId,
                                              trace_options=ParentTraceOptions}) ->
    case oc_sampler:should_sample(TraceId, ParentSpanId, ?IS_ENABLED(ParentTraceOptions)) of
        true ->
            1;
        false ->
            0
    end.

%%--------------------------------------------------------------------
%% @doc
%% Finish a span, setting the end_time.
%% @end
%%--------------------------------------------------------------------
-spec finish_span(maybe(opencensus:span_ctx())) -> boolean().
finish_span(#span_ctx{span_id=SpanId,
                      trace_options=TraceOptions}) when ?IS_ENABLED(TraceOptions) ->
    case ets:take(?SPAN_TAB, SpanId) of
        [SpanData] ->
            oc_span:finish_span(SpanData);
        _ ->
            false
    end;
finish_span(_) ->
    true.

%%--------------------------------------------------------------------
%% @doc
%% Returns true if trace is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(maybe(opencensus:span_ctx())) -> boolean().
is_enabled(undefined) ->
    false;
is_enabled(#span_ctx{trace_options=TraceOptions}) ->
    ?IS_ENABLED(TraceOptions).

%%--------------------------------------------------------------------
%% @doc
%% Put an attribute (a key/value pair) in the attribute map of a span.
%% If the attribute already exists it is overwritten with the new value.
%% Returns true if the data was successfully updated.
%% @end
%%--------------------------------------------------------------------
-spec put_attribute(unicode:unicode_binary(), opencensus:attribute_value(), maybe(opencensus:span_ctx())) -> boolean().
put_attribute(Key, Value, SpanCtx) ->
    lookup_and_replace(SpanCtx, fun(SpanData) ->
                                        oc_span:put_attribute(Key, Value, SpanData)
                                end).

%%--------------------------------------------------------------------
%% @doc
%% Merge a map of attributes with the current attributes of a span.
%% The new values overwrite the old if any keys are the same.
%% Returns true if the data was successfully updated.
%% @end
%%--------------------------------------------------------------------
-spec put_attributes(#{unicode:unicode_binary() => opencensus:attribute_value()},
                     maybe(opencensus:span_ctx())) -> boolean().
put_attributes(NewAttributes, SpanCtx) ->
    lookup_and_replace(SpanCtx, fun(SpanData) ->
                                        oc_span:put_attributes(NewAttributes, SpanData)
                                end).

%%--------------------------------------------------------------------
%% @doc
%% Add an Annotation or MessageEvent to the list of TimeEvents in a span.
%% Returns true if the data was successfully updated.
%% @end
%%--------------------------------------------------------------------
-spec add_time_event(opencensus:annotation() | opencensus:message_event(),
                     maybe(opencensus:span_ctx())) -> boolean().
add_time_event(TimeEvent, Span) ->
    add_time_event(wts:timestamp(), TimeEvent, Span).

-spec add_time_event(wts:timestamp(), opencensus:annotation() | opencensus:message_event(),
                     maybe(opencensus:span_ctx())) -> boolean().
add_time_event(Timestamp, TimeEvent, SpanCtx) ->
    lookup_and_replace(SpanCtx, fun(SpanData) ->
                                        oc_span:add_time_event(Timestamp, TimeEvent, SpanData)
                                end).

%%--------------------------------------------------------------------
%% @doc
%% Create an Annotation.
%% @end
%%--------------------------------------------------------------------
-spec annotation(unicode:unicode_binary(), opencensus:attributes()) -> opencensus:annotation().
annotation(Description, Attributes) ->
    #annotation{description=Description,
                attributes=Attributes}.

%%--------------------------------------------------------------------
%% @doc
%% Create a MessageEvent.
%% @end
%%--------------------------------------------------------------------
-spec message_event(opencensus:message_event_type(), integer(), integer(), integer()) -> opencensus:message_event().
message_event(MessageEventType, Id, UncompressedSize, CompressedSize) ->
    #message_event{type=MessageEventType,
                   id=Id,
                   uncompressed_size=UncompressedSize,
                   compressed_size=CompressedSize}.

%%--------------------------------------------------------------------
%% @doc
%% Set Status. Returns true if the data was successfully updated.
%% @end
%%--------------------------------------------------------------------
-spec set_status(integer(), unicode:unicode_binary(), maybe(opencensus:span_ctx()))-> boolean().
set_status(Code, Message, #span_ctx{span_id=SpanId,
                                    trace_options=TraceOptions}) when ?IS_ENABLED(TraceOptions) ->
    ets:update_element(?SPAN_TAB, SpanId, [{#span.status, #status{code=Code,
                                                                  message=Message}}]);
set_status(_Code, _Message, _Span) ->
    true.


%%--------------------------------------------------------------------
%% @doc
%% Add a Link to the list of Links in the span. Returns true if the
%% data was successfully updated.
%% @end
%%--------------------------------------------------------------------
-spec add_link(opencensus:link(), maybe(opencensus:span_ctx())) -> boolean().
add_link(Link, SpanCtx) ->
    lookup_and_replace(SpanCtx, fun(SpanData) ->
                                        oc_span:add_link(Link, SpanData)
                                end).

%%--------------------------------------------------------------------
%% @doc
%% Create a Link which can be added to a Span.
%% @end
%%--------------------------------------------------------------------
-spec link(opencensus:link_type(), opencensus:trace_id(), opencensus:span_id(), opencensus:attributes())
          -> opencensus:link().
link(LinkType, TraceId, SpanId, Attributes) ->
    #link{type=LinkType,
          trace_id=TraceId,
          span_id=SpanId,
          attributes=Attributes}.

%% Internal functions

lookup_and_replace(#span_ctx{span_id=SpanId,
                             trace_options=TraceOptions}, Fun) when ?IS_ENABLED(TraceOptions) ->
    case ets:lookup(?SPAN_TAB, SpanId) of
        [SpanData] ->
            1 =:= ets:select_replace(?SPAN_TAB, [{SpanData, [], [{const, Fun(SpanData)}]}]);
        _ ->
            false
    end;
lookup_and_replace(_, _) ->
    true.

parent_span_ctx_for_span_id(SpanId) ->
    case ets:lookup(?SPAN_TAB, SpanId) of
        [] ->
            undefined;
        [#span{parent_span_id=ParentSpanId}] ->
            span_ctx_for_span_id(ParentSpanId)
    end.

span_ctx_for_span_id(SpanId) ->
    case ets:lookup(?SPAN_TAB, SpanId) of
        [] ->
            undefined;
        [#span{trace_id=TraceId,
               trace_options=TraceOptions}] ->
            #span_ctx{trace_id=TraceId,
                      span_id=SpanId,
                      trace_options=TraceOptions}
    end.
