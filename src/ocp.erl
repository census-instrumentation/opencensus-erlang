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
%% @doc ocp uses the pdict instead of a ctx variable for tracking context.
%%      The functions fetch the current span context from the pdict and
%%      passes it through to the oc_trace function of the same name.
%% @end
%%%-----------------------------------------------------------------------
-module(ocp).

-export([with_tags/1,

         with_span_ctx/1,

         with_child_span/1,
         with_child_span/2,
         with_child_span/3,

         current_span_ctx/0,
         current_tags/0,

         finish_span/0,

         put_attribute/2,
         put_attributes/1,
         add_time_event/1,
         add_time_event/2,
         add_link/1,
         set_status/2]).

-include("opencensus.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Replaces the tags in the current context.
%% @end
%%--------------------------------------------------------------------
-spec with_tags(opencensus:tags()) -> maybe(opencensus:tags()).
with_tags(Map) ->
    put(?TAG_CTX, Map).

%%--------------------------------------------------------------------
%% @doc
%% Replaces the span in the current context.
%% @end
%%--------------------------------------------------------------------
-spec with_span_ctx(opencensus:span_ctx()) -> maybe(opencensus:span_ctx()).
with_span_ctx(Span) ->
    put(?SPAN_CTX, Span).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span as a child of the current span and replaces it.
%% @end
%%--------------------------------------------------------------------
-spec with_child_span(unicode:unicode_binary()) -> opencensus:maybe(opencensus:span_ctx()).
with_child_span(Name) ->
    with_span_ctx(oc_trace:start_span(Name, current_span_ctx(), #{})).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with attributes as a child of the current span
%% and replaces it.
%% @end
%%--------------------------------------------------------------------
-spec with_child_span(unicode:unicode_binary(), opencensus:attributes()) -> opencensus:maybe(opencensus:span_ctx()).
with_child_span(Name, Attributes) ->
    with_span_ctx(oc_trace:start_span(Name, current_span_ctx(), #{attributes => Attributes})).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span as a child of the current span and uses it as the
%% current span while running the function `Fun', finishing the span
%% and resetting the current span context after the function finishes.
%% @end
%%--------------------------------------------------------------------
-spec with_child_span(unicode:unicode_binary(), opencensus:attributes(), fun()) -> maybe(opencensus:span_ctx()).
with_child_span(Name, Attributes, Fun) ->
    CurrentSpanCtx = current_span_ctx(),
    NewSpanCtx = oc_trace:start_span(Name, CurrentSpanCtx, #{attributes => Attributes}),
    with_span_ctx(NewSpanCtx),
    try Fun()
    after
        oc_trace:finish_span(current_span_ctx()),
        with_span_ctx(CurrentSpanCtx)
    end.

-spec current_span_ctx() -> maybe(opencensus:span_ctx()).
current_span_ctx() ->
    get(?SPAN_CTX).

-spec current_tags() -> opencensus:tags().
current_tags() ->
    case get(?TAG_CTX) of
        undefined ->
            oc_tags:new();
        Tags ->
            Tags
    end.

%%--------------------------------------------------------------------
%% @doc
%% Finishes the span in the current pdict context.
%% @end
%%--------------------------------------------------------------------
-spec finish_span() -> boolean().
finish_span() ->
    oc_trace:finish_span(current_span_ctx()).

%%--------------------------------------------------------------------
%% @doc
%% Put an attribute (a key/value pair) in the attribute map of a span.
%% If the attribute already exists it is overwritten with the new value.
%% @end
%%--------------------------------------------------------------------
-spec put_attribute(unicode:unicode_binary(), opencensus:attribute_value()) -> boolean() | {error, invalid_attribute}.
put_attribute(Key, Value) when is_binary(Key)
                               , (is_binary(Value) orelse is_integer(Value) orelse is_boolean(Value)) ->
    oc_trace:put_attribute(Key, Value, current_span_ctx());
put_attribute(_Key, _Value) ->
    {error, invalid_attribute}.

%%--------------------------------------------------------------------
%% @doc
%% Merge a map of attributes with the attributes of current span.
%% The new values overwrite the old if any keys are the same.
%% @end
%%--------------------------------------------------------------------
-spec put_attributes(#{unicode:unicode_binary() => opencensus:attribute_value()}) -> boolean().
put_attributes(NewAttributes) ->
    oc_trace:put_attributes(NewAttributes, current_span_ctx()).

%%--------------------------------------------------------------------
%% @doc
%% Add an Annotation or MessageEvent to the list of TimeEvents in the
%% current span.
%% @end
%%--------------------------------------------------------------------
-spec add_time_event(opencensus:annotation() | opencensus:message_event()) -> boolean().
add_time_event(TimeEvent) ->
    oc_trace:add_time_event(TimeEvent, current_span_ctx()).

-spec add_time_event(wts:timestamp(), opencensus:annotation() | opencensus:message_event()) -> boolean().
add_time_event(Timestamp, TimeEvent) ->
    oc_trace:add_time_event(Timestamp, TimeEvent, current_span_ctx()).

%%--------------------------------------------------------------------
%% @doc
%% Set Status of current span.
%% @end
%%--------------------------------------------------------------------
-spec set_status(integer(), unicode:unicode_binary()) -> boolean().
set_status(Code, Message) ->
    oc_trace:set_status(Code, Message, current_span_ctx()).

%%--------------------------------------------------------------------
%% @doc
%% Add a Link to the list of Links in the current span.
%% @end
%%--------------------------------------------------------------------
-spec add_link(opencensus:link()) -> boolean().
add_link(Link) ->
    oc_trace:add_link(Link, current_span_ctx()).
