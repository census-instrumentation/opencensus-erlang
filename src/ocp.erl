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
%% @doc opencensus functionality using the pdict to track trace context
%% @end
%%%-----------------------------------------------------------------------
-module(ocp).

-export([start_trace/0,
         start_trace/1,
         start_span/1,
         finish_span/0,
         context/0,
         put_attribute/2,
         put_attributes/1]).

-include("opencensus.hrl").

-define(CONTEXT, current_context).
-define(KEY, current_span).
-define(SKEY, current_spans).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new trace context.
%% @end
%%--------------------------------------------------------------------
-spec start_trace() -> ok.
start_trace() ->
    put(?CONTEXT, opencensus:start_trace()),
    put(?SKEY, []).

-spec start_trace(opencensus:trace_context()) -> ok.
start_trace(TraceContext = #trace_context{}) ->
    put(?CONTEXT, TraceContext),
    put(?SKEY, []);
start_trace(_) ->
    put(?CONTEXT, undefined),
    put(?SKEY, []),
    error.

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span as a child of the current span, if one exists,
%% and pushes the parent on the span stack in the process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec start_span(unicode:unicode_binary()) -> opencensus:maybe(opencensus:span()).
start_span(Name) ->
    Ctx = case get(?KEY) of
              undefined ->
                  get(?CONTEXT);
              Span ->
                  case get(?SKEY) of
                      undefined ->
                          put(?SKEY, [Span]);
                      Spans ->
                          put(?SKEY, [Span | Spans])
                  end,
                  Span
          end,
    NewSpan = opencensus:start_span(Name, Ctx),
    put(?KEY, NewSpan),

    NewSpan.


%%--------------------------------------------------------------------
%% @doc
%% Finishes the current span and pops it from the stack of open spans.
%% @end
%%--------------------------------------------------------------------
-spec finish_span() -> opencensus:maybe(opencensus:span()).
finish_span() ->
    OldSpan = opencensus:finish_span(get(?KEY)),
    case get(?SKEY) of
        undefined ->
            put(?KEY, undefined),
            undefined;
        [] ->
            put(?KEY, undefined),
            undefined;
        [Span | Spans] ->
            put(?SKEY, Spans),
            put(?KEY, Span)
    end,
    OldSpan.

%%--------------------------------------------------------------------
%% @doc
%% Return the current trace context for the process.
%% @end
%%--------------------------------------------------------------------
-spec context() -> opencensus:maybe(opencensus:span()).
context() ->
    Span = get(?KEY),
    opencensus:context(Span).

%%--------------------------------------------------------------------
%% @doc
%% Put an attribute (a key/value pair) in the attribute map of a span.
%% If the attribute already exists it is overwritten with the new value.
%% @end
%%--------------------------------------------------------------------
-spec put_attribute(unicode:unicode_binary(), opencensus:attribute_value()) -> ok | {error, invalid_attribute}.
put_attribute(Key, Value) when is_binary(Key)
                             , (is_binary(Value) orelse is_integer(Value) orelse is_boolean(Value)) ->
    put(?KEY, opencensus:put_attribute(Key, Value, get(?KEY)));
put_attribute(_Key, _Value) ->
    {error, invalid_attribute}.

%%--------------------------------------------------------------------
%% @doc
%% Merge a map of attributes with the current attributes of a span.
%% The new values overwrite the old if any keys are the same.
%% @end
%%--------------------------------------------------------------------
-spec put_attributes(#{unicode:unicode_binary() => opencensus:attribute_value()}) -> ok.
put_attributes(NewAttributes) ->
    put(?KEY, opencensus:put_attributes(NewAttributes, get(?KEY))).
