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

-export([start_span/1,
         finish_span/0,
         child_span/1]).

-include("opencensus.hrl").

-define(KEY, current_span).
-define(SKEY, current_spans).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new trace with span stored in the process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec start_span(unicode:chardata()) -> opencensus:maybe(opencensus:span()).
start_span(Name) ->
    Span = opencensus:start_span(Name, opencensus:generate_trace_id(), undefined),
    put(?KEY, Span),
    Span.

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
%% Starts a new span as a child of the current span and pushes the
%% parent to the open span stack in the process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec child_span(unicode:chardata()) -> opencensus:maybe(opencensus:span()).
child_span(Name) ->
    Span = get(?KEY),
    NewSpan = opencensus:child_span(Name, Span),
    put(?KEY, NewSpan),
    case get(?SKEY) of
        undefined ->
            put(?SKEY, [Span]);
        Spans ->
            put(?SKEY, [Span | Spans])
    end,
    NewSpan.
