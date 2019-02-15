%%%-------------------------------------------------------------------------
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
%% @doc Functions to support the http header format of the tracecontext spec
%% Implements the spec found here
%% @end
%%%-------------------------------------------------------------------------
-module(oc_span_ctx_b3_headers).

-export([to_headers/1,
         from_headers/1]).

-include("opencensus.hrl").

-define(B3_TRACE_ID, <<"X-B3-TraceId">>).
-define(B3_SPAN_ID, <<"X-B3-SpanId">>).
-define(B3_SAMPLED, <<"X-B3-Sampled">>).

-define(IS_SAMPLED(S), S =:= "1" orelse S =:= <<"1">> orelse S =:= "true" orelse S =:= <<"true">>).

-spec to_headers(opencensus:span_ctx()) -> maybe(list()).
to_headers(#span_ctx{trace_id=TraceId,
                     span_id=SpanId}) when TraceId =:= 0
                                           ; SpanId =:= 0 ->
    [];
to_headers(#span_ctx{trace_id=TraceId,
                     span_id=SpanId,
                     trace_options=TraceOptions}) ->
    Options = case TraceOptions band 1 of 1 -> "1"; _ -> "0" end,
    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
    [{?B3_TRACE_ID, EncodedTraceId},
     {?B3_SPAN_ID, EncodedSpanId},
     {?B3_SAMPLED, Options}];
to_headers(undefined) ->
    [].

-spec from_headers(list() | map()) -> maybe(opencensus:span_ctx()).
from_headers(Headers) when is_map(Headers) ->
    from_headers(maps:to_list(Headers));
from_headers(Headers) when is_list(Headers) ->
    try
        TraceId = trace_id(Headers),
        SpanId = span_id(Headers),
        Sampled = lookup(?B3_SAMPLED, Headers),
        #span_ctx{trace_id=string_to_integer(TraceId, 16),
                  span_id=string_to_integer(SpanId, 16),
                  trace_options=case Sampled of True when ?IS_SAMPLED(True) -> 1; _ -> 0 end}
    catch
        throw:invalid ->
            undefined;

        %% thrown if _to_integer fails
        error:badarg ->
            undefined
    end;
from_headers(_) ->
    undefined.

trace_id(Headers) ->
    case lookup(?B3_TRACE_ID, Headers) of
        TraceId when is_list(TraceId) orelse is_binary(TraceId) ->
            case string:length(TraceId) =:= 32 orelse string:length(TraceId) =:= 16 of
                true ->
                    TraceId;
                _ ->
                    throw(invalid)
            end;
        _ ->
            throw(invalid)
    end.

span_id(Headers) ->
    case lookup(?B3_SPAN_ID, Headers) of
        SpanId when is_list(SpanId) orelse is_binary(SpanId) ->
            case string:length(SpanId) =:= 32 orelse string:length(SpanId) =:= 16 of
                true ->
                    SpanId;
                _ ->
                    throw(invalid)
            end;
        _ ->
            throw(invalid)
    end.

%% find a header in a list, ignoring case
lookup(_, []) ->
    undefined;
lookup(Header, [{H, Value} | Rest]) ->
    case string:equal(Header, H, true, none) of
        true ->
            Value;
        false ->
            lookup(Header, Rest)
    end.

string_to_integer(S, Base) when is_binary(S) ->
    binary_to_integer(S, Base);
string_to_integer(S, Base) when is_list(S) ->
    list_to_integer(S, Base).
