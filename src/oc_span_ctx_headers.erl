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
-module(oc_span_ctx_headers).

-export([to_headers/1,
         encode/1,
         from_headers/1,
         decode/1]).

-include("opencensus.hrl").

-define(VERSION, "00").

-define(ZERO_TRACEID, <<"00000000000000000000000000000000">>).
-define(ZERO_SPANID, <<"0000000000000000">>).

-define(HEADER_KEY, <<"traceparent">>).

-spec to_headers(opencensus:span_ctx() | undefined) -> [{binary(), iolist()}].
to_headers(#span_ctx{trace_id=TraceId,
                     span_id=SpanId})
  when TraceId =:= 0 orelse SpanId =:= 0 ->
    [];
to_headers(SpanCtx=#span_ctx{}) ->
    EncodedValue = encode(SpanCtx),
    [{?HEADER_KEY, EncodedValue}];
to_headers(undefined) ->
    [].

-spec encode(opencensus:span_ctx()) -> iolist().
encode(#span_ctx{trace_id=TraceId,
                 span_id=SpanId,
                 trace_options=TraceOptions}) ->
    Options = case TraceOptions band 1 of 1 -> <<"01">>; _ -> <<"00">> end,
    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
    [?VERSION, "-", EncodedTraceId, "-", EncodedSpanId, "-", Options].

-spec from_headers(list() | map()) -> maybe(opencensus:span_ctx()).
from_headers(Headers) when is_map(Headers) ->
    decode(maps:get(?HEADER_KEY, Headers, undefined));
from_headers(Headers) when is_list(Headers) ->
    case lists:keyfind(?HEADER_KEY, 1, Headers) of
        {_, Value} ->
            decode(Value);
        _ ->
            undefined
    end.

decode(TraceContext) when is_list(TraceContext) ->
    decode(list_to_binary(TraceContext));
decode(<<?VERSION, "-", TraceId:32/binary, "-", SpanId:16/binary, _/binary>>)
  when TraceId =:= ?ZERO_TRACEID orelse SpanId =:= ?ZERO_SPANID ->
    undefined;
decode(<<?VERSION, "-", TraceId:32/binary, "-", SpanId:16/binary, "-", Opts:2/binary, _/binary>>) ->
    try
        #span_ctx{trace_id=binary_to_integer(TraceId, 16),
                  span_id=binary_to_integer(SpanId, 16),
                  trace_options=case Opts of <<"01">> -> 1; _ -> 0 end}
    catch
        %% to integer from base 16 string failed
        error:badarg ->
            undefined
    end;
decode(_) ->
    undefined.
