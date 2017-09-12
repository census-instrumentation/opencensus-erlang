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
%% @doc Functions to support the http header format of the tracecontext-spec
%% Implements the spec found here
%% [https://github.com/TraceContext/tracecontext-spec/blob/018cd514b/HTTP_HEADER_FORMAT.md]
%% @end
%%%-------------------------------------------------------------------------
-module(oc_trace_context_headers).

-export([encode/1,
         decode/1]).

-include("opencensus.hrl").

-define(VERSION, "00").

-define(ZERO_TRACEID, <<"00000000000000000000000000000000">>).
-define(ZERO_SPANID, <<"0000000000000000">>).

-spec encode(opencensus:trace_context()) -> {ok, iolist()} | {error, invalid}.
encode(#trace_context{trace_id=TraceId,
                      span_id=SpanId}) when TraceId =:= 0
                                          ;  SpanId =:= 0 ->
    {error, invalid};
encode(#trace_context{trace_id=TraceId,
                      span_id=SpanId,
                      enabled=Enabled}) ->
    Options = case Enabled of true -> <<"01">>; _ -> <<"00">> end,
    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
    {ok, [?VERSION, "-", EncodedTraceId, "-", EncodedSpanId, "-", Options]}.

-spec decode(iolist() | binary()) -> {ok, opencensus:trace_context()} | {error, invalid}.
decode(TraceContext) when is_list(TraceContext) ->
    decode(list_to_binary(TraceContext));
decode(<<?VERSION, "-", TraceId:32/binary, "-", SpanId:16/binary, _/binary>>) when TraceId =:= ?ZERO_TRACEID
                                                                                 ;  SpanId =:= ?ZERO_SPANID ->
    {error, invalid};
decode(<<?VERSION, "-", TraceId:32/binary, "-", SpanId:16/binary, "-", Enabled:2/binary, _Rest/binary>>) ->
    {ok, #trace_context{trace_id=binary_to_integer(TraceId, 16),
                        span_id=binary_to_integer(SpanId, 16),
                        enabled=case Enabled of <<"01">> -> true; _ -> false end}};
decode(_) ->
    {error, invalid}.
