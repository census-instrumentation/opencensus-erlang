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
%% @end
%%%-------------------------------------------------------------------------
-module(oc_trace_context_headers).

-export([encode/1,
         decode/1]).

-include("opencensus.hrl").

-define(VERSION, "00").

-spec encode(opencensus:trace_context()) -> iolist().
encode(#trace_context{trace_id=TraceId,
                      span_id=SpanId,
                      enabled=Enabled}) ->
    Options = case Enabled of true -> <<"01">>; _ -> <<"00">> end,
    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
    [?VERSION, "-", EncodedTraceId, "-", EncodedSpanId, "-", Options].

-spec decode(binary()) -> opencensus:trace_context().
decode(TraceContext) when is_list(TraceContext) ->
    decode(list_to_binary(TraceContext));
decode(<<?VERSION, "-", TraceId:32/binary, "-", SpanId:16/binary, "-", Enabled:2/binary, _Rest/binary>>) ->
    #trace_context{trace_id=binary_to_integer(TraceId, 16),
                   span_id=binary_to_integer(SpanId, 16),
                   enabled=case Enabled of <<"01">> -> true; _ -> false end}.
