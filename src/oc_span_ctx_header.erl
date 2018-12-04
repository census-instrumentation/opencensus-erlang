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
-module(oc_span_ctx_header).

-export([field_name/0,
         encode/1,
         decode/1]).

-include("opencensus.hrl").

-define(VERSION, "00").

-define(ZERO_TRACEID, <<"00000000000000000000000000000000">>).
-define(ZERO_SPANID, <<"0000000000000000">>).

field_name() ->
    <<"traceparent">>.

-spec encode(opencensus:span_ctx()) -> maybe(iolist()).
encode(#span_ctx{trace_id=TraceId,
                 span_id=SpanId}) when TraceId =:= 0
                                       ;  SpanId =:= 0 ->
    undefined;
encode(#span_ctx{trace_id=TraceId,
                 span_id=SpanId,
                 trace_options=TraceOptions}) ->
    Options = case TraceOptions band 1 of 1 -> <<"01">>; _ -> <<"00">> end,
    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
    [?VERSION, "-", EncodedTraceId, "-", EncodedSpanId, "-", Options].

-spec decode(iodata()) -> maybe(opencensus:span_ctx()).
decode(TraceContext) when is_list(TraceContext) ->
    decode(list_to_binary(TraceContext));
decode(<<?VERSION, "-", TraceId:32/binary, "-", SpanId:16/binary, _/binary>>) when TraceId =:= ?ZERO_TRACEID
                                                                                 ;  SpanId =:= ?ZERO_SPANID ->
    undefined;
decode(<<?VERSION, "-", TraceId:32/binary, "-", SpanId:16/binary, "-", TraceOptions:2/binary, _Rest/binary>>) ->
    #span_ctx{trace_id=binary_to_integer(TraceId, 16),
              span_id=binary_to_integer(SpanId, 16),
              trace_options=case TraceOptions of <<"01">> -> 1; _ -> 0 end};
decode(_) ->
    undefined.
