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
%% @doc Functions to support the binary format trace context serialization.
%% Implements the spec found here
%% [https://github.com/census-instrumentation/opencensus-specs/blob/7b426409/encodings/BinaryEncoding.md]
%% @end
%%%-------------------------------------------------------------------------
-module(oc_span_ctx_binary).

-export([encode/1,
         decode/1]).

-include("opencensus.hrl").

-define(VERSION, 0).

-define(TRACE_ID_FIELD_NUM, 0).
-define(SPAN_ID_FIELD_NUM, 1).
-define(TRACE_OPTIONS_FIELD_NUM, 2).

-spec encode(opencensus:span_ctx()) -> maybe(binary()).
encode(#span_ctx{trace_id=TraceId,
                 span_id=SpanId}) when TraceId =:= 0
                                       ; SpanId =:= 0 ->
    undefined;
encode(#span_ctx{trace_id=TraceId,
                 span_id=SpanId,
                 trace_options=TraceOptions}) ->
    Options = case TraceOptions band 1 of 1 -> <<1:8>>; _ -> <<0:8>> end,
    <<?VERSION:8, 0:8, TraceId:128, 1:8, SpanId:64, 2:8, Options/binary>>.

-spec decode(binary()) -> maybe(opencensus:span_ctx()).
decode(<<0:8/integer, VersionFormat/binary>>) ->
    decode_v0(VersionFormat, #span_ctx{}).

decode_v0(<<>>, TraceContext) ->
    TraceContext;
decode_v0(<<?TRACE_ID_FIELD_NUM:8/signed-integer, TraceId:128/integer, _/binary>>, _)
  when TraceId =:= 0 ->
    undefined;
decode_v0(<<?TRACE_ID_FIELD_NUM:8/signed-integer, TraceId:128/integer, Rest/binary>>, TraceContext) ->
    decode_v0(Rest, TraceContext#span_ctx{trace_id=TraceId});
decode_v0(<<?SPAN_ID_FIELD_NUM:8/signed-integer, SpanId:64/integer, _/binary>>, _)
  when SpanId =:= 0 ->
    undefined;
decode_v0(<<?SPAN_ID_FIELD_NUM:8/signed-integer, SpanId:64/integer, Rest/binary>>, TraceContext) ->
    decode_v0(Rest, TraceContext#span_ctx{span_id=SpanId});
decode_v0(<<?TRACE_OPTIONS_FIELD_NUM:8/signed-integer, TraceOptions:8/signed-integer, Rest/binary>>, TraceContext) ->
    decode_v0(Rest, TraceContext#span_ctx{trace_options=TraceOptions});
decode_v0(_, TraceContext) ->
    TraceContext.
