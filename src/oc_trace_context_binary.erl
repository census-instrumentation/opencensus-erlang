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
%% @doc Functions to support the binary format trace context serialization
%% @end
%%%-------------------------------------------------------------------------
-module(oc_trace_context_binary).

-export([encode/1,
         decode/1]).

-include("opencensus.hrl").

-define(VERSION, 0).

-define(TRACE_ID_FIELD_NUM, 0).
-define(SPAN_ID_FIELD_NUM, 1).
-define(TRACE_OPTIONS_FIELD_NUM, 2).

-spec encode(opencensus:trace_context()) -> binary().
encode(#trace_context{trace_id=TraceId,
                      span_id=SpanId,
                      enabled=Enabled}) ->
    Options = case Enabled of true -> <<1:8>>; _ -> <<0:8>> end,
    <<?VERSION:8, 0:8, TraceId:128, 1:8, SpanId:64, 2:8, Options/binary>>.

-spec decode(binary()) -> opencensus:trace_context().
decode(<<0:8/integer, VersionFormat/binary>>) ->
    decode_0(VersionFormat, #trace_context{}).

decode_0(<<>>, TraceContext) ->
    TraceContext;
decode_0(<<?TRACE_ID_FIELD_NUM:8/signed-integer, TraceId:128/integer, Rest/binary>>, TraceContext) ->
    decode_0(Rest, TraceContext#trace_context{trace_id=TraceId});
decode_0(<<?SPAN_ID_FIELD_NUM:8/signed-integer, SpanId:64/integer, Rest/binary>>, TraceContext) ->
    decode_0(Rest, TraceContext#trace_context{span_id=SpanId});
decode_0(<<?TRACE_OPTIONS_FIELD_NUM:8/signed-integer, _TraceOptions:7, Enabled:1, Rest/binary>>, TraceContext) ->
    decode_0(Rest, TraceContext#trace_context{enabled=case Enabled of 1 -> true; _ -> false end});
decode_0(_, TraceContext) ->
    TraceContext.
