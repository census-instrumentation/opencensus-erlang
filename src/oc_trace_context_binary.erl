%%%-------------------------------------------------------------------
%% @doc opencensus
%% @end
%%%-------------------------------------------------------------------
-module(oc_trace_context_binary).

-export([encode/1,
         decode/1]).

-include("opencensus.hrl").

-define(VERSION, 0).

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
decode_0(<<0:8/signed-integer, TraceId:128/integer, Rest/binary>>, TraceContext) ->
    decode_0(Rest, TraceContext#trace_context{trace_id=TraceId});
decode_0(<<1:8/signed-integer, SpanId:64/integer, Rest/binary>>, TraceContext) ->
    decode_0(Rest, TraceContext#trace_context{span_id=SpanId});
decode_0(<<2:8/signed-integer, _TraceOptions:7, Enabled:1, Rest/binary>>, TraceContext) ->
    decode_0(Rest, TraceContext#trace_context{enabled=case Enabled of 1 -> true; _ -> false end});
decode_0(_, TraceContext) ->
    TraceContext.
