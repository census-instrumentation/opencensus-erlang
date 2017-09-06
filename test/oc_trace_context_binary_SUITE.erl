%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_trace_context_binary_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [encode_decode, decode_with_extra_junk].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

encode_decode(_Config) ->
    %% TraceId: 85409434994488837557643013731547696719
    %% SpanId: 7017280452245743464
    %% Enabled: true
    Binary = <<0,0,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,1,97,98,99,100,101,102,103,104,2,1>>,
    Decoded = oc_trace_context_binary:decode(Binary),
    Encoded = oc_trace_context_binary:encode(Decoded),

    ?assertMatch(Binary, Encoded).

decode_with_extra_junk(_Config) ->
    %% TraceId: 85409434994488837557643013731547696719
    %% SpanId: 7017280452245743464
    %% Enabled: true
    Binary = <<0,0,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,1,97,98,99,100,101,102,103,104,2,1>>,
    BinaryWithJunk = <<Binary/binary,23,4,5>>,
    Decoded = oc_trace_context_binary:decode(BinaryWithJunk),
    Encoded = oc_trace_context_binary:encode(Decoded),

    ?assertMatch(Binary, Encoded).
