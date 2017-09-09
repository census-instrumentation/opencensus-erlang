%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_trace_context_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [encode_decode, decode_with_extra_junk, encode_decode_headers].

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

encode_decode_headers(_Config) ->
    %% TraceId: 4bf92f3577b34da6a3ce929d0e0e4736
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: true
    Header = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,
    Decoded = oc_trace_context_headers:decode(Header),
    Encoded = oc_trace_context_headers:encode(Decoded),
    ?assertEqual(Header, list_to_binary(Encoded)),
    ?assertEqual(Decoded, oc_trace_context_headers:decode(Encoded)).
