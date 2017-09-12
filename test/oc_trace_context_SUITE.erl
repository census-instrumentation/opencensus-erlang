%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_trace_context_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

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
    {ok, Decoded} = oc_trace_context_binary:decode(Binary),
    {ok, Encoded} = oc_trace_context_binary:encode(Decoded),
    ?assertMatch(Binary, Encoded),

    InvalidBothIdsBinary = <<0:8,0:8,0:128,1:8,0:64,2:8,1>>,
    ?assertEqual({error, invalid}, oc_trace_context_binary:decode(InvalidBothIdsBinary)),

    InvalidTraceIdBinary = <<0:8,0:8,0:128,1:8,1:64,2:8,1>>,
    ?assertEqual({error, invalid}, oc_trace_context_binary:decode(InvalidTraceIdBinary)),

    InvalidSpanIdBinary = <<0:8,0:8,1:128,1:8,0:64,2:8,1>>,
    ?assertEqual({error, invalid}, oc_trace_context_binary:decode(InvalidSpanIdBinary)).


decode_with_extra_junk(_Config) ->
    %% TraceId: 85409434994488837557643013731547696719
    %% SpanId: 7017280452245743464
    %% Enabled: true
    Binary = <<0,0,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,1,97,98,99,100,101,102,103,104,2,1>>,
    BinaryWithJunk = <<Binary/binary,23,4,5>>,
    {ok, Decoded} = oc_trace_context_binary:decode(BinaryWithJunk),
    {ok, Encoded} = oc_trace_context_binary:encode(Decoded),

    ?assertMatch(Binary, Encoded).

encode_decode_headers(_Config) ->
    %% TraceId: 4bf92f3577b34da6a3ce929d0e0e4736
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: true
    Header = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,
    {ok, Decoded} = oc_trace_context_headers:decode(Header),
    {ok, Encoded} = oc_trace_context_headers:encode(Decoded),
    ?assertEqual(Header, list_to_binary(Encoded)),
    ?assertEqual({ok, Decoded}, oc_trace_context_headers:decode(Encoded)),

    %% TraceId: 4bf92f3577b34da6a3ce929d0e0e4736
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: false
    DisabledHeader = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00">>,
    {ok, DisabledDecoded} = oc_trace_context_headers:decode(DisabledHeader),
    {ok, DisabledEncoded} = oc_trace_context_headers:encode(DisabledDecoded),
    ?assertEqual(DisabledHeader, list_to_binary(DisabledEncoded)),
    ?assertEqual({ok, DisabledDecoded}, oc_trace_context_headers:decode(DisabledEncoded)),
    ?assertNot(DisabledDecoded#trace_context.enabled),

    %% Decode invalid headers
    InvalidSpanIdHeader = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-00">>,
    {error, invalid} = oc_trace_context_headers:decode(InvalidSpanIdHeader),

    InvalidTraceIdHeader = <<"00-00000000000000000000000000000000-00f067aa0ba902b7-00">>,
    {error, invalid} = oc_trace_context_headers:decode(InvalidTraceIdHeader),

    InvalidBothIdsHeader = <<"00-00000000000000000000000000000000-0000000000000000-00">>,
    {error, invalid} = oc_trace_context_headers:decode(InvalidBothIdsHeader),


    %% Encode invalid trace contexts
    InvalidTC = #trace_context{trace_id = 0,
                               span_id = 0,
                               enabled = false},
    {error, invalid} = oc_trace_context_headers:encode(InvalidTC),

    InvalidTraceIdTC = #trace_context{trace_id = 85409434994488837557643013731547696719,
                                      span_id = 0,
                                      enabled = true},
    {error, invalid} = oc_trace_context_headers:encode(InvalidTraceIdTC),

    InvalidSpanIdTC = #trace_context{trace_id = 0,
                                     span_id = 7017280452245743464,
                                     enabled = false},
    {error, invalid} = oc_trace_context_headers:encode(InvalidSpanIdTC).
