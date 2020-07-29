%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_span_ctx_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

-define(W3C_HEADERS(TraceParent, TraceState), [{<<"traceparent">>, TraceParent},
                                               {<<"tracestate">>, TraceState}]).
-define(B3_HEADERS(TraceId, SpanId, Sampled), [{<<"X-B3-TraceId">>, TraceId},
                                               {<<"X-B3-SpanId">>, SpanId},
                                               {<<"X-B3-Sampled">>, Sampled}]).

all() ->
    [encode_decode, decode_with_extra_junk, w3c_encode_decode_headers,
     b3_encode_decode_headers, tracestate].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% TraceId: 85409434994488837557643013731547696719
%% SpanId: 7017280452245743464
%% Enabled: true
-define(EXAMPLE_BIN, <<0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
                       79, 1, 97, 98, 99, 100, 101, 102, 103, 104, 2, 1>>).

reencode(Binary) ->
    Decoded = oc_propagation_binary:decode(Binary),
    Encoded = oc_propagation_binary:encode(Decoded),
    Encoded.

encode_decode(_Config) ->
    Encoded = reencode(?EXAMPLE_BIN),

    ?assertMatch(?EXAMPLE_BIN, Encoded),

    InvalidBothIdsBinary = <<0:8, 0:8, 0:128, 1:8, 0:64, 2:8, 1>>,
    ?assertEqual(undefined, oc_propagation_binary:decode(InvalidBothIdsBinary)),

    InvalidTraceIdBinary = <<0:8, 0:8, 0:128, 1:8, 1:64, 2:8, 1>>,
    ?assertEqual(undefined, oc_propagation_binary:decode(InvalidTraceIdBinary)),

    InvalidSpanIdBinary = <<0:8, 0:8, 1:128, 1:8, 0:64, 2:8, 1>>,
    ?assertEqual(undefined, oc_propagation_binary:decode(InvalidSpanIdBinary)).


decode_with_extra_junk(_Config) ->
    Binary = ?EXAMPLE_BIN,
    BinaryWithJunk = <<Binary/binary, 23, 4, 5>>,
    Encoded = reencode(BinaryWithJunk),

    ?assertMatch(Binary, Encoded).

w3c_encode_decode_headers(_Config) ->
    %% TraceId: 4bf92f3577b34da6a3ce929d0e0e4736
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: true
    Header = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,
    Decoded = oc_propagation_http_tracecontext:decode(Header),
    Encoded = oc_propagation_http_tracecontext:encode(Decoded),
    ?assertEqual(Header, list_to_binary(Encoded)),
    ?assertEqual(Decoded, oc_propagation_http_tracecontext:decode(Encoded)),

    %% TraceId: 4bf92f3577b34da6a3ce929d0e0e4736
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: false
    DisabledHeader = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00">>,
    DisabledDecoded = oc_propagation_http_tracecontext:decode(DisabledHeader),
    DisabledEncoded = oc_propagation_http_tracecontext:encode(DisabledDecoded),
    ?assertEqual(DisabledHeader, list_to_binary(DisabledEncoded)),
    ?assertEqual(DisabledDecoded, oc_propagation_http_tracecontext:decode(DisabledEncoded)),
    ?assertEqual(0, DisabledDecoded#span_ctx.trace_options),

    Headers = ?W3C_HEADERS(<<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,
                           <<"congo=congosFirstPosition,rojo=rojosFirstPosition">>),
    SpanCtx = oc_propagation_http_tracecontext:from_headers(Headers),
    EncodedHeaders = oc_propagation_http_tracecontext:to_headers(SpanCtx),
    ?assertMatch(SpanCtx, oc_propagation_http_tracecontext:from_headers(EncodedHeaders)),

    %% Decode invalid headers
    ?assertEqual(undefined, oc_propagation_http_tracecontext:from_headers([])),

    InvalidSpanIdHeader = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-00">>,
    ?assertEqual(undefined, oc_propagation_http_tracecontext:decode(InvalidSpanIdHeader)),

    InvalidTraceIdHeader = <<"00-00000000000000000000000000000000-00f067aa0ba902b7-00">>,
    ?assertEqual(undefined, oc_propagation_http_tracecontext:decode(InvalidTraceIdHeader)),

    InvalidBothIdsHeader = <<"00-00000000000000000000000000000000-0000000000000000-00">>,
    ?assertEqual(undefined, oc_propagation_http_tracecontext:decode(InvalidBothIdsHeader)),

    NoCtx = undefined,
    NoCtxEncoded = oc_propagation_http_tracecontext:to_headers(NoCtx),
    ?assertEqual(NoCtx, oc_propagation_http_tracecontext:from_headers(NoCtxEncoded)),

    %% Encode invalid trace contexts
    InvalidTC = #span_ctx{trace_id = 0,
                          span_id = 0,
                          trace_options = false},
    ?assertEqual([], oc_propagation_http_tracecontext:to_headers(InvalidTC)),

    InvalidTraceIdTC = #span_ctx{trace_id = 85409434994488837557643013731547696719,
                                 span_id = 0,
                                 trace_options = true},
    ?assertEqual([], oc_propagation_http_tracecontext:to_headers(InvalidTraceIdTC)),

    InvalidSpanIdTC = #span_ctx{trace_id = 0,
                                span_id = 7017280452245743464,
                                trace_options = false},
    ?assertEqual([], oc_propagation_http_tracecontext:to_headers(InvalidSpanIdTC)).

b3_encode_decode_headers(_Config) ->
    %% TraceId: 4bf92f3577b34da6a3ce929d0e0e4736
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: true
    Headers = ?B3_HEADERS(<<"4bf92f3577b34da6a3ce929d0e0e4736">>, <<"00f067aa0ba902b7">>, <<"1">>),
    Decoded = oc_propagation_http_b3:from_headers(Headers),
    Encoded = oc_propagation_http_b3:to_headers(Decoded),
    compare_b3_headers(Encoded, Headers),
    ?assertEqual(Decoded, oc_propagation_http_b3:from_headers(Encoded)),

    %% TraceId: 4bf92f3577b34da6
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: true
    ShortTraceIDHeaders = ?B3_HEADERS(<<"4bf92f3577b34da6">>, <<"00f067aa0ba902b7">>, <<"1">>),
    ShortTraceIDDecoded = oc_propagation_http_b3:from_headers(ShortTraceIDHeaders),
    ShortTraceIDEncoded = oc_propagation_http_b3:to_headers(ShortTraceIDDecoded),
    compare_b3_headers(ShortTraceIDEncoded, ShortTraceIDHeaders),
    ?assertEqual(ShortTraceIDDecoded, oc_propagation_http_b3:from_headers(ShortTraceIDEncoded)),

    %% TraceId: 4bf92f3577b34da6a3ce929d0e0e4736
    %% SpanId: 00f067aa0ba902b7
    %% Enabled: false
    DisabledHeaders = ?B3_HEADERS(<<"4bf92f3577b34da6a3ce929d0e0e4736">>, <<"00f067aa0ba902b7">>, <<"0">>),
    DisabledDecoded = oc_propagation_http_b3:from_headers(DisabledHeaders),
    DisabledEncoded = oc_propagation_http_b3:to_headers(DisabledDecoded),
    compare_b3_headers(DisabledEncoded, DisabledHeaders),
    ?assertEqual(DisabledDecoded, oc_propagation_http_b3:from_headers(DisabledEncoded)),
    ?assertEqual(0, DisabledDecoded#span_ctx.trace_options),

    %% Decode invalid headers
    ?assertEqual(undefined, oc_propagation_http_b3:from_headers([])),

    InvalidBase16 = ?B3_HEADERS(<<"zbf92f3577b34da6a3ce929d0e0e4736">>, <<"00f067aa0ba902b7">>, <<"0">>),
    ?assertEqual(undefined, oc_propagation_http_b3:from_headers(InvalidBase16)),

    InvalidSpanIdLength = ?B3_HEADERS(<<"4bf92f3577b34da6a3ce929d0e0e4736">>, <<"00f067aa0ba90">>, <<"0">>),
    ?assertEqual(undefined, oc_propagation_http_b3:from_headers(InvalidSpanIdLength)),

    InvalidSpanIdHeader = ?B3_HEADERS(<<"4bf92f3577b34da6a3ce929d0e0e4736">>, <<"0000000000000">>, <<"1">>),
    ?assertEqual(undefined, oc_propagation_http_b3:from_headers(InvalidSpanIdHeader)),

    InvalidTraceIdHeader = ?B3_HEADERS(<<"00000000000000000000000000000000">>, <<"00f067aa0ba90">>, <<"0">>),
    ?assertEqual(undefined, oc_propagation_http_b3:from_headers(InvalidTraceIdHeader)),

    InvalidBothIdsHeader = ?B3_HEADERS(<<"00000000000000000000000000000000">>, <<"0000000000000">>, <<"0">>),
    ?assertEqual(undefined, oc_propagation_http_b3:from_headers(InvalidBothIdsHeader)),

    NoCtx = undefined,
    NoCtxEncoded = oc_propagation_http_b3:to_headers(NoCtx),
    ?assertEqual(NoCtx, oc_propagation_http_b3:from_headers(NoCtxEncoded)),

    %% Encode invalid trace contexts
    InvalidTC = #span_ctx{trace_id = 0,
                          span_id = 0,
                          trace_options = false},
    ?assertEqual([], oc_propagation_http_b3:to_headers(InvalidTC)),

    InvalidTraceIdTC = #span_ctx{trace_id = 85409434994488837557643013731547696719,
                                 span_id = 0,
                                 trace_options = true},
    ?assertEqual([], oc_propagation_http_b3:to_headers(InvalidTraceIdTC)),

    InvalidSpanIdTC = #span_ctx{trace_id = 0,
                                span_id = 7017280452245743464,
                                trace_options = false},
    ?assertEqual([], oc_propagation_http_b3:to_headers(InvalidSpanIdTC)).

compare_b3_headers(Encoded, Orig) ->
    lists:all(fun({Key, Value}) ->
                      string:equal(Value, element(2, lists:keyfind(Key, 1, Orig)))
              end, Encoded).

tracestate(_Config) ->
    %% fail on empty key
    ?assertMatch(undefined, oc_tracestate:new(undefined, [{"", "emptykey"}])),

    %% fail on too long key
    ?assertMatch(undefined, oc_tracestate:new(undefined, [{lists:flatten(lists:duplicate(257, "k")),
                                                           "value"}])),

    %% fail on too long value
    ?assertMatch(undefined, oc_tracestate:new(undefined, [{"key",
                                                           lists:flatten(lists:duplicate(257, "v"))}])),

    Entries = [{"congo", "congosFirstPosition"}, {"rojo", "rojosFirstPosition"}],
    TS0 = oc_tracestate:new(undefined, Entries),
    TS = oc_tracestate:new(TS0, [{"congo", "congosSecondPosition"}]),

    %% order matters
    ?assertMatch([{"rojo", "rojosFirstPosition"}, {"congo", "congosSecondPosition"}],
                 TS#tracestate.entries),

    %% return undefined if any entry is invalid
    ?assertMatch(undefined,
                 oc_tracestate:new(TS0, [{"congo", "congosSecondPosition"}, {"rojo", <<1234>>}])).
