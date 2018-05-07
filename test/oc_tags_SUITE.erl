%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_tags_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     basic,
     ctx,
     encode_decode,
     encode_decode_headers,
     invalid_tags
    ].

encode_decode(_Config) ->
    Empty = #{},
    {ok, EmptyIOList} = oc_tag_ctx_binary:encode(Empty),
    EmptyEncodedT = iolist_to_binary(EmptyIOList),
    ?assertMatch({ok, Empty}, oc_tag_ctx_binary:decode(EmptyEncodedT)),

    T = #{"key-1" => "value-1",
          "key-2" => "value-2"},
    {ok, IOList} = oc_tag_ctx_binary:encode(T),
    EncodedT = iolist_to_binary(IOList),
    ?assertMatch({ok, T}, oc_tag_ctx_binary:decode(EncodedT)),
    ok.

encode_decode_headers(_Config) ->
    Empty = #{},
    EmptyHeader = oc_tag_ctx_header:encode(Empty),
    ?assertMatch(<<"0">>, EmptyHeader),
    ?assertMatch({ok, Empty}, oc_tag_ctx_header:decode(EmptyHeader)),

    T = #{'key-1' => "value-1",
          "key-2" => "value-2"},
    Header = oc_tag_ctx_header:encode(T),
    ?assertMatch("AAAFa2V5LTEHdmFsdWUtMQAFa2V5LTIHdmFsdWUtMg==", Header),
    ?assertMatch({ok, #{"key-1" := "value-1",
                        "key-2" := "value-2"}}, oc_tag_ctx_header:decode(Header)),
    ok.

invalid_tags(_Config) ->
    TooLongString = lists:duplicate($a, 256),
    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put(TooLongString, "fine value", #{})),
    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put("fine key", TooLongString, #{})),
    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put(TooLongString, TooLongString, #{})),

    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put("invalid", "k\x7f", #{})),

    TooManyBytes = maps:from_list([{integer_to_list(X), "some value"} || X <- lists:seq(1, 8192)]),
    ?assertMatch({error, {oc_tag_ctx_binary, encoding_too_large}}, oc_tag_ctx_binary:encode(TooManyBytes)),
    ok.

basic(_Config) ->
    Tags = oc_tags:new(#{'key-1' => "value-1",
                         <<"key-2">> => "value-2"}),
    {ok, Tags1} = oc_tags:put("key-3", <<"value-3">>, Tags),
    {ok, Tags2} = oc_tags:put("key-4", "value-4", Tags1),


    ?assertMatch(#{'key-1' := "value-1",
                   <<"key-2">> := "value-2",
                   "key-3" := <<"value-3">>,
                   "key-4" := "value-4"}, oc_tags:to_map(Tags2)).

ctx(_Config) ->
    Ctx = oc_tags:new_ctx(ctx:new(), #{"key-1" => "value-1",
                                       "key-2" => "value-2"}),
    Tags = oc_tags:from_ctx(Ctx),

    ?assertMatch(#{"key-1" := "value-1",
                   "key-2" := "value-2"}, oc_tags:to_map(Tags)).
