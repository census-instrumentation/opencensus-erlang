%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_tags_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [encode_decode, invalid_tags].

encode_decode(_Config) ->
    Empty = #{},
    {ok, EmptyIOList} = oc_tag_context_binary:encode(Empty),
    EmptyEncodedT = iolist_to_binary(EmptyIOList),
    ?assertMatch({ok, Empty}, oc_tag_context_binary:decode(EmptyEncodedT)),

    T = #{"key-1" => "value-1",
          "key-2" => "value-2"},
    {ok, IOList} = oc_tag_context_binary:encode(T),
    EncodedT = iolist_to_binary(IOList),
    ?assertMatch({ok, T}, oc_tag_context_binary:decode(EncodedT)),
    ok.

invalid_tags(_Config) ->
    TooLongString = lists:duplicate($a, 256),
    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put(TooLongString, "fine value", #{})),
    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put("fine key", TooLongString, #{})),
    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put(TooLongString, TooLongString, #{})),

    ?assertMatch({error, {oc_tags, invalid_tag}}, oc_tags:put("invalid", "k\x7f", #{})),

    TooManyBytes = maps:from_list([{integer_to_list(X), "some value"} || X <- lists:seq(1, 8192)]),
    ?assertMatch({error, {oc_tag_context_binary, encoding_too_large}}, oc_tag_context_binary:encode(TooManyBytes)),
    ok.
