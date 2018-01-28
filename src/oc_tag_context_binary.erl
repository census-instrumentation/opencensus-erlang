-module(oc_tag_context_binary).

-export([encode/1,
         decode/1,

         format_error/1]).

-define(VERSION, 0).
-define(TAG_CONTEXT_FIELD_ID, 0).
-define(SIZE_LIMIT, 8192). %% bytes

-spec encode(#{}) -> {ok, iolist()} | {error, any()}.
encode(TagContext) ->
    try maps:fold(fun(Key, Value, {Size, Acc}) when Size < ?SIZE_LIMIT ->
                          KeySize = length(Key),
                          ValueSize = length(Value),
                          EncodedKeySize = encode_varint(KeySize),
                          EncodedValueSize = encode_varint(ValueSize),
                          {Size+KeySize+ValueSize+size(EncodedKeySize)+size(EncodedValueSize),
                            [Acc | [<<?TAG_CONTEXT_FIELD_ID:8/integer>>, EncodedKeySize, Key, EncodedValueSize, Value]]};
                     (_, _, _)->
                          throw({?MODULE, encoding_too_large})
                  end, {0, [<<?VERSION:8/integer>>]}, TagContext) of
        {_Size, IOList} ->
            {ok, IOList}
    catch
        throw:Reason ->
            {error, Reason}
    end.

-spec decode(binary()) -> {ok, #{}} | {error, any()}.
decode(<<?VERSION:8/integer, TagContext/binary>>) ->
    case size(TagContext) of
        Size when Size =< ?SIZE_LIMIT ->
            decode(TagContext, #{});
        _ ->
            {error, {?MODULE, decoding_too_large}}
    end;
decode(<<V:8/integer, _/binary>>) ->
    {error, {?MODULE, {unsupported_version, V}}}.

decode(<<>>, Map) ->
    {ok, Map};
decode(<<?TAG_CONTEXT_FIELD_ID:8/integer, Rest0/binary>>, Map) ->
    {KeySize, Rest1} = decode_varint(Rest0),
    <<Key:KeySize/binary, Rest2/binary>> = Rest1,
    {ValueSize, Rest3} = decode_varint(Rest2),
    <<Value:ValueSize/binary, Rest4/binary>> = Rest3,
    Key1 = unicode:characters_to_list(Key, latin1),
    Value1 = unicode:characters_to_list(Value, latin1),
    case oc_tags:verify_key(Key1) andalso oc_tags:verify_value(Value1) of
        true ->
            decode(Rest4, Map#{Key1 => Value1});
        false ->
            {error, {oc_tags, invalid_tag}}
    end.

format_error(encoding_too_large) ->
    io_lib:format("invalid tag context: size greater than the limit of ~p bytes", [?SIZE_LIMIT]);
format_error(decoding_too_large) ->
    io_lib:format("invalid tag context: size greater than the limit of ~p bytes", [?SIZE_LIMIT]);
format_error({unsupported_version, V}) ->
    io_lib:format("unsupported version of encoded tag context: ~p is greater than suported version ~p", [V, ?VERSION]).


%% encode/decode of varints shamelessly stolen from https://github.com/whatyouhide/small_ints

-spec encode_varint(integer()) -> binary().
encode_varint(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I>>;
encode_varint(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varint(I bsr 7))/binary>>;
encode_varint(I) ->
    erlang:error({badarg, I}).

-spec decode_varint(binary()) -> {non_neg_integer(), binary()}.
decode_varint(Bin) ->
    decode_varint(Bin, 0, 0).

decode_varint(<<1:1, Number:7, Rest/binary>>, Position, Acc) ->
    decode_varint(Rest, Position + 7, (Number bsl Position) + Acc);
decode_varint(<<0:1, Number:7, Rest/binary>>, Position, Acc) ->
    {(Number bsl Position) + Acc, Rest}.
