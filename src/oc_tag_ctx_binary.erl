%%%------------------------------------------------------------------------
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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(oc_tag_ctx_binary).

-export([encode/1,
         decode/1,

         format_error/1]).

-define(VERSION, 0).
-define(TAG_CONTEXT_FIELD_ID, 0).
-define(SIZE_LIMIT, 8192). %% bytes

-spec encode(#{}) -> {ok, iolist()} | {error, any()}.
encode(TagContext) ->
    try maps:fold(fun(Key0, Value0, {Size, Acc}) when Size < ?SIZE_LIMIT ->
                          Key = normalize_proto_string(Key0),
                          KeySize = length(Key),
                          Value = normalize_proto_string(Value0),
                          ValueSize = length(Value),
                          EncodedKeySize = encode_varint(KeySize),
                          EncodedValueSize = encode_varint(ValueSize),
                          {Size+KeySize+ValueSize+size(EncodedKeySize)+size(EncodedValueSize),
                           [Acc | [<<?TAG_CONTEXT_FIELD_ID:8/integer>>, EncodedKeySize,
                                   Key, EncodedValueSize, Value]]};
                     (_, _, _)->
                          throw({?MODULE, encoding_too_large})
                  end, {0, [<<?VERSION:8/integer>>]}, TagContext) of
        {_Size, IOList} ->
            {ok, IOList}
    catch
        throw:Reason ->
            {error, Reason}
    end.

normalize_proto_string(PS) when is_atom(PS) ->
    atom_to_list(PS);
normalize_proto_string(PS) when is_binary(PS) ->
    binary_to_list(PS);
normalize_proto_string(PS) ->
    PS.

-spec decode(binary()) -> {ok, oc_tags:tags()} | {error, any()}.
decode(<<>>) ->
    {ok, oc_tags:new()};
decode(<<?VERSION:8/integer, TagContext/binary>>) ->
    case size(TagContext) of
        Size when Size =< ?SIZE_LIMIT ->
            decode(TagContext, {ok, oc_tags:new()});
        _ ->
            %% no partial failures
            {error, {?MODULE, decoding_too_large}}
    end;
decode(<<V:8/integer, _/binary>>) ->
    {error, {?MODULE, {unsupported_version, V}}}.

-spec decode(binary(), {ok, oc_tags:tags()} | {error, any()}) -> {ok, oc_tags:tags()} | {error, any()}.
decode(<<>>, TagsOrError) ->
    TagsOrError;
decode(_, Error={error, _}) ->
    Error;
decode(<<?TAG_CONTEXT_FIELD_ID:8/integer, Rest0/binary>>, {ok, Tags}) ->
    {KeySize, Rest1} = decode_varint(Rest0),
    <<Key:KeySize/binary, Rest2/binary>> = Rest1,
    {ValueSize, Rest3} = decode_varint(Rest2),
    <<Value:ValueSize/binary, Rest4/binary>> = Rest3,
    Key1 = unicode:characters_to_list(Key, latin1),
    Value1 = unicode:characters_to_list(Value, latin1),

    %% add tag and continue processing if no failure
    decode(Rest4, oc_tags:put(Key1, Value1, Tags)).

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
