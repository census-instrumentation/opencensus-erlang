-module(oc_tags).

-export([new/0,
         put/3,

         verify_key/1,
         verify_value/1,

         format_error/1]).

-export_types([key/0,
               value/0]).

-type key() :: unicode:latin1_charlist().
-type value() :: unicode:latin1_charlist().
-type tags() :: #{key() => value()}.

-spec new() -> tags().
new() ->
    #{}.

-spec put(key(), value(), tags()) -> tags().
put(Key, Value, Tags) ->
    case verify_key(Key) andalso verify_value(Value) of
        true ->
            maps:put(Key, Value, Tags);
        false ->
            {error, {?MODULE, invalid_tag}}
    end.

verify_key(Key) ->
    KeyLength = erlang:length(Key),
    KeyLength > 0 andalso KeyLength < 256 andalso
        io_lib:printable_latin1_list(Key).

verify_value(Value) ->
    erlang:length(Value) < 256 andalso io_lib:printable_latin1_list(Value).

format_error(invalid_tag) ->
    "tag key and value value must be ascii strings less than 256 characters".
