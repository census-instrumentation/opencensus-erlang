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
-module(oc_tags).

-export([new/0,
         new/1,
         new_ctx/2,
         from_ctx/1,
         to_map/1,
         put/3,
         verify_key/1,
         verify_value/1,
         format_error/1]).

-export_types([key/0,
               value/0,
               tags/0]).

-include("opencensus.hrl").

-type key()   :: atom() | binary() | unicode:latin1_charlist().
-type value() :: binary() | unicode:latin1_charlist().
-type tags()  :: #{key() => value()}.

-spec new() -> tags().
new() ->
    #{}.

-spec new(maps:map()) -> tags().
new(Map) ->
    maps:fold(fun(K, V, Acc) ->
                      case put(K, V, Acc) of
                          {ok, Acc1} ->
                              Acc1;
                          {error, _} ->
                              Acc
                      end
              end, #{}, Map).

-spec new_ctx(ctx:t(), tags()) -> ctx:t().
new_ctx(Ctx, Tags) ->
    ctx:with_value(Ctx, ?TAG_CTX, Tags).

-spec from_ctx(ctx:t()) -> tags().
from_ctx(Ctx) ->
    ctx:get(Ctx, ?TAG_CTX, #{}).

-spec to_map(tags()) -> maps:map().
to_map(Tags) ->
    Tags.

-spec put(key(), value(), tags()) -> {ok, tags()} | {error, term()}.
put(Key, Value, Tags) ->
    case verify_key(Key) andalso verify_value(Value) of
        true ->
            {ok, maps:put(Key, Value, Tags)};
        false ->
            {error, {?MODULE, invalid_tag}}
    end.

verify_key(Key) when is_atom(Key) ->
  verify_key(atom_to_list(Key));
verify_key(Key) when is_binary(Key) ->
  verify_key(binary_to_list(Key));
verify_key(Key) ->
    KeyLength = erlang:length(Key),
    KeyLength > 0 andalso KeyLength < 256 andalso
        io_lib:printable_latin1_list(Key).

verify_value(Value) when is_binary(Value) ->
  verify_value(binary_to_list(Value));
verify_value(Value) ->
    erlang:length(Value) < 256 andalso io_lib:printable_latin1_list(Value).

format_error(invalid_tag) ->
    "tag key and value value must be ascii strings less than 256 characters".
