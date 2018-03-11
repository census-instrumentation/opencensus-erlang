%%%-------------------------------------------------------------------------
%% Copyright 2018, OpenCensus Authors
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
%% @doc Functions to support sending tags over http as an http header
%% @end
%%%-------------------------------------------------------------------------
-module(oc_tag_ctx_header).

-export([field_name/0,
         encode/1,
         decode/1]).

-include("opencensus.hrl").

field_name() ->
    <<"Trace-State">>.

-spec encode(oc_tags:tags()) -> maybe(iodata()).
encode(Tags) when map_size(Tags) =:= 0 ->
    <<"0">>;
encode(Tags) ->
    {ok, IOList} = oc_tag_ctx_binary:encode(Tags),
    base64:encode_to_string(iolist_to_binary(IOList)).

-spec decode(iodata()) -> maybe(oc_tags:tags()).
decode("0") ->
    #{};
decode(<<"0">>) ->
    #{};
decode(Thing) ->
    try base64:decode(iolist_to_binary(Thing)) of
        Data ->
            {ok, Tags} = oc_tag_ctx_binary:decode(Data),
            Tags
    catch
        _:_ ->
            undefined
    end.
