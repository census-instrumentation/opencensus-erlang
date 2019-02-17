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
%% @doc `oc_tracestate' implements support for the Tracestate header of the
%% W3C TraceContext propagation format.
%% @end
%%%-------------------------------------------------------------------------
-module(oc_tracestate).

-export([new/2,
         add/2,
         is_valid/1,
         are_valid/1,
         format_error/1]).

-include("oc_logger.hrl").
-include("opencensus.hrl").

-define(MAX_PAIRS, 32).
-define(KEY_WITHOUT_VENDOR, "[a-z][_0-9a-z\-\*\/]{0,255}").
-define(KEY_WITH_VENDOR, "[a-z][_0-9a-z\-\*\/]{0,240}@[a-z][_0-9a-z\-\*\/]{0,13}").
-define(KEY_FORMAT, "^((" ++ ?KEY_WITHOUT_VENDOR ++ ")|(" ++ ?KEY_WITH_VENDOR ++ "))$").
-define(VALUE_FORMAT, "^([\x20-\x2b\x2d-\x3c\x3e-\x7e]{0,255}[\x21-\x2b\x2d-\x3c\x3e-\x7e])$").

-define(KEY_RE_MP, element(2, re:compile(?KEY_FORMAT))).
-define(VALUE_RE_MP, element(2, re:compile(?VALUE_FORMAT))).

-type key() :: unicode:latin1_chardata().
-type value() :: unicode:latin1_chardata().
-type entry() :: {key(), value()}.
-type entries() :: [entry()].

%% create tracestate from parent
-spec new(opencensus:tracestate(), list()) -> maybe(opencensus:tracestate()).
new(undefined, []) ->
    undefined;
new(undefined, Entries) ->
    case are_valid(Entries) of
        true ->
            #tracestate{entries=Entries};
        {false, Error} ->
            ?LOG_INFO(format_error(Error)),
            undefined
    end;
new(#tracestate{entries=ParentEntries}, Entries) ->
    case are_valid(Entries) of
        true ->
            case add(#tracestate{entries=ParentEntries}, Entries) of
                {error, Reason} ->
                    ?LOG_INFO(format_error(Reason)),
                    undefined;
                Tracestate ->
                    Tracestate
            end;
        {false, Error} ->
            ?LOG_INFO(format_error(Error)),
            undefined
    end.

-spec remove(key(), entries()) -> entries().
remove(Key, Entries) ->
    lists:keydelete(Key, 1, Entries).

-spec add(maybe(opencensus:tracestate()), entries()) -> opencensus:tracestate() | {error, term()}.
add(undefined, Entries) ->
    add(#tracestate{entries=[]}, Entries);
add(Tracestate=#tracestate{entries=CurrentEntries0}, Entries) ->
    case are_valid(Entries) of
        true ->
            CurrentEntries = lists:foldl(fun({Key, _Value}, Acc) ->
                                                 remove(Key, Acc)
                                         end, CurrentEntries0, Entries),
            CurrentLen = length(CurrentEntries),
            EntriesLen = length(Entries),
            case CurrentLen + EntriesLen > ?MAX_PAIRS of
                true ->
                    {error, {exceeds_max, CurrentLen, EntriesLen}};
                false ->
                    Tracestate#tracestate{entries=CurrentEntries ++ Entries}
            end;
        {false, _} ->
            Tracestate
    end.

-spec are_valid(entries()) -> true | {false, {invalid_entry, entry()}}.
are_valid([]) ->
    true;
are_valid([Entry | Rest]) ->
    case is_valid(Entry) of
        true ->
            are_valid(Rest);
        false ->
            {false, {invalid_entry, Entry}}
    end.

-spec is_valid(entry()) -> boolean().
is_valid({Key, Value}) ->
    re:run(Key, ?KEY_RE_MP, [{capture, none}]) =/= nomatch
        andalso re:run(Value, ?VALUE_RE_MP, [{capture, none}]) =/= nomatch.

-spec format_error(term()) -> string().
format_error({exceeds_max, CurrentLen, EntriesLen}) ->
    io_lib:format("adding ~b key-value pairs to current ~b pairs exceeds the limit of ~b",
                  [EntriesLen, CurrentLen, ?MAX_PAIRS]);
format_error({invalid_entry, Entry}) ->
    io_lib:format("invalid tracestate entry ~p", [Entry]).
