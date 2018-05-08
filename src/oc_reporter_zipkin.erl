%%%------------------------------------------------------------------------
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
%% @doc Exports spans to Zipkin
%% @end
%%%------------------------------------------------------------------------


-module(oc_reporter_zipkin).

-include("opencensus.hrl").
-include("oc_logger.hrl").

-behaviour(oc_reporter).

-define(DEFAULT_ZIPKIN_ADDRESS, "http://localhost:9411/api/v2/spans").
-define(DEFAULT_LOCAL_ENDPOINT, #{<<"serviceName">> => node()}).

-export([init/1,
         report/2]).

init(Opts) ->
    Address = zipkin_address(Opts),
    LocalEndpoint = local_endpoint(Opts),
    {Address, LocalEndpoint}.

report(Spans, {Address, LocalEndpoint}) ->
    ZSpans = [zipkin_span(Span, LocalEndpoint) || Span <- Spans],

    try jsx:encode(ZSpans) of
        JSON ->
            case httpc:request(post, {Address, [], "application/json", JSON}, [], []) of
                {ok, {{_, 202, _}, _, _}} ->
                    ok;
                {ok, {{_, Code, _}, _, Message}} ->
                    ?LOG_ERROR("Zipkin: Unable to send spans, Zipkin reported an error: ~p : ~p",
                              [Code, Message]);

                {error, Reason} ->
                    ?LOG_ERROR("Zipkin: Unable to send spans, client error: ~p", [Reason])
            end
    catch
        error:Error ->
            ?LOG_ERROR("Zipkin: Can't spans encode to json: ~p", [Error])
    end.

zipkin_span(Span, LocalEndpoint) ->
    (optional_fields(Span))#{
       <<"traceId">> => iolist_to_binary(io_lib:format("~32.16.0b", [Span#span.trace_id])),
       <<"name">> => iolist_to_binary(Span#span.name),
       <<"id">> => iolist_to_binary(io_lib:format("~16.16.0b", [Span#span.span_id])),
       <<"timestamp">> => wts:to_absolute(Span#span.start_time),
       <<"duration">> => wts:duration(Span#span.start_time, Span#span.end_time),
       <<"debug">> => false, %% TODO: get from attributes?
       <<"shared">> => false, %% TODO: get from attributes?
       <<"localEndpoint">> => LocalEndpoint,
       %% <<"remoteEndpoint">> =>  %% TODO: get from attributes?
       <<"annotations">> => [],
       <<"tags">> => to_tags(Span#span.attributes) %% TODO: merge with oc_tags?
     }.

to_tag(_Name, Value) when is_function(Value) ->
    Value();
to_tag(_Name, Value) when is_list(Value) ->
    list_to_binary(Value);
to_tag(_Name, Value) ->
    Value.

to_tags(Attributes) ->
    maps:map(fun(Name, Value) ->
                     to_tag(Name, Value)
             end, Attributes).

zipkin_address(Options) ->
    proplists:get_value(address, Options, ?DEFAULT_ZIPKIN_ADDRESS).

local_endpoint(Options) ->
    proplists:get_value(local_endpoint, Options, ?DEFAULT_LOCAL_ENDPOINT).

optional_fields(Span) ->
    lists:foldl(fun(Field, Acc) ->
                        case span_field(Field, Span) of
                            undefined ->
                                Acc;
                            Value ->
                                maps:put(Field, Value, Acc)
                        end
                end, #{}, [<<"kind">>, <<"parentId">>]).

span_field(<<"parentId">>, #span{parent_span_id=undefined}) ->
    undefined;
span_field(<<"parentId">>, #span{parent_span_id=ParentId}) ->
    iolist_to_binary(io_lib:format("~16.16.0b", [ParentId]));
span_field(<<"kind">>, #span{kind=?SPAN_KIND_UNSPECIFIED}) ->
    undefined;
span_field(<<"kind">>, #span{kind=?SPAN_KIND_SERVER}) ->
    <<"SERVER">>;
span_field(<<"kind">>, #span{kind=?SPAN_KIND_CLIENT}) ->
    <<"CLIENT">>.
