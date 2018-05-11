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
%% @doc
%% @end
%%%-----------------------------------------------------------------------
-module(oc_stat_transform).

-export([parse_transform/2]).

%% @doc
%% `oc_stat_transform' is a parse transform that can detect `oc_stat:record' calls
%% with constant measure names and generate remote measure module call from that.
%% At the run-time this means we don't have to do a lookup for the module name and
%% if measure doesn't exist, `{unknown_measure, Name}' error will be thrown.
%% @end
parse_transform(Forms, _Options) ->
    HiForms = lists:map(fun walk_ast/1, Forms),
    HiForms.

walk_ast({function, Line, Name, Args, Clauses}) ->
    {function, Line, Name, Args, walk_clauses([], Clauses)};

walk_ast(Form) ->
    Form.

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|Rest]) ->
    reset_gensym(),
    walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)}|Acc], Rest).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|R]) ->
    walk_body([transform_statement(H)|Acc], R).

transform_statement({call, Line,
                     {remote, _, {atom, _, oc_stat}, {atom, _, record}},
                     [Tags, {cons, _, _, _} = Measurements]}=_Stmt) ->
    gen_record_calls(Line, Tags, erl_syntax:list_elements(Measurements));
transform_statement({call, Line,
                     {remote, _, {atom, _, oc_stat}, {atom, _, record}},
                     [Tags, {MType, _, _}=Measurement, Value]}=_Stmt)
  when is_atom(MType) orelse is_binary(MType) orelse is_list(MType) ->
    gen_record_calls(Line, Tags, [{tuple, Line, [Measurement, Value]}]);
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
    Stmt.

%% =============================================================================
%% private
%% =============================================================================

gen_record_calls(Line, Tags, Measurements) ->
    CTags = {var, Line, gensym("CTags")},
    GTags = {var, Line, gensym("GTags")},
    {block, Line,
     [{match, Line, CTags, Tags},
      {match, Line, GTags, gen_prepare_tags(Line, CTags)}]
     ++
         [measure_module_record_call(Line, MeasureName, GTags, Value)
          || {tuple, _, [{_, _, MeasureName}, Value]} <- Measurements]}.

measure_module_record_call(Line, MeasureName, GTags, Value) ->
    {'try', Line,
     [{call, Line,
       {remote, Line, {atom, Line, oc_stat_measure:module_name(MeasureName)}, {atom, Line, record}},
       [GTags, Value]}],
     [{clause, Line, [{var, Line, '_'}], [], [{atom, 279, 'ok'}]}],
     [{clause, Line,
       [{tuple, Line,
         [{atom, Line, error}, {atom, Line, undef}, {var, Line, '_'}]}],
       [],
       [{call, Line,
         {remote, Line, {atom, Line, erlang}, {atom, Line, error}},
         [{tuple, Line,
           [{atom, Line, unknown_measure}, erl_parse:abstract(MeasureName)]}]}]}],
     []}.

gen_prepare_tags(Line, CTags) ->
    {'case',  Line, CTags,
     [{clause, Line,
       [{var, Line, '_'}],
       [[{call, Line, {atom, Line, is_map}, [CTags]}]],
       [CTags]},
      {clause, Line,
       [{var, Line, '_'}],
       [],
       [{call, Line,
         {remote, Line, {atom, Line, oc_tags}, {atom, Line, from_ctx}},
         [CTags]}]}]}.

gensym(Name) ->
    put(oc_gensym_counter, get(oc_gensym_counter) + 1),
    list_to_atom(
      lists:flatten(
        io_lib:format("$oc_gen_~s_~B$", [Name, get(oc_gensym_counter)]))).

reset_gensym() ->
    put(oc_gensym_counter, 0).

