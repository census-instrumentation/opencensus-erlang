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
%% Measure represents a type of metric to be tracked and recorded.
%% For example, latency, request Mb/s, and response Mb/s are measures
%% to collect from a server.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_stat_measure).

-export([new/3,
         module_name/1,
         maybe_module_name/1,
         regen_record/2,
         delete_measure/1,
         prepare_tags/1]).

-export([parse_transform/2]).

-export_types([name/0,
               description/0,
               unit/0]).

-type name() :: atom() | binary() | string().
-type description() :: binary() | string().
-type unit() :: atom().


%% @doc
%% Creates and registers a measure. If a measure with the same name
%% already exists, old measure returned.
%% @end
-spec new(name(), description(), unit()) -> oc_stat_view:measure().
new(Name, Description, Unit) ->
    oc_stat_view:register_measure(Name, Description, Unit).

-spec module_name(name()) -> module().
module_name(Name) when is_atom(Name) ->
    list_to_atom(module_name_str(Name)).

module_name_str(Name) when is_atom(Name) ->
    name_template(atom_to_list(Name));
module_name_str(Name) when is_binary(Name) ->
    name_template(binary_to_list(Name));
module_name_str(Name) when is_list(Name) ->
    name_template(binary_to_list(iolist_to_binary(Name))).

name_template(Name) ->
    lists:flatten(["$_MEASURE_", Name]).

maybe_module_name(Name) ->
    list_to_existing_atom(module_name_str(Name)).

regen_record(Name, VSs) ->
    regen_module(Name, gen_add_sample_calls(VSs), erl_parse:abstract(VSs)).

delete_measure(Name) ->
    ErrorA = erl_parse:abstract({unknown_measure, Name}),
    regen_module(Name,
                 gen_add_sample_calls([])
                 ++ [{call, 1,
                      {remote, 1, {atom, 1, erlang}, {atom, 1, error}},
                      [ErrorA]}],
                 {call, 1,
                  {remote, 1, {atom, 1, erlang}, {atom, 1, error}},
                  [ErrorA]}).

prepare_tags(Tags) when is_map(Tags) ->
    Tags;
prepare_tags(Ctx) ->
    oc_tags:from_ctx(Ctx).

regen_module(Name, RecordBody, Subs) ->
    ModuleName = module_name(Name),
    ModuleNameStr = atom_to_list(ModuleName),
    {ok, Module, Binary} =
        compile:forms(
          [{attribute, 1, file,
            {ModuleNameStr,
             1}},
           {attribute, 1, module, ModuleName},
           {attribute, 1, export,
            [{record, 2}]},
           {attribute, 1, export,
            [{subs, 0}]},
           {function, 1, record, 2,
            [{clause, 1, [{var, 1, 'ContextTags'}, {var, 1, 'Value'}], [],
              RecordBody ++ [{atom, 1, ok}]
             }]},
           {function, 1, subs, 0,
            [{clause, 1, [], [],
              [Subs]
             }]},
           {eof, 2}]),

    {module, Module} = code:load_binary(Module, ModuleNameStr, Binary).

gen_add_sample_calls([]) ->
    [{match, 1, {var, 1, '_'}, {var, 1, 'ContextTags'}},
     {match, 1, {var, 1, '_'}, {var, 1, 'Value'}}];
gen_add_sample_calls(VSs) ->
    lists:map(fun oc_stat_view:gen_add_sample/1, VSs).

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
    walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)}|Acc], Rest).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|R]) ->
    walk_body([transform_statement(H)|Acc], R).

transform_statement({call, Line,
                     {remote, _, {atom, _, oc_stat}, {atom, _, record}},
                     [Tags, {atom, _, MeasureName}, Value]}=_Stmt) ->
    measure_module_record_call(Line, Tags, MeasureName, Value);
transform_statement({call, Line,
                     {remote, _, {atom, _, oc_stat}, {atom, _, record}},
                     [Tags,  Measures0]}=_Stmt) ->

    Measures = erl_syntax:list_elements(Measures0),
    %% TODO: prepare tags first
    {block, Line,
     [measure_module_record_call(Line, Tags, MeasureName, Value)
      || {tuple, _, [{atom, _, MeasureName}, Value]} <- Measures]};
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
    Stmt.

measure_module_record_call(Line, Tags, MeasureName, Value) ->
    {call, Line,
     {remote, Line, {atom, Line, module_name(MeasureName)}, {atom, Line, record}},
     [{call, Line, {remote, Line, {atom, Line, ?MODULE}, {atom, Line, prepare_tags}}, [Tags]}, Value]}.
