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

%% user api
-export([new/3,
         exists/1]).

%% codegen
-export([measure_module/1,
         module_name/1,
         maybe_module_name/1,
         regen_record/2,
         delete_measure/1]).

%% unsafe api, needs snychronization
-export([register_/1,
         add_subscription_/2,
         remove_subscription_/2,
         terminate_/0]).

-export(['__init_backend__'/0]).

-export([parse_transform/2]).

-export_types([name/0,
               description/0,
               unit/0,
               measure/0]).

-record(measure, {name        :: name(),
                  module      :: module(),
                  description :: description(),
                  unit        :: unit()}).

-type name()        :: atom() | binary() | string().
-type description() :: binary() | string().
-type unit()        :: atom().
-type measure()     :: #measure{}.

-define(MEASURES_TABLE, ?MODULE).

%% @doc
%% Creates and registers a measure. If a measure with the same name
%% already exists, old measure returned.
%% @end
-spec new(name(), description(), unit()) -> oc_stat_view:measure().
new(Name, Description, Unit) ->
    gen_server:call(oc_stat, {measure_register,
                              #measure{name=Name,
                                       module=oc_stat_measure:module_name(Name),
                                       description=Description,
                                       unit=Unit}}).
%% @doc
%% Returns a measure with the `Name' or `false'..
%% @end
-spec exists(name()) -> measure() | false.
exists(Name) ->
    case ets:lookup(?MEASURES_TABLE, Name) of
        [Measure] ->
            Measure;
        _ -> false
    end.

%% =============================================================================
%% internal
%% =============================================================================

%% @private
register_(#measure{name=Name}=Measure) ->
    case exists(Name) of
        false ->
            insert_measure_(Measure);
        OldMeasure ->
            OldMeasure
    end.

%% @private
insert_measure_(#measure{module=Module}=Measure) ->
    ets:insert(?MEASURES_TABLE, Measure),
    regen_record(Module, []),
    Measure.

%% @private
add_subscription_(Name, VS) ->
    case exists(Name) of
        false ->
            {error, {unknown_measure, Name}};
        #measure{module=Module} ->
            Subs = Module:subs(),
            regen_record(Module, [VS | Subs]),
            ok
    end.

%% @private
remove_subscription_(Name, VS) ->
    case exists(Name) of
        false ->
            ok;
        #measure{module=Module} ->
            Subs = Module:subs(),
            regen_record(Module, lists:delete(VS, Subs)),
            ok
    end.

%% @private
terminate_() ->
    [delete_measure(M) || M <- ets:tab2list(?MEASURES_TABLE)].

%% @private
'__init_backend__'() ->
    ?MEASURES_TABLE = ets:new(?MEASURES_TABLE, [set, named_table, public, {keypos, 2}, {read_concurrency, true}]),
    ok.

%% =============================================================================
%% codegen
%% =============================================================================

%% @private
measure_module(Name) ->
    case ets:lookup(?MEASURES_TABLE, Name) of
        [#measure{module=Module}] ->
            Module;
        _ -> erlang:error({unknown_measure, Name})
    end.

%% @private
-spec module_name(name()) -> module().
module_name(Name) ->
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

regen_record(ModuleName, VSs) ->
    regen_module(ModuleName, gen_add_sample_calls(VSs), erl_parse:abstract(VSs)).

delete_measure(#measure{name=Name, module=Module}) ->
    ErrorA = erl_parse:abstract({unknown_measure, Name}),
    regen_module(Module,
                 gen_add_sample_calls([])
                 ++ [{call, 1,
                      {remote, 1, {atom, 1, erlang}, {atom, 1, error}},
                      [ErrorA]}],
                 {call, 1,
                  {remote, 1, {atom, 1, erlang}, {atom, 1, error}},
                  [ErrorA]}).

regen_module(ModuleName, RecordBody, Subs) ->
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
    lists:map(fun oc_stat_view:gen_add_sample_/1, VSs).

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
       {remote, Line, {atom, Line, module_name(MeasureName)}, {atom, Line, record}},
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
