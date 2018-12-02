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
%%
%% Measure is a generic interface for recording values in aggregations
%% via subscribed views.
%% When recording a value, we have to obtain the list of all subscribed views
%% and call respective aggregations. We use code generation to optimize this.
%% When a view subscribed or unsubscribed we regenerate unrolled loop in a
%% special module (one for each measure). Module names generated from measurement
%% names (1-to-1). If we know a measure name at the compile time, we can eliminate
%% the module name lookup and inject remote call directly, replacing `oc_stat:record'
%% with `<GENERATED_MEASURE_MODULE>:record'.
%% For that {parse_transform, oc_stat_measure} option must be used.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_stat_measure).

%% user api
-export([new/3,
         exists/1,
         unit/1]).

%% codegen
-export([measure_module/1,
         record_module/1,
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

-export_types([name/0,
               description/0,
               unit/0,
               measure/0]).

-record(measure, {name          :: name(),
                  module        :: module(),
                  record_module :: module(),
                  description   :: description(),
                  unit          :: unit()}).

-type name()        :: atom() | binary() | string().
-type description() :: binary() | string().
-type unit()        :: atom().
-type measure()     :: #measure{}.

-define(MEASURES_TABLE, ?MODULE).

-dialyzer({nowarn_function, regen_module/3}).
-dialyzer({nowarn_function, insert_measure_/1}).
-dialyzer({nowarn_function, regen_record/2}).
-dialyzer({nowarn_function, delete_measure/1}).

%% @doc
%% Creates and registers a measure. If a measure with the same name
%% already exists, old measure returned.
%% @end
-spec new(name(), description(), unit()) -> oc_stat_view:measure().
new(Name, Description, Unit) ->
    RecordModule = application:get_env(opencensus, stat_record_module, oc_stat_measure:module_name(Name)),
    gen_server:call(oc_stat, {measure_register,
                              #measure{name=Name,
                                       module=oc_stat_measure:module_name(Name),
                                       record_module=RecordModule,
                                       description=Description,
                                       unit=Unit}}).
%% @doc
%% Returns a measure with the `Name' or `false'..
%% @end
-spec exists(name() | measure()) -> measure() | false.
exists(#measure{name=Name}) ->
    exists(Name);
exists(Name) ->
    case ets:lookup(?MEASURES_TABLE, Name) of
        [Measure] ->
            Measure;
        _ -> false
    end.

-spec unit(measure()) -> unit().
unit(#measure{unit=Unit}) ->
    Unit.

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
add_subscription_(Measure, VS) ->
    case exists(Measure) of
        false ->
            {error, {unknown_measure, Measure}};
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

record_module(Name) ->
    case ets:lookup(?MEASURES_TABLE, Name) of
        [#measure{record_module=Module}] ->
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

%% @private
maybe_module_name(Name) ->
    list_to_existing_atom(module_name_str(Name)).

%% @private
regen_record(ModuleName, VSs) ->
    regen_module(ModuleName, gen_add_sample_calls(VSs), erl_parse:abstract(VSs)).

%% @private
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

%% @private
regen_module(ModuleName, RecordBody, Subs) ->
    ModuleNameStr = atom_to_list(ModuleName),
    {ok, Module, Binary} =
        compile:forms(
          [{attribute, 1, file,
            {ModuleNameStr,
             1}},
           {attribute, 1, module, ModuleName},
           {attribute, 1, export,
            [{record, 3}]},
           {attribute, 1, export,
            [{subs, 0}]},
           {function, 1, record, 3,
            [{clause, 1, [{var, 1, '_MeasureName'}, {var, 1, 'ContextTags'}, {var, 1, 'Value'}], [],
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
