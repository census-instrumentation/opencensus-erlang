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
%% View allows users to aggregate the recorded Measurements.
%% Views need to be passed to the subscribe function to be before data will be
%% collected and sent to exporters.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_stat_view).

-export([new/1,
         new/5,
         new/6,
         register/1,
         register/5,
         register/6,
         deregister/1,
         is_registered/1,
         subscribe/1,
         subscribe/5,
         subscribe/6,
         unsubscribe/1,
         is_subscribed/1,
         export/1]).

%% -export([preload/1]).

%% unsafe api, needs snychronization
-export([register_/1,
         register_subscribe_/1,
         deregister_/1,
         subscribe_/1,
         unsubscribe_/1]).

-export([gen_add_sample_/1,
         tag_values_/2,
         all_subscribed_/0]).

-export_types([name/0,
               description/0,
               view/0,
               view_data/0,
               measure/0]).

-export(['__init_backend__'/0]).

-include("opencensus.hrl").

%% enable ets:fun2ms parse transform
-include_lib("stdlib/include/ms_transform.hrl").

-dialyzer({nowarn_function, subscribe_/1}).
-dialyzer({nowarn_function, mark_view_as_subscribed_/1}).

-define(VIEWS_TABLE, ?MODULE).
-define(MEASURES_TABLE, oc_stat_view_subs).

-record(view, {name                        :: name() | '_',
               measure                     :: oc_stat_measure:measure()
                                            | measure_name() | '_',
               unit                        :: oc_stat_measure:unit(),
               subscribed          = false :: boolean(),
               description         = ""    :: description() | '_',
               ctags               = #{}   :: oc_tags:tags() | '_',
               tags                = []    :: [oc_tags:key()] | '_',
               aggregation                 :: aggregation() | '_',
               aggregation_options = []    :: aggregation_options() | '_'}).

-record(v_s, {name                :: name(),
              tags                :: [oc_tags:key()],
              aggregation         :: aggregation(),
              aggregation_options :: aggregation_options()}).

-type name()        :: atom() | binary() | string().
-type description() :: binary() | string().
-type view_data()   :: #{name        := name(),
                         description := description(),
                         ctags       := oc_tags:tags(),
                         tags        := [oc_tags:key()],
                         data        := oc_stat_aggregation:data()}.
-type view()        :: #view{}.
-type v_s()         :: #v_s{}.

%% @doc
%% Creates a View from a map.
%% @end
new(Map) when is_map(Map) ->
    new(maps:get(name, Map), maps:get(measure, Map), maps:get(unit, Map, undefined),
        maps:get(description, Map), maps:get(tags, Map, []), maps:get(aggregation, Map)).

%% @doc
%% Creates a View. This view needs to be registered and subscribed to a measure
%% in order to start aggregating data.
%% @end
new(Name, Measure, Description, Tags, Aggregation) ->
    new(Name, Measure, undefined, Description, Tags, Aggregation).

new(Name, Measure, Unit, Description, Tags, Aggregation) ->
    {CTags, Keys} = normalize_tags(Tags),
    {AggregationModule, AggregationOptions} = normalize_aggregation(Aggregation),
    #view{name=Name,
          measure=Measure,
          unit=Unit,
          description=Description,
          ctags=CTags,
          tags=Keys,
          aggregation=AggregationModule,
          aggregation_options=AggregationOptions}.


%% @doc
%% Registers the view created from arguments.
%%
%% Returns `{error, {unknown_measure, Measure}}' if `Measure' is unknown.<br/>
%% Returns `{error, {view_already_exists, Name}}' if a
%% view with `Name' already registered.
%% @end
register(Name, Measure, Description, Tags, Aggregation) ->
    register(new(Name, Measure, undefined, Description, Tags, Aggregation)).

register(Name, Measure, Unit, Description, Tags, Aggregation) ->
    register(new(Name, Measure, Unit, Description, Tags, Aggregation)).

%% @doc
%% Registers the view. Aggregation initialized with AggregationOptions.
%%
%% Returns `{error, {unknown_measure, Measure}}' if `Measure' is unknown.<br/>
%% Returns `{error, {view_already_exists, Name}}' if a
%% view with `Name' already registered.
%% @end
-spec register(view()) -> {ok, view()} | {error, any()}.
register(#view{}=View) ->
    gen_server:call(?STAT_SERVER, {view_register, View}).

%% @doc
%% Deregisters the view. If subscribed, unsubscribes and therefore
%% existing aggregation data cleared.
%%
%% Returns `{error, {unknown_view, Name}}' if the view is unknown.
%% @end
-spec deregister(name() | view()) -> ok.
deregister(#view{name=Name}) ->
    deregister(Name);
deregister(Name) ->
    gen_server:call(?STAT_SERVER, {view_deregister, Name}).

%% @doc
%% Returns true if the view is registered.
%% @end
-spec is_registered(name() | view()) -> boolean().
is_registered(#view{name=Name}) ->
    is_registered(Name);
is_registered(Name) ->
    case view_by_name(Name) of
        unknown ->
            false;
        _ ->
            true
    end.

%% @doc
%% A shortcut: Creates, Registers, and Subscribes a view in one call.
%%
%% Returns `{error, {unknown_measure, Measure}}' if `Measure' is unknown.
%% @end
subscribe(Name, Measure, Description, Tags, Aggregation) ->
    subscribe(Name, Measure, undefined, Description, Tags, Aggregation).

subscribe(Name, Measure, Unit, Description, Tags, Aggregation) ->
    View = new(Name, Measure, Unit, Description, Tags, Aggregation),
    gen_server:call(?STAT_SERVER, {view_register_subscribe, View}).

%% @doc
%% Subscribe the View, When subscribed, a view can aggregate measure data and export it.
%%
%% Returns `{error, {unknown_view, Name}}' if `Name' view is unknown.
%% @end
-spec subscribe(name() | view() | map()) -> {ok, view()} | {error, any()}.
subscribe(Map) when is_map(Map) ->
    View = new(Map),
    gen_server:call(?STAT_SERVER, {view_register_subscribe, View});
subscribe(#view{name=Name}) ->
    subscribe(Name);
subscribe(Name) ->
    gen_server:call(?STAT_SERVER, {view_subscribe, Name}).

%% @doc
%% Unsubscribes the View. When unsubscribed a view no longer aggregates measure data
%% and exports it. Also all existing aggregation data cleared.
%%
%% Returns `{error, {unknown_view, Name}}' if `Name' view is unknown.
%% @end
-spec unsubscribe(name() | view()) -> ok.
unsubscribe(#view{name=Name}) ->
    unsubscribe(Name);
unsubscribe(Name) ->
    gen_server:call(?STAT_SERVER, {view_unsubscribe, Name}).

%% @doc
%% Returns true if the view is exporting data by subscription.
%% @end
-spec is_subscribed(name() | view()) -> boolean().
is_subscribed(#view{name=Name}) ->
    is_subscribed(Name);
is_subscribed(Name) ->
    case view_by_name(Name) of
        {ok, #view{subscribed=Subscribed}} ->
            Subscribed;
        _ ->
            false
    end.

%% %% @doc
%% %% Loads and subscribes views from the `List' in one shot.
%% %% Usually used for loading views from configuration on app start.
%% %% @end
%% preload(List) ->
%%     [begin
%%          NV = new(V),
%%          case register_(NV) of
%%              {ok, RV} ->
%%                  subscribe_(RV);
%%              {error, Error} ->
%%                  %% TODO: should it crash?
%%                  ?LOG_INFO("Unable to preload view ~p. Error is ~p", [NV#view.name, Error])
%%          end
%%      end || V <- List].

%% @doc
%% Returns a snapshot of the View's data.
%% @end
-spec export(view()) -> view_data().
export(#view{name=Name, description=Description,
             unit=VUnit, measure=Measure,
             ctags=CTags, tags=Keys,
             aggregation=AggregationModule,
             aggregation_options=AggregationOptions}) ->
    %% TODO: maybe just store multiplier as unit measure??
    MUnit = oc_stat_measure:unit(Measure),
    Data = AggregationModule:export(Name, AggregationOptions),
    #{name => Name,
      description => Description,
      ctags => CTags,
      tags => lists:reverse(Keys),
      data => oc_stat_aggregation:convert(Data, MUnit, VUnit)}.

%% =============================================================================
%% internal
%% =============================================================================

%% @private
gen_add_sample_(ViewSub) ->
    TagsA = erl_parse:abstract(ViewSub#v_s.tags),
    ViewNameA = erl_parse:abstract(ViewSub#v_s.name),
    AggregationModuleA = erl_parse:abstract(ViewSub#v_s.aggregation),
    AggregationOptionsA = erl_parse:abstract(ViewSub#v_s.aggregation_options),

    {call, 1, {remote, 1, AggregationModuleA, {atom, 1, add_sample}},
     [ViewNameA,
      {call, 1, {remote, 1, {atom, 1, oc_stat_view}, {atom, 1, tag_values_}},
       [{var, 1, 'ContextTags'}, TagsA]},
      {var, 1, 'Value'},
      AggregationOptionsA]}.

%% @private
all_subscribed_() ->
    ets:match_object(?VIEWS_TABLE, #view{subscribed=true, _='_'}).

%% @private
register_(#view{measure=Measure}=View) ->
    case oc_stat_measure:exists(Measure) of
        false -> {error, {unknown_measure, Measure}};
        RMeasure ->
            case validate_view_unit_and_measure(View, RMeasure) of
                {error, _} = Error -> Error;
                UView -> register__(UView)
            end
    end.

register__(#view{name=Name}=View) ->
    SView = View#view{subscribed=true},
    case ets:lookup(?VIEWS_TABLE, Name) of
        [View] ->
            {ok, View};
        [SView] ->
            {ok, SView};
        [_] ->
            {error, {view_already_exists, Name}};
        _ ->
            try init_view_aggregation(View) of
                IView ->
                    ets:insert(?VIEWS_TABLE, IView),
                    {ok, IView}
            catch
                _:Error ->
                    clear_view_aggregation(View),
                    {error, Error}
            end
    end.

%% @private
register_subscribe_(View) ->
    case register_(View) of
        {ok, V} ->
            subscribe_(V);
        Error ->
            Error
    end.

%% @private
deregister_(Name) ->
    case ets:take(?VIEWS_TABLE, Name) of
        [] -> ok;
        [View] -> unsubscribe_(View)
    end.

%% @private
subscribe_(#view{subscribed=true}) ->
    ok;
subscribe_(#view{measure=Measure}=View) ->
    VS = vs_from_view(View),
    case oc_stat_measure:add_subscription_(Measure, VS) of
        ok ->
            {ok, mark_view_as_subscribed_(View)};
        Error ->
            Error
    end;
subscribe_(Name) ->
    case view_by_name(Name) of
        {ok, View} ->
            subscribe_(View);
        unknown ->
            {error, {unknown_view, Name}}
    end.

%% @private
unsubscribe_(#view{subscribed=false}) ->
    ok;
unsubscribe_(#view{measure=Measure}=View) ->
    VS = vs_from_view(View),
    oc_stat_measure:remove_subscription_(Measure, VS),
    UView = mark_view_as_unsubscribed_(View),
    clear_view_aggregation(View),
    {ok, UView};
unsubscribe_(Name) ->
    case view_by_name(Name) of
        {ok, View} ->
            unsubscribe_(View);
        unknown ->
            {error, {unknown_view, Name}}
    end.

%% @private
tag_values_(Tags, Keys) ->
    lists:foldl(fun(Key, Acc) ->
                        [maps:get(Key, Tags, undefined) | Acc]
                end, [], Keys).

%% @private
'__init_backend__'() ->
    ?VIEWS_TABLE = ets:new(?VIEWS_TABLE, [set, named_table, public, {keypos, 2}, {read_concurrency, true}]),
    ok.

%% =============================================================================
%% private
%% =============================================================================

-spec vs_from_view(view()) -> v_s().
vs_from_view(#view{name=Name,
                   tags=Keys,
                   aggregation=AggregationModule,
                   aggregation_options=AggregationOptions}) ->
    #v_s{name=Name,
         tags=Keys,
         aggregation=AggregationModule,
         aggregation_options=AggregationOptions}.

validate_view_unit_and_measure(#view{unit=undefined}=View, Measure) ->
    MUnit = oc_stat_measure:unit(Measure),
    case oc_stat_unit:must_convert(MUnit) of
        true ->
            {error, {invalid_unit, "view must override measure unit", MUnit}};
        false ->
            View#view{measure=Measure}
    end;
validate_view_unit_and_measure(#view{unit=VUnit}=View, Measure) ->
    MUnit = oc_stat_measure:unit(Measure),
    case oc_stat_unit:is_comparable(VUnit, MUnit) of
        false ->
            {error, {not_comparable_units, MUnit, VUnit}};
        true ->
            View#view{measure=Measure}
    end.

init_view_aggregation(#view{name=Name,
                            tags=Keys,
                            aggregation=AggregationModule,
                            aggregation_options=AggregationOptions} = View) ->
    NAggregationOptions = AggregationModule:init(Name, Keys, AggregationOptions),
    View#view{subscribed=false,
              aggregation_options=NAggregationOptions}.

clear_view_aggregation(#view{name=Name,
                             aggregation=AggregationModule,
                             aggregation_options=AggregationOptions}) ->
    AggregationModule:clear_rows(Name, AggregationOptions).

mark_view_as_subscribed_(View) ->
    SView = View#view{subscribed=true},
    swap(?VIEWS_TABLE, View, SView),
    SView.

mark_view_as_unsubscribed_(View) ->
    UView = View#view{subscribed=false},
    swap(?VIEWS_TABLE, View, UView),
    UView.

swap(Where, What, With) ->
    ets:select_replace(Where, [{What, [], [{const, With}]}]).

view_by_name(Name) ->
    case ets:lookup(?VIEWS_TABLE, Name) of
        [View] ->
            {ok, View};
        [] -> unknown
    end.

normalize_aggregation({Module, Options}) ->
    {Module, Options};
normalize_aggregation(Module) when is_atom(Module) ->
    {Module, []}.

normalize_tags([]) ->
    {#{}, []};
normalize_tags(Tags) ->
    normalize_tags(Tags, {#{}, []}).

normalize_tags([], {Map, List}) ->
    {Map, lists:reverse(List)};
normalize_tags([First|Rest], {Map, List}) when is_map(First) ->
    normalize_tags(Rest, {maps:merge(Map, First), List});
normalize_tags([First|Rest], {Map, List}) when is_atom(First) ->
    normalize_tags(Rest, {Map, [First | List]}).
