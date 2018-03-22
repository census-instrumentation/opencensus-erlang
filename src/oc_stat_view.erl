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
-module(oc_stat_view).

-export([new/1,
         new/5,
         register/5,
         register/1,
         deregister/1,
         is_registered/1,
         subscribe/5,
         subscribe/1,
         unsubscribe/1,
         is_subscribed/1]).

-export([preload/1]).

-export([measure_views/1,
         add_sample/3,
         all_subscribed/0,
         export/1]).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export_types([name/0,
               description/0,
               view_data/0]).

-export(['__init_backend__'/0]).

-include("opencensus.hrl").

%% enable ets:fun2ms parse transform
-include_lib("stdlib/include/ms_transform.hrl").

-define(VIEWS_TABLE, ?MODULE).
-define(MEASURES_TABLE, oc_stat_view_subs).

-record(view, {name                :: name() | '_',
               measure             :: measure_name() | '_',
               subscribed          :: boolean(),
               description         :: description() | '_',
               ctags               :: oc_tags:tags() | '_',
               tags                :: [oc_tags:key()] | '_',
               aggregation         :: aggregation() | '_',
               aggregation_options :: aggregation_options() | '_'}).

-record(v_s, {name                :: name(),
              tags                :: [oc_tags:key()],
              aggregation         :: aggregation(),
              aggregation_options :: aggregation_options()}).

-record(measure, {name :: name(),
                  subs :: [#v_s{}]}).

-record(state, {}).

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
    new(maps:get(name, Map), maps:get(measure, Map), maps:get(description, Map),
        maps:get(tags, Map, []), maps:get(aggregation, Map)).

%% @doc
%% Creates a View. This view needs to be registered and subscribed to a measure
%% in order to start aggregating data.
%% @end
new(Name, Measure, Description, Tags, Aggregation) ->
    {CTags, Keys} = normalize_tags(Tags),
    {AggregationModule, AggregationOptions} = normalize_aggregation(Aggregation),
    #view{name=Name,
          measure=Measure,
          description=Description,
          subscribed=false,
          ctags=CTags,
          tags=Keys,
          aggregation=AggregationModule,
          aggregation_options=AggregationOptions}.


%% @doc
%% Registers the view created from arguments.
%% @end
register(Name, Measure, Description, Tags, Aggregation) ->
    register(new(Name, Measure, Description, Tags, Aggregation)).

%% @doc
%% Registers the view. Aggregation initialized with AggregationOptions.
%% @end
-spec register(view()) -> {ok, view()} | {error, any()}.
register(#view{}=View) ->
    gen_server:call(?MODULE, {register, View}).

%% @doc
%% Deregisters the view. If subscribed, unsubscribes and therefore
%% existing aggregation data cleared.
%% @end
-spec deregister(name() | view()) -> ok.
deregister(#view{name=Name}) ->
    deregister(Name);
deregister(Name) ->
    gen_server:call(?MODULE, {deregister, Name}).

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
%% A shortcut. Creates, Registers, and Subscribes a view in one call.
%% @end
subscribe(Name, Measure, Description, Tags, Aggregation) ->
    {ok, RView} = register(new(Name, Measure, Description, Tags, Aggregation)),
    {ok, SView} = subscribe(RView),
    {ok, SView}.

%% @doc
%% Subscribe the View, When subscribed, a view can aggregate measure data and export it.
%% @end
-spec subscribe(name() | view()) -> {ok, view()} | {error, any()}.
subscribe(#view{name=Name}) ->
    subscribe(Name);
subscribe(Name) ->
    gen_server:call(?MODULE, {subscribe, Name}).

%% @doc
%% Unsubscribes the View. When unsubscribed a view no longer aggregates measure data
%% and exports it. Also all existing aggregation data cleared.
%% @end
-spec unsubscribe(name() | view()) -> ok.
unsubscribe(#view{name=Name}) ->
    unsubscribe(Name);
unsubscribe(Name) ->
    gen_server:call(?MODULE, {unsubscribe, Name}).

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

%% @doc
%% Loads and subscribes views from the `List' in one shot.
%% Usually used for loading views from configuration on app start.
%% @end
preload(List) ->
    [begin
         NV = new(V),
         case register_(NV) of
             {ok, RV} ->
                 subscribe_(RV);
             {error, Error} ->
                 %% TODO: should it crash?
                 error_logger:info_msg("Unable to preload view ~p. Error is ~p", [NV#view.name, Error])
         end
     end || V <- List].

%% @private
measure_views(Measure) ->
    case ets:lookup(?MEASURES_TABLE, Measure) of
        [#measure{subs=Subs}] ->
            Subs;
        _ -> []
    end.

%% @private
-spec add_sample(v_s(), oc_tags:tags(), number()) -> ok.
add_sample(ViewSub, ContextTags, Value) ->
    TagValues = tag_values(ContextTags, ViewSub#v_s.tags),
    AM = ViewSub#v_s.aggregation,
    AM:add_sample(ViewSub#v_s.name, TagValues, Value, ViewSub#v_s.aggregation_options),
    ok.

%% @private
all_subscribed() ->
    ets:match_object(?VIEWS_TABLE, #view{subscribed=true, _='_'}).

%% @doc
%% Returns a snapshot of the View's data.
%% @end
-spec export(view()) -> view_data().
export(#view{name=Name, description=Description,
             ctags=CTags, tags=Keys,
             aggregation=AggregationModule,
             aggregation_options=AggregationOptions}) ->
    #{name => Name,
      description => Description,
      ctags => CTags,
      tags => lists:reverse(Keys),
      data => AggregationModule:export(Name, AggregationOptions)}.

%% gen_server implementation

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call({register, #view{measure=Measure,
                             name=Name,
                             description=Description,
                             ctags=CTags,
                             tags=Keys,
                             aggregation=AggregationModule}=View}, _From, State) ->
    case ets:lookup(?VIEWS_TABLE, Name) of
        [#view{measure=Measure,
               name=Name,
               description=Description,
               ctags=CTags,
               tags=Keys,
               aggregation=AggregationModule}=OldView] ->
            {reply, {ok, OldView}, State};
        [_] ->
            {reply, {error, {already_exists, Name}}, State};
        _ ->
            {reply, register_(View), State}
    end;
handle_call({deregister, Name}, _From, State) ->
    case ets:take(?VIEWS_TABLE, Name) of
        [] -> ok;
        [View] -> unsubscribe_(View)
    end,
    {reply, ok, State};
handle_call({subscribe, Name}, _From,  State) ->
    case view_by_name(Name) of
        {ok, View} ->
            case subscribe_(View) of
                ok -> {reply, {ok, View#view{subscribed=true}}, State}
            end;
        unknown ->
            {reply, {error, {unknown_view, Name}}, State}
    end;
handle_call({unsubscribe, Name}, _From,  State) ->
    case view_by_name(Name) of
        {ok, View} ->
            case unsubscribe_(View) of
                ok ->
                    {reply, {ok, View#view{subscribed=false}}, State}
            end;
        unknown ->
            {reply, {error, {unknown_view, Name}}, State}
    end;
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%% private

register_(#view{measure=Measure,
                name=Name,
                description=Description,
                ctags=CTags,
                tags=Keys,
                aggregation=AggregationModule,
                aggregation_options=AggregationOptions}) ->
    NAggregationOptions = AggregationModule:init(Name, Keys, AggregationOptions),
    try
        NView = #view{measure=Measure,
                      name=Name,
                      subscribed=false,
                      description=Description,
                      ctags=CTags,
                      tags=Keys,
                      aggregation=AggregationModule,
                      aggregation_options=NAggregationOptions},
        ets:insert(?VIEWS_TABLE, NView),
        {ok, NView}
    catch
        _:Error -> AggregationModule:clear_rows(Name, NAggregationOptions),
                   {error, Error}
    end.

subscribe_(#view{subscribed=true}) ->
    ok;
subscribe_(#view{measure=Measure,
                 name=Name,
                 tags=Keys,
                 aggregation=AggregationModule,
                 aggregation_options=AggregationOptions}=View) ->
    VS = #v_s{name=Name,
              tags=Keys,
              aggregation=AggregationModule,
              aggregation_options=AggregationOptions},
    case ets:lookup(?MEASURES_TABLE, Measure) of
        [] ->
            ets:insert(?MEASURES_TABLE, #measure{name=Measure,
                                                 subs=[VS]});
        [#measure{subs=Subs}=M] ->
            swap(?MEASURES_TABLE, M, M#measure{subs=[VS | Subs]})
    end,
    mark_view_as_subscribed_(View),
    ok.

unsubscribe_(#view{subscribed=false}) ->
    ok;
unsubscribe_(#view{measure=Measure,
                   name=Name,
                   tags=Keys,
                   aggregation=AggregationModule,
                   aggregation_options=AggregationOptions}=View) ->
    VS = #v_s{name=Name,
              tags=Keys,
              aggregation=AggregationModule,
              aggregation_options=AggregationOptions},
    case ets:lookup(?MEASURES_TABLE, Measure) of
        [] ->
            ok;
        [#measure{subs=Subs}=M] ->
            swap(?MEASURES_TABLE, M, M#measure{subs=lists:delete(VS, Subs)})
    end,
    mark_view_as_unsubscribed_(View),
    #view{aggregation=AggregationModule,
          aggregation_options=AggregationOptions} = View,
    AggregationModule:clear_rows(Name, AggregationOptions),
    ok.

mark_view_as_subscribed_(View) ->
    swap(?VIEWS_TABLE, View, View#view{subscribed=true}).

mark_view_as_unsubscribed_(View) ->
    swap(?VIEWS_TABLE, View, View#view{subscribed=false}).

%% privates

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

tag_values(Tags, Keys) ->
    lists:foldl(fun(Key, Acc) ->
                        [maps:get(Key, Tags, undefined) | Acc]
                end, [], Keys).

'__init_backend__'() ->
    ?VIEWS_TABLE = ets:new(?VIEWS_TABLE, [set, named_table, public, {keypos, 2}, {read_concurrency, true}]),
    ?MEASURES_TABLE = ets:new(?MEASURES_TABLE, [set, named_table, public, {keypos, 2}, {read_concurrency, true}]),
    ok.
