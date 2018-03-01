-module(oc_stat_view).

-export([register/5,
         deregister/1,
         subscribe/1,
         subscribe/5,
         batch_subscribe/1,
         subscribed/1,
         unsubscribe/1,
         registered/1]).

-export([measure_views/1,
         subscribed/0,
         add_sample/3,
         export/1]).

-export_types([name/0,
               description/0,
               view_data/0]).

-export(['__init_backend__'/0]).

-include("opencensus.hrl").

-define(NAME_POS, 2).
-define(SUBSCRIBED_POS, 3).

-type name() :: atom() | binary() | string().
-type description() :: binary() | string().

-type view_data() :: #{name := name(),
                       description := description(),
                       ctags := oc_tags:tags(),
                       data := oc_stat_aggregation:data()}.


-spec register(name(), description(), oc_tags:tags(),
               measure_name(), aggregation()) -> ok.
register(Name, Description, Tags, Measure, Aggregation) ->
    %% TODO: check Measure exists?
    register(Name, Description, Tags, Measure, Aggregation, false).

-spec deregister(name()) -> ok.
deregister(Name) ->
    ets:delete(?MODULE, Name),
    ok.

-spec subscribe(map() | name()) -> ok.
subscribe(#{name := Name, description := Description, tags := Tags,
            measure := Measure, aggregation := Aggregation}) ->
    subscribe(Name, Description, Tags, Measure, Aggregation);
subscribe(Name) ->
    ets:update_element(?MODULE, Name, {?SUBSCRIBED_POS, true}),
    ok.

-spec subscribe(name(), description(), oc_tags:tags(),
                measure_name(), aggregation()) -> ok.
subscribe(Name, Description, Tags, Measure, Aggregation) ->
    %% TODO: check Measure exists?
    register(Name, Description, Tags, Measure, Aggregation, true).

%% @doc
%% Subscribe many `Views' at once.
%% @end
-spec batch_subscribe(list(name() | map())) -> ok.
batch_subscribe(Views) when is_list(Views) ->
    [ok = subscribe(View) || View <- Views],
    ok.

-spec register(name(), description(), oc_tags:tags(),
               measure_name(), aggregation(), boolean()) -> ok | no_return().
register(Name, Description, Tags, Measure, Aggregation, Subscribed) ->
    NAggregation = normalize_aggregation(Aggregation),
    NTags = normalize_tags(Tags),
    case ets:match_object(?MODULE, {Measure, Name, '_', '_', '_', '_'}) of
        [{Measure, Name, '_', Description, NTags, NAggregation}] ->
            ets:update_element(?MODULE, Name, {?SUBSCRIBED_POS, Subscribed});
        [_] ->
            erlang:error({already_exists, Name});
        _ ->
            {AggregationModule, AggregationOptions} = NAggregation,
            {_, Keys} = NTags,
            %% TODO: transaction?
            %% HACK: Keys reversed because tag_value reverses
            NAggregationOptions = AggregationModule:init(Name, lists:reverse(Keys), AggregationOptions),
            ets:insert(?MODULE, {Measure, Name, Subscribed, Description, NTags,
                                 {AggregationModule, NAggregationOptions}})
    end,
    ok.

-spec unsubscribe(name()) -> ok.
unsubscribe(Name) ->
    ets:update_element(?MODULE, Name, {?SUBSCRIBED_POS, false}),
    ok.

%% @doc
%% Checks whether a view `Name' is registered.
%% @end
-spec registered(name()) -> boolean().
registered(Name) ->
    ets:lookup_element(?MODULE, Name, ?NAME_POS) =/= [].

measure_views(Measure) ->
    ets:lookup(?MODULE, Measure).

subscribed({_Measure, _Name, Subscribed, _Description, _Tags, _Aggregation}) ->
    Subscribed.

subscribed() ->
    ets:match_object(?MODULE, {'_', '_', true, '_', '_', '_'}).

add_sample({_Measure, Name, _Subscribed, _Description, ViewTags, Aggregation}, ContextTags, Value) ->
    {_, Keys} = ViewTags,
    TagValues = tag_values(ContextTags, Keys),
    {AggregationModule, AggregationOptions} = Aggregation,
    AggregationModule:add_sample(Name, TagValues, Value, AggregationOptions).

-spec export(tuple()) -> view_data().
export({_Measure, Name, _, Description, ViewTags, Aggregation}) ->
    {AggregationModule, AggregationOptions} = Aggregation,
    {CTags, _Keys} = ViewTags,
    #{name => Name,
      description => Description,
      ctags => CTags,
      data => AggregationModule:export(Name, AggregationOptions)}.

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
    ?MODULE = ets:new(?MODULE, [bag, named_table, public, {read_concurrency, true}]),
    ok.
