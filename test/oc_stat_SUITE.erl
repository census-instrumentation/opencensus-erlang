-module(oc_stat_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

-define(VD, [#{name := "last_video_size",
               description := "last processed video size",
               ctags := #{ctag := value},
               tags := [],
               data := #{type := latest,
                         rows := [#{tags := [],
                                    value := 1024}]}},
             #{name := "video_size",
               description :=
                   "number of videos processed processed over time",
               ctags := #{ctag := value},
               tags := [],
               data := #{type := distribution,
                         rows :=
                             [#{tags := [],
                                value := #{buckets := [{0, 0},
                                                       {65536, 6},
                                                       {4294967296, 0},
                                                       {infinity, 0}],
                                           count := 6,
                                           mean := 1024.5,
                                           sum := 6147}}]}},
             #{name := "video_count",
               description :=
                   "number of videos processed processed over time",
               ctags := #{ctag := value},
               tags := [type],
               data := #{type := count,
                         rows := [#{tags := ["mpeg"],
                                    value := 6}]}},
             #{name := "video_sum",
               description := "video_size_sum",
               ctags := #{sum_tag := value},
               tags := [category, type],
               data := #{type := sum,
                         rows := [#{tags := ["category1", "mpeg"],
                                    value := #{count := 6,
                                               mean := 1024.5,
                                               sum := 6147}}]}}]).

all() ->
    [
     views_and_measures,
     operations,
     parse_transform,
     full
    ].

init_per_suite(Config) ->
    application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    application:unload(opencensus),
    ok.

init_per_testcase(full, Config) ->
    Exporters = [{oc_stat_exporter_pid, self()}],
    application:set_env(opencensus, stat, [{handlers, Exporters}]),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(_Name, Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok = application:stop(counters),
    ok.

%% ===================================================================
%% TESTS
%% ===================================================================

views_and_measures(_Config) ->
    Tags = #{type => "mpeg",
             category => "category1"},
    Ctx = oc_tags:new_ctx(ctx:new(), Tags),

    ?assertError({unknown_measure, 'my.org/measures/video_count'},
                 oc_stat:record(Ctx, 'my.org/measures/video_count', 1)),

    oc_stat_measure:new('my.org/measures/video_count', "", ""),

    %% can record measure without views
    ?assertMatch(ok, oc_stat:record(Ctx, 'my.org/measures/video_count', 1)),

    ?assertError({unknown_measure, 'my.org/measures/video_size'},
                 oc_stat:record(Ctx, 'my.org/measures/video_size', 1)),

    ?assertMatch({error, {unknown_measure, 'my.org/measures/video_size'}},
                 oc_stat_view:subscribe(
                   "video_sum",
                   'my.org/measures/video_size',
                   "video_size_sum",
                   [#{sum_tag => value},
                    type, category],
                   oc_stat_aggregation_sum)),

    ?assertMatch({error, {unknown_measure, 'my.org/measures/video_size'}},
                 oc_stat_view:register(
                   "video_sum",
                   'my.org/measures/video_size',
                   "video_size_sum",
                   [#{sum_tag => value},
                    type, category],
                   oc_stat_aggregation_sum)),

    ?assertMatch({error, {unknown_view, "video_sum"}},
                 oc_stat_view:subscribe(
                   "video_sum")),

    oc_stat_measure:new('my.org/measures/video_size', "", ""),

    {ok, _} = oc_stat_view:subscribe(
                "video_sum",
                'my.org/measures/video_size',
                "video_size_sum",
                [#{sum_tag => value},
                 type, category],
                oc_stat_aggregation_sum),

    ?assertMatch(ok, oc_stat:record(Ctx, 'my.org/measures/video_size', 10)),

    ok = application:stop(opencensus),
    {ok, _} = application:ensure_all_started(opencensus),

    ?assertMatch({error, {unknown_measure, 'my.org/measures/video_size'}},
                 oc_stat_view:subscribe(
                   "video_sum",
                   'my.org/measures/video_size',
                   "video_size_sum",
                   [#{sum_tag => value},
                    type, category],
                   oc_stat_aggregation_sum)),

    ?assertError({unknown_measure, 'my.org/measures/video_size'},
                 oc_stat:record(Ctx, 'my.org/measures/video_size', 1)).

operations(_Config) ->
    oc_stat_measure:new('my.org/measures/video_count', "", ""),

    View = oc_stat_view:new(
             "video_count",
             'my.org/measures/video_count',
             "number of videos processed processed over time",
             [#{ctag => value},
              type],
             {oc_stat_aggregation_pid, pid_to_list(self())}),

    {ok, RView} = oc_stat_view:register(View),
    receive
        aggregation_init -> ok
    after 5000 ->
            ct:fail("aggregation init wasn't called on register")
    end,
    true = oc_stat_view:is_registered(RView),
    false = oc_stat_view:is_subscribed(RView),

    {ok, SView} = oc_stat_view:subscribe(RView),
    true = oc_stat_view:is_subscribed(RView),

    {ok, UView} = oc_stat_view:unsubscribe(SView),
    false = oc_stat_view:is_subscribed(UView),
    receive
        aggregation_clear_rows -> ok
    after 5000 ->
            ct:fail("aggregation data wasn't cleared on unsubscribe")
    end,

    {ok, RView} = oc_stat_view:register(View),
    {ok, SView} = oc_stat_view:subscribe(RView),
    ok = oc_stat_view:deregister(View),
    receive
        aggregation_clear_rows -> ok
    after 5000 ->
            ct:fail("aggregation data wasn't cleared on deregister")
    end.

parse_transform(_Config) ->
    ?assertError({unknown_measure, "qwe"},
                 oc_stat_pt_user:record_non_existing()),

    oc_stat_measure:new('my.org/measures/video_size', "", "").

full(_Config) ->
    oc_stat_measure:new('my.org/measures/video_size', "", ""),

    {ok, _} = oc_stat_view:subscribe(
                #{
                  name => "video_size",
                  description => "number of videos processed processed over time",
                  tags => [#{ctag => value}],
                  measure => 'my.org/measures/video_size',
                  aggregation => {oc_stat_aggregation_distribution, [{buckets, [0, 1 bsl 16, 1 bsl 32]}]}
                 }),

    {ok, _} = oc_stat_view:subscribe(
                "video_count",
                'my.org/measures/video_size',
                "number of videos processed processed over time",
                [#{ctag => value},
                 type],
                oc_stat_aggregation_count),

    {ok, _} = oc_stat_view:subscribe(
                "video_sum",
                'my.org/measures/video_size',
                "video_size_sum",
                [#{sum_tag => value},
                 type, category],
                oc_stat_aggregation_sum),

    {ok, _} = oc_stat_view:subscribe(
                "last_video_size",
                'my.org/measures/video_size',
                "last processed video size",
                [#{ctag => value}],
                oc_stat_aggregation_latest),

    Tags = #{type => "mpeg",
             category => "category1"},
    Ctx = oc_tags:new_ctx(ctx:new(), Tags),

    oc_stat:record(Ctx, 'my.org/measures/video_size', 1),
    oc_stat:record(Tags, [{'my.org/measures/video_size', 1},
                          {'my.org/measures/video_size', 1024}]),
    oc_stat:record(Tags, 'my.org/measures/video_size', 4096),
    oc_stat:record(Ctx, [{'my.org/measures/video_size', 1},
                         {'my.org/measures/video_size', 1024}]),

    ?assertMatch(?VD,
                 lists:sort(oc_stat:export())),

    % ?assertMatch(true, oc_stat_exporter:registered(oc_stat_exporter_pid)),

    receive
        {view_data, Thing} ->
            ?assertMatch(?VD, lists:sort(Thing))
    after 10000 ->
            ?assertMatch(?VD, timeout)
    end.

maps_to_sorted_lists(Maps) ->
    List = [{maps:get(name, Map), maps:to_list(Map)} || Map <- Maps],
    [Map1 || {_, Map1} <- lists:keysort(1, List)].
