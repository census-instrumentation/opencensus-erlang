-module(oc_stat_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [
     full
    ].

init_per_suite(Config) ->
    application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(full, Config) ->
    Views = [#{
                name => "video_size",
                description => "number of videos processed processed over time",
                tags => [#{ctag => value}],
                measure => 'my.org/measures/video_size_sum',
                aggregation => {oc_stat_distribution_aggregation, [{buckets, [0, 1 bsl 16, 1 bsl 32]}]}
              }],

    application:set_env(opencensus, stat, [{views, Views}]),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(_Name, Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

%% ===================================================================
%% TESTS
%% ===================================================================

full(_Config) ->
    ok = oc_stat_view:subscribe(
           "video_count",
           "number of videos processed processed over time",
           [#{ctag => value},
            type],
           'my.org/measures/video_count',
           oc_stat_count_aggregation),

    ok = oc_stat_view:subscribe(
           "video_sum",
           "video_size_sum",
           [#{sum_tag => value},
            type, category],
           'my.org/measures/video_size_sum',
           oc_stat_sum_aggregation),

    ok = oc_stat_view:subscribe(
           "last_video_size",
           "last processed video size",
           [#{ctag => value}],
           'my.org/measures/video_size_sum',
           oc_stat_latest_aggregation),

    Ctx = oc_tags:new_ctx(ctx:new(), #{type => "mpeg",
                                       category => "category1"}),

    oc_stat:record('my.org/measures/video_count', Ctx, 1),
    oc_stat:record('my.org/measures/video_count', Ctx, 1),
    oc_stat:record('my.org/measures/video_size_sum', Ctx, 1024),
    oc_stat:record('my.org/measures/video_size_sum', Ctx, 4096),

    ?assertMatch([#{name := "last_video_size",
                    description := "last processed video size",
                    type := latest,
                    ctags := #{ctag := value},
                    rows := [#{tags := #{},
                               value := 4096}]},
                  #{name := "video_count",
                    description :=
                        "number of videos processed processed over time",
                    type := count,
                    ctags := #{ctag := value},
                    rows := [#{tags := #{"type" := "mpeg"},
                               value := 2}]},
                  #{name := "video_size",
                    description :=
                        "number of videos processed processed over time",
                    type := distribution,
                    ctags := #{ctag := value},
                    rows :=
                        [#{tags := #{},
                           buckets := [{0, 0},
                                       {65536, 2},
                                       {4294967296, 0},
                                       {infinity, 0}],
                           count := 2,
                           mean := 2560.0,
                           sum := 5120}]},
                  #{name := "video_sum",
                    description := "video_size_sum",
                    type := sum,
                    ctags := #{sum_tag := value},
                    rows := [#{tags := #{"category" := "category1",
                                         "type" := "mpeg"},
                               count := 2,
                               mean := 2560.0,
                               sum := 5120}]}],
                 lists:sort(oc_stat:export())),
    ?assertMatch(2, prometheus_counter:value("video_count", ["mpeg"])),
    ?assertMatch({2, 5120}, prometheus_summary:value("video_sum", ["category1", "mpeg"])),
    ?assertMatch({[0, 2, 0, 0], 5120}, prometheus_histogram:value("video_size", [])),
    ?assertMatch(4096, prometheus_gauge:value("last_video_size", [])).
