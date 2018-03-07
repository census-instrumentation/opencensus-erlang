-module(oc_stat_exporter_prometheus_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("prometheus/include/prometheus_model.hrl").

all() ->
    [
     prometheus_collector
    ].

init_per_suite(Config) ->
    application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(prometheus_collector, Config) ->
    Views = [#{
               name => "video_size",
               description => "number of videos processed processed over time",
               tags => [#{ctag => value}],
               measure => 'my.org/measures/video_size_sum',
               aggregation => {oc_stat_aggregation_distribution, [{buckets, [0, 1 bsl 16, 1 bsl 32]}]}
              }],

    application:set_env(opencensus, stat, [{views, Views}]),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
init_per_testcase(_Name, Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok = application:stop(prometheus),
    ok.

%% ===================================================================
%% TESTS
%% ===================================================================

prometheus_collector(_Config) ->

    ok = oc_stat_view:subscribe(
           "video_count",
           "number of videos processed processed over time",
           [#{ctag => value},
            type],
           'my.org/measures/video_count',
           oc_stat_aggregation_count),

    ok = oc_stat_view:subscribe(
           "video_sum",
           "video_size_sum",
           [#{sum_tag => value},
            type, category],
           'my.org/measures/video_size_sum',
           oc_stat_aggregation_sum),

    ok = oc_stat_view:subscribe(
           "last_video_size",
           "last processed video size",
           [#{ctag => value}],
           'my.org/measures/video_size_sum',
           oc_stat_aggregation_latest),

    Tags = #{type => "mpeg",
             category => "category1"},
    Ctx = oc_tags:new_ctx(ctx:new(), Tags),

    oc_stat:record(Ctx, 'my.org/measures/video_count', 1),
    oc_stat:record(Tags, [{'my.org/measures/video_count', 1},
                          {'my.org/measures/video_size_sum', 1024}]),
    oc_stat:record(Tags, 'my.org/measures/video_size_sum', 4096),
    oc_stat:record(Ctx, [{'my.org/measures/video_count', 1},
                         {'my.org/measures/video_size_sum', 1024}]),

    ?assertMatch([#'MetricFamily'{name = "last_video_size",
                                  help = "last processed video size",
                                  type = 'GAUGE',
                                  metric = [#'Metric'{
                                               label = [#'LabelPair'{name = <<"ctag">>,
                                                                     value = <<"value">>}],
                                               gauge = #'Gauge'{value = 1024}}]},
                  #'MetricFamily'{name = "video_count",
                                  help = "number of videos processed processed over time",
                                  type = 'COUNTER',
                                  metric = [#'Metric'{
                                               label = [#'LabelPair'{name = <<"ctag">>,
                                                                     value = <<"value">>},
                                                        {'LabelPair', "type", "mpeg"}],
                                               counter = #'Counter'{value = 3}}]},
                  #'MetricFamily'{name = "video_size",
                                  help = "number of videos processed processed over time",
                                  type = 'HISTOGRAM',
                                  metric = [#'Metric'{
                                               label = [#'LabelPair'{name = <<"ctag">>,
                                                                     value = <<"value">>}],
                                               histogram =
                                                   #'Histogram'{sample_count = 3,
                                                                sample_sum = 6144,
                                                                bucket = [#'Bucket'{cumulative_count=0,
                                                                                    upper_bound=0},
                                                                          #'Bucket'{cumulative_count=3,
                                                                                    upper_bound=65536},
                                                                          #'Bucket'{cumulative_count=3,
                                                                                    upper_bound=4294967296},
                                                                          #'Bucket'{cumulative_count=3,
                                                                                    upper_bound=infinity}]}}]},
                  #'MetricFamily'{name = "video_sum",
                                  help = "video_size_sum",
                                  type = 'SUMMARY',
                                  metric = [#'Metric'{
                                               label = [#'LabelPair'{name = <<"sum_tag">>,
                                                                     value = <<"value">>},
                                                        #'LabelPair'{name = "category",
                                                                     value = "category1"},
                                                        #'LabelPair'{name = "type",
                                                                     value = "mpeg"}],
                                               summary = #'Summary'{sample_count = 3,
                                                                    sample_sum = 6144}}]}],
                 lists:sort(prom_collect_mf_to_list('_qwe_', oc_stat_exporter_prometheus))).

%% ===================================================================
%% Helpers
%% ===================================================================

prom_collect_mf_to_list(Collector) ->
    prom_collect_mf_to_list(default, Collector).

prom_collect_mf_to_list(Registry, Collector) ->
    try
        Callback = fun (MF) ->
                           put(Collector, [MF|get_list(Collector)])
                   end,
        prometheus_collector:collect_mf(Registry, Collector, Callback),

        get_list(Collector)
    after
        erase(Collector)
    end.

get_list(Key) ->
    case get(Key) of
        undefined ->
            [];
        Value ->
            Value
    end.
