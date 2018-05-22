-module(oc_stat_unit_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [conversion,
     no_conversion,
     errors].

init_per_suite(Config) ->
    application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok = application:stop(counters),
    ok.

conversion(_Config) ->
    oc_stat_measure:new(http_request_duration, "Http request duration", native_time_unit),
    oc_stat_view:subscribe(http_request_duration_seconds,
                           http_request_duration, second, "desc", [], oc_stat_aggregation_sum),
    oc_stat:record(#{}, http_request_duration, 1200000),
    oc_stat:record(#{}, http_request_duration, 1200000000),

    ?assertMatch([#{name := http_request_duration_seconds,
                    description := "desc",
                    tags := [],
                    ctags := #{},
                    data := #{rows := [#{tags := [],
                                         value := #{count := 2,
                                                    mean := 0.6006,
                                                    sum := 1.2012}}],
                              type := sum}}],
                 oc_stat:export()).

no_conversion(_Config) ->
    oc_stat_measure:new(http_request_duration, "Http request duration", microsecond),
    oc_stat_view:subscribe(http_request_duration_microsecond,
                           http_request_duration, "desc", [], oc_stat_aggregation_sum),
    oc_stat:record(#{}, http_request_duration, 1200000),
    oc_stat:record(#{}, http_request_duration, 1200000000),

    ?assertMatch([#{name := http_request_duration_microsecond,
                    description := "desc",
                    tags := [],
                    ctags := #{},
                    data := #{rows := [#{tags := [],
                                         value := #{count := 2,
                                                    mean := 6.006e8,
                                                    sum := 1201200000}}],
                              type := sum}}],
                 oc_stat:export()).

errors(_Config) ->
    oc_stat_measure:new(http_request_duration, "Http request duration", native_time_unit),
    ?assertMatch({error, {invalid_unit, "view must override measure unit",
                          native_time_unit}},
                 oc_stat_view:subscribe(http_request_duration_seconds,
                                        http_request_duration, "desc", [], oc_stat_aggregation_sum)),

    oc_stat_measure:new(http_requests, "Http requests count", "req"),
    ?assertMatch({error, {not_comparable_units, "req", "ms"}},
                 oc_stat_view:subscribe(http_requests_count,
                                        http_requests, "ms", "desc", [], oc_stat_aggregation_count)).

%% ===================================================================
%% Private functions
%% ===================================================================
