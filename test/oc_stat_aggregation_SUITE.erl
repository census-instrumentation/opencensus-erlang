-module(oc_stat_aggregation_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [conversion].

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
    ?assertMatch(#{rows := [#{tags := [],
                              value := #{count := 2,
                                         mean := 60060,
                                         sum := 120120}}],
                   type := sum},
                 oc_stat_aggregation:convert(#{rows => [#{tags => [],
                                                          value => #{count => 2,
                                                                     mean => 60060,
                                                                     sum => 120120}}],
                                               type => sum},
                                             unit, undefined)),


    ?assertMatch(#{rows := [#{tags := [],
                              value := 120.120}],
                   type := latest},
                 oc_stat_aggregation:convert(#{rows => [#{tags => [],
                                                          value => 120120}],
                                               type => latest},
                                             millisecond, second)),

    ?assertMatch(#{rows := [#{tags := [],
                              value := 120120}],
                   type := count},
                 oc_stat_aggregation:convert(#{rows => [#{tags => [],
                                                          value => 120120}],
                                               type => count},
                                             millisecond, second)),

    ?assertMatch(#{rows := [#{tags := [],
                              value := #{count := 2,
                                         mean := 60.060,
                                         sum := 120.120}}],
                   type := sum},
                 oc_stat_aggregation:convert(#{rows => [#{tags => [],
                                                          value => #{count => 2,
                                                                     mean => 60060,
                                                                     sum => 120120}}],
                                               type => sum},
                                             millisecond, second)),

    ?assertMatch(#{rows := [#{tags := [],
                              value := #{count := 2,
                                         mean := 60.060,
                                         sum := 120.120,
                                         buckets := [{100.000, 0}, {infinity, 2}]}}],
                   type := distribution},
                 oc_stat_aggregation:convert(#{rows => [#{tags => [],
                                                          value => #{count => 2,
                                                                     mean => 60060,
                                                                     sum => 120120,
                                                                     buckets => [{100000, 0}, {infinity, 2}]}}],
                                               type => distribution},
                                             millisecond, second)).

%% ===================================================================
%% Private functions
%% ===================================================================
