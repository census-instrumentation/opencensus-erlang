-module(oc_stat_pt_user).

-export([record_non_existing/0,
         record_ok/0]).

-compile({parse_transform, oc_stat_transform}).

record_non_existing() ->
    oc_stat:record(#{}, "qwe", 1).

record_ok() ->
    oc_stat_measure:record(#{}, 'my.org/measures/video_size', 1024).
