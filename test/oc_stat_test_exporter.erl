-module(oc_stat_test_exporter).

-export([export/2]).

export(ViewData, Pid) ->
    Pid ! {view_data, ViewData}.
