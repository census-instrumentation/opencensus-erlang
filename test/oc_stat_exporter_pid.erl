-module(oc_stat_exporter_pid).

-export([export/2]).

export(ViewData, Pid) ->
    Pid ! {view_data, ViewData}.
