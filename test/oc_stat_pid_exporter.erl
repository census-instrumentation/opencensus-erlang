-module(oc_stat_pid_exporter).

-export([export/2]).

export(ViewData, Pid) ->
    Pid ! {view_data, ViewData}.
