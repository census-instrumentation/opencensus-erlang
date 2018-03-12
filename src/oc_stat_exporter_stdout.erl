-module(oc_stat_exporter_stdout).

-export([export/2]).

export(ViewData, _) ->
    [io:format("~s: ~p~n", [Name, Data]) || #{name := Name,
                                              data := Data} <- ViewData].
