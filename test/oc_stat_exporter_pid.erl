-module(oc_stat_exporter_pid).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2]).

init(Pid) -> {ok, Pid}.

handle_call(_Msg, State) -> {ok, ok, State}.

handle_event({stats, ViewData}, Pid) ->
    Pid ! {view_data, ViewData}.
