-module(oc_stat_aggregation_pid).

-export([init/3,
         type/0,
         add_sample/4,
         export/2,
         clear_rows/2]).

-behavior(oc_stat_aggregation).

init(_Name, _Keys, Pid) ->
    Pid ! aggregation_init,
    Pid.

type() ->
    pid.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(_Name, _Tags, _Value, _Options) ->
    ok.

export(_Name, _Options) ->
    #{type=>type(),
      rows=>[]}.

clear_rows(_Name, Pid) ->
    Pid ! aggregation_clear_rows,
    ok.
