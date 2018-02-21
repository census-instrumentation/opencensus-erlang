-module(oc_stat_sum_aggregation).

-export([init/4,
         add_sample/4,
         export/2]).

init(Name, Description, {CTags, Keys}, Options) ->
  prometheus_summary:declare([{name, Name},
                              {help, Description},
                              {labels, Keys},
                              {constant_labels, CTags}]),
  Options.

add_sample(Name, Tags, Value, _Options) ->
  prometheus_summary:observe(Name, Tags, Value).

export(_Name, _Options) ->
  erlang:error(not_supported).
