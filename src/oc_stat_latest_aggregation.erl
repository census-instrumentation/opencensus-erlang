-module(oc_stat_latest_aggregation).

-export([init/4,
         type/0,
         add_sample/4,
         export/2]).

init(Name, Description, {CTags, Keys}, Options) ->
    prometheus_gauge:declare([{name, Name},
                              {help, Description},
                              {labels, Keys},
                              {constant_labels, CTags}]),
    Options.

type() ->
    latest.

add_sample(Name, Tags, Value, _Options) ->
    prometheus_gauge:set(Name, Tags, Value).

export(Name, _Options) ->
    lists:map(fun({Tags, Value}) ->
                      #{tags => maps:from_list(Tags),
                        value => Value}
              end, prometheus_gauge:values(default, Name)).
