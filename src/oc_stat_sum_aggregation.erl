-module(oc_stat_sum_aggregation).

-export([init/4,
         type/0,
         add_sample/4,
         export/2]).

init(Name, Description, {CTags, Keys}, Options) ->
    prometheus_summary:declare([{name, Name},
                                {help, Description},
                                {labels, Keys},
                                {constant_labels, CTags}]),
    Options.

type() ->
    sum.

add_sample(Name, Tags, Value, _Options) ->
    prometheus_summary:observe(Name, Tags, Value).

export(Name, _Options) ->
    lists:map(fun({Tags, Count, Sum}) ->
                      #{tags => maps:from_list(Tags),
                        count => Count,
                        sum => Sum,
                        mean => Sum / Count}
              end, prometheus_summary:values(default, Name)).
