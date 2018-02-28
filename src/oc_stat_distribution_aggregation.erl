-module(oc_stat_distribution_aggregation).

-export([init/4,
         type/0,
         add_sample/4,
         export/2]).

init(Name, Description, {CTags, Keys}, Options) ->
    Buckets = prometheus_buckets:new(proplists:get_value(buckets, Options, default)),
    prometheus_histogram:declare([{name, Name},
                                  {help, Description},
                                  {labels, Keys},
                                  {constant_labels, CTags},
                                  {buckets, Buckets}]),
    Buckets.

type() ->
    distribution.

add_sample(Name, Tags, Value, Buckets) ->
    Position = prometheus_buckets:position(Buckets, Value),
    prometheus_histogram:pobserve(default, Name, Tags, Buckets, Position, Value).

export(Name, _Options) ->
    lists:map(fun({Tags, Buckets, Sum}) ->
                      Count = lists:foldl(fun({_Bound, C}, Acc) ->
                                                  C + Acc
                                          end, 0, Buckets),
                      #{tags => maps:from_list(Tags),
                        count => Count,
                        sum => Sum,
                        mean => Sum / Count,
                        buckets => Buckets}
              end, prometheus_histogram:values(default, Name)).
