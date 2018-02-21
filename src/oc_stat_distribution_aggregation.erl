-module(oc_stat_distribution_aggregation).

-export([init/4,
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

add_sample(Name, Tags, Value, Buckets) ->
  Position = prometheus_buckets:position(Buckets, Value),
  prometheus_histogram:pobserve(default, Name, Tags, Buckets, Position, Value).

export(_Name, _Options) ->
  erlang:error(not_supported).
