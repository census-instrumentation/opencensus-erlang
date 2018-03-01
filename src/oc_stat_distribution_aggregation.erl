-module(oc_stat_distribution_aggregation).

-export([init/3,
         type/0,
         add_sample/4,
         export/2]).

-behavior(oc_stat_aggregation).

init(Name, Keys, Options) ->
    Buckets = prometheus_buckets:new(proplists:get_value(buckets, Options, default)),
    prometheus_histogram:declare([{name, Name},
                                  {registry, '__opencensus__'},
                                  {help, ""},
                                  {labels, Keys},
                                  {buckets, Buckets}]),
    Buckets.

type() ->
    distribution.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, Buckets) ->
    Position = prometheus_buckets:position(Buckets, Value),
    prometheus_histogram:pobserve('__opencensus__', Name, Tags, Buckets, Position, Value).

export(Name, _Options) ->
    Rows = lists:map(fun({Tags, Buckets, Sum}) ->
                             Count = lists:foldl(fun({_Bound, C}, Acc) ->
                                                         C + Acc
                                                 end, 0, Buckets),
                             #{tags => maps:from_list(Tags),
                               value => #{count => Count,
                                          sum => Sum,
                                          mean => Sum / Count,
                                          buckets => Buckets}}
                     end, prometheus_histogram:values('__opencensus__', Name)),
    #{type => type(),
      rows => Rows}.
