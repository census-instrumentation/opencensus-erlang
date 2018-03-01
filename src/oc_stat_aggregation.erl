-module(oc_stat_aggregation).

-export_types(data/0).

-type data_rows(AggregationValue) :: [#{tags := oc_tags:tags(),
                                        value := AggregationValue}].

-type data(Type, AggregationValue) :: #{type := Type,
                                        rows := data_rows(AggregationValue)}.

-type data() :: data(latest, number())
              | data(count, number())
              | data(sum, #{count := non_neg_integer(),
                            mean := number(),
                            sum := number()})
              | data(distribution, #{count := non_neg_integer(),
                                     mean := number(),
                                     sum := number(),
                                     buckets := [{number, non_neg_integer()}]}).

-type keys()  :: [oc_tags:key()].

-callback init(oc_stat_view:name(), keys(), any()) -> any().

-callback type() -> latest | count | sum | distribution.

-callback add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.

-callback export(oc_stat_view:name(), any()) -> data().
