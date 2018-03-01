-module(oc_stat_sum_aggregation).

-export([init/3,
         type/0,
         add_sample/4,
         export/2]).

-behavior(oc_stat_aggregation).

init(Name, Keys, Options) ->
    prometheus_summary:declare([{name, Name},
                                {registry, '__opencensus__'},
                                {help, ""},
                                {labels, Keys}]),
    Options.

type() ->
    sum.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, _Options) ->
    prometheus_summary:observe('__opencensus__', Name, Tags, Value),
    ok.

export(Name, _Options) ->
    Rows = lists:map(fun({Tags, Count, Sum}) ->
                             #{tags => maps:from_list(Tags),
                               value => #{count => Count,
                                          sum => Sum,
                                          mean => Sum / Count}}
                     end, prometheus_summary:values('__opencensus__', Name)),
    #{type => type(),
      rows => Rows}.
