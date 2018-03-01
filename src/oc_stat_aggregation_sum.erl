-module(oc_stat_aggregation_sum).

-export([init/3,
         type/0,
         add_sample/4,
         export/2]).

-behavior(oc_stat_aggregation).

-include("opencensus.hrl").

init(Name, Keys, Options) ->
    prometheus_summary:declare([{name, Name},
                                {registry, ?PROM_REGISTRY},
                                {help, ""},
                                {labels, Keys}]),
    Options.

type() ->
    sum.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, _Options) ->
    prometheus_summary:observe(?PROM_REGISTRY, Name, Tags, Value),
    ok.

export(Name, _Options) ->
    Rows = lists:map(fun({Tags, Count, Sum}) ->
                             #{tags => maps:from_list(Tags),
                               value => #{count => Count,
                                          sum => Sum,
                                          mean => Sum / Count}}
                     end, prometheus_summary:values(?PROM_REGISTRY, Name)),
    #{type => type(),
      rows => Rows}.
