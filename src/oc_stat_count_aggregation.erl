-module(oc_stat_count_aggregation).

-include("opencensus.hrl").

-export([init/3,
         type/0,
         add_sample/4,
         export/2]).

-export_types([value/0]).

init(Name, Keys, Options) ->
    prometheus_counter:declare([{name, Name},
                                {registry, ?PROM_REGISTRY},
                                {help, ""},
                                {labels, Keys}]),
    Options.

type() ->
    count.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, _Options) ->
    prometheus_counter:inc(?PROM_REGISTRY, Name, Tags, Value),
    ok.

export(Name, _Options) ->
    Rows = lists:map(fun({Tags, Value}) ->
                             #{tags => maps:from_list(Tags),
                               value => Value}
                     end, prometheus_counter:values(?PROM_REGISTRY, Name)),
    #{type => type(),
      rows => Rows}.
