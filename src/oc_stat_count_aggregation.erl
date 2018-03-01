-module(oc_stat_count_aggregation).

-export([init/3,
         type/0,
         add_sample/4,
         export/2]).

-export_types([value/0]).

init(Name, Keys, Options) ->
    prometheus_counter:declare([{name, Name},
                                {registry, '__opencensus__'},
                                {help, ""},
                                {labels, Keys}]),
    Options.

type() ->
    count.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, _Options) ->
    prometheus_counter:inc('__opencensus__', Name, Tags, Value),
    ok.

export(Name, _Options) ->
    Rows = lists:map(fun({Tags, Value}) ->
                             #{tags => maps:from_list(Tags),
                               value => Value}
                     end, prometheus_counter:values('__opencensus__', Name)),
    #{type => type(),
      rows => Rows}.
