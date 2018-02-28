-module(oc_stat_count_aggregation).

-export([init/4,
         type/0,
         add_sample/4,
         export/2]).

-export_types([value/0]).

init(Name, Description, {CTags, Keys}, Options) ->
    prometheus_counter:declare([{name, Name},
                                {help, Description},
                                {labels, Keys},
                                {constant_labels, CTags}]),
    Options.

type() ->
    count.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, _Options) ->
    prometheus_counter:inc(Name, Tags, Value),
    ok.

export(Name, _Options) ->
    Rows = lists:map(fun({Tags, Value}) ->
                             #{tags => maps:from_list(Tags),
                               value => Value}
                     end, prometheus_counter:values(default, Name)),
    #{type => type(),
      rows => Rows}.
