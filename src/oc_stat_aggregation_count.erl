-module(oc_stat_aggregation_count).

-include("opencensus.hrl").

-export([init/3,
         type/0,
         add_sample/4,
         export/2]).

-export_types([value/0]).

init(_Name, _Keys, Options) ->
    Options.

type() ->
    count.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, Options) ->
    case counters_simple:inc(Name, Tags, Value) of
        unknown ->
            case counters_simple:new(Name, Tags, Value) of
                ok -> ok;
                false ->
                    add_sample(Name, Tags, Value, Options)
            end;
        _ ->
            ok
    end.

export(Name, _Options) ->
    Rows = maps:values(maps:map(fun(Tags, Value) ->
                                        #{tags => Tags,
                                          value => Value}
                                end,
                                counters_simple:value(Name))),
    #{type => type(),
      rows => Rows}.
