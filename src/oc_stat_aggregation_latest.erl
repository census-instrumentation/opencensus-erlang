-module(oc_stat_aggregation_latest).

-export([init/3,
         type/0,
         add_sample/4,
         export/2]).

-behavior(oc_stat_aggregation).

init(_Name, _Keys, Options) ->
    Options.

type() ->
    latest.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, Options) ->
    case counters_counter:set(Name, Tags, Value) of
        unknown ->
            case counters_counter:new(Name, Tags, Value) of
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
                                counters_counter:value(Name))),
    #{type => type(),
      rows => Rows}.
