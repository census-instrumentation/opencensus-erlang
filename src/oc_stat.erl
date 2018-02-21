-module(oc_stat).

-export([record/3,
         export/0]).

-include("opencensus.hrl").

-spec record(measure_name(), ctx:t(), number()) -> number().
record(MeasureName, Ctx, Value) ->
    Tags = oc_tags:from_ctx(Ctx),
    [oc_stat_view:add_sample(View, Tags, Value)
     || View <- oc_stat_view:measure_views(MeasureName),
        oc_stat_view:subscribed(View)],
    Value.

export() ->
    [oc_stat_view:export(View) || View <- oc_stat_view:subscribed()].
