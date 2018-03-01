-module(oc_stat_exporter_prometheus).

-export([collect_mf/2,
         deregister_cleanup/1]).

-behaviour(prometheus_collector).

collect_mf(_Registry, Callback) ->
    ViewDatas = oc_stat:export(),
    [Callback(view_data_to_mf(ViewData)) || ViewData <- ViewDatas],
    ok.

deregister_cleanup(_Registry) ->
    ok.

-spec view_data_to_mf(oc_stat_view:view_data()) -> prometheus_model:'MetricFamily'().
view_data_to_mf(#{name := Name,
                  description := Description,
                  ctags := CTags,
                  data := #{type := Type,
                            rows := Rows}}) ->
    FullRows = augment_rows_tags(Rows, CTags),
    Metrics = rows_to_metrics(Type, FullRows),
    prometheus_model_helpers:create_mf(Name, Description, to_prometheus_type(Type), Metrics).

augment_rows_tags(Rows, CTags) ->
    [{maps:to_list(maps:merge(CTags, Tags)), Value}
     || #{tags := Tags, value := Value} <- Rows].

rows_to_metrics(latest, Rows) ->
    prometheus_model_helpers:gauge_metrics(Rows);
rows_to_metrics(count, Rows) ->
    prometheus_model_helpers:counter_metrics(Rows);
rows_to_metrics(sum, Rows) ->
    [prometheus_model_helpers:summary_metric(Tags, Count, Sum)
     || {Tags, #{count := Count, sum := Sum}} <- Rows];
rows_to_metrics(distribution, Rows) ->
    [prometheus_model_helpers:histogram_metric(
       Tags, sum_bucket_counters(Buckets), Count, Sum)
     || {Tags, #{count := Count, sum := Sum, buckets := Buckets}} <- Rows].

sum_bucket_counters([{Bound, Start} | Counters]) ->
  sum_bucket_counters(Counters, [{Bound, Start}], Start).

sum_bucket_counters([], LAcc, _CAcc) ->
  LAcc;
sum_bucket_counters([{Bucket, Counter} | Counters], LAcc, CAcc) ->
  sum_bucket_counters(Counters, LAcc ++ [{Bucket, CAcc + Counter}], CAcc + Counter).

to_prometheus_type(latest) ->
    gauge;
to_prometheus_type(count) ->
    counter;
to_prometheus_type(sum) ->
    summary;
to_prometheus_type(distribution) ->
    histogram.



