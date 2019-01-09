-module(oc_metrics_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("oc_metrics.hrl").

all() ->
    [self_producer_default].

init_per_suite(Config) ->
    _ = application:load(opencensus),
    _ = application:ensure_all_started(opencensus),
    oc_producer_registry:add_producer(test_registry, oc_self_producer),
    Config.

end_per_suite(_Config) ->
    ok.

self_producer_default(_Config) ->
    SpanName1 = <<"span-1">>,
    Span1 = oc_trace:start_span(SpanName1, undefined),

    ?assertMatch([#oc_metric{descriptor=#oc_metric_descriptor{
                                           name="oc_span_buffer_bytes",
                                           description="Size of the spans ETS table",
                                           unit= <<"1">>,
                                           type='GAUGE_INT64',
                                           label_keys=[]},
                             timeseries=[#oc_time_series{
                                            start_timestamp=undefined,
                                            label_values=[],
                                            points=[#oc_point{
                                                       timestamp={_, _},
                                                       value=S1}]}],
                             resource=undefined},
                  #oc_metric{descriptor=#oc_metric_descriptor{
                                           name="oc_span_buffer_size",
                                           description="Count of spans in the ETS table",
                                           unit= <<"1">>,
                                           type='GAUGE_INT64',
                                           label_keys=[]},
                             timeseries=[#oc_time_series{
                                            start_timestamp=undefined,
                                            label_values=[],
                                            points=[#oc_point{
                                                       timestamp={_, _},
                                                       value=1}]}],
                             resource=undefined}] when S1 > 5000,
                                                       lists:sort(oc_producer_registry:read_to_list(test_registry))),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpan1 = oc_trace:start_span(ChildSpanName1, Span1, #{}),

    ?assertMatch([#oc_metric{descriptor=#oc_metric_descriptor{
                                           name="oc_span_buffer_bytes",
                                           description="Size of the spans ETS table",
                                           unit= <<"1">>,
                                           type='GAUGE_INT64',
                                           label_keys=[]},
                             timeseries=[#oc_time_series{
                                            start_timestamp=undefined,
                                            label_values=[],
                                            points=[#oc_point{
                                                       timestamp={_, _},
                                                       value=S2}]}],
                             resource=undefined},
                  #oc_metric{descriptor=#oc_metric_descriptor{
                                           name="oc_span_buffer_size",
                                           description="Count of spans in the ETS table",
                                           unit= <<"1">>,
                                           type='GAUGE_INT64',
                                           label_keys=[]},
                             timeseries=[#oc_time_series{
                                            start_timestamp=undefined,
                                            label_values=[],
                                            points=[#oc_point{
                                                       timestamp={_, _},
                                                       value=2}]}],
                             resource=undefined}] when S2 > 10000,
                                                       lists:sort(oc_producer_registry:read_to_list(test_registry))),
    oc_trace:finish_span(ChildSpan1),
    oc_trace:finish_span(Span1).
