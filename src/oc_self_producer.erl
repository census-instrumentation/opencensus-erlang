%% @doc
%% Produces metrics for internal state like spans queue size, views count, etc.
-module(oc_self_producer).

-export([read/2]).

-include("oc_metrics.hrl").
-include("opencensus.hrl").

read(_Registry, Callback) ->
    spans_buffer_metrics(Callback).

spans_buffer_metrics(Callback) ->
    {Count, Bytes} = oc_span_sweeper:storage_size(),
    Callback(#oc_metric{descriptor = #oc_metric_descriptor{
                                        name = "oc_span_buffer_bytes",
                                        description = "Size of the spans ETS table",
                                        type = 'GAUGE_INT64'
                                       },
                        timeseries = [#oc_time_series{
                                         points = [#oc_point{timestamp = wts:timestamp(),
                                                             value = Bytes}]
                                        }
                                     ]
                       }),
    Callback(#oc_metric{descriptor = #oc_metric_descriptor{
                                        name = "oc_span_buffer_size",
                                        description = "Count of spans in the ETS table",
                                        type = 'GAUGE_INT64'
                                       },
                        timeseries = [#oc_time_series{
                                         points = [#oc_point{timestamp = wts:timestamp(),
                                                             value = Count}]
                                        }
                                     ]
                       }).
