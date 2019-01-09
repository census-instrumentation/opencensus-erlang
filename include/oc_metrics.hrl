-type oc_string() :: iodata().
-type oc_float() :: float() | integer() | infinity | '-infinity' | nan.
-type oc_double() :: float() | integer() | infinity | '-infinity' | nan.
-type oc_int64() :: integer().
-type oc_uint64() :: non_neg_integer().
-type oc_int32() :: integer().
-type oc_uint32() :: integer().

-type registry() :: atom().

%% Defines a label key associated with a metric descriptor.
-record(oc_label_key,
        {key = <<>>                     :: oc_string(),
         description = <<>>             :: oc_string()
        }).

-type oc_label_key() :: #oc_label_key{}.

%% Represents the value of label
%% @type oc_label_value(v) = #oc_label_value{value = binary() | iolist(),
%%                                     present = boolean()}
-record(oc_label_value,
        {value                          :: oc_string() | undefined, %% the value of the label
         present                        :: boolean() %% if false the value field is ignored and considered not set;
                                                     %% This is used to differentiate a missing label from an empty string. 
        }).

-type oc_label_value() :: #oc_label_value{}.

-record(oc_explicit_bucket_options,
        {bounds = []                    :: [oc_double()]
        }).

-type oc_explicit_bucket_options() :: #oc_explicit_bucket_options{}.

-record(exemplar,
        {value = 0.0                    :: oc_double(),
         timestamp = undefined          :: wts:timestamp() | undefined,
         attachments = #{}              :: #{oc_string() := oc_string()}
        }).

-type exemplar() :: #exemplar{}.

-record(oc_bucket,
        {count = 0                      :: oc_int64(),
         exemplar = undefined           :: oc_metrics:exemplar() | undefined
        }).

-type oc_bucket() :: #oc_bucket{}.

-record(oc_distribution,
        {count = 0                      :: oc_int32(),
         sum = 0.0                      :: oc_double(),
         sum_of_squared_deviation = 0.0 :: oc_double(),
         bucket_options = undefined     :: oc_explicit_bucket_options() | undefined,
         buckets = undefined            :: [oc_bucket()] | undefined
        }).

-type oc_distribution() :: #oc_distribution{}.

-record(oc_value_at_percentile,
        {percentile = 0.0               :: oc_double(),
         value = 0.0                    :: oc_double()
        }).

-type oc_value_at_percentile() :: #oc_value_at_percentile{}.

-record(oc_snapshot,
        {count = undefined              :: oc_int64() | undefined,
         sum = undefined                :: oc_double() | undefined,
         percentile_values = []         :: [oc_value_at_percentile()] | undefined
        }).

-type oc_snapshot() :: #oc_snapshot{}.

-record(oc_summary,
        {count = 0                      :: oc_int64(),
         sum = 0.0                      :: oc_double(),
         snapshot = undefined           :: oc_snapshot() | undefined
        }).

-type oc_summary() :: #oc_summary{}.

-record(oc_point,
        {timestamp = undefined          :: wts:timestamp() | undefined,
         value                          :: oc_int64() | oc_double() |
                                           oc_distribution() | oc_summary()
        }).

-type oc_point() :: #oc_point{}.

-record(oc_time_series,
        {start_timestamp = undefined    :: wts:timestamp() | undefined,
         label_values = []              :: [oc_label_value()],
         points = []                    :: [oc_point()] 
        }).

-type oc_time_series() :: #oc_time_series{}.
-record(oc_metric_descriptor,
        {name = <<>>                    :: oc_string(),
         description = <<>>             :: oc_string(),
         unit = <<"1">>                 :: oc_string(),
         type = 'UNSPECIFIED'           :: 'UNSPECIFIED' |
                                           'GAUGE_INT64' |
                                           'GAUGE_DOUBLE' |
                                           'GAUGE_DISTRIBUTION' |
                                           'CUMULATIVE_INT64' |
                                           'CUMULATIVE_DOUBLE' |
                                           'CUMULATIVE_DISTRIBUTION' |
                                           'SUMMARY',
         label_keys = []                :: [oc_label_key()]     %% The label keys associated with the metric descriptor.
        }).

-type oc_metric_descriptor() :: #oc_metric_descriptor{}.

-record(oc_resource,
        {type = <<>>                    :: oc_string(),
         labels = #{}                   :: #{oc_string() := oc_string()}
        }).

-type oc_resource() :: #oc_resource{}.

-record(oc_metric,
        {descriptor                     :: oc_metric_descriptor() | oc_string(),
         timeseries = []                :: [oc_time_series()],
         resource = undefined           :: oc_resource() | undefined
        }).

-type oc_metric() :: #oc_metric{}.

-type metric_callback() ::
        fun((oc_producer_registry:registry(), oc_metric()) -> any()).
