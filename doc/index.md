

# OpenCensus Erlang library #

__Version:__ 0.5.0

## Erlang stats collection and distributed tracing framework

[![CircleCI](https://circleci.com/gh/census-instrumentation/opencensus-erlang.svg?style=svg)](https://circleci.com/gh/census-instrumentation/opencensus-erlang)
[![codecov](https://codecov.io/gh/census-instrumentation/opencensus-erlang/branch/master/graph/badge.svg)](https://codecov.io/gh/census-instrumentation/opencensus-erlang)
[![Hex.pm](https://img.shields.io/hexpm/v//opencensus.svg?maxAge=2592000)](https://hex.pm/packages/opencensus)
[![Hex.pm](https://img.shields.io/hexpm/dt/opencensus.svg?maxAge=2592000)](https://hex.pm/packages/opencensus)


#### <a name="Using_with_Rebar3_project">Using with Rebar3 project</a> ####

Add as dependency to `rebar.config`:

```erlang
{deps, [opencensus]}.
```

Or to use the latest from git master branch:

```erlang
{deps, [{opencensus, {git, "https://github.com/census-instrumentation/opencensus-erlang.git", {branch, "master"}}}]}.
```

### <a name="Tags">Tags</a> ###

Tags represent propagated key-value pairs. They are propagated using the pdict or `Ctx` record in the same process and can be encoded to be transmitted on the wire. 

```erlang
ocp:update_tags(#{http_server_method => "GET"}).
```

### <a name="Tracing">Tracing</a> ###

#### <a name="Creating_Spans">Creating Spans</a> ####

Span data is stored and manipulated in an ETS table. Span context must be tracked in order to create child span's, propagate span context across process or node boundaries and for the library to which span data to manipulate in the context it is called.

`opencensus` provides two methods for tracking this context, the process dictionary and a variable holding a `ctx` record.

With `ocp` the span context is tracked in the current process`s process dictionary.

```erlang
some_fun() ->
  ocp:with_child_span(<<"some_fun/0">>,
                      fun() ->
                          ... body ..
                      end).
```

More details on working with spans can be found [here](span.md) and in the modules documentation for [ocp](ocp.md), [oc_trace](oc_trace.md) and [oc_span](oc_span.md).

#### <a name="Propagating_Span_Context">Propagating Span Context</a> ####

Opencensus comes with two forms of span context encoding for sending over the wire. `oc_span_ctx_header` encodes a span context suitable for transfering as an HTTP header and `oc_span_ctx_binary` will encode and decode a binary form used in GRPC and other binary protocols.

For example, creating the header for sending with an HTTP client might look like:

```erlang
EncodedSpanCtx = oc_span_ctx_header:encode(ocp:current_span_ctx()),
Headers = [{oc_span_ctx_header:field_name(), EncodedSpanCtx}],
```

#### <a name="Samplers">Samplers</a> ####

[oc_sampler_never](oc_sampler_never.md): Never enable a new trace, but keeps a trace enabled if its propagated context is enabled.

[oc_sampler_always](oc_sampler_always.md): Enable every new trace for sampling.

[oc_sampler_probability](oc_sampler_probability.md): Takes a probability, default 0.5, that any new trace will be sampled.


#### <a name="Reporters">Reporters</a> ####

[Google Cloud Trace](https://github.com/tsloughter/oc_google_reporter): Support for v1 in master, v2 and grpc coming soon;

[Prometheus](https://github.com/deadtrickster/opencensus-erlang-prometheus): Exports spans as Prometheus metrics.

[DataDog][oc_datadog]: Export spans to DataDog APM

#### <a name="Sweeper">Cleaning Up Abandoned Spans</a> ####

Active spans have their data stored in an ETS table. When a span is finished it is removed from the active spans table and moved to a table handled by the reporter process. If a span isn't finished, either because of a mistake in the code creating and finishing spans, or the process with open spans crashes before being able to finish the spans, there would be a memory leak.

The `oc_span_sweeper` process checks for active spans which started greater than a configurable (`span_ttl`) duration, with a default of 5 minutes. There are 4 strategies for handling a span that is older than the time to live (`strategy`):

* `drop`: Spans are removed from the active span table and a log message is written with the total number of spans being dropped in this sweep.
* `finish`: Each span is finished as is.
* `failed_attribute_and_finish`: An attribute `finished_by_sweeper` with value `true` is added to the span data and then the span is finished.
* Custom function: Any funtion with type spec `fun((opencensus:span()) -> ok)` can be used. Note that the span is not removed from the active spans table if this method is used and the function must do the removal if it deems it necessary.

An example configuration in `sys.config` to run a check every 5 minutes, dropping active spans older than 5 minutes can be found in the example project `helloworld`, `examples/helloworld/config/sys.config`, the sweeper snippet looks like:

``` erlang
{sweeper, #{interval => 300000,
            strategy => drop,
            span_ttl => 300000}}
```

To disable sweeping set `interval` to `infinity`.

### <a name="Logging">Logging</a> ###

OTP-21 includes a new logging framework. When a context is created with a span (for example `ocp:with_child_span/1` or `oc_trace:with_child_span/2`) opencensus will update the current process's logger metadata to include the `trace_id`, `span_id` and `trace_options` with the latest ids under the key `span_ctx`, `trace_options` will be `1` if the trace is enabled. To use these with the default formatter you can create a custom template that includes them if they exist like so:

```
{logger_formatter,
  #{template => [time, " ", pid, " ",
                 {[span_ctx, trace_id], ["trace_id=", [span_ctx, trace_id], " "], []},
                 {[span_ctx, span_id], ["span_id=", [span_ctx, span_id], " "], []},
                 {[span_ctx, trace_options], ["trace_options=", [span_ctx, trace_options], " "], []},
                 msg, "\n"]}}
```

### <a name="Stats">Stats</a> ###

OpenCensus stats collection happens in two stages:

* Definition of measures and recording of data points
* Definition of views and aggregation of the recorded data

#### <a name="Defining_Measures">Defining Measures</a> ####

```erlang
oc_stat_measure:new('opencensus.io/http/server/server_latency', "Time between first byte of "
                    "request headers read to last byte of response sent, or terminal error.",
                    milli_seconds).
```

#### <a name="Recording">Recording</a> ####

Measurements are data points associated with a measure. `ocp:record` implicitly tags the set of Measurements with the tags from the pdict context:

```erlang
ocp:record('opencensus.io/http/server/server_latency', ServerLatency),
```

#### <a name="Views">Views</a> ####

Views are how Measures are aggregated. You can think of them as queries over the set of recorded data points (measurements).

Views have two parts: the tags to group by and the aggregation type used.

Currently four types of aggregations are supported:

* `oc_stat_aggregation_count`: Count aggregation is used to count the number of times a sample was recorded.
* `oc_stat_aggregation_distribution`: Distribution aggregation is used to provide a histogram of the values of the samples.
* `oc_stat_aggregation_sum`: Sum aggregation is used to sum up all sample values.
* `oc_stat_aggregation_latest`: Saves only the last datapoint.

Here we create a view with the distribution aggregation over our measure:

```erlang
oc_stat_view:subscribe(#{name => "opencensus.io/http/server/server_latency",
                         description => "Latency distribution of HTTP requests",
                         tags => [http_server_method, http_server_path],
                         measure => 'opencensus.io/http/server/server_blatency',
                         aggregation => {oc_stat_aggregation_distribution, 
                                         [{buckets, [0, 100, 300, 650, 800, 1000]}]}})
```

#### <a name="Stat Exporters">Stat Exporters</a> ####

[Prometheus](https://github.com/deadtrickster/opencensus-erlang-prometheus): Exports stat views as Prometheus metrics. Simply register as a collector:

```
prometheus_registry:register_collector(oc_stat_exporter_prometheus)
```

[DogStatsD][oc_datadog]: Export stat views as DataDog metrics.

### Development

```sh
$ rebar3 compile
```

Running tests:

```sh
$ rebar3 ct
```

#### Updating OpenCensus standard protobuf encoder and decoder

Language independent interface types for Census are found in the `opencensus-proto` repo. The opencensus Erlang app provides functionality for converting from the apps internal representation to the standard protobuf interface. Below are the steps to update the Erlang module and header for encoding and decoding the protobufs:

```sh
$ git clone https://github.com/census-instrumentation/opencensus-proto priv/opencensus-proto
$ rebar3 protobuf compile
```

[oc_datadog]: https://github.com/hauleth/oc_datadog
