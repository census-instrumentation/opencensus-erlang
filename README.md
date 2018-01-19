## Erlang stats collection and distributed tracing framework

[![CircleCI](https://circleci.com/gh/census-instrumentation/opencensus-erlang.svg?style=svg)](https://circleci.com/gh/census-instrumentation/opencensus-erlang)

[![Coverage Status](https://coveralls.io/repos/github/census-instrumentation/opencensus-erlang/badge.svg?branch=master)](https://coveralls.io/github/census-instrumentation/opencensus-erlang?branch=master)

### Using with Rebar3 project

Add as dependency to `rebar.config`:

```
{deps, [opencensus]}.
```

Or to use the latest from git master branch:
        
```
{deps, [{opencensus, {git, "https://github.com/census-instrumentation/opencensus-erlang.git", {branch, "master"}}}]}.
```

#### Creating Spans

##### Parse Transform

The parse transform provides an attribute to decorate functions with that will start a span, wrap the contents in a `try` and finish the span in an `after` clause. Add the parse transform to the compile opts in `rebar.config`:

```erlang
{erl_opts, [{parse_transform, oc_transform}]}.
```

And use:

```erlang
-trace([]).
function_to_trace() ->
  ... body ...
```

##### Process Dictionary

```erlang
some_fun() ->
  ocp:start_span(<<"some_fun/0">>),
  try
    ... body ...
  after
    ocp:finish_span()
  end.
```

##### Manual

Span's can be tracked manually in a variable:

```erlang
some_fun() ->
  Span0 = opencensus:start_span(<<"some_fun/0">>, opencensus:generate_trace_id(), undefined),
  try
    ... body ...
  after
    opencensus:finish_span(Span0)
  end.
```

### Samplers

* `oc_sampler_never`: Never enable a new trace, but keeps a trace enabled if its propagated context is enabled.
* `oc_sampler_always`: Enable every new trace for sampling.
* `oc_sampler_probability`: Takes a probability, default 0.5, that any new trace will be sampled.

### Reporters

* [Google Cloud Trace](https://github.com/tsloughter/oc_google_reporter): Support for v1 in master, v2 and grpc coming soon;
* [Prometheus](https://github.com/deadtrickster/opencensus-erlang-prometheus): Exports spans as Prometheus metrics.

### Working with Spans

#### Attributes

A span has a map of attributes providing details about the span. The key is a binary string and the value of the attribute can be a binary string, integer, or boolean.

```erlang
Span1 = opencensus:put_attribute(<<"/instance_id">>, <<"my-instance">>, Span),
```

#### Time Events

A time event is a timestamped annotation with user-supplied key-value pairs or a message event to represent a message (not specificly an Erlang message) sent to or received from another span.

The `message_event` consists of a type, identifier and size of the message. `Id` is an identifier for the event's message that can be used to match `SENT` and `RECEIVED` `message_event`s. For example, this field could represent a sequence ID for a streaming RPC. It is recommended to be unique within a Span. If `CompressedSize` is `0` it is assumed to be the same as `UncompressedSize`.

```erlang
Event = opencensus:message_event(?MESSAGE_EVENT_TYPE_SENT, Id, UncompressedSize, CompressedSize)
Span1 = opencensus:add_time_event(Event, Span),
```

#### Links

Links are useful in cases like a job queue. A job is created with a span context and when run wants to report a new span. The job isn't a direct child of the span that inserted it into the queue, but it is related. The job creates a link to the span that created it.

```erlang
Span = opencensus:start_span(<<"running job">>, TraceId),
Link = link(?LINK_TYPE_PARENT_LINKED_SPAN, TraceId, ParentSpanId, #{}),
Span1 = opencensus:add_link(Link, Span),
... run job ...
opencensus:finish_span(Span1).
```

### Stats

### Development

```
$ rebar3 compile
```

Running tests:

```
$ rebar3 ct
```

#### Updating OpenCensus standard protobuf encoder and decoder

Language independent interface types for Census are found in the `opencensus-proto` repo. The opencensus Erlang app provides functionality for converting from the apps internal representation to the standard protobuf interface. Below are the steps to update the Erlang module and header for encoding and decoding the protobufs:

```
$ git clone https://github.com/census-instrumentation/opencensus-proto priv/opencensus-proto
$ rebar3 protobuf compile
```
