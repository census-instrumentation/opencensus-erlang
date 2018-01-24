## Erlang stats collection and distributed tracing framework

[![CircleCI](https://circleci.com/gh/census-instrumentation/opencensus-erlang.svg?style=svg)](https://circleci.com/gh/census-instrumentation/opencensus-erlang)
[![Coverage Status](https://coveralls.io/repos/github/census-instrumentation/opencensus-erlang/badge.svg?branch=master)](https://coveralls.io/github/census-instrumentation/opencensus-erlang?branch=master)
[![Hex.pm](https://img.shields.io/hexpm/v//opencensus.svg?maxAge=2592000)](https://hex.pm/packages/opencensus)
[![Hex.pm](https://img.shields.io/hexpm/dt/opencensus.svg?maxAge=2592000)](https://hex.pm/packages/opencensus)

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

### Attributes

### Time Events

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
