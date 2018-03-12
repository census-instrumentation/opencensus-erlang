

# Module oc_sampler #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Behaviour each sampler must implement.

__This module defines the `oc_sampler` behaviour.__<br /> Required callback functions: `init/1`, `should_sample/4`.

<a name="description"></a>

## Description ##
The function `should_trace`
is given the trace id of the current trace, if one exists, the span id
of what would be the parent of any new span in this trace and a boolean
for whether the trace was enabled in the process that propagated this
context.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td>Called at the start of a trace.</td></tr><tr><td valign="top"><a href="#should_sample-3">should_sample/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

Called at the start of a trace.

<a name="should_sample-3"></a>

### should_sample/3 ###

`should_sample(TraceId, SpanId, Enabled) -> any()`

