

# Module oc_sampler_probability #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This sampler assumes the lower 64 bits of the trace id are
randomly distributed around the whole (long) range.

__Behaviours:__ [`oc_sampler`](oc_sampler.md).

<a name="description"></a>

## Description ##
The sampler creates
an upper bound id based on the configured probability and compares the
lower 64 bits of the trace id to for the sampling decision.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#should_sample-4">should_sample/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

`init(Opts) -> any()`

<a name="should_sample-4"></a>

### should_sample/4 ###

`should_sample(TraceId, X2, X3, IdUpperBound) -> any()`

