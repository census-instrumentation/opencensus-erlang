

# Module oc_sampler_period_or_count #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This sampler makes sure you will have at least 1 sample during `period`
or `1/count` samples otherwise.

__Behaviours:__ [`oc_sampler`](oc_sampler.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#should_sample-4">should_sample/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

`init(Opts) -> any()`

<a name="should_sample-4"></a>

### should_sample/4 ###

`should_sample(TraceId, X2, X3, X4) -> any()`

