

# Module oc_trace_reporter #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module has the behaviour that each reporter must implement
and creates the buffer of trace spans to be reported.

__Behaviours:__ [`oc_internal_timer`](oc_internal_timer.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ping-0">ping/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#store_span-1">store_span/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ping-0"></a>

### ping/0 ###

`ping() -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Handlers) -> any()`

<a name="store_span-1"></a>

### store_span/1 ###

<pre><code>
store_span(Span::<a href="opencensus.md#type-span">opencensus:span()</a>) -&gt; ok | {error, invalid_span} | {error, no_report_buffer}
</code></pre>
<br />

