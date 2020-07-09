

# Module opencensus #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

opencensus main module.

<a name="types"></a>

## Data Types ##




### <a name="type-annotation">annotation()</a> ###


<pre><code>
annotation() = #annotation{description = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a> | undefined, attributes = <a href="opencensus.md#type-attributes">opencensus:attributes()</a>}
</code></pre>




### <a name="type-attribute_value">attribute_value()</a> ###


<pre><code>
attribute_value() = any()
</code></pre>




### <a name="type-attributes">attributes()</a> ###


<pre><code>
attributes() = #{<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a> =&gt; <a href="#type-attribute_value">attribute_value()</a>}
</code></pre>




### <a name="type-link">link()</a> ###


<pre><code>
link() = #link{type = <a href="opencensus.md#type-link_type">opencensus:link_type()</a>, trace_id = <a href="opencensus.md#type-trace_id">opencensus:trace_id()</a>, span_id = <a href="opencensus.md#type-span_id">opencensus:span_id()</a>, attributes = <a href="opencensus.md#type-attributes">opencensus:attributes()</a>}
</code></pre>




### <a name="type-link_type">link_type()</a> ###


<pre><code>
link_type() = TYPE_UNSPECIFIED | CHILD_LINKED_SPAN | PARENT_LINKED_SPAN
</code></pre>




### <a name="type-links">links()</a> ###


<pre><code>
links() = [<a href="#type-link">link()</a>]
</code></pre>




### <a name="type-message_event">message_event()</a> ###


<pre><code>
message_event() = #message_event{type = <a href="opencensus.md#type-message_event_type">opencensus:message_event_type()</a>, id = integer(), uncompressed_size = integer(), compressed_size = integer()}
</code></pre>




### <a name="type-message_event_type">message_event_type()</a> ###


<pre><code>
message_event_type() = TYPE_UNSPECIFIED | SENT | RECEIVED
</code></pre>




### <a name="type-span">span()</a> ###


<pre><code>
span() = #span{name = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>, trace_id = <a href="opencensus.md#type-trace_id">opencensus:trace_id()</a> | undefined, span_id = <a href="opencensus.md#type-span_id">opencensus:span_id()</a> | undefined, parent_span_id = <a href="opencensus.md#type-span_id">opencensus:span_id()</a> | undefined, tracestate = <a href="opencensus.md#type-tracestate">opencensus:tracestate()</a> | undefined, trace_options = integer() | undefined, kind = <a href="opencensus.md#type-span_kind">opencensus:span_kind()</a>, start_time = <a href="wts.md#type-timestamp">wts:timestamp()</a>, end_time = <a href="wts.md#type-timestamp">wts:timestamp()</a> | undefined, attributes = <a href="opencensus.md#type-attributes">opencensus:attributes()</a>, stack_trace = <a href="opencensus.md#type-stack_trace">opencensus:stack_trace()</a> | undefined, links = <a href="opencensus.md#type-links">opencensus:links()</a>, time_events = <a href="opencensus.md#type-time_events">opencensus:time_events()</a>, status = <a href="opencensus.md#type-status">opencensus:status()</a> | undefined, same_process_as_parent_span = boolean() | undefined, child_span_count = integer() | undefined}
</code></pre>




### <a name="type-span_ctx">span_ctx()</a> ###


<pre><code>
span_ctx() = #span_ctx{trace_id = <a href="opencensus.md#type-trace_id">opencensus:trace_id()</a> | undefined, span_id = <a href="opencensus.md#type-span_id">opencensus:span_id()</a> | undefined, trace_options = integer() | undefined, tracestate = <a href="opencensus.md#type-tracestate">opencensus:tracestate()</a> | undefined}
</code></pre>




### <a name="type-span_id">span_id()</a> ###


<pre><code>
span_id() = non_neg_integer()
</code></pre>




### <a name="type-stack_trace">stack_trace()</a> ###


<pre><code>
stack_trace() = [<a href="erlang.md#type-stack_item">erlang:stack_item()</a>]
</code></pre>




### <a name="type-status">status()</a> ###


<pre><code>
status() = #status{code = integer(), message = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>}
</code></pre>




### <a name="type-tags">tags()</a> ###


<pre><code>
tags() = <a href="oc_tags.md#type-tags">oc_tags:tags()</a>
</code></pre>




### <a name="type-time_events">time_events()</a> ###


<pre><code>
time_events() = [{<a href="wts.md#type-timestamp">wts:timestamp()</a>, <a href="#type-annotation">annotation()</a> | <a href="#type-message_event">message_event()</a>}]
</code></pre>




### <a name="type-trace_id">trace_id()</a> ###


<pre><code>
trace_id() = non_neg_integer()
</code></pre>




### <a name="type-tracestate">tracestate()</a> ###


<pre><code>
tracestate() = #tracestate{entries = [{<a href="unicode.md#type-latin1_chardata">unicode:latin1_chardata()</a>, <a href="unicode.md#type-latin1_chardata">unicode:latin1_chardata()</a>}]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#generate_span_id-0">generate_span_id/0</a></td><td>
Generates a 64 bit random integer to use as a span id.</td></tr><tr><td valign="top"><a href="#generate_trace_id-0">generate_trace_id/0</a></td><td>
Generates a 128 bit random integer to use as a trace id.</td></tr><tr><td valign="top"><a href="#http_status_to_trace_status-1">http_status_to_trace_status/1</a></td><td>
Convert HTTP status code to Trace status code.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="generate_span_id-0"></a>

### generate_span_id/0 ###

<pre><code>
generate_span_id() -&gt; <a href="#type-span_id">span_id()</a>
</code></pre>
<br />

Generates a 64 bit random integer to use as a span id.

<a name="generate_trace_id-0"></a>

### generate_trace_id/0 ###

<pre><code>
generate_trace_id() -&gt; <a href="#type-trace_id">trace_id()</a>
</code></pre>
<br />

Generates a 128 bit random integer to use as a trace id.

<a name="http_status_to_trace_status-1"></a>

### http_status_to_trace_status/1 ###

<pre><code>
http_status_to_trace_status(S::integer()) -&gt; integer()
</code></pre>
<br />

Convert HTTP status code to Trace status code.

