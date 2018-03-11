

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
annotation() = #annotation{}
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
link() = #link{}
</code></pre>




### <a name="type-link_type">link_type()</a> ###


<pre><code>
link_type() = ?LINK_TYPE_UNSPECIFIED | ?LINK_TYPE_CHILD_LINKED_SPAN | ?LINK_TYPE_PARENT_LINKED_SPAN
</code></pre>




### <a name="type-links">links()</a> ###


<pre><code>
links() = [<a href="#type-link">link()</a>]
</code></pre>




### <a name="type-message_event">message_event()</a> ###


<pre><code>
message_event() = #message_event{}
</code></pre>




### <a name="type-message_event_type">message_event_type()</a> ###


<pre><code>
message_event_type() = ?MESSAGE_EVENT_TYPE_UNSPECIFIED | ?MESSAGE_EVENT_TYPE_SENT | ?MESSAGE_EVENT_TYPE_RECEIVED
</code></pre>




### <a name="type-span">span()</a> ###


<pre><code>
span() = #span{}
</code></pre>




### <a name="type-span_ctx">span_ctx()</a> ###


<pre><code>
span_ctx() = #span_ctx{}
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
status() = #status{}
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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#generate_span_id-0">generate_span_id/0</a></td><td>
Generates a 64 bit random integer to use as a span id.</td></tr><tr><td valign="top"><a href="#generate_trace_id-0">generate_trace_id/0</a></td><td>
Generates a 128 bit random integer to use as a trace id.</td></tr></table>


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

