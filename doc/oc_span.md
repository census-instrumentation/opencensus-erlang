

# Module oc_span #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Functions for functional manipulation of span data.

<a name="types"></a>

## Data Types ##




### <a name="type-maybe">maybe()</a> ###


<pre><code>
maybe(T) = T | undefined
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_link-2">add_link/2</a></td><td>
Add a Link to the list of Links in the span.</td></tr><tr><td valign="top"><a href="#add_time_event-2">add_time_event/2</a></td><td>
Add an Annotation or MessageEvent to the list of TimeEvents in a span.</td></tr><tr><td valign="top"><a href="#add_time_event-3">add_time_event/3</a></td><td></td></tr><tr><td valign="top"><a href="#annotation-2">annotation/2</a></td><td>
Create an Annotation.</td></tr><tr><td valign="top"><a href="#finish_span-2">finish_span/2</a></td><td>
Finish a span, setting the end_time and sending to the reporter.</td></tr><tr><td valign="top"><a href="#link-4">link/4</a></td><td>
Create a Link which can be added to a Span.</td></tr><tr><td valign="top"><a href="#message_event-4">message_event/4</a></td><td>
Create a MessageEvent.</td></tr><tr><td valign="top"><a href="#put_attribute-3">put_attribute/3</a></td><td>
Put an attribute (a key/value pair) in the attribute map of a span.</td></tr><tr><td valign="top"><a href="#put_attributes-2">put_attributes/2</a></td><td>
Merge a map of attributes with the current attributes of a span.</td></tr><tr><td valign="top"><a href="#set_status-3">set_status/3</a></td><td>
Set Status.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_link-2"></a>

### add_link/2 ###

<pre><code>
add_link(Link, Span) -&gt; Span
</code></pre>

<ul class="definitions"><li><code>Link = <a href="opencensus.md#type-link">opencensus:link()</a></code></li><li><code>Span = <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span">opencensus:span()</a>)</code></li></ul>

Add a Link to the list of Links in the span.

<a name="add_time_event-2"></a>

### add_time_event/2 ###

<pre><code>
add_time_event(TimeEvent, Span) -&gt; Span
</code></pre>

<ul class="definitions"><li><code>TimeEvent = <a href="opencensus.md#type-annotation">opencensus:annotation()</a> | <a href="opencensus.md#type-message_event">opencensus:message_event()</a></code></li><li><code>Span = <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span">opencensus:span()</a>)</code></li></ul>

Add an Annotation or MessageEvent to the list of TimeEvents in a span.

<a name="add_time_event-3"></a>

### add_time_event/3 ###

`add_time_event(Timestamp, TimeEvent, Span) -> any()`

<a name="annotation-2"></a>

### annotation/2 ###

<pre><code>
annotation(Description, Attributes) -&gt; Annotation
</code></pre>

<ul class="definitions"><li><code>Description = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li><li><code>Attributes = <a href="opencensus.md#type-attributes">opencensus:attributes()</a></code></li><li><code>Annotation = <a href="opencensus.md#type-annotation">opencensus:annotation()</a></code></li></ul>

Create an Annotation.

<a name="finish_span-2"></a>

### finish_span/2 ###

<pre><code>
finish_span(Span_ctx::<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>, Span::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span">opencensus:span()</a>)) -&gt; true
</code></pre>
<br />

Finish a span, setting the end_time and sending to the reporter.

<a name="link-4"></a>

### link/4 ###

<pre><code>
link(LinkType, TraceId, SpanId, Attributes) -&gt; Link
</code></pre>

<ul class="definitions"><li><code>LinkType = <a href="opencensus.md#type-link_type">opencensus:link_type()</a></code></li><li><code>TraceId = <a href="opencensus.md#type-trace_id">opencensus:trace_id()</a></code></li><li><code>SpanId = <a href="opencensus.md#type-span_id">opencensus:span_id()</a></code></li><li><code>Attributes = <a href="opencensus.md#type-attributes">opencensus:attributes()</a></code></li><li><code>Link = <a href="opencensus.md#type-link">opencensus:link()</a></code></li></ul>

Create a Link which can be added to a Span.

<a name="message_event-4"></a>

### message_event/4 ###

<pre><code>
message_event(MessageEventType, Id, UncompressedSize, CompressedSize) -&gt; MessageEvent
</code></pre>

<ul class="definitions"><li><code>MessageEventType = <a href="opencensus.md#type-message_event_type">opencensus:message_event_type()</a></code></li><li><code>Id = integer()</code></li><li><code>UncompressedSize = integer()</code></li><li><code>CompressedSize = integer()</code></li><li><code>MessageEvent = <a href="opencensus.md#type-message_event">opencensus:message_event()</a></code></li></ul>

Create a MessageEvent.

<a name="put_attribute-3"></a>

### put_attribute/3 ###

<pre><code>
put_attribute(Key, Value, Span) -&gt; Span
</code></pre>

<ul class="definitions"><li><code>Key = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li><li><code>Value = <a href="opencensus.md#type-attribute_value">opencensus:attribute_value()</a></code></li><li><code>Span = <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span">opencensus:span()</a>)</code></li></ul>

Put an attribute (a key/value pair) in the attribute map of a span.
If the attribute already exists it is overwritten with the new value.

<a name="put_attributes-2"></a>

### put_attributes/2 ###

<pre><code>
put_attributes(Attributes, Span) -&gt; Span
</code></pre>

<ul class="definitions"><li><code>Attributes = #{<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a> =&gt; <a href="opencensus.md#type-attribute_value">opencensus:attribute_value()</a>}</code></li><li><code>Span = <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span">opencensus:span()</a>)</code></li></ul>

Merge a map of attributes with the current attributes of a span.
The new values overwrite the old if any keys are the same.

<a name="set_status-3"></a>

### set_status/3 ###

<pre><code>
set_status(Code, Message, Span) -&gt; Span
</code></pre>

<ul class="definitions"><li><code>Code = integer()</code></li><li><code>Message = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li><li><code>Span = <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span">opencensus:span()</a>)</code></li></ul>

Set Status.

