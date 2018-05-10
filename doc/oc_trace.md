

# Module oc_trace #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The main module for working with the span in a current context.

<a name="description"></a>

## Description ##
This
module provides functions for getting the current span context from
a ctx variable, creating new spans, and manipulating the span data
for the span in the current context.
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
Create an Annotation.</td></tr><tr><td valign="top"><a href="#current_span_ctx-1">current_span_ctx/1</a></td><td>
Return the current span context in a <code>Ctx</code> or <code>undefined</code>.</td></tr><tr><td valign="top"><a href="#finish_span-1">finish_span/1</a></td><td>
Finish a span, setting the end_time.</td></tr><tr><td valign="top"><a href="#from_ctx-1">from_ctx/1</a></td><td>
Return the span context, if it exists, from Ctx.</td></tr><tr><td valign="top"><a href="#is_enabled-1">is_enabled/1</a></td><td>
Returns true if trace is enabled.</td></tr><tr><td valign="top"><a href="#link-4">link/4</a></td><td>
Create a Link which can be added to a Span.</td></tr><tr><td valign="top"><a href="#message_event-4">message_event/4</a></td><td>
Create a MessageEvent.</td></tr><tr><td valign="top"><a href="#parent_span_ctx-1">parent_span_ctx/1</a></td><td></td></tr><tr><td valign="top"><a href="#put_attribute-3">put_attribute/3</a></td><td>
Put an attribute (a key/value pair) in the attribute map of a span.</td></tr><tr><td valign="top"><a href="#put_attributes-2">put_attributes/2</a></td><td>
Merge a map of attributes with the current attributes of a span.</td></tr><tr><td valign="top"><a href="#set_status-3">set_status/3</a></td><td>
Set Status.</td></tr><tr><td valign="top"><a href="#start_span-2">start_span/2</a></td><td>
Create a new span, detached from any context.</td></tr><tr><td valign="top"><a href="#start_span-3">start_span/3</a></td><td></td></tr><tr><td valign="top"><a href="#with_child_span-2">with_child_span/2</a></td><td>
Create a child span with parent from the current context <code>Ctx</code>.</td></tr><tr><td valign="top"><a href="#with_child_span-3">with_child_span/3</a></td><td></td></tr><tr><td valign="top"><a href="#with_span_ctx-2">with_span_ctx/2</a></td><td>
Set the current span context in a context to <code>SpanCtx</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_link-2"></a>

### add_link/2 ###

<pre><code>
add_link(Link::<a href="opencensus.md#type-link">opencensus:link()</a>, SpanCtx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

Add a Link to the list of Links in the span. Returns true if the
data was successfully updated.

<a name="add_time_event-2"></a>

### add_time_event/2 ###

<pre><code>
add_time_event(TimeEvent::<a href="opencensus.md#type-annotation">opencensus:annotation()</a> | <a href="opencensus.md#type-message_event">opencensus:message_event()</a>, Span::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

Add an Annotation or MessageEvent to the list of TimeEvents in a span.
Returns true if the data was successfully updated.

<a name="add_time_event-3"></a>

### add_time_event/3 ###

<pre><code>
add_time_event(Timestamp::<a href="wts.md#type-timestamp">wts:timestamp()</a>, TimeEvent::<a href="opencensus.md#type-annotation">opencensus:annotation()</a> | <a href="opencensus.md#type-message_event">opencensus:message_event()</a>, SpanCtx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

<a name="annotation-2"></a>

### annotation/2 ###

<pre><code>
annotation(Description::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>, Attributes::<a href="opencensus.md#type-attributes">opencensus:attributes()</a>) -&gt; <a href="opencensus.md#type-annotation">opencensus:annotation()</a>
</code></pre>
<br />

Create an Annotation.

<a name="current_span_ctx-1"></a>

### current_span_ctx/1 ###

<pre><code>
current_span_ctx(Ctx::<a href="ctx.md#type-t">ctx:t()</a>) -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

Return the current span context in a `Ctx` or `undefined`.

<a name="finish_span-1"></a>

### finish_span/1 ###

<pre><code>
finish_span(Span_ctx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

Finish a span, setting the end_time.

<a name="from_ctx-1"></a>

### from_ctx/1 ###

<pre><code>
from_ctx(Ctx::<a href="ctx.md#type-t">ctx:t()</a>) -&gt; <a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>
</code></pre>
<br />

Return the span context, if it exists, from Ctx.

<a name="is_enabled-1"></a>

### is_enabled/1 ###

<pre><code>
is_enabled(Span_ctx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

Returns true if trace is enabled.

<a name="link-4"></a>

### link/4 ###

<pre><code>
link(LinkType::<a href="opencensus.md#type-link_type">opencensus:link_type()</a>, TraceId::<a href="opencensus.md#type-trace_id">opencensus:trace_id()</a>, SpanId::<a href="opencensus.md#type-span_id">opencensus:span_id()</a>, Attributes::<a href="opencensus.md#type-attributes">opencensus:attributes()</a>) -&gt; <a href="opencensus.md#type-link">opencensus:link()</a>
</code></pre>
<br />

Create a Link which can be added to a Span.

<a name="message_event-4"></a>

### message_event/4 ###

<pre><code>
message_event(MessageEventType::<a href="opencensus.md#type-message_event_type">opencensus:message_event_type()</a>, Id::integer(), UncompressedSize::integer(), CompressedSize::integer()) -&gt; <a href="opencensus.md#type-message_event">opencensus:message_event()</a>
</code></pre>
<br />

Create a MessageEvent.

<a name="parent_span_ctx-1"></a>

### parent_span_ctx/1 ###

<pre><code>
parent_span_ctx(Span_ctx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a> | <a href="opencensus.md#type-span">opencensus:span()</a>)) -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

<a name="put_attribute-3"></a>

### put_attribute/3 ###

<pre><code>
put_attribute(Key::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>, Value::<a href="opencensus.md#type-attribute_value">opencensus:attribute_value()</a>, SpanCtx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

Put an attribute (a key/value pair) in the attribute map of a span.
If the attribute already exists it is overwritten with the new value.
Returns true if the data was successfully updated.

<a name="put_attributes-2"></a>

### put_attributes/2 ###

<pre><code>
put_attributes(NewAttributes::#{<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a> =&gt; <a href="opencensus.md#type-attribute_value">opencensus:attribute_value()</a>}, SpanCtx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

Merge a map of attributes with the current attributes of a span.
The new values overwrite the old if any keys are the same.
Returns true if the data was successfully updated.

<a name="set_status-3"></a>

### set_status/3 ###

<pre><code>
set_status(Code::integer(), Message::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>, Span_ctx::<a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)) -&gt; boolean()
</code></pre>
<br />

Set Status. Returns true if the data was successfully updated.

<a name="start_span-2"></a>

### start_span/2 ###

`start_span(Name, SpanCtx) -> any()`

Create a new span, detached from any context.

<a name="start_span-3"></a>

### start_span/3 ###

<pre><code>
start_span(Name, SpanCtx, Options) -&gt; SpanCtx
</code></pre>

<ul class="definitions"><li><code>Name = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li><li><code>SpanCtx = <a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a></code></li><li><code>Options = #{remote_parent =&gt; boolean(), sampler =&gt; module(), kind =&gt; <a href="opencensus.md#type-span_kind">opencensus:span_kind()</a>, attributes =&gt; <a href="opencensus.md#type-attributes">opencensus:attributes()</a>}</code></li></ul>

<a name="with_child_span-2"></a>

### with_child_span/2 ###

<pre><code>
with_child_span(Ctx, Name) -&gt; Ctx
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="ctx.md#type-t">ctx:t()</a></code></li><li><code>Name = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li></ul>

Create a child span with parent from the current context `Ctx`. And
sets it as the current span context in `Ctx`.

<a name="with_child_span-3"></a>

### with_child_span/3 ###

<pre><code>
with_child_span(Ctx, Name, Options) -&gt; Ctx
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="ctx.md#type-t">ctx:t()</a></code></li><li><code>Name = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li><li><code>Options = #{remote_parent =&gt; boolean(), sampler =&gt; module(), attributes =&gt; <a href="opencensus.md#type-attributes">opencensus:attributes()</a>}</code></li></ul>

<a name="with_span_ctx-2"></a>

### with_span_ctx/2 ###

<pre><code>
with_span_ctx(Ctx, SpanCtx) -&gt; Ctx
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="ctx.md#type-t">ctx:t()</a></code></li><li><code>SpanCtx = <a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a></code></li></ul>

Set the current span context in a context to `SpanCtx`. Or to a new
span context with name `Name` that is the child of the span context
in `Ctx`, if it exists.

