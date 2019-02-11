

# Module ocp #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

ocp uses the pdict instead of a ctx variable for tracking context.

<a name="description"></a>

## Description ##
The functions fetch the current span context from the pdict and
passes it through to the oc_trace function of the same name.
<a name="types"></a>

## Data Types ##




### <a name="type-maybe">maybe()</a> ###


<pre><code>
maybe(T) = T | undefined
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_link-1">add_link/1</a></td><td>
Add a Link to the list of Links in the current span.</td></tr><tr><td valign="top"><a href="#add_time_event-1">add_time_event/1</a></td><td>
Add an Annotation or MessageEvent to the list of TimeEvents in the
current span.</td></tr><tr><td valign="top"><a href="#add_time_event-2">add_time_event/2</a></td><td></td></tr><tr><td valign="top"><a href="#current_span_ctx-0">current_span_ctx/0</a></td><td></td></tr><tr><td valign="top"><a href="#current_tags-0">current_tags/0</a></td><td></td></tr><tr><td valign="top"><a href="#finish_span-0">finish_span/0</a></td><td>
Finishes the span in the current pdict context.</td></tr><tr><td valign="top"><a href="#put_attribute-2">put_attribute/2</a></td><td>
Put an attribute (a key/value pair) in the attribute map of a span.</td></tr><tr><td valign="top"><a href="#put_attributes-1">put_attributes/1</a></td><td>
Merge a map of attributes with the attributes of current span.</td></tr><tr><td valign="top"><a href="#record-2">record/2</a></td><td>
Records a measurement with tags from the pdict context.</td></tr><tr><td valign="top"><a href="#set_status-2">set_status/2</a></td><td>
Set Status of current span.</td></tr><tr><td valign="top"><a href="#spawn-1">spawn/1</a></td><td>
Starts a new process using <code>erlang:spawn/1</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn-2">spawn/2</a></td><td>
Starts a new process using <code>erlang:spawn/2</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn-3">spawn/3</a></td><td>
Starts a new process using <code>erlang:spawn/3</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn-4">spawn/4</a></td><td>
Starts a new process using <code>erlang:spawn/4</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_link-1">spawn_link/1</a></td><td>
Starts a new process using <code>erlang:spawn_link/1</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_link-2">spawn_link/2</a></td><td>
Starts a new process using <code>erlang:spawn_link/2</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_link-3">spawn_link/3</a></td><td>
Starts a new process using <code>erlang:spawn_link/3</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_link-4">spawn_link/4</a></td><td>
Starts a new process using <code>erlang:spawn_link/4</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_monitor-1">spawn_monitor/1</a></td><td>
Starts a new process using <code>erlang:spawn_monitor/1</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_monitor-3">spawn_monitor/3</a></td><td>
Starts a new process using <code>erlang:spawn_monitor/3</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_opt-2">spawn_opt/2</a></td><td>
Starts a new process using <code>erlang:spawn_opt/2</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_opt-3">spawn_opt/3</a></td><td>
Starts a new process using <code>erlang:spawn_opt/3</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_opt-4">spawn_opt/4</a></td><td>
Starts a new process using <code>erlang:spawn_opt/4</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#spawn_opt-5">spawn_opt/5</a></td><td>
Starts a new process using <code>erlang:spawn_opt/5</code> with current_span_ctx
and current_tags from the calling process.</td></tr><tr><td valign="top"><a href="#update_tags-1">update_tags/1</a></td><td>
Merges the tags in the current context with a map of tags.</td></tr><tr><td valign="top"><a href="#with_child_span-1">with_child_span/1</a></td><td>
Starts a new span as a child of the current span and replaces it.</td></tr><tr><td valign="top"><a href="#with_child_span-2">with_child_span/2</a></td><td>
Starts a new span with attributes as a child of the current span
and replaces it.</td></tr><tr><td valign="top"><a href="#with_child_span-3">with_child_span/3</a></td><td>
Starts a new span as a child of the current span and uses it as the
current span while running the function <code>Fun</code>, finishing the span
and resetting the current span context after the function finishes.</td></tr><tr><td valign="top"><a href="#with_span_ctx-1">with_span_ctx/1</a></td><td>
Replaces the span in the current context.</td></tr><tr><td valign="top"><a href="#with_tags-1">with_tags/1</a></td><td>
Replaces the tags in the current context.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_link-1"></a>

### add_link/1 ###

<pre><code>
add_link(Link::<a href="opencensus.md#type-link">opencensus:link()</a>) -&gt; boolean()
</code></pre>
<br />

Add a Link to the list of Links in the current span.

<a name="add_time_event-1"></a>

### add_time_event/1 ###

<pre><code>
add_time_event(TimeEvent::<a href="opencensus.md#type-annotation">opencensus:annotation()</a> | <a href="opencensus.md#type-message_event">opencensus:message_event()</a>) -&gt; boolean()
</code></pre>
<br />

Add an Annotation or MessageEvent to the list of TimeEvents in the
current span.

<a name="add_time_event-2"></a>

### add_time_event/2 ###

<pre><code>
add_time_event(Timestamp::<a href="wts.md#type-timestamp">wts:timestamp()</a>, TimeEvent::<a href="opencensus.md#type-annotation">opencensus:annotation()</a> | <a href="opencensus.md#type-message_event">opencensus:message_event()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="current_span_ctx-0"></a>

### current_span_ctx/0 ###

<pre><code>
current_span_ctx() -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

<a name="current_tags-0"></a>

### current_tags/0 ###

<pre><code>
current_tags() -&gt; <a href="opencensus.md#type-tags">opencensus:tags()</a>
</code></pre>
<br />

<a name="finish_span-0"></a>

### finish_span/0 ###

<pre><code>
finish_span() -&gt; ok | {error, invalid_span} | {error, no_report_buffer}
</code></pre>
<br />

Finishes the span in the current pdict context.

<a name="put_attribute-2"></a>

### put_attribute/2 ###

<pre><code>
put_attribute(Key::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>, Value::<a href="opencensus.md#type-attribute_value">opencensus:attribute_value()</a>) -&gt; boolean() | {error, invalid_attribute}
</code></pre>
<br />

Put an attribute (a key/value pair) in the attribute map of a span.
If the attribute already exists it is overwritten with the new value.

<a name="put_attributes-1"></a>

### put_attributes/1 ###

<pre><code>
put_attributes(NewAttributes::#{<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a> =&gt; <a href="opencensus.md#type-attribute_value">opencensus:attribute_value()</a>}) -&gt; boolean()
</code></pre>
<br />

Merge a map of attributes with the attributes of current span.
The new values overwrite the old if any keys are the same.

<a name="record-2"></a>

### record/2 ###

<pre><code>
record(MeasureName::<a href="oc_stat_measure.md#type-name">oc_stat_measure:name()</a>, Value::number()) -&gt; ok
</code></pre>
<br />

Records a measurement with tags from the pdict context.

Raises `{unknown_measure, MeasureName}` if measure doesn't exist.

<a name="set_status-2"></a>

### set_status/2 ###

<pre><code>
set_status(Code::integer(), Message::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>) -&gt; boolean()
</code></pre>
<br />

Set Status of current span.

<a name="spawn-1"></a>

### spawn/1 ###

`spawn(Fun) -> any()`

Starts a new process using `erlang:spawn/1` with current_span_ctx
and current_tags from the calling process.

<a name="spawn-2"></a>

### spawn/2 ###

`spawn(Node, Fun) -> any()`

Starts a new process using `erlang:spawn/2` with current_span_ctx
and current_tags from the calling process.

<a name="spawn-3"></a>

### spawn/3 ###

`spawn(M, F, A) -> any()`

Starts a new process using `erlang:spawn/3` with current_span_ctx
and current_tags from the calling process.

<a name="spawn-4"></a>

### spawn/4 ###

`spawn(Node, M, F, A) -> any()`

Starts a new process using `erlang:spawn/4` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_link-1"></a>

### spawn_link/1 ###

`spawn_link(Fun) -> any()`

Starts a new process using `erlang:spawn_link/1` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_link-2"></a>

### spawn_link/2 ###

`spawn_link(Node, Fun) -> any()`

Starts a new process using `erlang:spawn_link/2` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_link-3"></a>

### spawn_link/3 ###

`spawn_link(M, F, A) -> any()`

Starts a new process using `erlang:spawn_link/3` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_link-4"></a>

### spawn_link/4 ###

`spawn_link(Node, M, F, A) -> any()`

Starts a new process using `erlang:spawn_link/4` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_monitor-1"></a>

### spawn_monitor/1 ###

`spawn_monitor(Fun) -> any()`

Starts a new process using `erlang:spawn_monitor/1` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_monitor-3"></a>

### spawn_monitor/3 ###

`spawn_monitor(M, F, A) -> any()`

Starts a new process using `erlang:spawn_monitor/3` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_opt-2"></a>

### spawn_opt/2 ###

`spawn_opt(Fun, Opt) -> any()`

Starts a new process using `erlang:spawn_opt/2` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_opt-3"></a>

### spawn_opt/3 ###

`spawn_opt(Node, Fun, Opt) -> any()`

Starts a new process using `erlang:spawn_opt/3` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_opt-4"></a>

### spawn_opt/4 ###

`spawn_opt(M, F, A, Opt) -> any()`

Starts a new process using `erlang:spawn_opt/4` with current_span_ctx
and current_tags from the calling process.

<a name="spawn_opt-5"></a>

### spawn_opt/5 ###

`spawn_opt(Node, M, F, A, Opt) -> any()`

Starts a new process using `erlang:spawn_opt/5` with current_span_ctx
and current_tags from the calling process.

<a name="update_tags-1"></a>

### update_tags/1 ###

<pre><code>
update_tags(Map::<a href="maps.md#type-map">maps:map()</a>) -&gt; <a href="opencensus.md#type-tags">opencensus:tags()</a>
</code></pre>
<br />

Merges the tags in the current context with a map of tags.

<a name="with_child_span-1"></a>

### with_child_span/1 ###

<pre><code>
with_child_span(Name::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>) -&gt; <a href="opencensus.md#type-maybe">opencensus:maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

Starts a new span as a child of the current span and replaces it.

<a name="with_child_span-2"></a>

### with_child_span/2 ###

<pre><code>
with_child_span(Name::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>, Attributes::<a href="opencensus.md#type-attributes">opencensus:attributes()</a>) -&gt; <a href="opencensus.md#type-maybe">opencensus:maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

Starts a new span with attributes as a child of the current span
and replaces it.

<a name="with_child_span-3"></a>

### with_child_span/3 ###

<pre><code>
with_child_span(Name::<a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a>, Attributes::<a href="opencensus.md#type-attributes">opencensus:attributes()</a>, Fun::function()) -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

Starts a new span as a child of the current span and uses it as the
current span while running the function `Fun`, finishing the span
and resetting the current span context after the function finishes.

<a name="with_span_ctx-1"></a>

### with_span_ctx/1 ###

<pre><code>
with_span_ctx(SpanCtx::<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>) -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

Replaces the span in the current context.

<a name="with_tags-1"></a>

### with_tags/1 ###

<pre><code>
with_tags(Map::<a href="opencensus.md#type-tags">opencensus:tags()</a>) -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-tags">opencensus:tags()</a>)
</code></pre>
<br />

Replaces the tags in the current context.

