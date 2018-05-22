

# Module oc_stat_measure #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Measure represents a type of metric to be tracked and recorded.

<a name="description"></a>

## Description ##

For example, latency, request Mb/s, and response Mb/s are measures
to collect from a server.

Measure is a generic interface for recording values in aggregations
via subscribed views.
When recording a value, we have to obtain the list of all subscribed views
and call respective aggregations. We use code generation to optimize this.
When a view subscribed or unsubscribed we regenerate unrolled loop in a
special module (one for each measure). Module names generated from measurement
names (1-to-1). If we know a measure name at the compile time, we can eliminate
the module name lookup and inject remote call directly, replacing `oc_stat:record`
with `<GENERATED_MEASURE_MODULE>:record`.
For that {parse_transform, oc_stat_measure} option must be used.
<a name="types"></a>

## Data Types ##




### <a name="type-description">description()</a> ###


<pre><code>
description() = binary() | string()
</code></pre>




### <a name="type-measure">measure()</a> ###


<pre><code>
measure() = #measure{name = <a href="#type-name">name()</a>, module = module(), description = <a href="#type-description">description()</a>, unit = <a href="#type-unit">unit()</a>}
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = atom() | binary() | string()
</code></pre>




### <a name="type-unit">unit()</a> ###


<pre><code>
unit() = atom()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exists-1">exists/1</a></td><td>
Returns a measure with the <code>Name</code> or <code>false</code>..</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>
Creates and registers a measure.</td></tr><tr><td valign="top"><a href="#unit-1">unit/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exists-1"></a>

### exists/1 ###

<pre><code>
exists(Measure::<a href="#type-name">name()</a> | <a href="#type-measure">measure()</a>) -&gt; <a href="#type-measure">measure()</a> | false
</code></pre>
<br />

Returns a measure with the `Name` or `false`..

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Name::<a href="#type-name">name()</a>, Description::<a href="#type-description">description()</a>, Unit::<a href="#type-unit">unit()</a>) -&gt; <a href="oc_stat_view.md#type-measure">oc_stat_view:measure()</a>
</code></pre>
<br />

Creates and registers a measure. If a measure with the same name
already exists, old measure returned.

<a name="unit-1"></a>

### unit/1 ###

<pre><code>
unit(Measure::<a href="#type-measure">measure()</a>) -&gt; <a href="#type-unit">unit()</a>
</code></pre>
<br />

