

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
<a name="types"></a>

## Data Types ##




### <a name="type-description">description()</a> ###


<pre><code>
description() = binary() | string()
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-3">new/3</a></td><td>
Creates and registers a measure.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Name::<a href="#type-name">name()</a>, Description::<a href="#type-description">description()</a>, Unit::<a href="#type-unit">unit()</a>) -&gt; <a href="oc_stat_view.md#type-measure">oc_stat_view:measure()</a>
</code></pre>
<br />

Creates and registers a measure. If a measure with the same name
already exists, old measure returned.

