

# Module oc_stat #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

OpenCensus Stats package.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#export-0">export/0</a></td><td>Exports view_data of all subscribed views.</td></tr><tr><td valign="top"><a href="#record-2">record/2</a></td><td>
Records multiple measurements at once.</td></tr><tr><td valign="top"><a href="#record-3">record/3</a></td><td>
Records one or multiple measurements with the same tags at once.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="export-0"></a>

### export/0 ###

<pre><code>
export() -&gt; [<a href="oc_stat_view.md#type-view_data">oc_stat_view:view_data()</a>]
</code></pre>
<br />

Exports view_data of all subscribed views

<a name="record-2"></a>

### record/2 ###

<pre><code>
record(Tags::<a href="ctx.md#type-t">ctx:t()</a> | <a href="oc_tags.md#type-tags">oc_tags:tags()</a>, Measures::[{<a href="oc_stat_measure.md#type-name">oc_stat_measure:name()</a>, number()}]) -&gt; ok
</code></pre>
<br />

Records multiple measurements at once.

Can be optimized with `oc_stat_measure` parse transform.

Raises `{unknown_measure, MeasureName}` if measure doesn't exist.

<a name="record-3"></a>

### record/3 ###

<pre><code>
record(Tags::<a href="ctx.md#type-t">ctx:t()</a> | <a href="oc_tags.md#type-tags">oc_tags:tags()</a>, MeasureName::<a href="oc_stat_measure.md#type-name">oc_stat_measure:name()</a>, Value::number()) -&gt; ok
</code></pre>
<br />

Records one or multiple measurements with the same tags at once.
If there are any tags in the context, measurements will be tagged with them.

Can be optimized with `oc_stat_measure` parse transform.

Raises `{unknown_measure, MeasureName}` if measure doesn't exist.

