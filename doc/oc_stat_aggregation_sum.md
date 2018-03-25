

# Module oc_stat_aggregation_sum #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Sum indicates that data collected and aggregated
with this method will be summed up.

<a name="description"></a>

## Description ##
For example, accumulated request bytes can be aggregated by using
Sum.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_sample-4">add_sample/4</a></td><td></td></tr><tr><td valign="top"><a href="#clear_rows-2">clear_rows/2</a></td><td></td></tr><tr><td valign="top"><a href="#export-2">export/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#type-0">type/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_sample-4"></a>

### add_sample/4 ###

<pre><code>
add_sample(Name::<a href="oc_stat_view.md#type-name">oc_stat_view:name()</a>, Tags::<a href="oc_tags.md#type-tags">oc_tags:tags()</a>, Value::number(), Options::any()) -&gt; ok
</code></pre>
<br />

<a name="clear_rows-2"></a>

### clear_rows/2 ###

`clear_rows(Name, Options) -> any()`

<a name="export-2"></a>

### export/2 ###

`export(Name, Options) -> any()`

<a name="init-3"></a>

### init/3 ###

`init(Name, Keys, Options) -> any()`

<a name="type-0"></a>

### type/0 ###

`type() -> any()`

