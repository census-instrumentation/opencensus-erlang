

# Module oc_stat_view #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

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




### <a name="type-view_data">view_data()</a> ###


<pre><code>
view_data() = #{name =&gt; <a href="#type-name">name()</a>, description =&gt; <a href="#type-description">description()</a>, ctags =&gt; <a href="oc_tags.md#type-tags">oc_tags:tags()</a>, tags =&gt; [<a href="oc_tags.md#type-key">oc_tags:key()</a>], data =&gt; <a href="oc_stat_aggregation.md#type-data">oc_stat_aggregation:data()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#__init_backend__-0">'__init_backend__'/0</a></td><td></td></tr><tr><td valign="top"><a href="#add_sample-3">add_sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#batch_subscribe-1">batch_subscribe/1</a></td><td>
Subscribe many <code>Views</code> at once.</td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td></td></tr><tr><td valign="top"><a href="#export-1">export/1</a></td><td></td></tr><tr><td valign="top"><a href="#measure_views-1">measure_views/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-5">register/5</a></td><td></td></tr><tr><td valign="top"><a href="#registered-1">registered/1</a></td><td>
Checks whether a view <code>Name</code> is registered.</td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-5">subscribe/5</a></td><td></td></tr><tr><td valign="top"><a href="#subscribed-0">subscribed/0</a></td><td></td></tr><tr><td valign="top"><a href="#subscribed-1">subscribed/1</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="__init_backend__-0"></a>

### '__init_backend__'/0 ###

`__init_backend__() -> any()`

<a name="add_sample-3"></a>

### add_sample/3 ###

`add_sample(X1, ContextTags, Value) -> any()`

<a name="batch_subscribe-1"></a>

### batch_subscribe/1 ###

<pre><code>
batch_subscribe(Views::[<a href="#type-name">name()</a> | map()]) -&gt; ok
</code></pre>
<br />

Subscribe many `Views` at once.

<a name="deregister-1"></a>

### deregister/1 ###

<pre><code>
deregister(Name::<a href="#type-name">name()</a>) -&gt; ok
</code></pre>
<br />

<a name="export-1"></a>

### export/1 ###

<pre><code>
export(X1::tuple()) -&gt; <a href="#type-view_data">view_data()</a>
</code></pre>
<br />

<a name="measure_views-1"></a>

### measure_views/1 ###

`measure_views(Measure) -> any()`

<a name="register-5"></a>

### register/5 ###

<pre><code>
register(Name::<a href="#type-name">name()</a>, Description::<a href="#type-description">description()</a>, Tags::<a href="oc_tags.md#type-tags">oc_tags:tags()</a>, Measure::<a href="#type-measure_name">measure_name()</a>, Aggregation::<a href="#type-aggregation">aggregation()</a>) -&gt; ok
</code></pre>
<br />

<a name="registered-1"></a>

### registered/1 ###

<pre><code>
registered(Name::<a href="#type-name">name()</a>) -&gt; boolean()
</code></pre>
<br />

Checks whether a view `Name` is registered.

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(Name::map() | <a href="#type-name">name()</a>) -&gt; ok
</code></pre>
<br />

<a name="subscribe-5"></a>

### subscribe/5 ###

<pre><code>
subscribe(Name::<a href="#type-name">name()</a>, Description::<a href="#type-description">description()</a>, Tags::<a href="oc_tags.md#type-tags">oc_tags:tags()</a>, Measure::<a href="#type-measure_name">measure_name()</a>, Aggregation::<a href="#type-aggregation">aggregation()</a>) -&gt; ok
</code></pre>
<br />

<a name="subscribed-0"></a>

### subscribed/0 ###

`subscribed() -> any()`

<a name="subscribed-1"></a>

### subscribed/1 ###

`subscribed(X1) -> any()`

<a name="unsubscribe-1"></a>

### unsubscribe/1 ###

<pre><code>
unsubscribe(Name::<a href="#type-name">name()</a>) -&gt; ok
</code></pre>
<br />

