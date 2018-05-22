

# Module oc_stat_view #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

View allows users to aggregate the recorded Measurements.

<a name="description"></a>

## Description ##
Views need to be passed to the subscribe function to be before data will be
collected and sent to exporters.
<a name="types"></a>

## Data Types ##




### <a name="type-aggregation">aggregation()</a> ###


<pre><code>
aggregation() = module()
</code></pre>




### <a name="type-aggregation_options">aggregation_options()</a> ###


<pre><code>
aggregation_options() = any()
</code></pre>




### <a name="type-description">description()</a> ###


<pre><code>
description() = binary() | string()
</code></pre>




### <a name="type-measure_name">measure_name()</a> ###


<pre><code>
measure_name() = atom() | binary() | string()
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = atom() | binary() | string()
</code></pre>




### <a name="type-view">view()</a> ###


<pre><code>
view() = #view{name = <a href="#type-name">name()</a> | _, measure = <a href="oc_stat_measure.md#type-measure">oc_stat_measure:measure()</a> | <a href="#type-measure_name">measure_name()</a> | _, unit = <a href="oc_stat_measure.md#type-unit">oc_stat_measure:unit()</a>, subscribed = boolean(), description = <a href="#type-description">description()</a> | _, ctags = <a href="oc_tags.md#type-tags">oc_tags:tags()</a> | _, tags = [<a href="oc_tags.md#type-key">oc_tags:key()</a>] | _, aggregation = <a href="#type-aggregation">aggregation()</a> | _, aggregation_options = <a href="#type-aggregation_options">aggregation_options()</a> | _}
</code></pre>




### <a name="type-view_data">view_data()</a> ###


<pre><code>
view_data() = #{name =&gt; <a href="#type-name">name()</a>, description =&gt; <a href="#type-description">description()</a>, ctags =&gt; <a href="oc_tags.md#type-tags">oc_tags:tags()</a>, tags =&gt; [<a href="oc_tags.md#type-key">oc_tags:key()</a>], data =&gt; <a href="oc_stat_aggregation.md#type-data">oc_stat_aggregation:data()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td>
Deregisters the view.</td></tr><tr><td valign="top"><a href="#export-1">export/1</a></td><td>
Returns a snapshot of the View's data.</td></tr><tr><td valign="top"><a href="#is_registered-1">is_registered/1</a></td><td>
Returns true if the view is registered.</td></tr><tr><td valign="top"><a href="#is_subscribed-1">is_subscribed/1</a></td><td>
Returns true if the view is exporting data by subscription.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>
Creates a View from a map.</td></tr><tr><td valign="top"><a href="#new-5">new/5</a></td><td>
Creates a View.</td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>
Registers the view.</td></tr><tr><td valign="top"><a href="#register-5">register/5</a></td><td>
Registers the view created from arguments.</td></tr><tr><td valign="top"><a href="#register-6">register/6</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td>
Subscribe the View, When subscribed, a view can aggregate measure data and export it.</td></tr><tr><td valign="top"><a href="#subscribe-5">subscribe/5</a></td><td>
A shortcut: Creates, Registers, and Subscribes a view in one call.</td></tr><tr><td valign="top"><a href="#subscribe-6">subscribe/6</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td>
Unsubscribes the View.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deregister-1"></a>

### deregister/1 ###

<pre><code>
deregister(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; ok
</code></pre>
<br />

Deregisters the view. If subscribed, unsubscribes and therefore
existing aggregation data cleared.

Returns `{error, {unknown_view, Name}}` if the view is unknown.

<a name="export-1"></a>

### export/1 ###

<pre><code>
export(View::<a href="#type-view">view()</a>) -&gt; <a href="#type-view_data">view_data()</a>
</code></pre>
<br />

Returns a snapshot of the View's data.

<a name="is_registered-1"></a>

### is_registered/1 ###

<pre><code>
is_registered(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; boolean()
</code></pre>
<br />

Returns true if the view is registered.

<a name="is_subscribed-1"></a>

### is_subscribed/1 ###

<pre><code>
is_subscribed(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; boolean()
</code></pre>
<br />

Returns true if the view is exporting data by subscription.

<a name="new-1"></a>

### new/1 ###

`new(Map) -> any()`

Creates a View from a map.

<a name="new-5"></a>

### new/5 ###

`new(Name, Measure, Description, Tags, Aggregation) -> any()`

Creates a View. This view needs to be registered and subscribed to a measure
in order to start aggregating data.

<a name="new-6"></a>

### new/6 ###

`new(Name, Measure, Unit, Description, Tags, Aggregation) -> any()`

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(View::<a href="#type-view">view()</a>) -&gt; {ok, <a href="#type-view">view()</a>} | {error, any()}
</code></pre>
<br />

Registers the view. Aggregation initialized with AggregationOptions.

Returns `{error, {unknown_measure, Measure}}` if `Measure` is unknown.<br />
Returns `{error, {view_already_exists, Name}}` if a
view with `Name` already registered.

<a name="register-5"></a>

### register/5 ###

`register(Name, Measure, Description, Tags, Aggregation) -> any()`

Registers the view created from arguments.

Returns `{error, {unknown_measure, Measure}}` if `Measure` is unknown.<br />
Returns `{error, {view_already_exists, Name}}` if a
view with `Name` already registered.

<a name="register-6"></a>

### register/6 ###

`register(Name, Measure, Unit, Description, Tags, Aggregation) -> any()`

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(Map::<a href="#type-name">name()</a> | <a href="#type-view">view()</a> | map()) -&gt; {ok, <a href="#type-view">view()</a>} | {error, any()}
</code></pre>
<br />

Subscribe the View, When subscribed, a view can aggregate measure data and export it.

Returns `{error, {unknown_view, Name}}` if `Name` view is unknown.

<a name="subscribe-5"></a>

### subscribe/5 ###

`subscribe(Name, Measure, Description, Tags, Aggregation) -> any()`

A shortcut: Creates, Registers, and Subscribes a view in one call.

Returns `{error, {unknown_measure, Measure}}` if `Measure` is unknown.

<a name="subscribe-6"></a>

### subscribe/6 ###

`subscribe(Name, Measure, Unit, Description, Tags, Aggregation) -> any()`

<a name="unsubscribe-1"></a>

### unsubscribe/1 ###

<pre><code>
unsubscribe(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; ok
</code></pre>
<br />

Unsubscribes the View. When unsubscribed a view no longer aggregates measure data
and exports it. Also all existing aggregation data cleared.

Returns `{error, {unknown_view, Name}}` if `Name` view is unknown.

