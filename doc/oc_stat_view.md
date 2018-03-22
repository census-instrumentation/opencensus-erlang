

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




### <a name="type-v_s">v_s()</a> ###


<pre><code>
v_s() = #v_s{name = <a href="#type-name">name()</a>, tags = [<a href="oc_tags.md#type-key">oc_tags:key()</a>], aggregation = <a href="#type-aggregation">aggregation()</a>, aggregation_options = <a href="#type-aggregation_options">aggregation_options()</a>}
</code></pre>




### <a name="type-view">view()</a> ###


<pre><code>
view() = #view{name = <a href="#type-name">name()</a> | _, measure = <a href="#type-measure_name">measure_name()</a> | _, subscribed = boolean(), description = <a href="#type-description">description()</a> | _, ctags = <a href="oc_tags.md#type-tags">oc_tags:tags()</a> | _, tags = [<a href="oc_tags.md#type-key">oc_tags:key()</a>] | _, aggregation = <a href="#type-aggregation">aggregation()</a> | _, aggregation_options = <a href="#type-aggregation_options">aggregation_options()</a> | _}
</code></pre>




### <a name="type-view_data">view_data()</a> ###


<pre><code>
view_data() = #{name =&gt; <a href="#type-name">name()</a>, description =&gt; <a href="#type-description">description()</a>, ctags =&gt; <a href="oc_tags.md#type-tags">oc_tags:tags()</a>, tags =&gt; [<a href="oc_tags.md#type-key">oc_tags:key()</a>], data =&gt; <a href="oc_stat_aggregation.md#type-data">oc_stat_aggregation:data()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#__init_backend__-0">'__init_backend__'/0</a></td><td></td></tr><tr><td valign="top"><a href="#add_sample-3">add_sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#all_subscribed-0">all_subscribed/0</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td></td></tr><tr><td valign="top"><a href="#export-1">export/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_registered-1">is_registered/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_subscribed-1">is_subscribed/1</a></td><td></td></tr><tr><td valign="top"><a href="#measure_views-1">measure_views/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-5">new/5</a></td><td></td></tr><tr><td valign="top"><a href="#preload-1">preload/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-5">register/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-5">subscribe/5</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="__init_backend__-0"></a>

### '__init_backend__'/0 ###

`__init_backend__() -> any()`

<a name="add_sample-3"></a>

### add_sample/3 ###

<pre><code>
add_sample(ViewSub::<a href="#type-v_s">v_s()</a>, ContextTags::<a href="oc_tags.md#type-tags">oc_tags:tags()</a>, Value::number()) -&gt; ok
</code></pre>
<br />

<a name="all_subscribed-0"></a>

### all_subscribed/0 ###

`all_subscribed() -> any()`

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="deregister-1"></a>

### deregister/1 ###

<pre><code>
deregister(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; ok
</code></pre>
<br />

<a name="export-1"></a>

### export/1 ###

<pre><code>
export(View::<a href="#type-view">view()</a>) -&gt; <a href="#type-view_data">view_data()</a>
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(X1, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(Args) -> any()`

<a name="is_registered-1"></a>

### is_registered/1 ###

<pre><code>
is_registered(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_subscribed-1"></a>

### is_subscribed/1 ###

<pre><code>
is_subscribed(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="measure_views-1"></a>

### measure_views/1 ###

`measure_views(Measure) -> any()`

<a name="new-1"></a>

### new/1 ###

`new(Map) -> any()`

<a name="new-5"></a>

### new/5 ###

`new(Name, Measure, Description, Tags, Aggregation) -> any()`

<a name="preload-1"></a>

### preload/1 ###

`preload(List) -> any()`

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(View::<a href="#type-view">view()</a>) -&gt; {ok, #view{name = <a href="#type-name">name()</a> | _, measure = <a href="#type-measure_name">measure_name()</a> | _, subscribed = boolean(), description = <a href="#type-description">description()</a> | _, ctags = <a href="oc_tags.md#type-tags">oc_tags:tags()</a> | _, tags = [<a href="oc_tags.md#type-key">oc_tags:key()</a>] | _, aggregation = <a href="#type-aggregation">aggregation()</a> | _, aggregation_options = <a href="#type-aggregation_options">aggregation_options()</a> | _}} | {error, any()}
</code></pre>
<br />

<a name="register-5"></a>

### register/5 ###

`register(Name, Measure, Description, Tags, Aggregation) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; {ok, #view{name = <a href="#type-name">name()</a> | _, measure = <a href="#type-measure_name">measure_name()</a> | _, subscribed = boolean(), description = <a href="#type-description">description()</a> | _, ctags = <a href="oc_tags.md#type-tags">oc_tags:tags()</a> | _, tags = [<a href="oc_tags.md#type-key">oc_tags:key()</a>] | _, aggregation = <a href="#type-aggregation">aggregation()</a> | _, aggregation_options = <a href="#type-aggregation_options">aggregation_options()</a> | _}} | {error, any()}
</code></pre>
<br />

<a name="subscribe-5"></a>

### subscribe/5 ###

`subscribe(Name, Measure, Description, Tags, Aggregation) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(X1, X2) -> any()`

<a name="unsubscribe-1"></a>

### unsubscribe/1 ###

<pre><code>
unsubscribe(View::<a href="#type-name">name()</a> | <a href="#type-view">view()</a>) -&gt; ok
</code></pre>
<br />

