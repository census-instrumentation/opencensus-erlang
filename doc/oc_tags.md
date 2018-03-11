

# Module oc_tags #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = atom() | binary() | <a href="unicode.md#type-latin1_charlist">unicode:latin1_charlist()</a>
</code></pre>




### <a name="type-tags">tags()</a> ###


<pre><code>
tags() = #{<a href="#type-key">key()</a> =&gt; <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = binary() | <a href="unicode.md#type-latin1_charlist">unicode:latin1_charlist()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_ctx-1">from_ctx/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_ctx-2">new_ctx/2</a></td><td></td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#verify_key-1">verify_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#verify_value-1">verify_value/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format_error-1"></a>

### format_error/1 ###

`format_error(X1) -> any()`

<a name="from_ctx-1"></a>

### from_ctx/1 ###

<pre><code>
from_ctx(Ctx::<a href="ctx.md#type-t">ctx:t()</a>) -&gt; <a href="#type-tags">tags()</a>
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-tags">tags()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Map::<a href="maps.md#type-map">maps:map()</a>) -&gt; <a href="#type-tags">tags()</a>
</code></pre>
<br />

<a name="new_ctx-2"></a>

### new_ctx/2 ###

<pre><code>
new_ctx(Ctx::<a href="ctx.md#type-t">ctx:t()</a>, Tags::<a href="#type-tags">tags()</a>) -&gt; <a href="ctx.md#type-t">ctx:t()</a>
</code></pre>
<br />

<a name="put-3"></a>

### put/3 ###

<pre><code>
put(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Tags::<a href="#type-tags">tags()</a>) -&gt; {ok, <a href="#type-tags">tags()</a>} | {error, term()}
</code></pre>
<br />

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Tags::<a href="#type-tags">tags()</a>) -&gt; <a href="maps.md#type-map">maps:map()</a>
</code></pre>
<br />

<a name="verify_key-1"></a>

### verify_key/1 ###

`verify_key(Key) -> any()`

<a name="verify_value-1"></a>

### verify_value/1 ###

`verify_value(Value) -> any()`

