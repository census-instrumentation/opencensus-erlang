

# Module oc_tag_ctx_header #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Functions to support sending tags over http as an http header.

<a name="types"></a>

## Data Types ##




### <a name="type-maybe">maybe()</a> ###


<pre><code>
maybe(T) = T | undefined
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#field_name-0">field_name/0</a></td><td></td></tr><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Thing::iodata()) -&gt; {ok, <a href="oc_tags.md#type-tags">oc_tags:tags()</a>} | {error, any()}
</code></pre>
<br />

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Tags::<a href="oc_tags.md#type-tags">oc_tags:tags()</a>) -&gt; <a href="#type-maybe">maybe</a>(iodata())
</code></pre>
<br />

<a name="field_name-0"></a>

### field_name/0 ###

`field_name() -> any()`

<a name="format_error-1"></a>

### format_error/1 ###

`format_error(X1) -> any()`

