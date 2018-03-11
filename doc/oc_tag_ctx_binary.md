

# Module oc_tag_ctx_binary #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(X1::binary()) -&gt; {ok, #{}} | {error, any()}
</code></pre>
<br />

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(TagContext::#{}) -&gt; {ok, iolist()} | {error, any()}
</code></pre>
<br />

<a name="format_error-1"></a>

### format_error/1 ###

`format_error(X1) -> any()`

