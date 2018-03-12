

# Module oc_span_ctx_header #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Functions to support the http header format of the tracecontext spec
Implements the spec found here.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#field_name-0">field_name/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(TraceContext::iodata()) -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Span_ctx::<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>) -&gt; <a href="#type-maybe">maybe</a>(iolist())
</code></pre>
<br />

<a name="field_name-0"></a>

### field_name/0 ###

`field_name() -> any()`

