

# Module oc_span_ctx_binary #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Functions to support the binary format trace context serialization.

<a name="description"></a>

## Description ##
Implements the spec found here
[`https://github.com/census-instrumentation/opencensus-specs/blob/7b426409/encodings/BinaryEncoding.md`](https://github.com/census-instrumentation/opencensus-specs/blob/7b426409/encodings/BinaryEncoding.md)
<a name="types"></a>

## Data Types ##




### <a name="type-maybe">maybe()</a> ###


<pre><code>
maybe(T) = T | undefined
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(X1::binary()) -&gt; <a href="#type-maybe">maybe</a>(<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>)
</code></pre>
<br />

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Span_ctx::<a href="opencensus.md#type-span_ctx">opencensus:span_ctx()</a>) -&gt; <a href="#type-maybe">maybe</a>(binary())
</code></pre>
<br />

