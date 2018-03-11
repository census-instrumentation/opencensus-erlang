

# Module oc_reporter_sequential #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module allows sequential execution of multiple reporters.

<a name="types"></a>

## Data Types ##




### <a name="type-opts">opts()</a> ###


<pre><code>
opts() = [{<a href="#type-reporter">reporter()</a>, <a href="#type-reporter_opts">reporter_opts()</a>}]
</code></pre>




### <a name="type-reporter">reporter()</a> ###


<pre><code>
reporter() = atom()
</code></pre>




### <a name="type-reporter_opts">reporter_opts()</a> ###


<pre><code>
reporter_opts() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#report-2">report/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Config::[{<a href="#type-reporter">reporter()</a>, <a href="#type-reporter_opts">reporter_opts()</a>}]) -&gt; <a href="#type-opts">opts()</a>
</code></pre>
<br />

<a name="report-2"></a>

### report/2 ###

<pre><code>
report(Spans::[<a href="opencensus.md#type-spans">opencensus:spans()</a>, ...], Config::<a href="#type-opts">opts()</a>) -&gt; ok
</code></pre>
<br />

