

# Module oc_stat_exporter #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Exporter exports the collected records as view data.

__This module defines the `oc_stat_exporter` behaviour.__<br /> Required callback functions: `export/2`.

<a name="types"></a>

## Data Types ##




### <a name="type-exporter">exporter()</a> ###


<pre><code>
exporter() = module()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#batch_register-1">batch_register/1</a></td><td>
Register many <code>Exporters</code> at once.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td>
Deregisters an <code>Exporter</code>.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>Equivalent to <a href="#register-2"><tt>register(Exporter, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td>
Registers an <code>Exporter</code> with <code>Config</code>.</td></tr><tr><td valign="top"><a href="#registered-1">registered/1</a></td><td>
Checks whether <code>Exporter</code> is registered.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="batch_register-1"></a>

### batch_register/1 ###

<pre><code>
batch_register(Exporters) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Exporters = [<a href="#type-exporter">exporter()</a>]</code></li></ul>

Register many `Exporters` at once.

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="deregister-1"></a>

### deregister/1 ###

<pre><code>
deregister(Exporter) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Exporter = <a href="#type-exporter">exporter()</a></code></li></ul>

Deregisters an `Exporter`.

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

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(Exporter) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Exporter = <a href="#type-exporter">exporter()</a></code></li></ul>

Equivalent to [`register(Exporter, [])`](#register-2).

<a name="register-2"></a>

### register/2 ###

<pre><code>
register(Exporter, Config) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Exporter = <a href="#type-exporter">exporter()</a></code></li><li><code>Config = any()</code></li></ul>

Registers an `Exporter` with `Config`.
Collected data will be reported via all the
registered exporters. Once you no longer
want data to be exported, invoke [`deregister/1`](#deregister-1)
with the previously registered exporter.

<a name="registered-1"></a>

### registered/1 ###

<pre><code>
registered(Exporter) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Exporter = <a href="#type-exporter">exporter()</a></code></li></ul>

Checks whether `Exporter` is registered.

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(X1, State) -> any()`

