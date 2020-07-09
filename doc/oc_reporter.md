

# Module oc_reporter #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module has the behaviour that each reporter must implement
and creates the buffer of trace spans to be reported.

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `oc_reporter` behaviour.__<br /> Required callback functions: `init/1`, `report/2`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>Equivalent to <a href="#register-2"><tt>register(Reporter, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td>
Register new traces reporter <code>Reporter</code> with <code>Config</code>.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#store_span-1">store_span/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(X1, State, X3) -> any()`

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

`register(Reporter) -> any()`

Equivalent to [`register(Reporter, [])`](#register-2).

<a name="register-2"></a>

### register/2 ###

<pre><code>
register(Reporter::module(), Options::term()) -&gt; ok
</code></pre>
<br />

Register new traces reporter `Reporter` with `Config`.

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="store_span-1"></a>

### store_span/1 ###

<pre><code>
store_span(Span::<a href="opencensus.md#type-span">opencensus:span()</a>) -&gt; true | {error, invalid_span} | {error, no_report_buffer}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(X1, State) -> any()`

