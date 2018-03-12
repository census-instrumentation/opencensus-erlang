

# Module oc_server #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_link-2">add_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_span-1">add_span/1</a></td><td></td></tr><tr><td valign="top"><a href="#add_time_event-2">add_time_event/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_time_event-3">add_time_event/3</a></td><td></td></tr><tr><td valign="top"><a href="#finish_span-1">finish_span/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#put_attribute-3">put_attribute/3</a></td><td></td></tr><tr><td valign="top"><a href="#put_attributes-2">put_attributes/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_status-3">set_status/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_link-2"></a>

### add_link/2 ###

`add_link(Link, SpanCtx) -> any()`

<a name="add_span-1"></a>

### add_span/1 ###

`add_span(Span) -> any()`

<a name="add_time_event-2"></a>

### add_time_event/2 ###

`add_time_event(TimeEvent, Span) -> any()`

<a name="add_time_event-3"></a>

### add_time_event/3 ###

`add_time_event(Timestamp, TimeEvent, SpanCtx) -> any()`

<a name="finish_span-1"></a>

### finish_span/1 ###

`finish_span(SpanCtx) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(X1, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="put_attribute-3"></a>

### put_attribute/3 ###

`put_attribute(Key, Value, SpanCtx) -> any()`

<a name="put_attributes-2"></a>

### put_attributes/2 ###

`put_attributes(NewAttributes, SpanCtx) -> any()`

<a name="set_status-3"></a>

### set_status/3 ###

`set_status(Code, Message, SpanCtx) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

