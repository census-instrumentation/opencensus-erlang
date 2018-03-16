

# Module oc_trace_pb #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-opencensus.proto.trace.Span.Link.Type">'opencensus.proto.trace.Span.Link.Type'()</a> ###


<pre><code>
'opencensus.proto.trace.Span.Link.Type'() = TYPE_UNSPECIFIED | CHILD_LINKED_SPAN | PARENT_LINKED_SPAN
</code></pre>




### <a name="type-opencensus.proto.trace.Span.SpanKind">'opencensus.proto.trace.Span.SpanKind'()</a> ###


<pre><code>
'opencensus.proto.trace.Span.SpanKind'() = SPAN_KIND_UNSPECIFIED | SERVER | CLIENT
</code></pre>




### <a name="type-opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type">'opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type'()</a> ###


<pre><code>
'opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type'() = TYPE_UNSPECIFIED | SENT | RECEIVED
</code></pre>




### <a name="type-annotation_pb">annotation_pb()</a> ###


<pre><code>
annotation_pb() = #annotation_pb{}
</code></pre>




### <a name="type-attribute_value_pb">attribute_value_pb()</a> ###


<pre><code>
attribute_value_pb() = #attribute_value_pb{}
</code></pre>




### <a name="type-attributes_pb">attributes_pb()</a> ###


<pre><code>
attributes_pb() = #attributes_pb{}
</code></pre>




### <a name="type-bool_value_pb">bool_value_pb()</a> ###


<pre><code>
bool_value_pb() = #bool_value_pb{}
</code></pre>




### <a name="type-bytes_value_pb">bytes_value_pb()</a> ###


<pre><code>
bytes_value_pb() = #bytes_value_pb{}
</code></pre>




### <a name="type-double_value_pb">double_value_pb()</a> ###


<pre><code>
double_value_pb() = #double_value_pb{}
</code></pre>




### <a name="type-float_value_pb">float_value_pb()</a> ###


<pre><code>
float_value_pb() = #float_value_pb{}
</code></pre>




### <a name="type-int_32_value_pb">int_32_value_pb()</a> ###


<pre><code>
int_32_value_pb() = #int_32_value_pb{}
</code></pre>




### <a name="type-int_64_value_pb">int_64_value_pb()</a> ###


<pre><code>
int_64_value_pb() = #int_64_value_pb{}
</code></pre>




### <a name="type-link_pb">link_pb()</a> ###


<pre><code>
link_pb() = #link_pb{}
</code></pre>




### <a name="type-links_pb">links_pb()</a> ###


<pre><code>
links_pb() = #links_pb{}
</code></pre>




### <a name="type-message_event_pb">message_event_pb()</a> ###


<pre><code>
message_event_pb() = #message_event_pb{}
</code></pre>




### <a name="type-module_pb">module_pb()</a> ###


<pre><code>
module_pb() = #module_pb{}
</code></pre>




### <a name="type-span_pb">span_pb()</a> ###


<pre><code>
span_pb() = #span_pb{}
</code></pre>




### <a name="type-stack_frame_pb">stack_frame_pb()</a> ###


<pre><code>
stack_frame_pb() = #stack_frame_pb{}
</code></pre>




### <a name="type-stack_frames_pb">stack_frames_pb()</a> ###


<pre><code>
stack_frames_pb() = #stack_frames_pb{}
</code></pre>




### <a name="type-stack_trace_pb">stack_trace_pb()</a> ###


<pre><code>
stack_trace_pb() = #stack_trace_pb{}
</code></pre>




### <a name="type-status_pb">status_pb()</a> ###


<pre><code>
status_pb() = #status_pb{}
</code></pre>




### <a name="type-string_value_pb">string_value_pb()</a> ###


<pre><code>
string_value_pb() = #string_value_pb{}
</code></pre>




### <a name="type-time_event_pb">time_event_pb()</a> ###


<pre><code>
time_event_pb() = #time_event_pb{}
</code></pre>




### <a name="type-time_events_pb">time_events_pb()</a> ###


<pre><code>
time_events_pb() = #time_events_pb{}
</code></pre>




### <a name="type-timestamp_pb">timestamp_pb()</a> ###


<pre><code>
timestamp_pb() = #timestamp_pb{}
</code></pre>




### <a name="type-truncatable_string_pb">truncatable_string_pb()</a> ###


<pre><code>
truncatable_string_pb() = #truncatable_string_pb{}
</code></pre>




### <a name="type-u_int_32_value_pb">u_int_32_value_pb()</a> ###


<pre><code>
u_int_32_value_pb() = #u_int_32_value_pb{}
</code></pre>




### <a name="type-u_int_64_value_pb">u_int_64_value_pb()</a> ###


<pre><code>
u_int_64_value_pb() = #u_int_64_value_pb{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#enum_symbol_by_value_opencensus.proto.trace.Span.Link.Type-1">'enum_symbol_by_value_opencensus.proto.trace.Span.Link.Type'/1</a></td><td></td></tr><tr><td valign="top"><a href="#enum_symbol_by_value_opencensus.proto.trace.Span.SpanKind-1">'enum_symbol_by_value_opencensus.proto.trace.Span.SpanKind'/1</a></td><td></td></tr><tr><td valign="top"><a href="#enum_symbol_by_value_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type-1">'enum_symbol_by_value_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type'/1</a></td><td></td></tr><tr><td valign="top"><a href="#enum_value_by_symbol_opencensus.proto.trace.Span.Link.Type-1">'enum_value_by_symbol_opencensus.proto.trace.Span.Link.Type'/1</a></td><td></td></tr><tr><td valign="top"><a href="#enum_value_by_symbol_opencensus.proto.trace.Span.SpanKind-1">'enum_value_by_symbol_opencensus.proto.trace.Span.SpanKind'/1</a></td><td></td></tr><tr><td valign="top"><a href="#enum_value_by_symbol_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type-1">'enum_value_by_symbol_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type'/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_msg-2">decode_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode_msg-3">decode_msg/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_msg-1">encode_msg/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_msg-2">encode_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#enum_symbol_by_value-2">enum_symbol_by_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#enum_value_by_symbol-2">enum_value_by_symbol/2</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_enum_def-1">fetch_enum_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_msg_def-1">fetch_msg_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_rpc_def-2">fetch_rpc_def/2</a></td><td></td></tr><tr><td valign="top"><a href="#find_enum_def-1">find_enum_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#find_msg_def-1">find_msg_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#find_rpc_def-2">find_rpc_def/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_enum_names-0">get_enum_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_group_names-0">get_group_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg_defs-0">get_msg_defs/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg_names-0">get_msg_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg_or_group_names-0">get_msg_or_group_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_package_name-0">get_package_name/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_rpc_names-1">get_rpc_names/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_service_def-1">get_service_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_service_names-0">get_service_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#gpb_version_as_list-0">gpb_version_as_list/0</a></td><td></td></tr><tr><td valign="top"><a href="#gpb_version_as_string-0">gpb_version_as_string/0</a></td><td></td></tr><tr><td valign="top"><a href="#merge_msgs-2">merge_msgs/2</a></td><td></td></tr><tr><td valign="top"><a href="#merge_msgs-3">merge_msgs/3</a></td><td></td></tr><tr><td valign="top"><a href="#verify_msg-1">verify_msg/1</a></td><td></td></tr><tr><td valign="top"><a href="#verify_msg-2">verify_msg/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="enum_symbol_by_value_opencensus.proto.trace.Span.Link.Type-1"></a>

### 'enum_symbol_by_value_opencensus.proto.trace.Span.Link.Type'/1 ###

`enum_symbol_by_value_opencensus.proto.trace.Span.Link.Type(X1) -> any()`

<a name="enum_symbol_by_value_opencensus.proto.trace.Span.SpanKind-1"></a>

### 'enum_symbol_by_value_opencensus.proto.trace.Span.SpanKind'/1 ###

`enum_symbol_by_value_opencensus.proto.trace.Span.SpanKind(X1) -> any()`

<a name="enum_symbol_by_value_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type-1"></a>

### 'enum_symbol_by_value_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type'/1 ###

`enum_symbol_by_value_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type(X1) -> any()`

<a name="enum_value_by_symbol_opencensus.proto.trace.Span.Link.Type-1"></a>

### 'enum_value_by_symbol_opencensus.proto.trace.Span.Link.Type'/1 ###

`enum_value_by_symbol_opencensus.proto.trace.Span.Link.Type(X1) -> any()`

<a name="enum_value_by_symbol_opencensus.proto.trace.Span.SpanKind-1"></a>

### 'enum_value_by_symbol_opencensus.proto.trace.Span.SpanKind'/1 ###

`enum_value_by_symbol_opencensus.proto.trace.Span.SpanKind(X1) -> any()`

<a name="enum_value_by_symbol_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type-1"></a>

### 'enum_value_by_symbol_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type'/1 ###

`enum_value_by_symbol_opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type(X1) -> any()`

<a name="decode_msg-2"></a>

### decode_msg/2 ###

`decode_msg(Bin, MsgName) -> any()`

<a name="decode_msg-3"></a>

### decode_msg/3 ###

`decode_msg(Bin, MsgName, Opts) -> any()`

<a name="encode_msg-1"></a>

### encode_msg/1 ###

<pre><code>
encode_msg(Msg::#bool_value_pb{} | #message_event_pb{} | #truncatable_string_pb{} | #module_pb{} | #attribute_value_pb{} | #attributes_pb{} | #annotation_pb{} | #timestamp_pb{} | #time_event_pb{} | #float_value_pb{} | #int_32_value_pb{} | #bytes_value_pb{} | #string_value_pb{} | #int_64_value_pb{} | #stack_frame_pb{} | #stack_frames_pb{} | #status_pb{} | #u_int_32_value_pb{} | #link_pb{} | #stack_trace_pb{} | #double_value_pb{} | #links_pb{} | #u_int_64_value_pb{} | #time_events_pb{} | #span_pb{}) -&gt; binary()
</code></pre>
<br />

<a name="encode_msg-2"></a>

### encode_msg/2 ###

<pre><code>
encode_msg(Msg::#bool_value_pb{} | #message_event_pb{} | #truncatable_string_pb{} | #module_pb{} | #attribute_value_pb{} | #attributes_pb{} | #annotation_pb{} | #timestamp_pb{} | #time_event_pb{} | #float_value_pb{} | #int_32_value_pb{} | #bytes_value_pb{} | #string_value_pb{} | #int_64_value_pb{} | #stack_frame_pb{} | #stack_frames_pb{} | #status_pb{} | #u_int_32_value_pb{} | #link_pb{} | #stack_trace_pb{} | #double_value_pb{} | #links_pb{} | #u_int_64_value_pb{} | #time_events_pb{} | #span_pb{}, Opts::list()) -&gt; binary()
</code></pre>
<br />

<a name="enum_symbol_by_value-2"></a>

### enum_symbol_by_value/2 ###

`enum_symbol_by_value(X1, Value) -> any()`

<a name="enum_value_by_symbol-2"></a>

### enum_value_by_symbol/2 ###

`enum_value_by_symbol(X1, Sym) -> any()`

<a name="fetch_enum_def-1"></a>

### fetch_enum_def/1 ###

`fetch_enum_def(EnumName) -> any()`

<a name="fetch_msg_def-1"></a>

### fetch_msg_def/1 ###

`fetch_msg_def(MsgName) -> any()`

<a name="fetch_rpc_def-2"></a>

### fetch_rpc_def/2 ###

<pre><code>
fetch_rpc_def(ServiceName::term(), RpcName::term()) -&gt; no_return()
</code></pre>
<br />

<a name="find_enum_def-1"></a>

### find_enum_def/1 ###

`find_enum_def(X1) -> any()`

<a name="find_msg_def-1"></a>

### find_msg_def/1 ###

`find_msg_def(X1) -> any()`

<a name="find_rpc_def-2"></a>

### find_rpc_def/2 ###

`find_rpc_def(X1, X2) -> any()`

<a name="get_enum_names-0"></a>

### get_enum_names/0 ###

`get_enum_names() -> any()`

<a name="get_group_names-0"></a>

### get_group_names/0 ###

`get_group_names() -> any()`

<a name="get_msg_defs-0"></a>

### get_msg_defs/0 ###

`get_msg_defs() -> any()`

<a name="get_msg_names-0"></a>

### get_msg_names/0 ###

`get_msg_names() -> any()`

<a name="get_msg_or_group_names-0"></a>

### get_msg_or_group_names/0 ###

`get_msg_or_group_names() -> any()`

<a name="get_package_name-0"></a>

### get_package_name/0 ###

`get_package_name() -> any()`

<a name="get_rpc_names-1"></a>

### get_rpc_names/1 ###

`get_rpc_names(X1) -> any()`

<a name="get_service_def-1"></a>

### get_service_def/1 ###

`get_service_def(X1) -> any()`

<a name="get_service_names-0"></a>

### get_service_names/0 ###

`get_service_names() -> any()`

<a name="gpb_version_as_list-0"></a>

### gpb_version_as_list/0 ###

`gpb_version_as_list() -> any()`

<a name="gpb_version_as_string-0"></a>

### gpb_version_as_string/0 ###

`gpb_version_as_string() -> any()`

<a name="merge_msgs-2"></a>

### merge_msgs/2 ###

`merge_msgs(Prev, New) -> any()`

<a name="merge_msgs-3"></a>

### merge_msgs/3 ###

`merge_msgs(Prev, New, Opts) -> any()`

<a name="verify_msg-1"></a>

### verify_msg/1 ###

`verify_msg(Msg) -> any()`

<a name="verify_msg-2"></a>

### verify_msg/2 ###

`verify_msg(Msg, Opts) -> any()`

