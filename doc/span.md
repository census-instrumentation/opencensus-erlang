# <a name="Creating_Spans">Creating Spans</a> #

Span data is stored and manipulated in an ETS table. Span context must be tracked in order to create child span's, propagate span context across process or node boundaries and for the library to which span data to manipulate in the context it is called.

`opencensus` provides two methods for tracking this context, the process dictionary and a variable holding a `ctx` record.

<h5><a name="Process_Dictionary_as_Context">Process Dictionary as Context</a></h5>

With `ocp` the span context is tracked in the current process`s process dictionary.

```erlang
some_fun() ->
  ocp:with_child_span(<<"some_fun/0">>,
                      fun() ->
                          ... body ..
                      end).
```

<h5><a name="Parse_Transform">Parse Transform</a></h5>

The parse transform provides an attribute to decorate functions with that will start a span, wrap the contents in a `try` and finish the span in an `after` clause. Add the parse transform to the compile opts in `rebar.config`:

```erlang
{erl_opts, [{parse_transform, oc_transform}]}.
```

And use:

```erlang
-span([]).
function_to_trace() ->
  ...
  SpanCtx = ocp:current_span(), %% ocp works in a decorated function too
  ...
```

Since the tranformed functions use the process dictionary to store the context you can interact with the current span the same way as you do with `ocp`, covered in the previous section.

<h5><a name="Manual_Context_Tracking">Manual Context Tracking</a></h5>

`ctx` is a generic context library for Erlang. OpenCensus provides the option to use it in place of the process dictionary for tracking the span context.

In this example a function is passed a `ctx` variable `Ctx` that some instrumented library could have set the span context based on the incoming metadata of a request, like HTTP headers. The `oc_trace:new_span` function will check `Ctx` for a span context and create a child span of that span context if it exists, otherwise a root span will be created. We can pass the span context to another function, we could also createa a new `ctx` to pass (`oc_trace:with_span(Ctx, SpanCtx)`), to be further updated or have new children created:

```erlang
handler(Ctx, NextHandler) ->
  SpanCtx = oc_trace:with_child_span(Ctx, <<"span-name">>),
  try
    oc_trace:put_attribute(<<"key">>, <<"value">>, SpanCtx),
    {Code, Message} = NextHandler(SpanCtx),
    oc_trace:set_status(Code, Message, SpanCtx)
  after
    oc_trace:finish_span(SpanCtx)
  end.
```

<h5><a name="Manual_Span_Data_Handling">Manual Span Data Handling</a></h5>

The module `oc_span` has the functional span data manipulation functions, meaning ETS is not involved. Most users will not need this, but for potential alternative span data stores or context trackers they are necessary.


#### <a name="Working_with_Spans">Working with Spans</a> ####

<h5><a name="Attributes">Attributes</a></h5>

A span has a map of attributes providing details about the span. The key is a binary string and the value of the attribute can be a binary string, integer, or boolean.

```erlang
Span1 = oc_trace:put_attribute(<<"/instance_id">>, <<"my-instance">>, SpanCtx),
```

<h5><a name="Time_Events">Time Events</a></h5>

A time event is a timestamped annotation with user-supplied key-value pairs or a message event to represent a message (not specificly an Erlang message) sent to or received from another span.

The `message_event` consists of a type, identifier and size of the message. `Id` is an identifier for the event's message that can be used to match `SENT` and `RECEIVED` `message_event`s. For example, this field could represent a sequence ID for a streaming RPC. It is recommended to be unique within a Span. If `CompressedSize` is `0` it is assumed to be the same as `UncompressedSize`.

```erlang
Event = opencensus:message_event(?MESSAGE_EVENT_TYPE_SENT, Id, UncompressedSize, CompressedSize)
oc_trace:add_time_event(Event, SpanCtx),
```

<h5><a name="Links">Links</a></h5>

Links are useful in cases like a job queue. A job is created with a span context and when run wants to report a new span. The job isn't a direct child of the span that inserted it into the queue, but it is related. The job creates a link to the span that created it.

```erlang
SpanCtx = oc_trace:with_child_span(Ctx, <<"running job">>),
Link = oc_trace:link(?LINK_TYPE_PARENT_LINKED_SPAN, TraceId, ParentSpanId, #{}),
oc_trace:add_link(Link, SpanCtx),
... run job ...
oc_trace:finish_span(SpanCtx).
```
