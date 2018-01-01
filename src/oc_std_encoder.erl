-module(oc_std_encoder).

-export([to_proto/1,
         to_std_records/1]).

-include("opencensus.hrl").
-include("oc_trace_pb.hrl").

trunc_string(undefined) ->
    undefined;
trunc_string(V) when is_atom(V) ->
    trunc_string(atom_to_binary(V, utf8));
trunc_string(V) when is_list(V) ->
    trunc_string(list_to_binary(V));
trunc_string(V) when is_binary(V) ->
    #truncatable_string_pb{value=V,
                           truncated_byte_count=0}.

to_std_records(#message_event{type=Type,
                      id=Id,
                      uncompressed_size=UncompressedSize,
                      compressed_size=CompressedSize}) ->
    #message_event_pb{type=Type,
                      id=Id,
                      uncompressed_size=UncompressedSize,
                      compressed_size=CompressedSize};
to_std_records(#status{code=Code,
                       message=Message}) ->
    #status_pb{code=Code,
               message = Message};
to_std_records(#span{name=Name,
                     trace_id=TraceId,
                     span_id=SpanId,
                     parent_span_id=MaybeParentSpanId,
                     start_time=StartTime,
                     end_time=EndTime,
                     attributes=Attributes,
                     links=Links,
                     stack_trace=StackTrace,
                     time_events=TimeEvents,
                     status=Status,
                     same_process_as_parent_span=SameProcessAsParentSpan,
                     child_span_count=ChildSpanCount}) ->
    TraceIdBytes = <<TraceId:128>>,
    SpanIdBytes = <<SpanId:64>>,
    ParentSpanId = case MaybeParentSpanId of undefined -> <<>>; _ -> <<MaybeParentSpanId:64>> end,
    #span_pb{name=trunc_string(Name),
             trace_id=TraceIdBytes,
             span_id=SpanIdBytes,
             parent_span_id=ParentSpanId,
             start_time=to_std_timestamp(StartTime),
             end_time=to_std_timestamp(EndTime),
             attributes=to_std_attributes(Attributes),
             stack_trace=to_std_stack_trace(StackTrace),
             time_events=to_std_time_events(TimeEvents),
             links=to_std_links(Links),
             status=to_std_records(Status),
             same_process_as_parent_span=case SameProcessAsParentSpan of
                                             undefined -> undefined;
                                             _ -> #bool_value_pb{value=SameProcessAsParentSpan}
                                         end,
             child_span_count=case ChildSpanCount of
                                undefined -> undefined;
                                _ -> #u_int_32_value_pb{value=ChildSpanCount}
                            end};
to_std_records(undefined) ->
    undefined.

to_std_links(Links) ->
    {LinksPb, DroppedLinks} = lists:foldl(fun(#link{type=Type,
                                                    trace_id=TraceId,
                                                    span_id=SpanId,
                                                    attributes=Attributes}, {Acc, DroppedAcc}) ->
                                              TraceIdBytes = <<TraceId:128>>,
                                              SpanIdBytes = <<SpanId:64>>,
                                              {[#link_pb{type=Type,
                                                         trace_id=TraceIdBytes,
                                                         span_id=SpanIdBytes,
                                                         attributes=to_std_attributes(Attributes)} | Acc], DroppedAcc}
                                          end, {[], 0}, Links),
    #links_pb{link=LinksPb,
              dropped_links_count=DroppedLinks}.

to_std_timestamp({T, O}) ->
    Time = T + O,
    #timestamp_pb{seconds=erlang:convert_time_unit(Time, native, second),
                  nanos=erlang:convert_time_unit(Time, native, nanosecond) rem 1000000000}.

-spec to_std_stack_frame(erlang:stack_item()) -> oc_trace_pb:stack_frame_pb().
to_std_stack_frame({Module, Function, _Arity, Location}) ->
    Filename = proplists:get_value(file, Location, undefined),
    Line = proplists:get_value(line, Location, 0),
    #stack_frame_pb{function_name=trunc_string(Function),
                    original_function_name=trunc_string(Function),
                    file_name=trunc_string(Filename),
                    line_number=Line,
                    column_number=0,
                    load_module=#module_pb{module = trunc_string(Module),
                                           build_id = undefined},
                    source_version=undefined}.

-spec to_std_stack_trace([erlang:stack_item()] | undefined) -> oc_trace_pb:stack_trace_pb() | undefined.
to_std_stack_trace(undefined) -> undefined;
to_std_stack_trace(Frames) ->
    #stack_trace_pb{stack_frames=#stack_frames_pb{frame=[to_std_stack_frame(F) || F <- Frames]},
                    stack_trace_hash_id=0}.

to_std_attributes(Map) ->
    #attributes_pb{attribute_map=maps:map(fun(_, V) when is_binary(V) ->
                                                 #attribute_value_pb{value={string_value, trunc_string(V)}};
                                             (_, V) when is_integer(V) ->
                                                 #attribute_value_pb{value={int_value, V}};
                                             (_, V) when is_boolean(V) ->
                                                 #attribute_value_pb{value={bool_value, V}}
                                 end, Map),
      dropped_attributes_count=0}.

to_std_time_events(TimeEvents) ->
    {Events, DroppedAnnotations, DroppedMessageEvents} =
        lists:foldl(fun({Time, #annotation{description=Description,
                                           attributes=Attributes}},
                        {Acc, DroppedAnnotationsAcc, DroppedMessageAcc}) ->
                            Annotation = #annotation_pb{description=trunc_string(Description),
                                                        attributes=to_std_attributes(Attributes)},
                            {[#time_event_pb{time=to_std_timestamp(Time),
                                             value={annotation, Annotation}} | Acc],
                             DroppedAnnotationsAcc, DroppedMessageAcc};
                       ({Time, MessageEvent=#message_event{}},
                        {Acc, DroppedAnnotationsAcc, DroppedMessageAcc}) ->
                            MessageEventPb = to_std_records(MessageEvent),
                            {[#time_event_pb{time=to_std_timestamp(Time),
                                             value={message_event, MessageEventPb}} | Acc],
                             DroppedAnnotationsAcc, DroppedMessageAcc}
                    end, {[], 0, 0}, TimeEvents),

    #time_events_pb{time_event=Events,
                    dropped_annotations_count=DroppedAnnotations,
                    dropped_message_events_count=DroppedMessageEvents}.

to_proto(Span) ->
    oc_trace_pb:encode_msg(to_std_records(Span)).

