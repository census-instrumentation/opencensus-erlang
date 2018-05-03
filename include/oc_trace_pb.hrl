%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.1.5

-ifndef(oc_trace_pb).
-define(oc_trace_pb, true).

-define(oc_trace_pb_gpb_version, "4.1.5").

-ifndef('U_INT_32_VALUE_PB_PB_H').
-define('U_INT_32_VALUE_PB_PB_H', true).
-record(u_int_32_value_pb,
        {value = 0              :: non_neg_integer() | undefined % = 1, 32 bits
        }).
-endif.

-ifndef('TRUNCATABLE_STRING_PB_PB_H').
-define('TRUNCATABLE_STRING_PB_PB_H', true).
-record(truncatable_string_pb,
        {value = <<>>           :: iodata() | undefined, % = 1
         truncated_byte_count = 0 :: integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('ATTRIBUTE_VALUE_PB_PB_H').
-define('ATTRIBUTE_VALUE_PB_PB_H', true).
-record(attribute_value_pb,
        {value                  :: {string_value, #truncatable_string_pb{}} | {int_value, integer()} | {bool_value, boolean() | 0 | 1} | undefined % oneof
        }).
-endif.

-ifndef('ATTRIBUTES_PB_PB_H').
-define('ATTRIBUTES_PB_PB_H', true).
-record(attributes_pb,
        {attribute_map = #{}    :: #{iodata() := #attribute_value_pb{}} | undefined, % = 1
         dropped_attributes_count = 0 :: integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('FLOAT_VALUE_PB_PB_H').
-define('FLOAT_VALUE_PB_PB_H', true).
-record(float_value_pb,
        {value = 0.0            :: float() | integer() | infinity | '-infinity' | nan | undefined % = 1
        }).
-endif.

-ifndef('STATUS_PB_PB_H').
-define('STATUS_PB_PB_H', true).
-record(status_pb,
        {code = 0               :: integer() | undefined, % = 1, 32 bits
         message = <<>>         :: iodata() | undefined % = 2
        }).
-endif.

-ifndef('MESSAGE_EVENT_PB_PB_H').
-define('MESSAGE_EVENT_PB_PB_H', true).
-record(message_event_pb,
        {type = 'TYPE_UNSPECIFIED' :: 'TYPE_UNSPECIFIED' | 'SENT' | 'RECEIVED' | integer() | undefined, % = 1, enum opencensus.proto.trace.Span.TimeEvent.MessageEvent.Type
         id = 0                 :: non_neg_integer() | undefined, % = 2, 32 bits
         uncompressed_size = 0  :: non_neg_integer() | undefined, % = 3, 32 bits
         compressed_size = 0    :: non_neg_integer() | undefined % = 4, 32 bits
        }).
-endif.

-ifndef('LINK_PB_PB_H').
-define('LINK_PB_PB_H', true).
-record(link_pb,
        {trace_id = <<>>        :: iodata() | undefined, % = 1
         span_id = <<>>         :: iodata() | undefined, % = 2
         type = 'TYPE_UNSPECIFIED' :: 'TYPE_UNSPECIFIED' | 'CHILD_LINKED_SPAN' | 'PARENT_LINKED_SPAN' | integer() | undefined, % = 3, enum opencensus.proto.trace.Span.Link.Type
         attributes = undefined :: #attributes_pb{} | undefined % = 4
        }).
-endif.

-ifndef('BYTES_VALUE_PB_PB_H').
-define('BYTES_VALUE_PB_PB_H', true).
-record(bytes_value_pb,
        {value = <<>>           :: iodata() | undefined % = 1
        }).
-endif.

-ifndef('TIMESTAMP_PB_PB_H').
-define('TIMESTAMP_PB_PB_H', true).
-record(timestamp_pb,
        {seconds = 0            :: integer() | undefined, % = 1, 32 bits
         nanos = 0              :: integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('STRING_VALUE_PB_PB_H').
-define('STRING_VALUE_PB_PB_H', true).
-record(string_value_pb,
        {value = <<>>           :: iodata() | undefined % = 1
        }).
-endif.

-ifndef('MODULE_PB_PB_H').
-define('MODULE_PB_PB_H', true).
-record(module_pb,
        {module = undefined     :: #truncatable_string_pb{} | undefined, % = 1
         build_id = undefined   :: #truncatable_string_pb{} | undefined % = 2
        }).
-endif.

-ifndef('STACK_FRAME_PB_PB_H').
-define('STACK_FRAME_PB_PB_H', true).
-record(stack_frame_pb,
        {function_name = undefined :: #truncatable_string_pb{} | undefined, % = 1
         original_function_name = undefined :: #truncatable_string_pb{} | undefined, % = 2
         file_name = undefined  :: #truncatable_string_pb{} | undefined, % = 3
         line_number = 0        :: integer() | undefined, % = 4, 32 bits
         column_number = 0      :: integer() | undefined, % = 5, 32 bits
         load_module = undefined :: #module_pb{} | undefined, % = 6
         source_version = undefined :: #truncatable_string_pb{} | undefined % = 7
        }).
-endif.

-ifndef('STACK_FRAMES_PB_PB_H').
-define('STACK_FRAMES_PB_PB_H', true).
-record(stack_frames_pb,
        {frame = []             :: [#stack_frame_pb{}] | undefined, % = 1
         dropped_frames_count = 0 :: integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('STACK_TRACE_PB_PB_H').
-define('STACK_TRACE_PB_PB_H', true).
-record(stack_trace_pb,
        {stack_frames = undefined :: #stack_frames_pb{} | undefined, % = 1
         stack_trace_hash_id = 0 :: non_neg_integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('BOOL_VALUE_PB_PB_H').
-define('BOOL_VALUE_PB_PB_H', true).
-record(bool_value_pb,
        {value = false          :: boolean() | 0 | 1 | undefined % = 1
        }).
-endif.

-ifndef('ANNOTATION_PB_PB_H').
-define('ANNOTATION_PB_PB_H', true).
-record(annotation_pb,
        {description = undefined :: #truncatable_string_pb{} | undefined, % = 1
         attributes = undefined :: #attributes_pb{} | undefined % = 2
        }).
-endif.

-ifndef('U_INT_64_VALUE_PB_PB_H').
-define('U_INT_64_VALUE_PB_PB_H', true).
-record(u_int_64_value_pb,
        {value = 0              :: non_neg_integer() | undefined % = 1, 32 bits
        }).
-endif.

-ifndef('TIME_EVENT_PB_PB_H').
-define('TIME_EVENT_PB_PB_H', true).
-record(time_event_pb,
        {time = undefined       :: #timestamp_pb{} | undefined, % = 1
         value                  :: {annotation, #annotation_pb{}} | {message_event, #message_event_pb{}} | undefined % oneof
        }).
-endif.

-ifndef('TIME_EVENTS_PB_PB_H').
-define('TIME_EVENTS_PB_PB_H', true).
-record(time_events_pb,
        {time_event = []        :: [#time_event_pb{}] | undefined, % = 1
         dropped_annotations_count = 0 :: integer() | undefined, % = 2, 32 bits
         dropped_message_events_count = 0 :: integer() | undefined % = 3, 32 bits
        }).
-endif.

-ifndef('LINKS_PB_PB_H').
-define('LINKS_PB_PB_H', true).
-record(links_pb,
        {link = []              :: [#link_pb{}] | undefined, % = 1
         dropped_links_count = 0 :: integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('SPAN_PB_PB_H').
-define('SPAN_PB_PB_H', true).
-record(span_pb,
        {trace_id = <<>>        :: iodata() | undefined, % = 1
         span_id = <<>>         :: iodata() | undefined, % = 2
         parent_span_id = <<>>  :: iodata() | undefined, % = 3
         name = undefined       :: #truncatable_string_pb{} | undefined, % = 4
         kind = 'SPAN_KIND_UNSPECIFIED' :: 'SPAN_KIND_UNSPECIFIED' | 'SERVER' | 'CLIENT' | integer() | undefined, % = 14, enum opencensus.proto.trace.Span.SpanKind
         start_time = undefined :: #timestamp_pb{} | undefined, % = 5
         end_time = undefined   :: #timestamp_pb{} | undefined, % = 6
         attributes = undefined :: #attributes_pb{} | undefined, % = 7
         stack_trace = undefined :: #stack_trace_pb{} | undefined, % = 8
         time_events = undefined :: #time_events_pb{} | undefined, % = 9
         links = undefined      :: #links_pb{} | undefined, % = 10
         status = undefined     :: #status_pb{} | undefined, % = 11
         same_process_as_parent_span = undefined :: #bool_value_pb{} | undefined, % = 12
         child_span_count = undefined :: #u_int_32_value_pb{} | undefined % = 13
        }).
-endif.

-ifndef('INT_32_VALUE_PB_PB_H').
-define('INT_32_VALUE_PB_PB_H', true).
-record(int_32_value_pb,
        {value = 0              :: integer() | undefined % = 1, 32 bits
        }).
-endif.

-ifndef('INT_64_VALUE_PB_PB_H').
-define('INT_64_VALUE_PB_PB_H', true).
-record(int_64_value_pb,
        {value = 0              :: integer() | undefined % = 1, 32 bits
        }).
-endif.

-ifndef('DOUBLE_VALUE_PB_PB_H').
-define('DOUBLE_VALUE_PB_PB_H', true).
-record(double_value_pb,
        {value = 0.0            :: float() | integer() | infinity | '-infinity' | nan | undefined % = 1
        }).
-endif.

-endif.
