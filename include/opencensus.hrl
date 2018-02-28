%%%------------------------------------------------------------------------
%% Copyright 2017, OpenCensus Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%------------------------------------------------------------------------

-define(SPAN_TAB, oc_span_tab).

-define(SPAN_CTX, oc_span_ctx_key).
-define(TAG_CTX, oc_tag_ctx_key).

-define(MESSAGE_EVENT_TYPE_UNSPECIFIED, 'TYPE_UNSPECIFIED').
-define(MESSAGE_EVENT_TYPE_SENT, 'SENT').
-define(MESSAGE_EVENT_TYPE_RECEIVED, 'RECEIVED').

-define(LINK_TYPE_UNSPECIFIED, 'TYPE_UNSPECIFIED').
-define(LINK_TYPE_CHILD_LINKED_SPAN, 'CHILD_LINKED_SPAN').
-define(LINK_TYPE_PARENT_LINKED_SPAN, 'PARENT_LINKED_SPAN').

-type maybe(T) :: T | undefined.

-record(span_ctx, {
          %% 128 bit int trace id
          trace_id          :: opencensus:trace_id() | undefined,
          %% 64 bit int span id
          span_id           :: opencensus:span_id() | undefined,
          %% 8-bit integer, lowest bit is if it is sampled
          trace_options = 1 :: integer() | undefined
         }).

-record(span, {
          %% name of the span
          name                                    :: unicode:unicode_binary(),

          %% 128 bit int trace id
          trace_id                                :: opencensus:trace_id() | undefined,

          %% 64 bit int span id
          span_id                                 :: opencensus:span_id() | undefined,
          %% 64 bit int parent span
          parent_span_id                          :: opencensus:span_id() | undefined,

          %% 8-bit integer, lowest bit is if it is sampled
          trace_options = 1                       :: integer() | undefined,

          start_time                              :: wts:timestamp(),
          end_time                                :: wts:timestamp() | undefined,

          attributes = #{}                        :: opencensus:attributes(),

          %% optional stacktrace from where the span was started
          stack_trace                             :: opencensus:stack_trace() | undefined,

          %% links to spans in other traces
          links = []                              :: opencensus:links(),

          time_events = []                        :: opencensus:time_events(),

          %% An optional final status for this span.
          status = undefined                      :: opencensus:status() | undefined,

          %% A highly recommended but not required flag that identifies when a trace
          %% crosses a process boundary. True when the parent_span belongs to the
          %% same process as the current span.
          same_process_as_parent_span = undefined :: boolean() | undefined,

          %% An optional number of child spans that were generated while this span
          %% was active. If set, allows implementation to detect missing child spans.
          child_span_count = undefined            :: integer() | undefined
         }).

-record(link, {
          %% The relationship of the current span relative to the linked span:
          %% child, parent, or unspecified.
          type = 'TYPE_UNSPECIFIED' :: opencensus:link_type(),
          trace_id                  :: opencensus:trace_id(),
          span_id                   :: opencensus:span_id(),
          attributes = #{}          :: opencensus:attributes()
         }).

-record(message_event, {
          %% type of MessageEvent. Indicates whether the RPC message was sent or received.
          type = 'TYPE_UNSPECIFIED' :: opencensus:message_event_type(),

          %% identifier for the message, which must be unique in this span.
          id                        :: integer(),

          %% number of uncompressed bytes sent or received
          uncompressed_size         :: integer(),

          %% number of compressed bytes sent or received
          compressed_size           :: integer()
         }).

-record(annotation, {
          description      :: unicode:unicode_binary() | undefined,
          attributes = #{} :: opencensus:attributes()
         }).

-record(status, {
          code    :: integer(),
          %% developer-facing error message
          message :: unicode:unicode_binary()
         }).

-type measure_name() :: atom() | binary() | string().
-type aggregation() :: atom().
