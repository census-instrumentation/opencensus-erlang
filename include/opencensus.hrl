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

-record(trace_context, {
          trace_id   :: opencensus:trace_id() | undefined,
          span_id    :: opencensus:span_id() | undefined,
          enabled    :: boolean() | undefined,

          sampler    :: module(),
          reporter   :: module(),
          propagator :: module()
         }).

-record(span, {
          start_time  :: wts:timestamp(),
          end_time  :: wts:timestamp() | undefined,
          %% 128 bit int trace id
          trace_id   :: opencensus:trace_id() | undefined,
          %% name of the span
          name       :: unicode:unicode_binary(),
          %% 64 bit int span id
          span_id    :: opencensus:span_id() | undefined,
          %% 64 bit int parent span
          parent_span_id  :: opencensus:span_id() | undefined,
          %% microseconds between span start/end
          duration   :: opencensus:time_us(),

          annotations :: opencensus:annotations(),
          attributes  :: opencensus:attributes(),

          %% links to spans in other traces
          links       :: opencensus:links(),

          time_events :: opencensus:time_events(),

          %% An optional final status for this span.
          status      :: undefined,

          %% A highly recommended but not required flag that identifies when a trace
          %% crosses a process boundary. True when the parent_span belongs to the
          %% same process as the current span.
          %same_process_as_parent_span :: boolean(),

          %% An optional number of child spans that were generated while this span
          %% was active. If set, allows implementation to detect missing child spans.
          child_span_count = 0 :: integer()
         }).

-record(link, {
          %% The relationship of the current span relative to the linked span:
          %% child, parent, or unspecified.
          type :: opencensus:link_type(),
          trace_id :: opencensus:trace_id(),
          span_id    :: opencensus:span_id(),
          attributes  :: opencensus:attributes()
         }).

-record(links, {
          links :: [opencensus:link()],

          %% The number of dropped links after the maximum size was enforced. If
          %% this value is 0, then no links were dropped.
          dropped_links_count :: integer()
         }).

-record(time_event, {
          time :: opencensus:time_us(),
          value :: opencensus:annotation() | opencensus:network_event()
         }).

-record(network_event, {
          time :: opencensus:time_us(),

          %% type of NetworkEvent. Indicates whether the RPC message was sent or received.
          type :: opencensus:network_event_type(),

          %% identifier for the message, which must be unique in this span.
          message_id :: integer(),

          %% number of uncompressed bytes sent or received
          uncompressed_message_size :: integer(),

          %% number of compressed bytes sent or received
          compressed_message_size :: integer()
         }).

-record(time_events, {
          %% A collection of `TimeEvent`s.
          time_event :: [opencensus:time_event()],

          %% The number of dropped annotations in all the included time events.
          %% If the value is 0, then no annotations were dropped.
          dropped_annotations_count :: integer(),

          %% The number of dropped network events in all the included time events.
          %% If the value is 0, then no network events were dropped.
          dropped_network_events_count :: integer()
         }).

-record(status, {
          code :: integer(),
          %% developer-facing error message
          message :: unicode:unicode_binary(),
          details :: [term()]
         }).
