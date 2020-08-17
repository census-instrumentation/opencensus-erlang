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
%%
%% @doc opencensus main module
%% @end
%%%-------------------------------------------------------------------------
-module(opencensus).

-export([generate_trace_id/0,
         generate_span_id/0,
         http_status_to_trace_status/1,
         span_kind_client/0,
         span_kind_server/0,
         span_kind_unspecified/0]).

-include("opencensus.hrl").

-export_type([span_ctx/0,
              tracestate/0,
              trace_id/0,
              span_id/0,
              span/0,
              link/0,
              links/0,
              link_type/0,
              attributes/0,
              annotation/0,
              time_events/0,
              message_event/0,
              message_event_type/0,
              stack_trace/0,
              status/0,

              tags/0]).

-type span_ctx()           :: #span_ctx{}.
-type tracestate()         :: #tracestate{}.
-type trace_id()           :: non_neg_integer().
-type span_id()            :: non_neg_integer().
-type span()               :: #span{}.
-type stack_trace()        :: [erlang:stack_item()].
-type attribute_value()    :: any().
-type attributes()         :: #{unicode:unicode_binary() => attribute_value()}.
-type annotation()         :: #annotation{}.
-type span_kind()          :: ?SPAN_KIND_UNSPECIFIED | ?SPAN_KIND_SERVER | ?SPAN_KIND_CLIENT.
-type message_event()      :: #message_event{}.
-type message_event_type() :: ?MESSAGE_EVENT_TYPE_UNSPECIFIED | ?MESSAGE_EVENT_TYPE_SENT | ?MESSAGE_EVENT_TYPE_RECEIVED.
-type time_events()        :: [{wts:timestamp(), annotation() | message_event()}].
-type link()               :: #link{}.
-type links()              :: [link()].
-type link_type()          :: ?LINK_TYPE_UNSPECIFIED | ?LINK_TYPE_CHILD_LINKED_SPAN | ?LINK_TYPE_PARENT_LINKED_SPAN.
-type status()             :: #status{}.

-type tags()               :: oc_tags:tags().

%%--------------------------------------------------------------------
%% @doc
%% Generates a 128 bit random integer to use as a trace id.
%% @end
%%--------------------------------------------------------------------
-spec generate_trace_id() -> trace_id().
generate_trace_id() ->
    uniform(2 bsl 127 - 1). %% 2 shifted left by 127 == 2 ^ 128

%%--------------------------------------------------------------------
%% @doc
%% Generates a 64 bit random integer to use as a span id.
%% @end
%%--------------------------------------------------------------------
-spec generate_span_id() -> span_id().
generate_span_id() ->
    uniform(2 bsl 63 - 1). %% 2 shifted left by 63 == 2 ^ 64

%%--------------------------------------------------------------------
%% @doc
%% Convert HTTP status code to Trace status code.
%% @end
%%--------------------------------------------------------------------
-spec http_status_to_trace_status(integer()) -> integer().
http_status_to_trace_status(S) when S >= 0 andalso S =< 199 ->
    2;
http_status_to_trace_status(S) when S >= 200 andalso S =< 399 ->
    0;
http_status_to_trace_status(400) ->
    3;
http_status_to_trace_status(504) ->
    4;
http_status_to_trace_status(404) ->
    5;
http_status_to_trace_status(403) ->
    7;
http_status_to_trace_status(401) ->
    16;
http_status_to_trace_status(429) ->
    8;
http_status_to_trace_status(501) ->
    12;
http_status_to_trace_status(503) ->
    14;
http_status_to_trace_status(S) ->
    S.

%%--------------------------------------------------------------------
%% @doc
%% Returns atom for span kind client to set on trace.
%% @end
%%--------------------------------------------------------------------
-spec span_kind_client() -> span_kind().
span_kind_client() -> ?SPAN_KIND_CLIENT.

%%--------------------------------------------------------------------
%% @doc
%% Returns atom for span kind server to set on trace.
%% @end
%%--------------------------------------------------------------------
-spec span_kind_server() -> span_kind().
span_kind_server() -> ?SPAN_KIND_SERVER.

%%--------------------------------------------------------------------
%% @doc
%% Returns atom for span kind unspecified to set on trace.
%% @end
%%--------------------------------------------------------------------
-spec span_kind_unspecified() -> span_kind().
span_kind_unspecified() -> ?SPAN_KIND_UNSPECIFIED.


%%

%% Before OTP-20 rand:uniform could not give precision higher than 2^56.
%% Here we do a compile time check for support of this feature and will
%% combine multiple calls to rand if on an OTP version older than 20.0
-ifdef(OTP_RELEASE).
uniform(X) ->
    rand:uniform(X).
-else.
-define(TWO_POW_56, 2 bsl 55).

uniform(X) when X =< ?TWO_POW_56 ->
    rand:uniform(X);
uniform(X) ->
    R = rand:uniform(?TWO_POW_56),
    (uniform(X bsr 56) bsl 56) + R.
-endif.
