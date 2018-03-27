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
%% @doc Behaviour each sampler must implement. The function `should_trace'
%% is given the trace id of the current trace, if one exists, the span id
%% of what would be the parent of any new span in this trace and a boolean
%% for whether the trace was enabled in the process that propagated this
%% context.
%% @end
%%%------------------------------------------------------------------------

-module(oc_sampler).

-export([init/1,
         should_sample/3]).

-dialyzer({nowarn_function, should_sample/3}).

-include_lib("syntax_tools/include/merl.hrl").

-callback init(term()) -> term().

%% @doc Called at the start of a trace.
-callback should_sample(TraceId, SpanId, Enabled, Opts) -> boolean() when
      TraceId :: opencensus:trace_id() | undefined,
      SpanId :: opencensus:span_id() | undefined,
      Enabled :: boolean() | undefined,
      Opts :: term().

init({SamplerModule, SamplerInitArgs}) ->
    SamplerOpts = SamplerModule:init(SamplerInitArgs),

    ImplModule = ?Q(["-module(oc_sampler_impl).",
                     "-export([should_sample/3]).",
                     "should_sample(TraceId, SpanId, Enabled) -> ",
                     "    _@SamplerModule@:should_sample(TraceId, SpanId, Enabled, _@SamplerOpts@)."]),
    merl:compile_and_load(ImplModule),
    ok.

should_sample(TraceId, SpanId, Enabled) ->
    oc_sampler_impl:should_sample(TraceId, SpanId, Enabled).
