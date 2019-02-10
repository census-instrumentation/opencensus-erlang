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

-module(oc_trace_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-include("opencensus.hrl").

start_link(Opts) ->
  supervisor:start_link(?MODULE, Opts).

init(Opts) ->
  Interval = proplists:get_value(Opts, interval, 500),
  Handlers = proplists:get_value(Opts, handlers, []),

  Exporter = #{id => exporter,
               start => {oc_trace_reporter, start_link, [Handlers]}},
  % TODO: Rename oc_span_sweeper to oc_trace_sweeper
  Sweeper = #{id => sweeper,
              start => {oc_span_sweeper, start_link, []}},
  Timer = #{id => timer,
            start => {oc_internal_timer, start_link, [{interval, Interval},
                                                      {module, oc_trace_reporter}]}
           },

  ok = maybe_init_span_tab(),

  {ok, {#{strategy => one_for_one}, [Exporter, Timer, Sweeper]}}.

maybe_init_span_tab() ->
  case ets:info(?SPAN_TAB, name) of
    undefined ->
      ets:new(?SPAN_TAB, [named_table, public, {write_concurrency, true}, {keypos, #span.span_id}]),
      ok;
    _ ->
      ok
  end.
