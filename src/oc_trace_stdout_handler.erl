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

-module(oc_trace_stdout_handler).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2]).

init(Opts) -> {ok, Opts}.

handle_call(_Msg, State) -> {ok, ok, State}.

handle_event({spans, Spans}, State) ->
    [io:format("~p~n", [Span]) || Span <- Spans],

    {ok, State}.
