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
%% @doc This module allows sequential execution of multiple reporters.
%% @end
%%%-----------------------------------------------------------------------
-module(oc_sequential_reporter).

-behavior(oc_reporter).

-export([init/1,
         report/2]).

-type reporter() :: atom().
-type reporter_opts() :: term().
-type opts() :: [{reporter(), reporter_opts()}].

%%-
-spec init([{reporter(), reporter_opts()}]) -> opts().
init(Config) ->
    [{Reporter, Reporter:init(RConfig)} || {Reporter, RConfig} <- Config].

%%-
-spec report(nonempty_list(opencensus:spans()), opts()) -> ok.
report(Spans, Config) ->
    [Reporter:report(Spans, RConfig) || {Reporter, RConfig} <- Config],
    ok.
