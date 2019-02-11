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

-module(oc_internal_timer).

-callback ping() -> ok.

-export([start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {timer :: reference(),
                interval :: pos_integer(),
                module :: module()}).

start_link(Interval, Module) ->
    gen_server:start_link(?MODULE, {Interval, Module}, []).

init({Interval, Module}) ->
    Ref = erlang:send_after(Interval, self(), ping),

    {ok, #state{timer = Ref,
                interval = Interval,
                module = Module}}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(ping, #state{timer = Ref, interval = Interval, module = Mod}) ->
    _ = erlang:cancel_timer(Ref),
    ok = Mod:ping(),
    NewRef = erlang:send_after(Interval, self(), ping),

    {noreply, #state{timer = NewRef,
                     interval = Interval,
                     module = Mod}}.
