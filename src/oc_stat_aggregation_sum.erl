%%%------------------------------------------------------------------------
%% Copyright 2018, OpenCensus Authors
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
%% @doc
%% Sum indicates that data collected and aggregated
%% with this method will be summed up.
%% For example, accumulated request bytes can be aggregated by using
%% Sum.
%% @end
%%%-----------------------------------------------------------------------

-module(oc_stat_aggregation_sum).

-export([init/3,
         type/0,
         add_sample/4,
         export/2,
         clear_rows/2]).

-behavior(oc_stat_aggregation).

init(_Name, _Keys, Options) ->
    Options.

type() ->
    sum.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, Options) ->
    case counters_sum:observe(Name, Tags, Value) of
        unknown ->
            case counters_sum:new(Name, Tags, 1, Value) of
                ok -> ok;
                false ->
                    add_sample(Name, Tags, Value, Options)
            end;
        _ ->
            ok
    end.

export(Name, _Options) ->
    Rows = maps:values(maps:map(fun(Tags, {Count, Sum}) ->
                                        Mean = case Count of
                                                   0 -> 0;
                                                   _ -> Sum / Count
                                               end,
                                        #{tags => Tags,
                                          value => #{count => Count,
                                                     sum => Sum,
                                                     mean => Mean}}
                                end,
                                counters_sum:value(Name))),
    #{type => type(),
      rows => Rows}.

clear_rows(Name, _Options) ->
    counters_sum:remove(Name),
    ok.
