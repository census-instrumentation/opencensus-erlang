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
%% Distribution indicates that the desired aggregation is
%% a histogram distribution.
%% @end
%%%-----------------------------------------------------------------------

-module(oc_stat_aggregation_distribution).

-export([init/3,
         type/0,
         add_sample/4,
         export/2,
         clear_rows/2]).

-behavior(oc_stat_aggregation).

-include("opencensus.hrl").

init(_Name, _Keys, Options) ->
    Buckets = counters_buckets:new(proplists:get_value(buckets, Options, default)),
    Buckets.

type() ->
    distribution.

-spec add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.
add_sample(Name, Tags, Value, Buckets) ->
    Bound = counters_buckets:bound(Buckets, Value),
    case counters_distribution:observe(Name, Tags, Bound, Value) of
        unknown ->
            case counters_distribution:new(Name, Tags, Bound, 1, Value) of
                ok -> ok;
                false ->
                    add_sample(Name, Tags, Value, Buckets)
            end;
        _ ->
            ok
    end.

export(Name, Buckets) ->
    Rows = maps:values(
             maps:map(fun(Tags, BMap) ->
                              {Count, Sum} = maps:fold(fun(_Bound, {C, S}, {Ca, Sa}) ->
                                                               {Ca + C, Sa + S}
                                                       end, {0, 0}, BMap),

                              BucketsE = [begin
                                              {C, _} = maps:get(Bound, BMap, {0, 0}),
                                              {Bound, C}
                                          end || Bound <- Buckets],

                              Mean = case Count of
                                         0 -> 0;
                                         _ -> Sum / Count
                                     end,
                              #{tags => Tags,
                                value => #{count => Count,
                                           sum => Sum,
                                           mean => Mean,
                                           buckets => BucketsE}}
                      end,
                      counters_distribution:value(Name))),
    #{type => type(),
      rows => Rows}.

clear_rows(Name, _Options) ->
    counters_distribution:remove(Name),
    ok.
