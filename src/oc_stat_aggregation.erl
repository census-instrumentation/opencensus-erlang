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
%% Aggregation represents a data aggregation method.
%% @end
%%%-----------------------------------------------------------------------

-module(oc_stat_aggregation).

-export([convert/3]).

-export_types(data/0).

-type data_rows(AggregationValue) :: [#{tags := tv(),
                                        value := AggregationValue}].

-type data(Type, AggregationValue) :: #{type := Type,
                                        rows := data_rows(AggregationValue)}.

-type data() :: data(latest, number())
              | data(count, number())
              | data(sum, #{count := non_neg_integer(),
                            mean := number(),
                            sum := number()})
              | data(distribution, #{count := non_neg_integer(),
                                     mean := number(),
                                     sum := number(),
                                     buckets := [{number(), non_neg_integer()}]}).

-type keys() :: [oc_tags:key()].
-type tv()   :: [oc_tags:value()].

-callback init(oc_stat_view:name(), keys(), any()) -> any().

-callback type() -> latest | count | sum | distribution.

-callback add_sample(oc_stat_view:name(), oc_tags:tags(), number(), any()) -> ok.

-callback export(oc_stat_view:name(), any()) -> data().

-callback clear_rows(oc_stat_view:name(), any()) -> ok.

convert(Data, _From, undefined) ->
    Data;
convert(#{type := Type,
          rows := Rows}, From, To) ->
    #{type => Type,
      rows => convert_rows(Type, Rows, From, To)}.

convert_rows(latest, Rows, From, To) ->
    [Row#{value => oc_stat_unit:convert(Value, From, To)}
     || #{value := Value}=Row <- Rows];
convert_rows(count, Rows, From, To) ->
    [Row#{value => oc_stat_unit:convert(Value, From, To)}
     || #{value := Value}=Row <- Rows];
convert_rows(sum, Rows, From, To) ->
    [Row#{value => Value#{sum => oc_stat_unit:convert(Sum, From, To),
                          mean => oc_stat_unit:convert(Mean, From, To)}}
     || #{value := #{sum := Sum,
                     mean := Mean}=Value}=Row <- Rows];
convert_rows(distribution, Rows, From, To) ->
    [Row#{value => Value#{sum => oc_stat_unit:convert(Sum, From, To),
                          mean => oc_stat_unit:convert(Mean, From, To),
                          buckets => convert_buckets(Buckets, From, To)}}
     || #{value := #{sum := Sum,
                     mean := Mean,
                     buckets := Buckets}=Value}=Row <- Rows].

convert_buckets(Buckets, From, To) ->
    [{maybe_convert_bound(Bound, From, To), Counter} || {Bound, Counter} <- Buckets].

maybe_convert_bound(infinity, _From, _To) ->
    infinity;
maybe_convert_bound(Bound, From, To) ->
    oc_stat_unit:convert(Bound, From, To).
