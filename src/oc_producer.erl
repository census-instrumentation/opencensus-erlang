%% @doc
%% Producer is a source of metrics.
%%
%% Callbacks:
%%
%% - `read(Registry, Callback) -> ok' - called by exporters.
%% Read should call `Callback' with the current values of all metrics
%% supported by this metric provider.
%% The metrics should be unique for each combination of name and resource.
%%
%% - `cleanup(Registry) -> ok' - optional.
%% Called when producer removed from `Registry'
%%
%% @end
-module(oc_producer).

-include("oc_metrics.hrl").

-callback read(Registry, Callback) -> ok when
      Registry :: oc_producer_registry:registry(),
      Callback :: metric_callback().

-callback cleanup(Registry) -> ok when
      Registry :: oc_producer_registry:registry().

-optional_callbacks([cleanup/1]).
