%%%-------------------------------------------------------------------
%% @doc helloworld public API
%% @end
%%%-------------------------------------------------------------------

-module(helloworld_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    create_measures(),
    subscribe_views(),
    helloworld_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

create_measures() ->
  oc_stat_measure:new('my.org/measure/video_size', "Size of a processed video", bytes).

subscribe_views() ->
    oc_stat_view:subscribe(#{name => "video_size",
                             description => "size of processed videos",
                             tags => ['frontend'],
                             measure => 'my.org/measure/video_size',
                             aggregation => default_size_distribution()}).

default_size_distribution() ->
    {oc_stat_aggregation_distribution, [{buckets, [0, 1 bsl 16, 1 bsl 32]}]}.
