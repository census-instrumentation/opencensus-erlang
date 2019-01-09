%% @doc
%% Registry maintains a set of metric producers for exporting.
%% Most users will rely on the DefaultRegistry.
%% @end
-module(oc_producer_registry).

-export([read_to_list/0,
         read_to_list/1,

         read_all/1,
         read_all/2,

         add_producer/1,
         add_producer/2,

         remove_producer/1,
         remove_producer/2]).

%% @equiv read_all(default)
read_all(Callback) ->
    read_all(default, Callback).

%% @doc
%% Calls `Callback' for each metric produced by the metric producers in the `Registry'.
%% @end
read_all(Registry, Callback) ->
    [Producer:read(Registry, Callback) || {_, Producer} <- ets:lookup(?MODULE, Registry)],
    ok.

%%
read_to_list() ->
    read_to_list(default).

read_to_list(Registry) ->
    Ref = make_ref(),
    try
        Callback = fun (M) ->
                           put(Ref, [M|get_list(Ref)])
                   end,
        read_all(Registry, Callback),

        get_list(Ref)
    after
        erase(Ref)
    end.

get_list(Key) ->
    case get(Key) of
        undefined ->
            [];
        Value ->
            Value
    end.

%% @equiv(default, Producer)
add_producer(Producer) ->
    add_producer(default, Producer).

%% @doc
%% Adds `Producer' to the `Registry'.
%% @end
add_producer(Registry, Producer) ->
    ets:insert(?MODULE, {Registry, Producer}),
    ok.

%% @equiv remove_producer(default, Producer)
remove_producer(Producer) ->
    remove_producer(default, Producer).

remove_producer(Registry, Producer) ->
    ets:delete_object(?MODULE, {Registry, Producer}),
    case erlang:function_exported(Producer, cleanup, 1) of
        true -> Producer:cleanup(Registry);
        _ -> ok
    end,
    ok.
