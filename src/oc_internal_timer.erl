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
