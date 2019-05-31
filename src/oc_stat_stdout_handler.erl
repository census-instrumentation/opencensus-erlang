-module(oc_stat_stdout_handler).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2]).

init(Opts) -> {ok, Opts}.

handle_call(_Msg, State) -> {ok, ok, State}.

handle_event({stats, ViewData}, State) ->
    [io:format("~s: ~p~n", [Name, Data]) || #{name := Name,
                                              data := Data} <- ViewData],
    {ok, State}.
