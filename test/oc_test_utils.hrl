%% Try for 1 seconds
-define(UNTIL(X), (fun Until(I) when I =:= 10 ->
                           erlang:error(fail);
                       Until(I) ->
                           case X of
                               true ->
                                   ok;
                               false ->
                                   timer:sleep(100),
                                   Until(I+1)
                           end
                   end)(0)).

-define(OCP_FINISH(Tab),
        (fun() ->
                 __SpanCtx = ocp:current_span_ctx(),
                 ocp:finish_span(),

                 %% wait for span to be reported
                 ?UNTIL(ets:member(Tab, __SpanCtx#span_ctx.span_id))
         end)()).

-define(FINISH(Tab, SpanCtx),
        begin
            oc_trace:finish_span(SpanCtx),

            %% wait for span to be reported
            ?UNTIL(ets:member(Tab, SpanCtx#span_ctx.span_id))
        end).
