-ifdef('21.0').
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-endif.

-ifdef('21.0').
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(LOG_ERROR(Format, Args), error_logger:error_msg(Format, Args)).
-endif.

-ifdef('21.0').
-define(SET_LOG_METADATA(SpanCtx),
        case SpanCtx of
            undefined ->
                logger:update_process_metadata(#{span_ctx => undefined});
            _ ->
                logger:update_process_metadata(#{span_ctx => #{trace_id => io_lib:format("~32.16.0b", [SpanCtx#span_ctx.trace_id]),
                                                               span_id => io_lib:format("~16.16.0b", [SpanCtx#span_ctx.span_id])}})
        end).
-else.
-define(SET_LOG_METADATA(SpanCtx), skip).
-endif.
