-ifdef(OTP_RELEASE).
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-endif.

-ifdef(OTP_RELEASE).
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(LOG_ERROR(Format, Args), error_logger:error_msg(Format, Args)).
-endif.

-ifdef(OTP_RELEASE).
-define(SET_LOG_METADATA(SpanCtx),
        case SpanCtx of
            undefined ->
                logger:update_process_metadata(#{span_ctx => undefined});
            _ ->
                logger:update_process_metadata(#{span_ctx => #{trace_id => io_lib:format("~32.16.0b", [SpanCtx#span_ctx.trace_id]),
                                                               span_id => io_lib:format("~16.16.0b", [SpanCtx#span_ctx.span_id]),
                                                               trace_options => integer_to_list(SpanCtx#span_ctx.trace_options)}})
        end).
-else.
-define(SET_LOG_METADATA(SpanCtx), skip).
-endif.
