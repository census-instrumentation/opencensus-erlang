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
-define(SET_LOG_METADATA(TraceId, SpanId),
        logger:update_process_metadata(#{trace_id => io_lib:format("~32.16.0b", [TraceId]),
                                         span_id => io_lib:format("~16.16.0b", [SpanId])})).
-else.
-define(SET_LOG_METADATA(TraceId, SpanId), skip).
-endif.
