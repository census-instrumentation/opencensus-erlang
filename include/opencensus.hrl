-record(trace_context, {
          trace_id   :: opencensus:trace_id(),
          span_id    :: opencensus:span_id(),
          enabled    :: boolean(),

          sampler    :: module(),
          reporter   :: module(),
          propagator :: module()
}).

-record(span, {
          %% strt timestamp
          timestamp  :: opencensus:time_us(),
          %% 64 bit int trace id
          trace_id   :: opencensus:trace_id() | undefined,
          %% name of the span
          name       :: opencensus:info(),
          %% 64 bit int span id
          id         :: opencensus:span_id() | undefined,
          %% 64 bit int parent span
          parent_id  :: opencensus:span_id() | undefined,
          %% Tags
          tags = #{} :: otter:tags(),
          %%  logs
          logs = []  :: [{opencensus:info(), opencensus:info(),
                          opencensus:service()}],
          %% microseconds between span start/end
          duration   :: opencensus:time_us()
}).
