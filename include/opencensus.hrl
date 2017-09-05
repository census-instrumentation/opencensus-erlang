-record(trace_context, {
          trace_id   :: opencensus:trace_id() | undefined,
          span_id    :: opencensus:span_id() | undefined,
          enabled    :: boolean() | undefined,

          sampler    :: module(),
          reporter   :: module(),
          propagator :: module()
         }).

-record(span, {
          %% start timestamp
          timestamp  :: opencensus:time_us(),
          %% 128 bit int trace id
          trace_id   :: opencensus:trace_id() | undefined,
          %% name of the span
          name       :: unicode:chardata(),
          %% 64 bit int span id
          id         :: opencensus:span_id() | undefined,
          %% 64 bit int parent span
          parent_id  :: opencensus:span_id() | undefined,
          %% microseconds between span start/end
          duration   :: opencensus:time_us(),

          annotations :: opencensus:annotations(),
          attributes  :: opencensus:attributes(),

          %% link to a span in another trace
          links       :: opencensus:links()
         }).

-record(link, {
          type :: opencensus:link_type(),
          trace_id :: opencensus:trace_id(),
          span_id    :: opencensus:span_id(),
          attributes  :: opencensus:attributes()
         }).
