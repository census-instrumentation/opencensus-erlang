%%%-------------------------------------------------------------------
%% @doc opencensus
%% @end
%%%-------------------------------------------------------------------
-module(opencensus).

-export([generate_trace_id/0, generate_span_id/0]).

-include("opencensus.hrl").

-export_type([info/0, service/0, trace_id/0, span_id/0,
              trace_context/0,
              span/0, maybe_span/0, tags/0, maybe/1]).

-type info()          :: binary() | iolist() | atom() | integer().
-type ip4()           :: {0..255, 0..255, 0..255, 0..255}.
-type service()       :: binary() | list() | default | undefined | {binary() | list(), ip4(), integer()}.
-type trace_id()      :: non_neg_integer().
-type span_id()       :: non_neg_integer().
-type tag()           :: {info(), service()} | binary() | string() | atom().
-type tags()          :: #{binary() => tag()}.
-type trace_context() :: #trace_context{}.
-type span()          :: #span{}.
-type maybe(B)        :: B | undefined.
-type maybe_span()    :: maybe(span()).

%% timestamp in microseconds
-type time_us() :: non_neg_integer().

-spec generate_trace_id() -> non_neg_integer().
generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    Id.

-spec generate_span_id() -> non_neg_integer().
generate_span_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    Id.
