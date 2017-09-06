%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(ocp_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [multiple_child_spans].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

multiple_child_spans(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanName2 = <<"span-2">>,
    SpanName3 = <<"span-3">>,
    ocp:start_span(SpanName1),
    ?assertMatch(#span{name=SpanName1}, ocp:finish_span()),
    ocp:start_span(SpanName1),
    ocp:child_span(SpanName2),
    ocp:child_span(SpanName3),
    ?assertMatch(#span{name=SpanName3}, ocp:finish_span()),
    ?assertMatch(#span{name=SpanName2}, ocp:finish_span()),
    ocp:finish_span(),
    ?assertMatch(undefined, ocp:finish_span()).
