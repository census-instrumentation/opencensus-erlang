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
    [multiple_child_spans, attributes_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(opencensus),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

multiple_child_spans(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanName2 = <<"span-2">>,
    SpanName3 = <<"span-3">>,
    ocp:start_trace(),
    ocp:start_span(SpanName1),
    ?assertMatch(#span{name=SpanName1}, ocp:finish_span()),
    ocp:start_span(SpanName1),
    ocp:start_span(SpanName2),
    ocp:start_span(SpanName3),
    ?assertMatch(#span{name=SpanName3}, ocp:finish_span()),
    ?assertMatch(#span{name=SpanName2}, ocp:finish_span()),
    ocp:finish_span(),
    ?assertMatch(undefined, ocp:finish_span()).

attributes_test(_Config) ->
    SpanName1 = <<"span-1">>,
    ocp:start_trace(),
    ocp:start_span(SpanName1),
    ocp:put_attribute(<<"attr-0">>, <<"value-0">>),
    ocp:put_attributes(#{<<"attr-0">> => true,
                         <<"attr-4">> => 5423,
                         <<"attr-5">> => <<"value-5">>}),

    ChildSpanName1 = <<"child-span-1">>,
    ocp:start_span(ChildSpanName1),

    ocp:put_attribute(<<"attr-1">>, <<"value-1">>),
    ocp:put_attribute(<<"attr-2">>, 123),
    ocp:put_attribute(<<"attr-3">>, false),

    %% attribute keys must be binary strings and values must be binary strings, integers or booleans
    ?assertEqual({error, invalid_attribute}, ocp:put_attribute('attr-6', <<"value-6">>)),
    ?assertEqual({error, invalid_attribute}, ocp:put_attribute(<<"attr-7">>, 1.0)),

    FinishedChild = ocp:finish_span(),

    ?assertNot(maps:is_key(<<"attr-0">>, FinishedChild#span.attributes)),
    ?assertNot(maps:is_key(<<"attr-4">>, FinishedChild#span.attributes)),
    ?assertNot(maps:is_key(<<"attr-5">>, FinishedChild#span.attributes)),
    ?assertEqual(<<"value-1">>, maps:get(<<"attr-1">>, FinishedChild#span.attributes)),
    ?assertEqual(123, maps:get(<<"attr-2">>, FinishedChild#span.attributes)),
    ?assertEqual(false, maps:get(<<"attr-3">>, FinishedChild#span.attributes)),

    FinishedSpan = ocp:finish_span(),

    ?assertEqual(SpanName1, FinishedSpan#span.name),
    ?assert(FinishedSpan#span.end_time > FinishedSpan#span.start_time),
    ?assertEqual(true, maps:get(<<"attr-0">>, FinishedSpan#span.attributes)),
    ?assertEqual(5423, maps:get(<<"attr-4">>, FinishedSpan#span.attributes)),
    ?assertEqual(<<"value-5">>, maps:get(<<"attr-5">>, FinishedSpan#span.attributes)).
