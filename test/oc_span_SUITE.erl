%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(oc_span_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [modifications].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

modifications(_Config) ->
    Span = #span{trace_id=opencensus:generate_trace_id(),
                 span_id=opencensus:generate_span_id(),
                 start_time=wts:timestamp(),
                 attributes=#{}},
    Span1 = oc_span:put_attribute(<<"key">>, <<"value">>, Span),
    ?assertEqual(undefined, oc_span:put_attribute(<<"key">>, <<"value">>, undefined)),

    Span2 = oc_span:put_attributes(#{<<"key2">> => <<"value2">>}, Span1),
    ?assertEqual(undefined, oc_span:put_attributes(#{<<"key">> => <<"value">>}, undefined)),

    Annotation = oc_span:annotation(<<"description">>, #{<<"attr">> => <<"attr-value">>}),
    Span3 = oc_span:add_time_event(Annotation, Span2),
    ?assertEqual(undefined, oc_span:add_time_event(Annotation, undefined)),

    MessageEvent = oc_span:message_event(?MESSAGE_EVENT_TYPE_SENT, 1, 0, 0),
    Span4 = oc_span:add_time_event(MessageEvent, Span3),
    ?assertEqual(undefined, oc_span:add_time_event(MessageEvent, undefined)),

    Span5 = oc_span:set_status(1, <<"ok">>, Span4),
    ?assertEqual(undefined, oc_span:set_status(1, <<"ok">>, undefined)),

    Link = oc_span:link(?LINK_TYPE_UNSPECIFIED, opencensus:generate_trace_id(),
                        opencensus:generate_span_id(), #{<<"attr-1">> => <<"value-1">>}),
    Span6 = oc_span:add_link(Link, Span5),
    ?assertEqual(undefined, oc_span:add_link(Link, undefined)),

    Span7 = oc_span:set_kind(?SPAN_KIND_SERVER, Span6),
    ?assertEqual(undefined, oc_span:set_kind(?SPAN_KIND_SERVER, undefined)),

    ?assertEqual({error, no_report_buffer}, oc_span:finish_span(#span_ctx{}, Span7)),

    {ok, _} = application:ensure_all_started(opencensus),
    ?assertEqual(true, oc_span:finish_span(#span_ctx{}, Span7)),
    ?assertEqual(true, oc_span:finish_span(#span_ctx{}, undefined)),

    application:stop(opencensus).
