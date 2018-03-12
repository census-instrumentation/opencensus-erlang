-module(helloworld).

-export([process/0]).

process() ->
    %% create a tag for who sent the request and start a child span
    Tags = oc_tags:new(#{'frontend' => "mobile-ios9.3.5"}),
    ocp:with_child_span(<<"my.org/ProcessVideo">>),

    %% sleep for 0-10 seconds to simulate processing time
    timer:sleep(timer:seconds(rand:uniform(10))),

    %% finish the span
    ocp:finish_span(),

    %% record the size of the video
    oc_stat:record(Tags, 'my.org/measure/video_size', 25648).
