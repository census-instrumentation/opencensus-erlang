-module(oc_tag_context).

-export([new/2,
         from_ctx/1]).

-define(TAG_CONTEXT_KEY, oc_tag_context_key).

new(Ctx, Map) ->
    ctx:with_value(Ctx, ?TAG_CONTEXT_KEY, Map).

from_ctx(Ctx) ->
    ctx:get(Ctx, ?TAG_CONTEXT_KEY, oc_tags:new()).

