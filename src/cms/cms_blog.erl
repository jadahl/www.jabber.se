-module(cms_blog).
-export([parse_content/1, render_content/1]).

-include("include/utils.hrl").
-include("include/cms/cms_db.hrl").

parse_content(Post) ->
    lists:foldl(fun parse_blog_post_entry/2, #blog_post{}, Post).

parse_blog_post_entry({K, V}, BP) ->
    case K of
        title -> BP#blog_post{title = V};
        body -> BP#blog_post{body = V};
        tags -> BP#blog_post{tags = V};
        lang -> BP#blog_post{lang = V};
        _ ->
            ?LOG_INFO("Parser: Unknown entry ~p", [K]),
            BP
    end.

render_content(#blog_post{
        title = Title,
        body = Body,
        tags = Tags,
        lang = Lang}) ->
    {[
        {title, Title},
        {lang, Lang},
        {body, Body},
        {tags, Tags}
    ]}.

