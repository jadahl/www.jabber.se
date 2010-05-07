-module(db_post).
-export([parse_doc/2, render_post/1, get_posts/0, save_posts/2]).

-include("include/utils.hrl").
-include("include/cms/db.hrl").

parse_doc({K, V}, P) ->
    case K of
        '_id' -> P#db_post{id = V};
        title -> P#db_post{title = binary_to_list(V)};
        ts -> P#db_post{timestamp = list_to_integer(binary_to_list(V))};
        lang -> P#db_post{lang = V};
        tags -> P#db_post{tags = V};
        authors -> P#db_post{authors = lists:map(fun binary_to_list/1, V)};
        body -> P#db_post{body = binary_to_list(V)};

        type -> P;
        '_rev' -> P;
        _ ->
            ?LOG_INFO("Parser: Unknown entry ~p", [K]),
            P
    end.

render_post(#db_post{
        title = Title,
        timestamp = TimeStamp,
        lang = Lang,
        tags = Tags,
        authors = Authors,
        body = Body}) ->
    {
        undefined,
        post,
        [
            {title, Title},
            {ts, TimeStamp},
            {lang, Lang},
            {tags, Tags},
            {authors, Authors},
            {body, Body}
        ]
   
    }.

get_posts() ->
    View = db_controller:get_view("posts", []),
    Rows = db_utils:view_rows(View),
    lists:map(fun({_, _, Posts}) -> Posts end, db_utils:parse(fun parse_doc/2, #db_post{}, Rows)).

save_posts(Posts, Db) ->
    db_controller:save_docs(fun render_post/1, Posts, Db).

