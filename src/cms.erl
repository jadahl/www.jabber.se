-module(cms).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("nitrogen/include/wf.inc").

-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/menu.hrl").
-include("include/cms/db.hrl").
-export([body/0, atom/2, event/1]).

%
% Atom
%

% TODO
%
% * make id a checksum
%
%<entry>
%  <link href="http://example.org/2003/12/13/atom03"/>
%</entry>

post_to_atom_entry(#db_post{
        id = Id,
        timestamp = Timestamp,
        authors = Authors,
        title = Title,
        body = Body,
        tags = _Tag}) ->
    {entry,
        {[
            {title, [Title]},
            {id, [Id]},
            {published, utils:time_to_iso8601(Timestamp)},
            {[{author, {[{name, [Author]}]}} || Author <- Authors]},
            {content, [{type, "xhtml"}], {[{'div', [{xmlns, "http://www.w3c.org/1999/xhtml"}], {[Body]}}]}}
        ]}}.

get_last_updated(Contents) ->
    lists:foldl(
        fun (X, R) ->
                if 
                    X#db_post.timestamp > R#db_post.timestamp ->
                        X;
                    true ->
                        R
                end
        end, 0, Contents).

posts_to_atom(Contents, Url, Title, SubTitle) ->
    utils:to_xml({
            feed,
            [{xmlns, "http://www.w3.org/2005/Atom"}],
            {[
                {title, Title},
                {subtitle, SubTitle},
                {link, [{href, ?URL_BASE ++ Url}], []},
                {updated, utils:time_to_iso8601(get_last_updated(Contents))},
                {lists:map(fun post_to_atom_entry/1, Contents)}
            ]}}).

%
% HTML
%

post_to_html(#db_post{
        authors = Authors,
        title = Title,
        body = Body,
        tags = _Tags}) ->
    [#panel{
            body=
            [#label{class = blog_title, text = Title},
                #br{},
                #span{class = blog_by,
                    text = "by " ++
                    case Authors of
                        [Author] -> Author;
                        _ -> "unknown"
                    end},
                #br{},
                #p{class = blog_body, body = Body}]},
        #br{}].

%
% Entry points
%

body() ->
    Posts = db_post:get_posts(),
    [#h2{text = "News"} | lists:map(fun post_to_html/1, Posts)].

atom(Url, SubTitle) ->
    Contents = db_post:get_posts(),
    [<<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>">>, posts_to_atom(Contents, Url, ?TITLE, SubTitle)].

%
% Events
%

event(Event) ->
    ?LOG_WARNING("Unexpected event to cms module: ~p~n", [Event]).

