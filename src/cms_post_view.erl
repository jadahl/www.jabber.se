%
%    Jabber.se Web Application
%    Copyright (C) 2010-2011 Jonas Ådahl
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

-module(cms_post_view).
-export([supported_content_type/1,
         post_to_html/1, post_to_html/2, posts_to_atom/4]).

-include_lib("nitrogen_core/include/wf.hrl").

-include("include/utils.hrl").
-include("include/db/db.hrl").

%
% Content types
%

supported_content_type(ContentType) ->
    lists:member(cf_utils:to_binary(ContentType),
                 [<<"application/xhtml+xml">>,
                  <<"text/markdown">>]).

%
% Body transformations
%

post_body_to_html(Post) ->
    case Post#db_post.content_type of
        <<"application/xhtml+xml">> ->
            db_post:t(Post#db_post.body);
        <<"text/markdown">> ->
            S = markdown:conv(binary_to_list(db_post:t(Post#db_post.body))),
            cf_template:linkify(S, get(noscript) == true);
        _ ->
            "undefined"
    end.

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
        title = Titles,
        tags = _Tag} = Post) ->
    Title = db_post:t(Titles),
    Body = post_body_to_html(Post),

    {entry,
        {[
            {title, [Title]},
            {id, [Id]},
            {published, cf_utils:time_to_iso8601(Timestamp)},
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

posts_to_atom(Contents, URL, Title, SubTitle) ->
    % Generate HTML without javascript links
    put(noscript, true),

    Proto = case (wf_context:request_bridge()):scheme() of
        https -> "https://";
        _     -> "http://"
    end,
    cf_utils:to_xml({
            feed,
            [{xmlns, "http://www.w3.org/2005/Atom"}],
            {[
                {title, Title},
                {subtitle, SubTitle},
                {link, [{href, Proto ++ cf_config:host() ++ URL}], []},
                {updated, cf_utils:time_to_iso8601(get_last_updated(Contents))},
                {lists:map(fun post_to_atom_entry/1, Contents)}
            ]}}).

%
% HTML
%

post_to_html(Post) ->
    post_to_html(Post, false).

post_to_html(#db_post{
        authors = Authors,
        title = Titles,
        timestamp = Timestamp,
        tags = _Tags} = Post, Single) ->

    Body = post_body_to_html(Post),

    Title = case Single of
        true -> [];
        false -> 
            [
                #label{class = blog_title, text = db_post:t(Titles)},
                #br{},
                #span{
                    class = cms_post_date,
                    text = [" ", cf_utils:ts_to_date_s(Timestamp), " "]
                },
                " ",
                #span{class = cms_post_by,
                    text = ?TXT("by") ++ " " ++
                    case Authors of
                        [Author] -> cf_utils:to_string(Author);
                        _ -> "unknown"
                    end},
                #br{}
            ]
    end,

    [#panel{
            body=
            [
                Title,
                #panel{class = blog_body, body = Body},
                #br{}
            ]
        }
    ].


