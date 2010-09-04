%
%    Jabber.se Web Application
%    Copyright (C) 2010 Jonas Ådahl
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

-module(cms_manage_view).
-export([title/0, body/1, body/3]).

-include_lib("nitrogen/include/wf.hrl").
-include("include/utils.hrl").
-include("include/db/db.hrl").

title() ->
    ?T(msg_id_manage_dialog_title).

title_link(undefined, Post, Delegate) ->
    title_link(?T(msg_id_post_untitled), Post, Delegate);
title_link(Title, Post, Delegate) when is_binary(Title) ->
    title_link(binary_to_list(Title), Post, Delegate);
title_link(Title, Post, Delegate) when is_list(Title) ->
    case string:strip(Title) of
        [] ->
            title_link(undefined, Post, Delegate);
        _ ->
            #link{delegate = cms_post, postback = {open, Post, Delegate}, text = Title}
    end.

show_date(undefined) ->
    ?T(msg_id_post_not_published);
show_date(Timestamp) ->
    utils:ts_to_date_s(Timestamp).


panel(Posts, NoPostsText, Delegate) ->
    Rows = case Posts of
        [] ->
            #tablerow{
                cells = [#tablecell{text = NoPostsText, align = center, colspan = 3}]};
        _ ->
            [
                #tablerow{
                    cells = [
                        #tablecell{body = title_link(Post#db_post.title, Post, Delegate)},
                        #tablecell{text = Post#db_post.authors},
                        #tablecell{text = show_date(Post#db_post.timestamp)}
                    ]}
                || Post <- Posts
            ]
    end,

    #table{
        class = drafts_table,
        rows = [
            #tablerow{
                cells = [
                    #tableheader{text = ?T(msg_id_post_title)},
                    #tableheader{text = ?T(msg_id_post_authors)},
                    #tableheader{text = ?T(msg_id_post_date)}
                ]
            },
            Rows
        ]
    }.

body(Posts) ->
    body(Posts, ?T(msg_id_posts_empty), cms_manage).
body(Posts, NoPostsText, Delegate) ->
    panel(Posts, NoPostsText, Delegate).

