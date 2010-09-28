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
-export([title/0, body/4, body/5, set_page/3]).

-include_lib("nitrogen/include/wf.hrl").
-include("include/utils.hrl").
-include("include/db/db.hrl").
-include("include/ui.hrl").

-define(POSTS_PER_PAGE, 6).

-type posts_provider() :: fun((integer(), integer()) -> list(#db_post{})).

title() ->
    ?T(msg_id_manage_dialog_title).

title_link(Titles, Post, Delegate) when is_list(Titles) ->
    case db_post:value_prefer_locale(i18n:get_language(), Titles) of
        nothing ->
            title_link(undefined, undefined, Post, Delegate);
        {Locale, Title} when is_binary(Title) ->
            title_link(Title, Locale, Post, Delegate)
    end;
title_link(Title, Post, Delegate) when is_binary(Title) ->
    title_link(Title, undefined, Post, Delegate).

title_link(Title, Locale, Post, Delegate) ->
    LinkTitle = case Title of
        undefined ->
            ?T(msg_id_post_untitled);
        _ ->
            case string:strip(binary_to_list(Title)) of
                "" ->
                    ?T(msg_id_post_untitled);
                _ ->
                    Title
            end
    end,

    #link{
        delegate = cms_post,
        postback = {open, Post#db_post.id, Locale, Delegate},
        text = LinkTitle}.

set_page(PostsFun, New, Count) ->
    % update posts
    Posts = PostsFun(?POSTS_PER_PAGE * (New - 1), ?POSTS_PER_PAGE),
    wf_context:anchor(admin_dialog),
    wf:wire(cms_manage_table,
        #update_table{
            rows = posts_to_rows(Posts, cms_manage, false)
        }
    ),

    % update pager
    wf:wire(cms_manage_pager,
        #pager_set{
            new = New,
            count = Count,
            adapter = cms_manage}),
    ok.

show_date(undefined) ->
    ?T(msg_id_post_not_published);
show_date(Timestamp) ->
    utils:ts_to_date_s(Timestamp).

-spec posts_to_rows(list(#db_post{}), iolist(), module()) -> list(#tablerow{}).
posts_to_rows(Posts, Delegate) ->
    posts_to_rows(Posts, Delegate, false).
posts_to_rows(Posts, Delegate, Hidden) ->
    Style = ?WHEN_S(Hidden, ?HIDDEN),
    [
        #tablerow{
            style = Style,
            cells = [
                #tablecell{body = title_link(Post#db_post.title, Post, Delegate)},
                #tablecell{text = Post#db_post.authors},
                #tablecell{text = show_date(Post#db_post.timestamp)}
            ]
        }
        || Post <- Posts
    ].

-spec panel(posts_provider(), module(), integer(), iolist(), module()) -> term().
panel(PostsFun, Adapter, PostCount, NoPostsText, Delegate) ->
    Posts = PostsFun(0, ?POSTS_PER_PAGE),
    Pages = (PostCount div ?POSTS_PER_PAGE) + (if PostCount rem ?POSTS_PER_PAGE > 0 -> 1; true -> 0 end),

    [
        #table{
            class = posts_table,
            id = cms_manage_table,
            rows = [
                #tablerow{
                    cells = [
                        #tableheader{text = ?T(msg_id_post_title)},
                        #tableheader{text = ?T(msg_id_post_authors)},
                        #tableheader{text = ?T(msg_id_post_date)}
                    ]
                },

                case Posts of
                    [] ->
                        #tablerow{
                            cells = [
                                #tablecell{
                                    text = NoPostsText,
                                    align = center,
                                    colspan = 3
                                }
                            ]
                        };
                    _ ->
                        posts_to_rows(Posts, Delegate)
                end
            ]
        },

        #pager{
            id = cms_manage_pager,
            count = Pages,
            init_page = 1,
            adapter = Adapter
        }
    ].


-spec body(posts_provider(), module(), integer(), module()) -> term().
body(PostsFun, Adapter, PostCount, Delegate) ->
    body(PostsFun, Adapter, PostCount, ?T(msg_id_posts_empty), Delegate).

-spec body(posts_provider(), module(), integer(), iolist(), module()) -> term().
body(PostsFun, Adapter, PostCount, NoPostsText, Delegate) ->
    panel(PostsFun, Adapter, PostCount, NoPostsText, Delegate).

