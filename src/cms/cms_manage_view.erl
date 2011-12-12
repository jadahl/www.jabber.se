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
-export([title/0, no_posts_text/0, body/3, set_page/4, set_page_recounted/4]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("include/utils.hrl").
-include("include/db/db.hrl").
-include("include/ui.hrl").

-define(POSTS_PER_PAGE, 6).

-type posts_provider() :: fun((integer(), integer()) -> list(#db_post{})).

title() ->
    ?T(msg_id_manage_dialog_title).

no_posts_text() ->
    ?T(msg_id_posts_empty).

%
% Pager
%

-spec num_pages(integer()) -> integer().
num_pages(PostCount) ->
    (PostCount div ?POSTS_PER_PAGE) + (if PostCount rem ?POSTS_PER_PAGE > 0 -> 1; true -> 0 end).

-spec set_page_recounted(posts_provider(), module(), integer(), integer()) -> ok.
set_page_recounted(PostsFun, Adapter, New, PostCount) ->
    Pages = num_pages(PostCount),
    set_page(PostsFun, Adapter, New, Pages).

-spec set_page(posts_provider(), module(), integer(), integer()) -> ok.
set_page(PostsFun, Adapter, New, Count) ->
    % set anchor to dialog
    wf_context:anchor(admin_dialog),

    if
        Count == 0 ->
            % no more posts
            wf:wire(cms_manage_table,
                #update_table{rows = [no_posts_cell(Adapter)]}
            ),

            set_pager(New, Count, Adapter);
        true ->
            % update posts
            Posts = PostsFun(?POSTS_PER_PAGE * (New - 1), ?POSTS_PER_PAGE),

            if
                (Posts == []) and (New > 1) ->
                    set_page(PostsFun, Adapter, New - 1, Count);
                true ->
                    wf:wire(cms_manage_table,
                        #update_table{
                            rows = posts_to_rows(Posts, New, Adapter)
                        }
                    ),

                    set_pager(New, Count, Adapter)
            end
    end.

-spec set_pager(integer(), integer(), module()) -> ok.
set_pager(New, Count, Adapter) ->
    % update pager
    if
        Count > 1 ->
            wf:wire(cms_manage_pager,
                #pager_set{
                    new = New,
                    count = Count,
                    adapter = Adapter});
        true ->
            wf:remove(cms_manage_pager)
    end,
    ok.

%
% Table content
%

-spec title_link(db_post:content(), db_post:post(), module()) -> #link{}.
title_link(Titles, Post, Delegate) ->
    case db_post:value_prefer_locale(cf_i18n:get_language(), Titles) of
        nothing ->
            title_link(undefined, undefined, Post, Delegate);
        {Locale, Title} ->
            title_link(Title, Locale, Post, Delegate)
    end.

-spec title_link(undefined | binary(), atom(), db_post:post(), module()) -> #link{}.
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
        delegate = cms_compose,
        postback = {open, Post#db_post.id, Locale, Delegate},
        text = LinkTitle,
        title = ?T(msg_id_post_open)
    }.

remove_link(Post, CurrentPage, Delegate) ->
    #link{
        postback = {remove_post, Post#db_post.id, CurrentPage},
        delegate = Delegate,
        text = "✕",
        title = ?T(msg_id_post_remove)}.

show_date(undefined) ->
    ?T(msg_id_post_not_published);
show_date(Timestamp) ->
    cf_utils:ts_to_date_s(Timestamp).

no_posts_cell(Adapter) ->
    #tablerow{
        cells = [
            #tablecell{
                text = Adapter:no_posts_text(),
                align = center,
                colspan = 4
            }
        ]
    }.

-spec posts_to_rows(list(#db_post{}), integer(), module()) -> list(#tablerow{}).
posts_to_rows(Posts, CurrentPage, Delegate) ->
    [
        #tablerow{
            cells = [
                #tablecell{
                    body = title_link(Post#db_post.title, Post, Delegate),
                    class = title},
                #tablecell{
                    text = Post#db_post.authors,
                    class = authors},
                #tablecell{
                    text = show_date(Post#db_post.timestamp),
                    class = date},
                #tablecell{
                    body = remove_link(Post, CurrentPage, Delegate),
                    class = remove}
            ]
        }
        || Post <- Posts
    ].

-spec panel(posts_provider(), module(), integer()) -> term().
panel(PostsFun, Adapter, PostCount) ->
    Posts = PostsFun(0, ?POSTS_PER_PAGE),
    Pages = num_pages(PostCount),

    [
        #table{
            class = posts_table,
            id = cms_manage_table,
            rows = [
                #tablerow{
                    cells = [
                        #tableheader{text = ?T(msg_id_post_title), class = title},
                        #tableheader{text = ?T(msg_id_post_authors), class = authors},
                        #tableheader{text = ?T(msg_id_post_date), class = date},
                        #tableheader{class = remove}
                    ]
                },

                case Posts of
                    [] ->
                        no_posts_cell(Adapter);
                    _ ->
                        posts_to_rows(Posts, 1, Adapter)
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


-spec body(posts_provider(), module(), integer()) -> term().
body(PostsFun, Adapter, PostCount) ->
    panel(PostsFun, Adapter, PostCount).

