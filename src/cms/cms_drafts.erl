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

-module(cms_drafts).
-export([
        selected/0, left/0, title/0, body/0, hook/0,
        event/1, no_posts_text/0, pager_set/2
    ]).
-behaviour(gen_cms_admin_module).
-behaviour(gen_pager_adapter).

-include("include/utils.hrl").

%
% Admin control
%

selected() ->
    ok.

left() ->
    ok.

title() ->
    cms_drafts_view:title().

body() ->
    User = wf:user(),
    Count = db_post:get_draft_count(User),
    cms_drafts_view:body(posts_fun(User), ?MODULE, Count).

hook() ->
    ok.

%
% Utils
%

posts_fun(User) ->
    fun(Skip, Limit) ->
            db_post:get_drafts_by(User, Skip, Limit)
    end.

%
% Events
%

event({remove_post, Id, CurrentPage}) ->
    ?AUTH(event_remove_post(Id, CurrentPage)).

%
% Event handlers
%

event_remove_post(Id, CurrentPage) ->
    % remove post
    db_controller:delete_doc_by_id(Id),

    % update pager and table
    User = wf:user(),
    PostCount = db_post:get_draft_count(User),
    cms_manage_view:set_page_recounted(posts_fun(User), ?MODULE, CurrentPage, PostCount).

%
% Manage view adapter
%

no_posts_text() ->
    cms_drafts_view:no_posts_text().

%
% Pager adapter
%

pager_set(Index, Count) ->
    ?AUTH(pager_set2(Index, Count)).

pager_set2(Index, Count) ->
    User = wf:user(),
    cms_manage_view:set_page(fun(Skip, Limit) -> db_post:get_drafts_by(User, Skip, Limit) end, ?MODULE, Index, Count).

