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

-module(cms).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("nitrogen/include/wf.inc").

-include("include/utils.hrl").
-include("include/menu.hrl").
-include("include/db/db.hrl").
-export([body_single/1, body/0, atom/2, event/1]).

%
% Entry points
%

body_single(Id) ->
    Post = db_post:get_post(Id),
    Title = db_post:t(Post#db_post.title),
    [#h2{text = Title}, cms_view:post_to_html(Post, true)].

body() ->
    Posts = db_post:get_posts_by_view("news"),
    [#h2{text = ?T(msg_id_news)} | lists:map(fun cms_view:post_to_html/1, Posts)].

atom(Url, SubTitle) ->
    Contents = db_post:get_posts_by_view("news"),
    [<<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>">>, cms_view:posts_to_atom(Contents, Url, ?TITLE, SubTitle)].

%
% Events
%

event(Event) ->
    ?LOG_WARNING("Unexpected event to cms module: ~p~n", [Event]).

