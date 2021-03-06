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
-include_lib("nitrogen_core/include/wf.hrl").

-include("include/utils.hrl").
-include("include/menu.hrl").
-include("include/db/db.hrl").

-export([body_single/1, body/2, atom/3, event/1]).

%
% Entry points
%

body_single(Id) ->
    Post = db_post:get_post(Id),
    #db_post{title = Title} = Post,
    [#h2{text = db_post:t(Title)}, cms_post_view:post_to_html(Post, true)].

body(Title, View) ->
    Posts = db_post:get_posts_by_view(View),
    Title = ?TXT("News"),
    [#h2{text = Title} | lists:map(fun cms_post_view:post_to_html/1, Posts)].

atom(URL, SubTitle, View) ->
    Contents = db_post:get_posts_by_view(View),
    [<<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>">>,
     cms_post_view:posts_to_atom(Contents, URL, cf_config:title(), SubTitle)].

%
% Events
%

event(Event) ->
    ?LOG_WARNING("Unexpected event to cms module: ~p~n", [Event]).

