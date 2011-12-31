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

-module(cms_drafts_view).
-export([title/0, no_posts_text/0, body/3]).

-include("include/utils.hrl").
-include("include/ui.hrl").
-include("include/db/db.hrl").

title() ->
    ?TXT("Drafts").

no_posts_text() ->
    ?TXT("No drafts...").

body(PostsFun, Adapter, Count) ->
    cms_manage_view:body(PostsFun, Adapter, Count).
