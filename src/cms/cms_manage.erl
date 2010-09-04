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

-module(cms_manage).
-export([selected/0, left/0, title/0, body/0, hook/0]).
-behaviour(gen_cms_admin_module).

%
% Admin control
%

selected() ->
    ok.

left() ->
    ok.

title() ->
    cms_manage_view:title().

body() ->
    Drafts = db_post:get_posts_by(wf:user()),
    cms_manage_view:body(Drafts).

hook() ->
    ok.

