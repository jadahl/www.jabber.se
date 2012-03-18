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

-module(content_about).
-include_lib("nitrogen_core/include/wf.hrl").

-include("include/content.hrl").
-include("include/utils.hrl").
-include("include/menu.hrl").

-export([
        body/1
    ]).

body(["about"]) ->
    body1("page_about");
body(["about" | _] = Path) ->
    Id = cf_utils:join(Path, "/"),
    body1(Id).

body1(Id) ->
    #content{
        body = cms:body_single(Id),
        title = ?TXT("About")}.
