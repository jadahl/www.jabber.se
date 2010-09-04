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

-module(cms_admin).
-export([selected/0, left/0, title/0, body/0, hook/0, set_body/1, event/1]).
-behaviour(gen_cms_admin_module).

-include("include/utils.hrl").

%
% Control
%

set_body(?MODULE) ->
    cms_admin_view:set_body_close(title(), body());

set_body(Module) ->
    cms_admin_view:set_body_back(Module:title(), Module:body(), Module, ?MODULE),
    Module:hook().

select(Module) ->
    Module:selected(),
    set_body(Module).

back(Current, Module) ->
    Current:left(),
    set_body(Module).

%
% Admin control
%

selected() ->
    ok.

left() ->
    ok.

title() ->
    cms_admin_view:title().

body() ->
    cms_admin_view:body().

hook() ->
    ok.

%
% Events
%

event(admin) ->
    ?AUTH(cms_admin_view:show_dialog());

event({back, Current, Module}) ->
    ?AUTH(back(Current, Module));

event({select, Selected}) ->
    ?AUTH(select(Selected)).

