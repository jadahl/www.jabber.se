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

-module(stk_ui).
-include("include/ui.hrl").
-export([render/1, render_ui/1]).

render([C | Rest] = String) when is_number(C) and is_list(Rest) ->
    String;
render(List) when is_list(List) ->
    lists:map(fun render/1, List);
render(Record) when is_tuple(Record) ->
    case element(2, Record) of
        ui_content ->
            Render = render_ui(Record),
            Render;
        _ ->
            Record
    end;
render(Content) ->
    Content.

to_ui_base(UI) ->
    list_to_tuple([ui_base | lists:sublist(tuple_to_list(UI), 2, size(#ui_base{}) - 1)]). 

render_ui(UI) ->
    BaseUI = to_ui_base(UI),
    Module = BaseUI#ui_base.module,
    Module:render_ui(UI).
