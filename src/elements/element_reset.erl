%
%    Jabber.se Web Application
%    Copyright (C) 2011 Jonas Ådahl
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

-module(element_reset).
-export([render_element/1]).

-include("include/ui.hrl").

render_element(Record) ->
    Value = ["  ", wf:html_encode(Record#reset.text, Record#reset.html_encode), "  "], 
    wf_tags:emit_tag(input, [
        {type, reset},
        {class, [button, reset, Record#reset.class]},
        {style, Record#reset.style},
        {value, Value}
    ]).
