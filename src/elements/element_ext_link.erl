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

-module (element_ext_link).
-export([reflect/0, render_element/1]).
-include("include/ui.hrl").

maybe(_Key, undefined) ->
    [];
maybe(Key, Value) ->
    {Key, Value}.

reflect() -> record_info(fields, ext_link).

render_element(Record)->
    wf_tags:emit_tag(link, lists:flatten([
            maybe(href, Record#ext_link.url),
            maybe(type, Record#ext_link.type),
            maybe(rel, Record#ext_link.rel),
            maybe(title, Record#ext_link.title)])).
