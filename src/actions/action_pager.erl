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

-module(action_pager).
-export([render_action/1]).

-include("include/ui.hrl").
-include("include/utils.hrl").

%sub_id(Outer, Inner) ->
%    lists:flatten([Outer, " > .wfid_", atom_to_list(Inner)]).

render_action(#pager_set{target = _Target, new = New, count = Count, adapter = Adapter}) ->
    #update{
        type = update,
        elements = [
            element_pager:prev(New, Count, Adapter),
            [element_pager:num(Num, New, Count, Adapter) || Num <- utils:range(Count)],
            element_pager:next(New, Count, Adapter)
        ]
    }.

