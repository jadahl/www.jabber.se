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

-module(action_update_table).
-export([render_action/1]).

-include("include/ui.hrl").

render_action(#update_table{target = Target, rows = Rows, effect = Effect}) ->
    [
        % remove old rows except header
        #update{
            type = remove,
            target = Target ++ " .tablerow:gt(0)"
        },

        % insert new rows
        #update{
            type = insert_bottom,
            target = Target,
            elements = Rows
        },

        case Effect of
            undefined ->
                [];
            _ ->
                wf_utils:replace_with_base((wf_utils:get_actionbase(Effect))#actionbase{target = Target ++ " .tablerow:gt(0)"}, Effect)
        end
    ].

