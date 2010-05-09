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

-module(action_state_panel).
-export([render_action/1, set_action/2, set_action/3, show_action/2, hide_action/1, set/2]).

-include("include/ui.hrl").

%
% Render
%

render_action(#state_panel_set{
        target = Target,
        key = Key,
        validate_group = ValidateGroup}) ->
    action_state_panel:set_action(Key, ValidateGroup, Target);
render_action(#state_panel_show{target = Target, key = Key}) ->
    action_state_panel:show_action(Key, Target);
render_action(#state_panel_hide{target = Target}) ->
    action_state_panel:hide_action(Target).

%
% API
%

set_action(Key, Id) ->
    set_action(Key, undefined, Id).
set_action(Key, ValidateGroup, Id) ->
    #js_call{fname = "$Site.$state_panel_set", args = [Key, ValidateGroup, Id]}.

show_action(Key, Id) ->
    #js_call{fname = "$Site.$state_panel_show", args = [Key, Id]}.

hide_action(Id) ->
    #js_call{fname = "$Site.$state_panel_hide", args = [Id]}.

set(Key, Id) ->
    wf:wire(set_action(Key, Id)).

