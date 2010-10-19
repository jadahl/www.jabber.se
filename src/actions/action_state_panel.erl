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
-export([render_action/1, set_panel/2, set_action/4, show_action/2, show_action/3, hide_action/1]).

-include("include/ui.hrl").

%
% Render
%

render_action(#state_panel_set{
        target = Target,
        key = Key,
        animate = Animate,
        validate_group = ValidateGroup,
        actions = Actions}) ->
    set_action(Key, Animate, ValidateGroup, Target, Actions);
render_action(#state_panel_show{target = Target, key = Key, actions = Actions}) ->
    show_action(Key, Target, Actions);
render_action(#state_panel_hide{target = Target}) ->
    hide_action(Target).

%
% API
%

set_panel(Key, Target) ->
    wf:wire(Target, #state_panel_set{key = Key}).

set_action(Key, Animate, ValidateGroup, Id) ->
    set_action(Key, Animate, ValidateGroup, Id, []).
set_action(Key, Animate, ValidateGroup, Id, Actions) ->
    #site_cast{cast = state_panel_set, args = [Id, Key, Animate, ValidateGroup, {lambda, Actions}]}.

show_action(Key, Id) ->
    show_action(Key, Id, []).
show_action(Key, Id, Actions) ->
    #site_cast{cast = state_panel_show, args = [Key, Id, {lambda, Actions}]}.

hide_action(Id) ->
    #js_call{fname = "$Site.$state_panel_hide", args = [Id]}.
