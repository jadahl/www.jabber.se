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
-export([render_action/1, set_action/4, show_action/2, hide_action/1]).

-include("include/ui.hrl").

%
% Render
%

render_action(#state_panel_set{
        target = Target,
        key = Key,
        animate = Animate,
        validate_group = ValidateGroup}) ->
    set_action(Key, Animate, ValidateGroup, Target);
render_action(#state_panel_show{target = Target, key = Key}) ->
    show_action(Key, Target);
render_action(#state_panel_hide{target = Target}) ->
    hide_action(Target).

%
% API
%

%set_action_animate(Key, Id) ->
%    set_action(Key, undefined, true, Id).
%
%set_action_noanimate(Key, Id) ->
%    set_action(Key, undefined, false, Id).
%
%set_action(Key, Id) ->
%    set_action_noanimate(Key, Id).

set_action(Key, Animate, ValidateGroup, Id) ->
    #js_call{fname = "$Site.$state_panel_set", args = [Id, Key, Animate, ValidateGroup]}.

show_action(Key, Id) ->
    #js_call{fname = "$Site.$state_panel_show", args = [Key, Id]}.

hide_action(Id) ->
    #js_call{fname = "$Site.$state_panel_hide", args = [Id]}.

%set(Key, Id) ->
%    wf:wire(set_action(Key, Id)).

