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

-module(action_state_panel).
-export([render_action/1, set_panel/2]).

-include("include/ui.hrl").

%
% Render
%

render_action(#state_panel_set{
        key = Key,
        animate = Animate,
        validate_group = ValidateGroup,
        actions = Actions}) ->
    set_action(Key, Animate, ValidateGroup, Actions);
render_action(#state_panel_show{key = Key, actions = Actions}) ->
    show_action(Key, Actions);
render_action(#state_panel_hide{}) ->
    hide_action().

%
% API
%

set_panel(Key, Target) ->
    wf:wire(Target, #state_panel_set{key = Key}).

set_action(Key, Animate, ValidateGroup, Actions) ->
    Options = {[{animate, Animate},
                {validate_group, ValidateGroup}
                | if Actions == undefined -> [];
                     true -> [{callback, {function, Actions}}]
                  end]},
    #jquery_cast{cast = statePanelSet,
                 args = [Key, Options]}.

show_action(Key, Actions) ->
    #jquery_cast{cast = statePanelShow,
                 args = [Key, {[{callback, {function, Actions}}]}]}.

hide_action() ->
    #jquery_cast{cast = statePanelHide}.
