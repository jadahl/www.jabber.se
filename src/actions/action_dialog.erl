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

-module(action_dialog).
-export(
    [
        render_action/1,
        show/1, hide/1, set/3, corner/3, back/1,
        reset_actions/5
    ]).

-include("include/ui.hrl").
-include("include/utils.hrl").


render_action(#dialog_show{target = Target}) ->
    #js_call{fname = "$Site.$dialog_show", args = [Target]};

render_action(#dialog_hide{target = Target}) ->
    #js_call{fname = "$Site.$dialog_hide", args = [Target]};

render_action(#dyn_dialog_back{target = Id}) ->
    #jquery_cast{anchor = Id, target = corner_link, cast = click};

render_action(#dyn_dialog_corner{
        target = Target,
        corner = Corner,
        actions = Actions
    }) ->
    #update{anchor = Target, target = corner, elements = element_dialog:render_corner(Corner, Actions)};

render_action(#dyn_dialog_set{
        target = Target,
        title = Title,
        body = Body}) ->
    [
        #update{anchor = Target, target = title, elements = Title},
        #update{anchor = Target, target = body, elements = Body}
    ].

show(Id) ->
    wf:wire(#dialog_show{target = Id}).

hide(Id) ->
    wf:wire(#dialog_hide{target = Id}).

reset_actions(Id, Title, Body, Corner, Actions) ->
    [
        #dyn_dialog_corner{target = Id, corner = Corner, actions = Actions},
        #dyn_dialog_set{target = Id, title = Title, body = Body}
    ].

set(Id, Title, Body) ->
    wf:wire(#dyn_dialog_set{target = Id, title = Title, body = Body}).

corner(Id, Corner, Actions) ->
    wf:wire(#dyn_dialog_corner{target = Id, corner = Corner, actions = Actions}).

back(Id) ->
    wf:wire(#dyn_dialog_back{target = Id}).

