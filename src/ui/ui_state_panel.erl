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

-module(ui_state_panel).
-export([render_ui/1]).

-include_lib("nitrogen/include/wf.hrl").

-include("include/utils.hrl").
-include("include/ui.hrl").

%
% HTML rendering
%

render_ui(#ui_state_panel{id = Id, bodies = Bodies, visible = Visible, init_state = InitState} = UI) ->
    VisibleStyle = ?WHEN_S(not Visible, "display: none"),
    #panel{
        class = [state_panel_container],
        id = Id,
        style = VisibleStyle,
        body = #panel{
            class = [state_panel, UI#ui_state_panel.class],
            body = lists:map(fun ({Key, Value}) ->
                        IsInit = InitState == Key,
                        #panel{
                            id = Key,
                            class = ?EITHER(IsInit, [state_panel_active, state_panel_alt], state_panel_alt),
                            body = Value,
                            style = ?WHEN_S(not IsInit, "display: none")}
                end, Bodies)
        }}.

