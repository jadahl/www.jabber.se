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

-module(element_state_panel).
-export([render_element/1]).

-include_lib("nitrogen_core/include/wf.hrl").

-include("include/utils.hrl").
-include("include/ui.hrl").

%
% HTML rendering
%

render_element(#state_panel{
        id = Id,
        class = Class,
        inline = Inline,
        bodies = Bodies,
        visible = Visible,
        init_state = InitState}) ->
    VisibleStyle = ?WHEN_S(not Visible, "display: none"),
    InlineClass = ?EITHER(Inline, "state_panel_inline", "state_panel_block"),
    #panel{
        id = Id,
        style = VisibleStyle,
        class = [state_panel, InlineClass, Class],
        body = lists:map(fun({Key, Value}) ->
                    IsInit = InitState == Key,
                    #panel{
                        id = Key,
                        class = [InlineClass | ?EITHER(IsInit, [state_panel_active, state_panel_alt], state_panel_alt)],
                        body = Value,
                        style = case {IsInit, Visible} of
                            {true, true} -> "";
                            _ -> "display: none"
                        end}
            end, Bodies)
    }.

