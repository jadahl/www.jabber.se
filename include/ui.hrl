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

-ifndef(ui_hrl).
-define(ui_hrl, true).

-include_lib("nitrogen/include/wf.hrl").

%
% UI constructs
%
-define(UI_BASE(Module), type = ui_content, id, class = [], module = Module).
-record(ui_base, {?UI_BASE(undefined)}).
-record(ui_state_panel, {?UI_BASE(ui_state_panel), bodies, visible = false, init_state}).
-record(ui_dialog, {?UI_BASE(ui_dialog), body = []}).

%
% Actions
%

-record(state_panel_set, {?ACTION_BASE(action_state_panel), key, validate_group}).
-record(state_panel_show, {?ACTION_BASE(action_state_panel), key}).
-record(state_panel_hide, {?ACTION_BASE(action_state_panel)}).

-endif.
