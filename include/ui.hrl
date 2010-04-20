-ifndef(ui_hrl).
-define(ui_hrl, true).

-include_lib("nitrogen/include/wf.hrl").

%
% UI constructs
%
-define(UI_BASE(Module), type = ui_content, id, class = [], module = Module).
-record(ui_base, {?UI_BASE(undefined)}).
-record(ui_state_panel, {?UI_BASE(ui_state_panel), bodies, init_state}).
-record(ui_dialog, {?UI_BASE(ui_dialog), body = []}).

%
% Actions
%

-record(set_panel_state, {?ACTION_BASE(action_state_panel), type = set, key}).

-endif.
