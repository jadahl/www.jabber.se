-module(action_state_panel).
-export([render_action/1]).

-include("include/ui.hrl").

render_action(#set_panel_state{
        type = Type,
        target = Target,
        key = Key}) ->
    case Type of
        set ->
            ui_state_panel:get_set_call(Key, Target);
        hide ->
            ui_state_panel:get_hide_call(Target);
        _ ->
            []
    end.

