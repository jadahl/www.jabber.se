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

