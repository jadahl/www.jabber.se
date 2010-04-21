-module(ui_state_panel).
-export([render_ui/1]).

-include_lib("nitrogen/include/wf.hrl").

-include("include/utils.hrl").
-include("include/ui.hrl").

%
% HTML rendering
%

render_ui(#ui_state_panel{id = Id, bodies = Bodies, init_state = InitState} = UI) ->
    #panel{
        class = [state_panel_container],
        body =
        #panel{
            class = [state_panel, UI#ui_state_panel.class],
            style = "display: none",
            id = Id,
            body = lists:map(fun ({Key, Value}) ->
                        IsInit = InitState == Key,
                        #panel{
                            id = Key,
                            class = ?EITHER(IsInit, [state_panel_active, state_panel_alt], state_panel_alt),
                            body = Value,
                            style = "display: none"}
                end, Bodies)
        }}.

