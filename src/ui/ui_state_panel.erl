-module(ui_state_panel).
-export([render_ui/1, set/2, get_set_call/2]).

-include_lib("nitrogen/include/wf.hrl").

-include("include/utils.hrl").
-include("include/ui.hrl").

render_ui(#ui_state_panel{id = Id, bodies = Bodies, init_state = InitState} = UI) ->
    #panel{
        class = [state_panel, UI#ui_state_panel.class],
        id = Id,
        body = lists:map(fun ({Key, Value}) ->
                    IsInit = InitState == Key,
                    #panel{
                        id = Key,
                        class = ?EITHER(IsInit, [state_panel_active, state_panel_alt], state_panel_alt),
                        %?EITHER(IsInit, [state_panel_alt, state_panel_active], state_panel_alt),
                        body = Value,
                        style = ?WHEN_S(not IsInit, "display: none")}
            end, Bodies)
    }.

get_set_call(Key, Id) ->
    #js_call{fname = "$Site.$state_panel_set", args = [Key, Id]}.

set(Key, Id) ->
    wf:wire(get_set_call(Key, Id)).

