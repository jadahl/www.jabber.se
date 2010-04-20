-module(ui_dialog).
-export([render_ui/1]).

-include_lib("nitrogen/include/wf.hrl").

-include("include/utils.hrl").
-include("include/ui.hrl").

render_ui(#ui_dialog{body = Body, id = Id, class = Class}) ->
    #panel{
        class = [dialog, Class],
        id = Id,
        body = #panel{class = dialog_content, body = Body}
    }.

