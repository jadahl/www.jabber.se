-module (about).
-include_lib("nitrogen/include/wf.inc").

-include("include/menu.hrl").
-compile(export_all).

body() ->
    cms:body_single("page_about").
	
event(Event) ->
    ?LOG_WARNING("Unexpected event: ~p", [Event]).
