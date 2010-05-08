-module (about).
-include_lib("nitrogen/include/wf.inc").

-include("include/menu.hrl").
-compile(export_all).

body() ->
    cms:body_single("page_about").
	
event(Event) ->
    io:format("~p: Received event: ~p~n", [?MODULE, Event]).
