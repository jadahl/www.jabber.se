-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
    #template { file="./wwwroot/template.html" }.

title() ->
    "Jabber.se".

box() ->
    #panel{
        id=theDiv,
        %class=effects_target,
	class=woho,
        body=[
            "Jabber.se"
        ]}.

body() ->
    Event = #event { target=theDiv, type=click },
    [#literal{text = "Weclome to "},
     #link{
        text = "Jabber.se",
        actions = Event#event{ 
            actions=#effect{effect=highlight, options = [{"color", "black"}]}}},
     #literal{text="."},
     #br{},
     box(),
     #br{},
     #link{
        text = "news",
        url = "/web/cms"},
     #literal{
	text = " | "},
     #link{
        text = "about",
        url = "/web/about"},
     #literal{text = "."}].

event(_) -> ok.
