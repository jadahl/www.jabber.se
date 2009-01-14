-module (web_about).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"Jabber.se".

body() ->
	#label{text="Jabber.se is a free XMPP / Jabber service."}.
	
event(_) -> ok.
