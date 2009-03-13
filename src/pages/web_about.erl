-module (web_about).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"Jabber.se".

body() ->
	[#literal{text="<p class=\"about\">Jabber.se is a non profit public Jabber service. It's located at Lysator Computer Society at Linköping University, Sweden. It was originaly started by a group of Chalmers students and was run at Chalmers Computer Society (Chalmers Datorförening, CD) between 2004 and 2008, until it was moved to Lysator, due to loss of the computer hall, where it has been running ever since.</p>
	<p class=\"about\">Jabber.se is open for any one to use and is compeltely free. All software run on Jabber.se is Open Source and Free Software.</p>",
	html_encode=false}].
	
event(_) -> ok.
