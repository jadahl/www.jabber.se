-module (about).
-include_lib("nitrogen/include/wf.inc").

-include("include/menu.hrl").
-compile(export_all).

body() ->
    [
        #h2{text="About"},
        #h3{text="Info"},
        utils:text_to_hyper_text(
            ["Jabber.se is a non profit public Jabber service. It's located at ", {link, "http://www.lysator.liu.se/", "Lysator Computer Society"}, " at Linköping University, Sweden. It was originaly started by a group of Chalmers students and was run at Chalmers Computer Society (Chalmers Datorförening, CD) between 2004 and 2008, until it was moved to Lysator, due to loss of the computer hall, where it has been running ever since."]),
        utils:text_to_hyper_text(
            "Jabber.se is open for any one to use and is completely free. All software run on Jabber.se is Open Source and Free Software."),
        #br{},
        #h3{text="Contact"},
        #span{class = "contact", text="Chat room"},
        #list{
            class = "contact",
            body = [
                #listitem{body = ["Talks - ", utils:t_to_ht({muc, "talks@conference.jabber.se"})]}
            ]},
        #span{class = "contact", text="Administrators (via Jabber)"},
        #list{
            class = "contact",
            body = [
                #listitem{body = utils:t_to_ht({jid, "jonas@jabber.se"})},
                #listitem{body = utils:t_to_ht({jid, "legoscia@jabber.cd.chalmers.se"})}
            ]}
    ].
	
event(Event) ->
    io:format("~p: Received event: ~p~n", [?MODULE, Event]).
