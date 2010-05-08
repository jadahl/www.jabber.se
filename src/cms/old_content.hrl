-include("include/cms/db.hrl").

-define(OLD_CONTENT,
    [#db_post{
            timestamp = 1268486587, %%  ~ 2010-03-13 14:23
            authors = ["Jonas"],
            title = {[
                    {"en_US", "New Website!"},
                    {"sv_SE", "Ny Websida!"}
                ]},
            body = {[
                    {"en_US", "As you might notice there has been some work done on the Jabber.se website. The theoretical roadmap for this place contains things such as localization, registration and other administrative tasks. For anyone interested, this site is implemented in <a href=\"http://www.erlang.org/\">Erlang</a> using <a href=\"http://nitrogenproject.com/\">The Nitrogen Project</a> running on <a href=\"http://yaws.hyber.org/\">Yaws</a> with <a href=\"http://couchdb.apache.org/\">CouchDB</a> as a database backend."},
                    {"sv_SE", "Som du kanske ser har det skett en del med hemsidan för Jabber.se. Den teoretiska roadmap:en för den här sidan innehåller saker som lokalisering, registrering och andra administrativa funktioner. För den intresserade så är sidan implementerad i <a href=\"http://www.erlang.org/\">Erlang</a> användande av <a href=\"http://nitrogenproject.com/\">The Nitrogen Project</a> som körs via <a href=\"http://yaws.hyber.org/\">Yaws</a> med <a href=\"http://couchdb.apache.org/\">CouchDB</a> som databas-backend."}
                ]},
            tags = ["news"]},
        #db_post{
            timestamp = 1236930795, %% 2009-11-28 13:19
            authors = ["Jonas"],
            title = "Jabber.CD.Chalmers.Se",
            body = {[
                    {"en_US", "At one point, Chalmers will discontinue to administer the DNS alias that is <em>jabber.cd.chalmers.se</em>. This means that at some point, accounts under the <em>jabber.cd.chalmers.se</em> domain will stop working as before. The accounts will still be available for use, but you would have to manually set your XMPP client to connect to <em>jabber.se</em>. Affected accounts will also not be able to communicate with any accounts except the ones on <em>jabber.cd.chalmers.se</em> and <em>jabber.se</em>. We strongly suggest you to create a new account on <em>jabber.se</em>."},
                    {"sv_SE", "Chalmers kommer vid någon tidpunkt att sluta administrera DNS-aliaset <em>jabber.cd.chalmers.se</em>. Detta innebär att vid denna tidpunkt, konton på <em>jabber.cd.chalmers.se</em> kommer att sluta fungera som förut. Kontona kommer fortfarande finnas kvar, men du kommer behöva ställa in din XMPP-klient så att den ansluter till <em>jabber.se</em>. Påverkade konton kommer heller inte att kunna kommunicera med något annat konto förutom de på <em>jabber.cd.chalmers.se</em> och <em>jabber.se</em>. Vi råder alla som endast har konto på <em>jabber.cd.chalmers.se</em> att skapa ett nytt på <em>jabber.se</em>."}
                ]},
            tags = ["news"]},
        #db_post{
            timestamp = 1236930795,
            authors = ["Jonas"],
            title = {[
                    {"en_US", "Welcome to Jabber.se!"},
                    {"sv_SE", "Välkomen till Jabber.se!"}
                ]},
            body = {[
                    {"en_US", "This is the new website of Jabber.se and will slowly grow into something useful. At some point in time, you will be able to find various features related to Jabber, XMPP and Jabber.se, so stay tuned!"},
                    {"sv_SE", "Det här är Jabber.se's nya websida. Den kommer sakta växa till något användbart. Vid något tillfälle kommer du kunna hitta olika funktioner för Jabber, XMPP och Jabber.se."}
                ]},
            tags = ["news"]},

        %
        % About page
        %
        #db_post{
            id = "page_about",
            authors = ["Jonas"],
            title = {[
                    {"en_US", "About"},
                    {"sv_SE", "Om"}
                ]},
            body = {[
                    {"en_US", "<p>Jabber.se is a non profit public Jabber service. It's located at <a href=\"http://www.lysator.liu.se/\">Lysator Computer Society</a>, at Linköping University, Sweden. It was originaly started by a group of Chalmers students and was run at Chalmers Computer Society (Chalmers Datorförening, CD) between 2004 and 2008, until it was moved to Lysator, due to loss of the computer hall, where it has been running ever since.</p><p>Jabber.se is open for any one to use and is completely free. All software run on Jabber.se is Open Source and Free Software.</p><br/><h3>Contact</h3><span class=\"contact\">Chat room</span><ul><li>Talks - <a href=\"xmpp://talks@conference.jabber.se?join\">talks@conference.jabber.se</a></li></ul><span class=\"contact\">Administrators (via Jabber)</span><ul><li><a href=\"xmpp:jonas@jabber.se\">jonas@jabber.se</a></li><li><a href=\"legoscia@jabber.cd.chalmers.se\">legoscia@jabber.cd.chalmers.se</a></li></ul>"},
                    {"sv_SE", "<p>Jabber.se är en ideell publik Jabbertjänst. Den körs hos <a href=\"http://www.lysator.liu.se/\">Lysator</a>, en datorförening vid Linköpings universitet. Tjänsten var från början startad av en grupp Chalmersstudenter och kördes då på Chalmers Datorförening, mellan 2004 och 2008, tills den senare flyttades till Lysator, på grund av avsaknad av datorhall, där den har körts sen dess.</p><p>Jabber.se är öppen för vem som helst och det är helt gratis. All mjukvara som körs på Jabber.se är dessutom fri och av öppen källkod.</p><br/><h3>Kontakt</h3><span class=\"contact\">Chatrum</span><ul><li>Talks - <a href=\"xmpp://talks@conference.jabber.se?join\">talks@conference.jabber.se</a></li></ul><span class=\"contact\">Administratörer (via Jabber)</span><ul><li><a href=\"xmpp:jonas@jabber.se\">jonas@jabber.se</a></li><li><a href=\"legoscia@jabber.cd.chalmers.se\">legoscia@jabber.cd.chalmers.se</a></li></ul>"}]}
        }




    ]).


