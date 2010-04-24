-include("include/cms/cms_db.hrl").

-define(OLD_CONTENT,
    [#content{
            id = 124156,
            timestamp = 1268486587, %%  ~ 2010-03-13 14:23
            authors = ["Jonas"],
            type = blog_post,
            content = #blog_post{
                title = "New Website!",
                body = "As you might notice there has been some work done on the Jabber.se website. The theoretical roadmap for this place contains things such as localization, registration and other administrative tasks. For anyone interested, this site is implemented in <a href=\"http://www.erlang.org/\">Erlang</a> using <a href=\"http://nitrogenproject.com/\">The Nitrogen Project</a> running on <a href=\"http://yaws.hyber.org/\">Yaws</a> with <a href=\"http://couchdb.apache.org/\">CouchDB</a> as a database backend.",
                tags = ["news"]}},
        #content{
            id = 124153,
            timestamp = 1236930795, %% 2009-11-28 13:19
            authors = ["Jonas"],
            type = blog_post,
            content = #blog_post{
                title = "Jabber.CD.Chalmers.Se",
                body = "At one point, Chalmers will discontinue to administer the DNS alias that is jabber.cd.chalmers.se. This means that at some point, accounts under the jabber.cd.chalmers domain will stop working as before. The accounts will still be available for use, but you would have to manually set your XMPP client to connect to \"jabber.se\". Affected accounts will also not be able to communicate with any accounts except the ones on jabber.cd.chalmers.se and jabber.se. We strongly suggest you to create a new account on jabber.se.",
                tags = ["news"]}},
        #content{
            id = 124152,
            timestamp = 1236930795,
            authors = ["Jonas"],
            type = blog_post,
            content = #blog_post{
                title = "Welcome to Jabber.se!",
                body = "This is the new website of Jabber.se and will slowly grow into something useful. At some point in time, you will be able to find various features related to Jabber, XMPP and Jabber.se, so stay tuned!",
                tags = ["news"]}}]).
