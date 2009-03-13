-module(cms_db).
-include("src/cms/cms_db.hrl").
-compile(export_all).


init() ->
  ok.

post(_Post) ->
  ok.

get_all() ->
  [#content{
      id = 124152,
      timestamp = 1236930795,
      authors = ["Jonas"],
      content = #blog_post{
	title = "Welcome to Jabber.se!",
	body = "This is the new website of Jabber.se and will slowly grow into something useful. At some point in time, you will be able to find various features related to Jabber, XMPP and Jabber.se, so stay tuned!",
	tags = ["news"]}}].

