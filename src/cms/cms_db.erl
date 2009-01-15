-module(cms_db).
-include("src/cms/cms_db.hrl").
-compile(export_all).


init() ->
  ok.

post(_Post) ->
  ok.

get_all() ->
  [#content{
      id = 124151,
      timestamp = 0,
      authors = ["Jonas"],
      content = #blog_post{
	title = "Blog post #1",
	body = "Body",
	tags = ["test", "erlang"]}},

    #content{
      id = 124152,
      timestamp = 17200000,
      authors = ["Jonas"],
      content = #blog_post{
	title = "Blog post #2",
	body = "The Other Body",
	tags = ["nitrogen", "test"]}}].

