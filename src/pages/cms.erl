-module(cms).
-include_lib("nitrogen/include/wf.inc").
-include("src/pages/menu.hrl").
-include("src/cms/cms_db.hrl").
-export([body/1, event/1]).

body(Type) ->
  Contents = cms_db:get_posts(Type),
  [#h2{text = "News"} | lists:map(fun render_content/1, Contents)].

event(Event) ->
    io:format("~p: Received event: ~p~n", [?MODULE, Event]).

render_blog_post(#content{
    authors = Authors,
    content = #blog_post{
      title = Title,
      body = Body,
      tags = _Tags}}) ->
  [#panel{
      body=
      [#label{class = blog_title, text = Title},
	#span{class = blog_by,
                 text = "by " ++
	  case Authors of
	    [Author] -> Author;
	    _ -> "unknown"
	  end},
	#br{},
	#p{class = blog_body, body = Body}]},
    #br{}].

render_content(#content{content = #blog_post{}} = Content) ->
  render_blog_post(Content);

render_content(_) ->
  #label{text = "Unknown content"}.

