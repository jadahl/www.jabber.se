-module(web_cms).
-include_lib("nitrogen/include/wf.inc").
-include("src/cms/cms_db.hrl").
-compile(export_all).

main() ->
  #template{file="./wwwroot/template.html"}.

title() ->
  "Jabber.se".

render_blog_post(#content{
    authors = Authors,
    content = #blog_post{
      title = Title,
      body = Body,
      tags = _Tags}}) ->
  [#rounded_panel{
      body=
      [#label{text = Title},
	#span{text = Body},
	#br{},
	#literal{text = "By: " ++
	  case Authors of
	    [Author] -> Author;
	    _ -> "unknown"
	  end}]},
    #br{}].

render_content(#content{content = #blog_post{}} = Content) ->
  render_blog_post(Content);

render_content(_) ->
  #label{text = "Unknown content"}.

body() ->
  Contents = cms_db:get_all(),
  lists:map(fun render_content/1, Contents).

