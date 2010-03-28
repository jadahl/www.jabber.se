-module(cms).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("nitrogen/include/wf.inc").

-include("include/config.hrl").
-include("include/menu.hrl").
-include("include/cms/cms_db.hrl").
-export([body/1, atom/3, event/1]).

%%
% Atom
%%

% TODO
%
% * make id a checksum
%
%<entry>
%  <link href="http://example.org/2003/12/13/atom03"/>
%</entry>

blog_post_to_atom_entry(#content{
        id = Id,
        timestamp = Timestamp,
        authors = Authors,
        content = #blog_post{
            title = Title,
            body = Body,
            tags = _Tags}}) ->
    {entry,
        lists:flatten([
            {title, [Title]},
            {id, [integer_to_list(Id)]},
            {published, [utils:time_to_iso8601(Timestamp)]},
            [{author, [{name, [Author]}]} || Author <- Authors],
            {summary, [Body]}
        ])}.

content_to_atom_entry(#content{content = #blog_post{}} = Content) ->
    blog_post_to_atom_entry(Content);
content_to_atom_entry(_Content) ->
    undefined.

get_last_updated(Contents) ->
    lists:foldl(
        fun (X, R) ->
                if 
                    X#content.timestamp > R#content.timestamp ->
                        X;
                    true ->
                        R
                end
        end, 0, Contents).

contents_to_atom(Contents, Url, Title, SubTitle) ->
    {feed, [{xmlns, "http://www.w3.org/2005/Atom"}],
        lists:flatten([
                {title, [Title]},
                {subtitle, [SubTitle]},
                {link, [{href, ?URL_BASE ++ Url}], []},
                {updated, [utils:time_to_iso8601(get_last_updated(Contents))]},
                lists:map(fun content_to_atom_entry/1, Contents)
            ])}.

%%
% HTML
%%

blog_post_to_html(#content{
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

content_to_html(#content{content = #blog_post{}} = Content) ->
    blog_post_to_html(Content);
content_to_html(_) ->
    #label{text = "Unknown content"}.

%%
% Entry points
%%

body(Type) ->
    Contents = cms_db:get_posts(Type),
    [#h2{text = "News"} | lists:map(fun content_to_html/1, Contents)].

atom(Type, Url, SubTitle) ->
    Contents = cms_db:get_posts(Type),
    Attrs = [{prolog, ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>"]}],
    xmerl:export_simple([contents_to_atom(Contents, Url, ?TITLE, SubTitle)], xmerl_xml, Attrs).

event(Event) ->
    io:format("~p: Received event: ~p~n", [?MODULE, Event]).

