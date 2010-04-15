-module(feed).
-include_lib("nitrogen/include/wf.hrl").

-include("include/menu.hrl").
-export([get_feed_links/0]).

get_feed_link(#menu_element{
        title = Title,
        module = Module}) ->
    try
        case Module:atom_url() of
            {ok, AtomUrl} ->
                [#ext_link{
                        url = AtomUrl,
                        type = "application/atom+xml",
                        rel = "alternate",
                        title = Title ++ " atom feed"}];
            _ ->
                []
        end
    catch
        error:undef ->
            []
    end.

get_feed_links() ->
    lists:flatmap(fun get_feed_link/1, menu:get_menu_elements()).

