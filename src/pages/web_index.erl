-module(web_index).
-include_lib ("nitrogen/include/wf.inc").

-include("include/config.hrl").
-include("include/constants.hrl").
-include("include/menu.hrl").
-include("include/utils.hrl").

-compile(export_all).

%%
% Utils
%%

get_element_by_url(Url) ->
    get_element_by_url(Url, menu:get_menu_elements()).
get_element_by_url(Url, [E| _Es]) when Url == E#menu_element.url ->
    E;
get_element_by_url(Url, [_E| Es]) ->
    get_element_by_url(Url, Es);
get_element_by_url(_, _) ->
    none.

%%
% Menu
%%

menu_select(Module) ->
    menu_select(Module, []).
menu_select(Module, Options) ->
    try
        Body = {Module, body}(),
        change_body(Module, Body, Options)
    catch
        error:undef ->
            change_body(?DEFAULT_INDEX_MODULE, get_default_body(), Options)
    end.

menu() ->
    [
        % hidden input field for storing fragment value
        #hidden{
            id = fragment_path,
            text = ?DEFAULT_INDEX_MODULE,
            actions = [#event{type = init, postback = init}]},

        % menu elements
        #list{body = menu_items()}
    ].

menu_click_action(Url) ->
    {click, io_lib:format("set_fragment_path('~s');", [Url])}.

menu_items() ->
    MenuElements = menu:get_menu_elements(),
    [#listitem{
            body = #link{text = Title,
                url = Url,
                actions = [#event{type = menu_click_action(Url), postback = {menu, Module}}]
            }}
        || #menu_element{
            title = Title,
            url = Url,
            module = Module} <- MenuElements].

%%
% Feed
%%

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

get_feed_url(Module) ->
    try
        case {Module, atom_url}() of
            {ok, AtomUrl} ->
                #link{
                    class = ?ATOM_ICON_CLASS,
                    url = AtomUrl,
                    id = ?ATOM_ICON_ID};
            _ ->
                none
        end
    catch
        error:undef ->
            none
    end.

set_feed(Module) ->
    case get_feed_url(Module) of
        none ->
            wf:wire([], [], [#event{type = {call_js, "clear_atom_feed_icon", [?ATOM_ICON_ID]}}]);
        Icon ->
            wf:wire([], [], [#event{type = {call_js, "set_atom_feed_icon", [?ATOM_ICON_ID, Icon]}}])
    end.

%%
% Page body modification
%%

change_body(Module, Body) ->
    change_body(Module, Body, []).
change_body(Module, Body, Options) ->
    Animate = lists:any(fun(Option) -> Option == animate end, Options),
    Id = content_body,
    Event = #event{target = Id, type = init},

    % update dom
    ?WHEN(Animate, wf:wire(Event#event{actions=#hide{}})),
    wf:update(Id, Body),
    wf:wire(Event#event{actions = ?EITHER(Animate, #appear{}, #show{})}),
    set_feed(Module).

get_default_body() ->
    {?DEFAULT_INDEX_MODULE, body}().

%%
% Events
%%

event(init) ->
    Value = wf:q(fragment_path),
    MenuElement = case Value of
        [Path] when is_list(Path) ->
            get_element_by_url(Path);
        _ ->
            none
    end,
    case MenuElement of
        none ->
            change_body(?DEFAULT_INDEX_MODULE, get_default_body());
        _ ->
            menu_select(MenuElement#menu_element.module)
    end;
event({menu, Module}) ->
    menu_select(Module, [animate]);
event(Event) ->
    io:format("event: ~p~n", [Event]),
    ok.


%%
% Document entry points
%%

main() -> 
    #template { file="./wwwroot/template.html" }.

%%
% Template entry points
%%

head() ->
    get_feed_links().

title() ->
    ?TITLE.

body() ->
    [].

foot() ->
    [].

