-module(web_index).
-include("src/pages/menu.hrl").
-include("src/pages/config.hrl").

-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

menu() ->
    [
        % hidden input field for storing fragment value
        #hidden{id = fragment_path, text=?DEFAULT_INDEX_MODULE, actions=[#event{type=init, postback=init}]},

        % menu elements
        #list{body = menu_items()}
    ].

menu_items() ->
    MenuElements = menu:get_menu_elements(),
    [#listitem{
            body = #link{text = Title,
                url = Url,
                actions=[#event{type={click, io_lib:format("set_fragment_path('~s');", [Url])}, postback={menu, Module}}]
            }}
        || #menu_element{
            title = Title,
            url = Url,
            module = Module} <- MenuElements].

main() -> 
    #template { file="./wwwroot/template.html" }.

title() ->
    ?TITLE.

body() ->
    [].

foot() ->
    [].

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
            wf:update(body, #panel{body = get_default_body()});
        MenuElement ->
            event({menu, MenuElement#menu_element.module})
    end;
event({menu, Module}) ->
    try
        Body = {Module, body}(),
        wf:update(body, #panel{body = Body})
    catch
        error:undef ->
            wf:update(body, #panel{body = get_default_body()})
    end;
event(Event) ->
    io:format("event: ~p~n", [Event]),
    ok.

get_element_by_url(Url) ->
    get_element_by_url(Url, menu:get_menu_elements()).
get_element_by_url(Url, [E| _Es]) when Url == E#menu_element.url ->
    E;
get_element_by_url(Url, [_E| Es]) ->
    get_element_by_url(Url, Es);
get_element_by_url(_, _) ->
    none.

get_default_body() ->
    {?DEFAULT_INDEX_MODULE, body}().
