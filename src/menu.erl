-module(menu).
-include_lib ("nitrogen/include/wf.inc").

-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/menu.hrl").
-export([
        menu/0,
        get_menu_elements/0,
        full_title/1, full_url/1,
        get_element_by_url/1, get_element_by_url/2,
        get_element_by_module/1, get_element_by_module/2,
        get_element_by_path/1, get_element_by_path/2]).

get_menu_elements() ->
    ?MENU_ELEMENTS.

menu_event(#menu_element{url = Url}) ->
    #event{type = click, actions = [#js_call{fname = "$Site.$load_content", args = [Url]}]}.


full_title(#menu_element{title = Title}) ->
    ?TITLE ++ " - " ++ Title.

full_url(#menu_element{url = Url}) ->
    ?BASE_DIR ++ "index" ++ Url.

%
% menu_items() -> MenuItems
%   MenuItems = [MenuElement]
%   MenuElement = #menu_element{}
%
menu_items() ->
    MenuElements = menu:get_menu_elements(),
    [#listitem{
            body = #link{text = Title,
                url = Url,
                actions = [menu_event(MenuElement)]
            }}
        || #menu_element{
            title = Title,
            url = Url} = MenuElement <- MenuElements].

%
% get_menu_element_by_url(Url) -> Result
%   Url = string()
%   Result = #menu_element{} | none
%
get_element_by_url(Url) ->
    get_element_by_url(Url, menu:get_menu_elements()).
get_element_by_url(Url, Elements) ->
    utils:find_with(fun maybe_element_by_url/2, Url, Elements).

maybe_element_by_url(Element, Url) when Url == Element#menu_element.url ->
    {just, Element};
maybe_element_by_url(_, _) ->
    nothing.

%
% get_menu_element_by_module(Module) -> Result
%   Module = atom()
%   Result = #menu_element{} | none
%
get_element_by_module(Module) ->
    get_element_by_module(Module, menu:get_menu_elements()).
get_element_by_module(Module, Elements) ->
    utils:find_with(fun maybe_element_by_module/2, Module, Elements).

maybe_element_by_module(Element, Module) when Module == Element#menu_element.module ->
    {just, Element};
maybe_element_by_module(_, _) ->
    nothing.

%
% get_menu_element_by_path(Path) -> Result
%   Path = string()
%   Result = #menu_element{} | none
%
get_element_by_path(Path) ->
    get_element_by_path(Path, menu:get_menu_elements()).
get_element_by_path(Path, Elements) ->
    utils:find_with(fun maybe_element_by_path/2, Path, Elements).

maybe_element_by_path(#menu_element{url = [$# | Url]} = Element, Path) when Path == Url ->
    {just, Element};
maybe_element_by_path(_, _) ->
    nothing.

%
% Document entry points
%

menu() ->
    [
        % menu elements
        #list{body = menu_items()}
    ].

