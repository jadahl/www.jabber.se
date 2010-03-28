-module(menu).
-include("include/config.hrl").
-include("include/menu.hrl").
-export([get_menu_elements/0, get_element_by_path/2]).

get_menu_elements() ->
    ?MENU_ELEMENTS.

%
% returns: #menu_element{} || none
%
get_element_by_path([#menu_element{url = [Hash | Url]} = MenuElement | _MenuElements], Path) when (Hash == $#) and (Path == Url) ->
    MenuElement;
get_element_by_path([_ | MenuElements], Path) ->
    get_element_by_path(MenuElements, Path);
get_element_by_path([], _) ->
    none.
