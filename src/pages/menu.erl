-module(menu).
-include("src/pages/config.hrl").
-include("src/pages/menu.hrl").
-export([get_menu_elements/0]).

get_menu_elements() ->
    ?MENU_ELEMENTS.
