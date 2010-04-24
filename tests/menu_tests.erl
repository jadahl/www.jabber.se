-module(menu_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/menu.hrl").

-define(MENU_ELEMENTS, 
    [
        #menu_element{
            module = element1,
            title = "Element 1",
            url = "#element1"},
        #menu_element{
            module = element2,
            title = "Element 2",
            url = "#element2"},
        #menu_element{
            module = element3,
            title = "Element 3",
            url = "#element3"}
    ]).

getters_test() ->
    MenuElements = ?MENU_ELEMENTS,
    ?assert(nothing /= menu:get_element_by_url("#element3", MenuElements)),
    ?assert(nothing /= menu:get_element_by_module(element2, MenuElements)),
    ?assert(nothing /= menu:get_element_by_path("element1", MenuElements)),

    ?assert(nothing == menu:get_element_by_url("#no_url", MenuElements)),
    ?assert(nothing == menu:get_element_by_module(no_element, MenuElements)),
    ?assert(nothing == menu:get_element_by_path("no_element", MenuElements)).
