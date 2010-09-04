%
%    Jabber.se Web Application
%    Copyright (C) 2010 Jonas Ådahl
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

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
