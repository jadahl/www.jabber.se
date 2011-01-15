%
%    Jabber.se Web Application
%    Copyright (C) 2010-2011 Jonas Ådahl
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

-module(menu).
-include_lib ("nitrogen/include/wf.inc").

-include("include/ui.hrl").
-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/menu.hrl").

-export(
    [
        menu/0,
        get_menu_elements/0,
        menu_element_id/1,
        full_title/1,
        get_element_by_module/1, get_element_by_module/2
    ]).

get_menu_elements() ->
    ?MENU_ELEMENTS.

menu_element_id(#menu_element{module = Module}) ->
    list_to_atom("menu_" ++ atom_to_list(Module)).

menu_event(#menu_element{module = Module} = MenuElement) ->
    #event{type = click,
           actions = [#js_call{fname = "$Site.$trigger_menu",
                               args = [list_to_binary(atom_to_list(Module)),
                                       menu_element_id(MenuElement)]}]}.

full_title(#menu_element{title = Title}) ->
    ?TITLE ++ " - " ++ ?T(Title).

-spec menu_items() -> list(#listitem{}).
menu_items() ->
    MenuElements = menu:get_menu_elements(),
    [#listitem{
            body = #link{
                text = i18n:t(Title),
                id = menu_element_id(MenuElement),
                actions = [menu_event(MenuElement)]
            }}
        || #menu_element{title = Title} = MenuElement <- MenuElements].

%
% get_menu_element_by_module(Module) -> Result
%   Module = atom()
%   Result = #menu_element{} | none
%
-spec get_element_by_module(module()) -> {just, #menu_element{}} | nothing.
get_element_by_module(Module) ->
    get_element_by_module(Module, menu:get_menu_elements()).

-spec get_element_by_module(module(), [#menu_element{}]) ->
    {just, #menu_element{}} | nothing.
get_element_by_module(Module, Elements) ->
    utils:find_with(fun maybe_element_by_module/2, Module, Elements).

maybe_element_by_module(#menu_element{module = Module} = Element, Module) ->
    {just, Element};
maybe_element_by_module(_, _) ->
    nothing.

%
% Document entry points
%

menu() ->
    [
        % menu elements
        #list{body = menu_items()}
    ].

