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
-include_lib ("nitrogen_core/include/wf.hrl").

-include("include/ui.hrl").
-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/menu.hrl").

-export(
    [
        hide_spinner/0, menu/0,
        get_menu_elements/0,
        element_by_path/1, element_by_path/2,
        menu_element_id/1,
        full_title/1,
        menu_element_to_module/1
    ]).

get_menu_elements() ->
    ?MENU_ELEMENTS.

menu_element_to_module(#menu_element{path = Path}) ->
    Module = list_to_atom("content_" ++ Path),
    case config:content_enabled(Module) of
        true  -> Module;
        false -> undefined
    end.

element_by_path(Path) ->
    element_by_path(Path, get_menu_elements()).
element_by_path(Path, MenuElements) ->
    I = #menu_element.path,
    case lists:keysearch(Path, I, MenuElements) of
        {value, Element} -> Element;
        _                -> undefined
    end.

menu_element_id(#menu_element{path = Path}) ->
    list_to_atom("menu_" ++ [if C == $/ -> $_; true -> C end || C <- Path]).

menu_event(#menu_element{path = Path} = MenuElement) ->
    [#event{type = click,
            actions = [#show{target = menu_spinner},
                       #js_call{fname = "$Site.$trigger_menu",
                                args = [list_to_binary(Path),
                                        menu_element_id(MenuElement)]}]},
     #event{actions = [#jquery_attr{key = href, value = <<"javascript:">>}]}].

full_title(#menu_element{title = Title}) ->
    ?TITLE ++ " - " ++ ?T(Title).

-spec menu_item(#menu_element{}) -> #listitem{}.
menu_item(MenuElement) ->
    #listitem{body =
         #link{text = i18n:t(MenuElement#menu_element.title),
               url = "/" ++ MenuElement#menu_element.path,
               id = menu_element_id(MenuElement),
               actions = [menu_event(MenuElement)]}}.

-spec menu_items() -> list(#listitem{}).
menu_items() ->
    MenuElements = menu:get_menu_elements(),
    [menu_item(MenuElement) || MenuElement <- MenuElements].

hide_spinner() ->
    wf:wire(menu_spinner, #hide{}).

%
% Document entry points
%

menu() ->
    [
        % spinner for when content is loading
        #image{image = ?SPINNER_IMAGE_MENU,
               id = menu_spinner,
               style = ?HIDDEN},

        % menu elements
        #list{body = menu_items()}
    ].

