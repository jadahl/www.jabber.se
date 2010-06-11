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

-module(menu).
-include_lib ("nitrogen/include/wf.inc").

-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/menu.hrl").

-export(
    [
        menu/0,
        get_menu_elements/0,
        full_title/1, full_url/1,
        get_element_by_url/1, get_element_by_url/2,
        get_element_by_module/1, get_element_by_module/2,
        get_element_by_path/1, get_element_by_path/2
    ]).

get_menu_elements() ->
    ?MENU_ELEMENTS.

menu_event(#menu_element{url = Url, module = Module}) ->
    #event{type = click, actions = [#js_call{fname = "$Site.$load_content", args = [Url, Module]}]}.

full_title(#menu_element{title = Title}) ->
    ?TITLE ++ " - " ++ ?T(Title).

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
            body = #link{text = i18n:t(Title),
                url = Url,
                id = Module,
                actions = [menu_event(MenuElement)]
            }}
        || #menu_element{
            title = Title,
            module = Module,
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

