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

-module(feed).
-export([get_feed_links/0]).

-include("include/ui.hrl").

-include("include/utils.hrl").
-include("include/menu.hrl").

get_feed_link(#menu_element{title = Title} = MenuElement) ->
    case menu:menu_element_to_module(MenuElement) of
        undefined ->
            [];
        Module ->
            try
                case Module:atom_url() of
                    {ok, AtomUrl} ->
                        [#ext_link{
                                url = AtomUrl,
                                type = "application/atom+xml",
                                rel = "alternate",
                                title = ?T(Title) ++ " atom feed"}];
                    _ ->
                        []
                end
            catch
                error:undef ->
                    []
            end
    end.

get_feed_links() ->
    lists:flatmap(fun get_feed_link/1, menu:get_menu_elements()).

