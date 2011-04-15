%
%    Jabber.se Web Application
%    Copyright (C) 2011 Jonas Ådahl
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

-module(element_expandable).
-export([render_element/1]).

-include_lib("nitrogen_core/include/wf.hrl").

-include("include/ui.hrl").
-include("include/utils.hrl").

render_link(Id, {Category, Title, _Body}) ->
    LinkId = "exp_cat_" ++ atom_to_list(Category),
    #link{text = Title,
          id = LinkId,
          class = "expandable_link",
          delegate = element_expandable,
          actions = #event{type = click, actions = [
                  #site_cast{cast = expandable_toggle,
                             args = [Id, LinkId, Category]}]}}.

render_element(#expandable{id = Id,
                           categories = Categories,
                           init_category = _InitCategory}) ->
    CategoriesHeader = [render_link(Id, Category)
                        || Category <- Categories],
    Bodies = [{Category, Body} || {Category, _, Body} <- Categories],
    [#panel{body = [#panel{body = CategoriesHeader,
                           id = exp_header},
                    #state_panel{bodies = Bodies,
                                 visible = true,
                                 id = exp_body}]}].

