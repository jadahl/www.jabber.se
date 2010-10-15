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

-module(element_dialog).
-export([render_element/1, render_corner/2]).

-include_lib("nitrogen/include/wf.hrl").

-include("include/utils.hrl").
-include("include/ui.hrl").

-type dialog() :: #dialog{}.

corner_symbol(Corner) ->
    case Corner of
        back -> "↩";
        close -> "☒"
    end.

render_corner(Corner, Actions) ->
    #link{
        id = corner_link,
        text = corner_symbol(Corner),
        actions = #event{
            type = click,
            actions = Actions
        }
    }.

-spec render_element(dialog()) -> term().
render_element(#dialog{
        id = Id,
        class = Class,
        title = Title,
        title_class = TitleClass,
        body = Body}) ->
    Corner = #panel{id = corner},
    FinalTitleClass = lists:flatten([TitleClass, dyn_dialog_title]),

    #panel{
        class = [dialog, Class],
        id = Id,
        style = ?HIDDEN,
        body = [
            #panel{class = FinalTitleClass, body = [#h2{id = title, text = Title}, Corner]},
            #hr{},
            #panel{id = body, body = Body}
        ]
    }.

