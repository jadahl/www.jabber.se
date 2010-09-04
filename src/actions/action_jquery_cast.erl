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

-module(action_jquery_cast).
-export([render_action/1]).

-include("include/ui.hrl").

render_action(#select{
        target = Target}) ->
    #jquery_cast{
        target = Target,
        cast = select};

render_action(#focus{
        target = Target}) ->
    #jquery_cast{
        target = Target,
        cast = focus};

render_action(#jquery_cast{
        anchor = Anchor,
        target = Target,
        cast = Cast,
        arg = Arg}) ->
    JQueryCast = atom_to_list(Cast),
    EscapedArg = case Arg of
        undefined -> "";
        _ -> couchbeam_mochijson2:encode(Arg)
    end,
    wf:f("objs('~s', '~s').~s(~s);", [Target, Anchor, JQueryCast, EscapedArg]).

