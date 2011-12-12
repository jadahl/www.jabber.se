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

-module(action_jquery_cast).
-export([render_action/1]).

-include("include/ui.hrl").

render_action(#select{}) ->
    #jquery_cast{cast = select};

render_action(#focus{}) ->
    #jquery_cast{cast = focus};

render_action(#jquery_cast{
        anchor = Anchor,
        target = Target,
        cast = Cast,
        args = Args}) ->
    JQueryCast = atom_to_list(Cast),
    EscapedArgs = case Args of
        undefined -> "";
        _ -> cf_utils:join([action_js_call:escape(Arg1) || Arg1 <- Args], ",")
    end,
    [wf:f("objs('~s', '~s').~s(",
          [Target, Anchor, JQueryCast]),
     EscapedArgs,
     ");"].

