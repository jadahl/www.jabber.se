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

-module(action_js_call).
-export([render_action/1]).

-include("include/ui.hrl").

render_action(#js_call {
    fname = FName,
    args = Args}) ->

    EscapedArgs = lists:map(fun escape_arg/1, Args),
    [FName, "(", utils:join(EscapedArgs, ","), ");"].

escape_arg({lambda, Actions}) ->
    ["function() { ", Actions, "}"];
escape_arg(Arg) when is_integer(Arg) ->
    [integer_to_list(Arg)];
escape_arg(Arg) when is_float(Arg) ->
    [float_to_list(Arg)];
escape_arg(Arg) ->
    case escape_arg1(Arg) of
        none ->
            [];
        {str, Arg2} ->
            ["\"" ++ Arg2 ++ "\""];
        {raw, Arg3} ->
            [Arg3]
    end.

escape_arg1(Arg) when is_tuple(Arg) ->
    case wf_render_elements:render_elements([Arg]) of
        {ok, Script} ->
            {str, wf:js_escape(Script)};
        _ ->
            none
    end;
escape_arg1([C | _Cs] = Arg) when is_integer(C) ->
    {str, Arg};
escape_arg1(Arg) when is_list(Arg) ->
    {raw, "[" ++ lists:join(lists:flatmap(fun escape_arg/1, Arg), ", ") ++ "]"};
escape_arg1(undefined) ->
    {raw, "undefined"};
escape_arg1(true) ->
    {raw, "true"};
escape_arg1(false) ->
    {raw, "false"};
escape_arg1(Arg) when is_atom(Arg) ->
    {str, wf_render_actions:normalize_path(Arg)};
escape_arg1(_) ->
    none.
