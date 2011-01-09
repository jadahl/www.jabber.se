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

render_action(#js_call{fname = FName, args = Args}) ->

    EscapedArgs = lists:map(fun escape/1, Args),
    [FName, "(", utils:join(EscapedArgs, ","), ");"].

escape({function, Actions})              -> ["function() { ", Actions, "}"];
escape({list, List})                     -> [escape(Item) || Item <- List];
escape(undefined)                        -> jkeyword(undefined);
escape(true)                             -> jkeyword(true);
escape(false)                            -> jkeyword(false);
escape(Atom) when is_atom(Atom)          -> atom_to_id(Atom);
escape(Tuple) when is_tuple(Tuple)       -> render(Tuple);
escape(Integer) when is_integer(Integer) -> integer_to_list(Integer);
escape(Float) when is_float(Float)       -> float_to_list(Float);
escape(Binary) when is_binary(Binary)    -> jstr(Binary);
escape(IOList) when is_list(IOList)      -> jstr(IOList).

jkeyword(Atom) ->
    atom_to_list(Atom).

jstr(IOList) ->
    ["\"", IOList, "\""].

render(Element) ->
    Rendered = wf_render:render_elements([Element]),
    wf:js_escape(Rendered).

atom_to_id(Atom) ->
    jstr(wf_render_actions:normalize_path(Atom)).

