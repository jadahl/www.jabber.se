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

-module(db_doc).
-export([
        binary_to_atom/1, atom_to_binary/1,
        parse_doc/3, parse_rows/3, parse_row/3, render/2, view_rows/1,
        finalize_entry/1,
        edit_entry/3, push_to_entry/3, pop_from_entry/3, edit_or_add_entry/3,
        get_rev/1, set_rev/2, get_id/1
    ]).

-include("include/db/db.hrl").
-include("include/utils.hrl").

%
% Utilities
%

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).

atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%
% Parse helpers
%

simplify_entries(Binary) when is_binary(Binary) ->
    Binary;
simplify_entries(Entries) when is_list(Entries) ->
    lists:map(fun simplify_entries1/1, Entries).

simplify_entries1({Key, {Values}}) when is_list(Values) ->
    {binary_to_atom(Key), lists:map(fun simplify_entries1/1, Values)};
simplify_entries1({Key, Value}) ->
    {binary_to_atom(Key), Value}.

%
% Parsing
%

parse_doc(ParseFun, InitData, {Entries}) when is_list(Entries) ->
    lists:foldl(ParseFun, InitData, simplify_entries(Entries)).

parse_row(ParseFun, InitData, {Key, Id, {Entries}}) ->
    {Key, Id, lists:foldl(ParseFun, InitData, simplify_entries(Entries))}.

parse_rows(ParseFun, InitData, Rows) when is_list(Rows) ->
    [{Key, Id, lists:foldl(ParseFun, InitData, simplify_entries(Entries))} || {Key, Id, {Entries}} <- Rows].

%
% Render
%

post_render(Doc) ->
    {lists:flatmap(fun finalize_entry/1, Doc)}.

finalize_entry(V) when is_binary(V) ->
    [V];
finalize_entry({K, V}) ->
    case finalize_value(V) of
        undefined -> [];
        FValue -> [{utils:to_binary(K), FValue}]
    end.

finalize_value(Value) ->
    case Value of
        undefined -> undefined;
        _ when is_binary(Value) -> Value;
        _ when is_integer(Value) -> list_to_binary(integer_to_list(Value));
        _ when is_float(Value) -> list_to_binary(float_to_list(Value));
        _ when is_atom(Value) -> atom_to_binary(Value);
        [C | S] when is_number(C) and is_list(S) -> list_to_binary(Value);
        _ when is_list(Value) -> lists:map(fun finalize_value/1, Value);
        {Entry} when is_list(Entry) -> post_render(Entry);
        {Key, SubValue} -> {utils:to_binary(Key), finalize_value(SubValue)};
        _ -> erlang:error({finalize_invalid_value, Value})
    end.

render(RenderFun, InputData) when is_list(InputData) ->
    lists:map(fun(Input) -> render(RenderFun, Input) end, InputData);
render(RenderFun, InputData) ->
    {Id, Rev, Type, Doc} = RenderFun(InputData),
    Doc1 = ?MAYBE_CONS(Type, {type, Type}, Doc),
    Doc2 = ?MAYBE_CONS(Id, {'_id', Id}, Doc1),
    Doc3 = ?MAYBE_CONS(Rev, {'_rev', Rev}, Doc2),

    post_render(Doc3).

%
% Get
%

view_rows({_, _, _, Rows}) -> Rows.

%
% Reader helpers
%

get_entry(Key, {Entries}) ->
    case lists:keysearch(utils:to_binary(Key), 1, Entries) of
        {value, {_, Value}} -> Value;
        _ -> erlang:error({key_not_found, Key})
    end.

get_rev(Doc) ->
    get_entry('_rev', Doc).

get_id(Doc) ->
    get_entry('_id', Doc).

%
% Edit helpers
%

edit_or_add_entry(Key, Value, {Entries}) ->
    KeyB = utils:to_binary(Key),
    {utils:keyreplaceoraddwith(KeyB, 1, fun(_) -> {KeyB, finalize_value(Value)} end, Entries)}.

edit_entry(Key, Value, {Entries}) ->
    KeyB = utils:to_binary(Key),
    {utils:keyreplacewith(KeyB, 1, fun(_) -> {KeyB, finalize_value(Value)} end, Entries)}.

push_to_entry(Key, Value, {Entries}) ->
    {utils:keyreplacewith(utils:to_binary(Key), 1, fun({_, Values}) -> {Key, Values ++ [finalize_value(Value)]} end, Entries)}.

pop_from_entry(Key, Value, {Entries}) ->
    {utils:keyreplacewith(utils:to_binary(Key), 1, fun({_, Values}) -> {Key, Values -- [finalize_value(Value)]} end, Entries)}.

set_rev(Rev, Doc) ->
    edit_or_add_entry('_rev', Rev, Doc).
