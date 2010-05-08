-module(db_utils).
-export([binary_to_atom/1, atom_to_binary/1, parse/3, render/2, view_rows/1,
        finalize_entry/1
    ]).

-include("include/cms/db.hrl").

%
% Utilities
%

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).

atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%
% Parsing
%

simplify_entries(Binary) when is_binary(Binary) ->
    Binary;
simplify_entries(Entries) when is_list(Entries) ->
    lists:map(fun simplify_entries1/1, Entries).

simplify_entries1({Key, {Values}}) when is_list(Values) ->
    {binary_to_atom(Key), lists:map(fun simplify_entries1/1, Values)};
simplify_entries1({Key, Value}) ->
    {binary_to_atom(Key), Value}.

parse(ParseFun, InitData, Rows) when is_list(Rows) ->
    [{Key, Id, lists:foldl(ParseFun, InitData, simplify_entries(Entries))} || {Key, Id, {Entries}} <- Rows];
parse(ParseFun, InitData, {Key, Id, {Entries}}) ->
    {Key, Id, lists:foldl(ParseFun, InitData, simplify_entries(Entries))}.

%
% Render
%

post_render(Doc) ->
    {lists:flatmap(fun finalize_entry/1, Doc)}.

finalize_entry({K, V}) ->
    case finalize_value(V) of
        undefined -> [];
        FValue when is_atom(K) -> [{atom_to_binary(K), FValue}];
        FValue when is_list(K) -> [{list_to_binary(K), FValue}];
        FValue when is_binary(K) -> [{K, FValue}]
    end.

finalize_value(Value) ->
    case Value of
        undefined -> undefined;
        _ when is_integer(Value) -> list_to_binary(integer_to_list(Value));
        _ when is_float(Value) -> list_to_binary(float_to_list(Value));
        _ when is_atom(Value) -> atom_to_binary(Value);
        [C | S] when is_number(C) and is_list(S) -> list_to_binary(Value);
        _ when is_list(Value) -> lists:map(fun finalize_value/1, Value);
        {Entry} -> post_render(Entry)
    end.

render(RenderFun, InputData) when is_list(InputData) ->
    lists:map(fun(Input) -> render(RenderFun, Input) end, InputData);
render(RenderFun, InputData) ->
    {Id, Type, Doc} = RenderFun(InputData),
    Doc2 = case Type of
        undefined ->
            Doc;
        _ ->
            [{type, Type} | Doc]
    end,
    Doc3 = case Id of
        undefined ->
            Doc2;
        _ ->
            [{'_id', Id} | Doc2]
    end,

    post_render(Doc3).

%
% Get
%

view_rows({_, _, _, Rows}) -> Rows.

