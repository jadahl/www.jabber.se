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

-module(db_post).
-export([
        parse_doc/2, render_post/1, get_post/1, get_posts/0, get_posts/1, save_posts/2,
        safe_value_by_locale/2,
        maybe_default_value/2, default_value/1,
        maybe_value_by_locale/2, value_by_locale/2
    ]).

-include("include/utils.hrl").
-include("include/cms/db.hrl").

parse_doc({K, V}, P) ->
    case K of
        '_id' -> P#db_post{id = V};
        title -> P#db_post{title = V};
        ts -> P#db_post{timestamp = list_to_integer(binary_to_list(V))};
        tags -> P#db_post{tags = V};
        authors -> P#db_post{authors = lists:map(fun binary_to_list/1, V)};
        body -> P#db_post{body = V};

        type -> P;
        '_rev' -> P;
        _ ->
            ?LOG_INFO("Parser: Unknown entry ~p", [K]),
            P
    end.

render_post(#db_post{
        id = Id,
        title = Title,
        timestamp = TimeStamp,
        tags = Tags,
        authors = Authors,
        body = Body}) ->
    {
        Id,
        post,
        [
            {title, Title},
            {ts, TimeStamp},
            {tags, Tags},
            {authors, Authors},
            {body, Body}
        ]
   
    }.

get_post(Key) ->
    Key2 = case Key of
        {_, _} ->
            {db_utils:finalize_entry(Key)};
        _ when is_list(Key) -> list_to_binary(Key)
    end,

    View = db_controller:get_view("posts_by_id", [{<<"key">>, Key2}]),
    case db_utils:view_rows(View) of
        [Row] ->
            db_utils:parse(fun parse_doc/2, #db_post{}, Row);
        _ ->
            []
    end.

get_posts() ->
    get_posts("posts").
get_posts(ViewName) ->
    View = db_controller:get_view(ViewName, []),
    Rows = db_utils:view_rows(View),
    lists:map(fun({_, _, Posts}) -> Posts end, db_utils:parse(fun parse_doc/2, #db_post{}, Rows)).

save_posts(Posts, Db) ->
    db_controller:save_docs(fun render_post/1, Posts, Db).

%
% Locale
%

safe_value_by_locale(Locale, Values) ->
    case value_by_locale(Locale, Values) of
        nothing -> 
            case default_value(Values) of
                nothing -> 
                    ?LOG_WARNING("No valid translation found for '~p' in '~p'", [Locale, Values]),
                    [];
                Value2 -> Value2
            end;
        Value1 -> Value1
    end.

maybe_default_value(Value, _) when is_list(Value) ->
    {just, Value};
maybe_default_value(_, _) ->
    nothing.

default_value(Value) when is_binary(Value) ->
    Value;
default_value(Values) ->
    utils:find_with(fun maybe_default_value/2, none, Values).

maybe_value_by_locale({Locale1, Value}, Locale2) when Locale1 == Locale2 ->
    {just, Value};
maybe_value_by_locale(_, _) ->
    nothing.

value_by_locale(_, Value) when is_binary(Value) ->
    Value;
value_by_locale(Locale, Values) ->
    utils:find_with(fun maybe_value_by_locale/2, Locale, Values).
