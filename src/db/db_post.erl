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
        parse_doc/1, parse_row/1, parse_helper/2, render_post/1,
        open_post/1, get_post/1,
        get_posts_by/1, get_drafts_by/1,
        get_posts_by_view/1, get_posts_by_view/2,
        new_post/0, save_post/1, save_posts/2,
        t/1, safe_value_by_locale/2,
        maybe_default_value/2, default_value/1,
        maybe_value_by_locale/2, value_by_locale/2,
        update_post/1, set_body/2, push_tag/2, pop_tag/2
    ]).

-include("include/utils.hrl").
-include("include/db/db.hrl").

%
% Doc <--> Post
%

parse_doc(Doc) ->
    db_doc:parse_doc(fun parse_helper/2, #db_post{}, Doc).

parse_row(Row) ->
    db_doc:parse_row(fun parse_helper/2, #db_post{}, Row).

parse_rows(Rows) ->
    db_doc:parse_rows(fun parse_helper/2, #db_post{}, Rows).

parse_helper({K, V}, P) ->
    case K of
        '_id' -> P#db_post{id = V};
        state -> P#db_post{state = list_to_atom(binary_to_list(V))};
        title -> P#db_post{title = V};
        ts -> P#db_post{timestamp = list_to_integer(binary_to_list(V))};
        tags -> P#db_post{tags = V};
        authors -> P#db_post{authors = V};
        body -> P#db_post{body = V};

        type -> P;
        '_rev' -> P;
        _ ->
            ?LOG_INFO("Parser: Unknown entry ~p", [K]),
            P
    end.

-define(ensure_value_is(What, Value, Type), case Type(Value) of true -> true; _ -> erlang:error({invalid_value, What, Value}) end).
-define(ensure_all_values_are(What, Values, Type), lists:all(fun(ValueFromGivenValues) -> ?ensure_value_is(What, ValueFromGivenValues, Type) end, Values)).

is_content(Value) when is_binary(Value) ->
    true;
is_content(Value) when is_list(Value) ->
    is_string(Value);
is_content({List}) when is_list(List) ->
    lists:all(
        fun({Lang, Content}) ->
            case {i18n:is_lang(Lang), is_content_body(Content)} of
                {true, true} ->
                    true;
                _ ->
                    false
            end
        end, List);
is_content(_) ->
    false.
    

is_string(String) when is_list(String) ->
    lists:all(fun(C) -> is_integer(C) end, String);
is_string(_) ->
    false.

is_content_body(Content) ->
    is_binary(Content) or is_string(Content).

check_post(#db_post{
        state = State,
        title = Title,
        timestamp = TimeStamp,
        tags = Tags,
        authors = Authors,
        body = Body}) ->
    ?ensure_value_is(state, State, fun(V) -> lists:member(V, [public, draft]) end),
    ?ensure_value_is(title, Title, is_content),
    ?ensure_value_is(timestamp, TimeStamp, fun(V) -> is_integer(V) or (V == undefined) end),
    ?ensure_all_values_are(tags, Tags, is_content_body),
    ?ensure_all_values_are(authors, Authors, fun(W) -> is_string(W) or is_binary(W) end),
    ?ensure_value_is(body, Body, is_content),
    ok.

render_post(#db_post{
        id = Id,
        state = State,
        title = Title,
        timestamp = TimeStamp,
        tags = Tags,
        authors = Authors,
        body = Body} = Post) ->

    check_post(Post),

    {
        Id,
        post,
        [
            {title, Title},
            {state, State},
            {ts, TimeStamp},
            {tags, Tags},
            {authors, Authors},
            {body, Body}
        ]
   
    }.

%
% Pulling
%

open_post(Key) ->
    Key2 = case Key of
        {_, _} ->
            {db_doc:finalize_entry(Key)};
        _ -> utils:to_binary(Key)
    end,

    db_controller:open_doc(Key2).

get_post(Key) ->
    Doc = open_post(Key),
    parse_doc(Doc).

get_posts_by(Username) when is_list(Username) ->
    UsernameB = utils:to_binary(Username),
    get_posts_by_view("posts_by", [{<<"startkey">>, [UsernameB, null]}, {<<"endkey">>, [UsernameB, infinity]}]).

get_drafts_by(Username) when is_list(Username) ->
    UsernameB = utils:to_binary(Username),
    get_posts_by_view("drafts", [{<<"startkey">>, [UsernameB, null]}]).

get_posts_by_view(ViewName) ->
    get_posts_by_view(ViewName, []).
get_posts_by_view(ViewName, Params) ->
    View = db_controller:get_view(ViewName, Params),
    Rows = db_doc:view_rows(View),
    lists:map(fun({_, _, Posts}) -> Posts end, parse_rows(Rows)).

%
% Pushing
%

new_post() ->
    Doc = db_controller:render_and_save(fun render_post/1, #db_post{}),
    parse_doc(Doc).

save_post(Post) ->
    db_controller:render_and_save(fun render_post/1, Post).

save_posts(Posts, Db) ->
    db_controller:render_and_save_many(fun render_post/1, Posts, Db).

%
% Locale
%

t(Values) ->
    safe_value_by_locale(i18n:get_language(), Values).

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

%
% Editing
%

update_post(#db_post{id = undefined} = Post) ->
    save_post(Post);
update_post(Post) ->
    CurrentDoc = db_controller:open_doc(Post#db_post.id),
    NewDoc = db_doc:render(fun render_post/1, Post),
    NewDoc2 = db_doc:set_rev(db_doc:get_rev(CurrentDoc), NewDoc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc2)).

set_body(NewBody, Key) ->
    Doc = db_controller:open_doc(Key),
    NewDoc = db_doc:edit_entry(body, NewBody, Doc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc)).

push_tag(NewTag, Key) ->
    Doc = db_controller:open_doc(Key),
    NewDoc = db_doc:push_to_entry(tags, NewTag, Doc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc)).

pop_tag(Tag, Key) ->
    Doc = db_controller:open_doc(Key),
    NewDoc = db_doc:pop_from_entry(tags, Tag, Doc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc)).
