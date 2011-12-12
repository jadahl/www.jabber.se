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
        get_published_count/1, get_draft_count/1,
        get_posts_by/1, get_posts_by/3, get_drafts_by/1, get_drafts_by/3,
        get_published_by/1, get_published_by/2, get_published_by/3,
        get_posts_by_view/1, get_posts_by_view/2,
        new_post/0, save_post/1, save_posts/2,
        t/1,
        maybe_default_value/2, default_value/1,
        maybe_value_by_locale/2, value_by_locale/2, value_prefer_locale/2,
        set_title/3, set_body/3, set_state/2,
        update_post/1, edit_post/3, set_body/2, push_tag/2, pop_tag/2
    ]).

-include("include/utils.hrl").
-include("include/db/db.hrl").

%
% Types
%

-type locale()  :: undefined | sv_SE | en_US.
-type post()    :: #db_post{}.
-type value()   :: {locale(), binary()}.
-type content() :: binary() | {[value()]}.

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
        '_rev' -> P#db_post{rev = V};
        state -> P#db_post{state = list_to_atom(binary_to_list(V))};
        title -> P#db_post{title = V};
        ts -> P#db_post{timestamp = list_to_integer(binary_to_list(V))};
        edited -> P#db_post{edited = list_to_integer(binary_to_list(V))};
        tags -> P#db_post{tags = V};
        authors -> P#db_post{authors = V};
        content_type -> P#db_post{content_type = V};
        body -> P#db_post{body = V};

        type -> P;
        _ ->
            ?LOG_INFO("Parser: Unknown entry ~p", [K]),
            P
    end.

-define(ensure_value_is(What, Value, Type), Type(Value) orelse erlang:error({invalid_value, What, Value})).
-define(ensure_all_values_are(What, Values, Type), lists:all(fun(ValueFromGivenValues) -> ?ensure_value_is(What, ValueFromGivenValues, Type) end, Values)).

is_content(Value) when is_binary(Value) ->
    true;
is_content([]) ->
    true;
is_content(String) when is_list(String) and is_integer(hd(String)) ->
    true;
is_content({List}) when is_list(List) ->
    is_string(List) orelse
    lists:all(
        fun(Content) ->
            case Content of
                {Lang, Body} ->
                    case {Lang == undefined orelse cf_i18n:is_lang(Lang), is_content_body(Body)} of
                        {true, true} ->
                            true;
                        _ ->
                            false
                    end;
                _ ->
                    ?LOG_ERROR("Invalid content, '~p'", [Content]),
                    false
            end
        end, List);
is_content(_Value) ->
    ?LOG_ERROR("Invalid content '~p'", [_Value]),
    false.

is_string(String) when is_list(String) and (String /= "") ->
    lists:all(fun is_integer/1, String);
is_string(_String) ->
    false.

is_content_body(Content) ->
    is_binary(Content) orelse is_string(Content).

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
    ?ensure_all_values_are(authors, Authors, fun(W) -> is_string(W) or (is_binary(W) and (W /= <<>>)) end),
    ?ensure_value_is(body, Body, is_content),
    ok.

-spec render_post(post()) -> {binary(), binary(), post, any()}.
render_post(#db_post{
        id = Id,
        rev = Rev,
        state = State,
        title = Title,
        timestamp = TimeStamp,
        edited = Edited,
        tags = Tags,
        authors = Authors,
        content_type = ContentType,
        body = Body} = Post) ->

    check_post(Post),

    {
        Id,
        Rev,
        post,
        lists:flatten([
            {title, Title},
            {state, State},
            {ts, TimeStamp},
            if Edited == undefined -> [];
               true                -> {edited, Edited}
            end,
            {tags, Tags},
            {authors, Authors},
            {content_type, ContentType},
            {body, Body}
        ])
    }.

%
% Pulling
%

open_post(Key) ->
    Key2 = case Key of
        {_, _} ->
            {db_doc:finalize_entry(Key)};
        _ -> cf_utils:to_binary(Key)
    end,

    db_controller:open_doc(Key2).

get_published_count(Username) ->
    get_post_count(Username, posts_pub_count).

get_draft_count(Username) ->
    get_post_count(Username, posts_draft_count).

get_post_count(Username, ViewName) ->
    ViewB = cf_utils:to_binary(ViewName),
    UsernameB = list_to_binary(Username),
    View = db_controller:get_view(ViewB, [{<<"key">>, UsernameB}]),
    case db_doc:view_rows(View) of
        [{_, _, Count}] -> Count;
        [] -> 0
    end.

get_post(Key) ->
    Doc = open_post(Key),
    parse_doc(Doc).

get_posts_by(Username) when is_list(Username) ->
    get_posts_by(Username, 0).
get_posts_by(Username, StartIndex) ->
    get_posts_by(Username, StartIndex, ?DEFAULT_POSTS_PER_PAGE).
get_posts_by(Username, StartIndex, PostsPerPage) ->
    UsernameB = cf_utils:to_binary(Username),
    get_posts_by_view("posts_by",
                      [{<<"startkey">>, [UsernameB, null]},
                       {<<"endkey">>, [UsernameB, infinity]}
                       | db_utils:start_limit(StartIndex, PostsPerPage)]).

get_published_by(Username) when is_list(Username) ->
    get_published_by(Username, 0).
get_published_by(Username, StartIndex) ->
    get_published_by(Username, StartIndex, ?DEFAULT_POSTS_PER_PAGE).
get_published_by(Username, StartIndex, PostsPerPage) ->
    UsernameB = cf_utils:to_binary(Username),
    get_posts_by_view("published_by",
                      [{<<"startkey">>, [UsernameB, null]},
                       {<<"endkey">>, [UsernameB, infinity]}
                       | db_utils:start_limit(StartIndex, PostsPerPage)]).


get_drafts_by(Username) when is_list(Username) ->
    get_drafts_by(Username, 0).
get_drafts_by(Username, StartIndex) ->
    get_drafts_by(Username, StartIndex, ?DEFAULT_POSTS_PER_PAGE).
get_drafts_by(Username, StartIndex, PostsPerPage) ->
    UsernameB = cf_utils:to_binary(Username),
    get_posts_by_view("drafts_by",
                      [{<<"startkey">>, [UsernameB, null]},
                       {<<"endkey">>, [UsernameB, infinity]}
                       | db_utils:start_limit(StartIndex, PostsPerPage)]).

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

-spec t(content()) -> binary().
t(Values) ->
    safe_value_prefer_locale(cf_i18n:get_language(), Values).

%
% Simple function for getting any value hoping for the right locale
%

-spec safe_value_prefer_locale(locale(), content()) -> binary().
safe_value_prefer_locale(Locale, Values) ->
    case value_prefer_locale(Locale, Values) of
        nothing ->
            ?LOG_WARNING("No valid translation found for '~p' in '~p'", [Locale, Values]),
            <<>>;
        {_, Value} ->
            Value
    end.

%
% Locale functions
%

maybe_default_value({undefined, _} = Value, _) ->
    {just, Value};
maybe_default_value(_, _) ->
    nothing.

-spec default_value(content()) -> value() | nothing.
default_value(Value) when is_binary(Value) ->
    {undefined, Value};
default_value({Values}) when is_list(Values) ->
    cf_utils:just(cf_utils:find_with(fun maybe_default_value/2, none, Values)).

maybe_value_by_locale({Locale, _} = Value, Locale) ->
    {just, Value};
maybe_value_by_locale({undefined, _} = Value, undefined) ->
    {just, Value};
maybe_value_by_locale(_, _) ->
    nothing.

-spec value_by_locale(locale(), content()) -> value() | nothing.
value_by_locale(Locale, {Values}) when is_list(Values) ->
    cf_utils:just(cf_utils:find_with(fun maybe_value_by_locale/2, Locale, Values));
value_by_locale(undefined, Value) when is_binary(Value) ->
    {undefined, Value};
value_by_locale(_, _) ->
    nothing.

%
% value_by_any_locale(Values) -> {Value, Locale} | nothing
%   Values = list()
%   Value = binary()
%   Locale = locale()
%
-spec value_by_any_locale(content()) -> value() | nothing.
value_by_any_locale({[{_, _} = Value | _]}) ->
    Value;
value_by_any_locale({[]}) ->
    nothing.

%
% value_prefer_locale(Values) -> {Locale, Value} | nothing
%   Values = {list()}
%   Value = binary()
%   Locale = locale()
%
-spec value_prefer_locale(locale(), content()) -> value() | nothing.
value_prefer_locale(Locale, Values) ->
    case value_by_locale(Locale, Values) of
        nothing ->
            case default_value(Values) of
                nothing ->
                    value_by_any_locale(Values);
                DefaultValue ->
                    DefaultValue
            end;
        Value ->
            Value
    end.

%
% Editing - record
%

loose_keystore(Key, [{Key, _} | Vs], T) ->
    [T | Vs];
loose_keystore(Key, [V | Vs], T) ->
    [V | loose_keystore(Key, Vs, T)];
loose_keystore(_, [], T) ->
    [T].

store_by_locale(Value, Locale, {Values}) when is_binary(Value) and is_atom(Locale) and is_list(Values) ->
    {loose_keystore(Locale, Values, {Locale, Value})};
store_by_locale(Value, Locale, OtherValue) when is_binary(Value) and is_atom(Locale) and is_binary(OtherValue) ->
    {[{undefined, OtherValue}, {Locale, Value}]};
store_by_locale(Value, Locale, {Values}) when is_list(Values) ->
    store_by_locale(cf_utils:to_binary(Value), cf_utils:to_atom(Locale), {Values}).

set_title(undefined, _, Post) -> Post;
set_title(Title, Locale, Post) ->
    Post#db_post{title = store_by_locale(Title, Locale, Post#db_post.title)}.

set_body(undefined, _, Post) -> Post;
set_body(Body, Locale, Post) ->
    Post#db_post{body = store_by_locale(Body, Locale, Post#db_post.body)}.

set_state(undefined, Post) -> Post;
set_state(State, Post) ->
    Post#db_post{state = State}.

%
% Editing - directly
%

update_post(#db_post{id = undefined} = Post) ->
    save_post(Post);
update_post(Post) ->
    CurrentDoc = db_controller:open_doc(Post#db_post.id),
    NewDoc = db_doc:render(fun render_post/1, Post),
    NewDoc2 = db_doc:set_rev(db_doc:get_rev(CurrentDoc), NewDoc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc2)).

edit_post(NewBody, EntryKey, Key) ->
    Doc = db_controller:open_doc(Key),
    NewDoc = db_doc:edit_entry(EntryKey, NewBody, Doc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc)).

set_body(NewBody, Key) ->
    edit_post(NewBody, body, Key).

push_tag(NewTag, Key) ->
    Doc = db_controller:open_doc(Key),
    NewDoc = db_doc:push_to_entry(tags, NewTag, Doc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc)).

pop_tag(Tag, Key) ->
    Doc = db_controller:open_doc(Key),
    NewDoc = db_doc:pop_from_entry(tags, Tag, Doc),
    ?DB_HANDLE_RESULT(db_controller:save_doc(NewDoc)).
