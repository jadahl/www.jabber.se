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

-module(cms_compose).
-export([selected/0, left/0, title/0, body/0, hook/0, event/1]).
-behaviour(gen_cms_admin_module).

-include("include/ui.hrl").
-include("include/utils.hrl").
-include("include/db/db.hrl").

%
% In-memory cache (stores document id)
%

-define(POST_KEY, cms_post_current).

-spec current_post() -> undefined | binary().
current_post() ->
    wf:session(?POST_KEY).

-spec set_current_post(undefined | binary()) -> any().
set_current_post(Id) when is_binary(Id) orelse Id == undefined ->
    wf:session(?POST_KEY, Id).

-spec unset_current_post() -> any().
unset_current_post() ->
    set_current_post(undefined).

%
% Admin control
%

selected() ->
    % unset possible current document id
    unset_current_post(),

    remote_new_post().

left() ->
    event_close().

title() ->
    cms_compose_view:title().

body() ->
    Post = case current_post() of
        undefined ->
            empty_post();
        Id ->
            db_post:get_post(Id)
    end,

    body(Post).

body(Post) ->
    cms_compose_view:body(Post, i18n:get_language()).

body(Post, Locale) ->
    cms_compose_view:body(Post, Locale).

hook() ->
    cms_compose_view:wire_validators().

%
% Events
%

event({open, Id, Locale, Back}) ->
    ?AUTH(event_open(Id, Locale, Back));

% Compose events

event(publish) ->
    ?AUTH(event_publish());

event(unpublish) ->
    ?AUTH(event_unpublish());

event(save) ->
    ?AUTH(event_save());

event(discard) ->
    ?AUTH(event_discard());

% Tag events

event(language) ->
    ?AUTH(event_language());

event(copy_lang) ->
    ?AUTH(event_copy_language());

event(tag_alternatives) ->
    ?AUTH(event_tag_alternatives());

event({remove_tag, Id, Tag}) ->
    ?AUTH(event_remove_tag(Id, Tag));

event(add_tag) ->
    ?AUTH(event_add_tag(wf:q(post_dialog_tag_input)));

%
% Content type events
%

event(set_content_type) ->
    ContentType = wf:q(post_dialog_content_type_dropdown),

    case cms_post_view:supported_content_type(ContentType) of
        true ->
            Post = case current_post() of
                undefined ->
                    new_post();
                Id ->
                    db_post:get_post(Id)
            end,
            NewPost = Post#db_post{content_type = ContentType},
            db_post:save_post(NewPost);
        false ->
            ?LOG_WARNING("trying to set invalid content type '~p'",
                         [ContentType])
    end.

%
% Open / close
%

event_open(Id, Locale, Back) ->
    % set id in cache
    set_current_post(Id),

    % clear remote post data
    remote_new_post(),

    % load post from db
    Post = db_post:get_post(Id),

    % view ui
    cms_admin_view:set_body_back(title(), body(Post, Locale), ?MODULE, Back).

event_close() ->
    unset_current_post().

%
% Database document control
%

empty_post() ->
    #db_post{
        authors = [wf:user()],
        timestamp = unix_timestamp()
    }.

new_post() ->
    Id = save_new_post(empty_post()),
    set_current_post(Id),
    Id.

save_new_post(Post) ->
    Doc = db_post:save_post(Post),
    db_doc:get_id(Doc).

%
% Remote
%

remote_new_post() ->
    % new client side post object
    wf:wire(#js_call{fname = "$Site.$new_post"}).

%
% Document control
%

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.

-spec with_locale_do(atom(), fun((atom()) -> any())) -> any() | none().
with_locale_do(Id, Fun) ->
    Locale = wf:q(Id),
    case Locale == "undefined" orelse i18n:is_lang(Locale) of
        true ->
            Fun(list_to_atom(Locale));
        _ ->
            ?LOG_ERROR("Invalid locale ~p", [Locale])
    end.

event_publish() ->
    ?LOG_INFO("Publishing draft", []),
    with_locale_do(post_dialog_language_drop_down,
        fun(Locale) ->
            % publish and save
            do_save(Locale, public),

            % clear cache
            unset_current_post(),

            % update ui
            cms_admin_view:back()
        end).

event_unpublish() ->
    ?LOG_INFO("Unpublishing post", []),
    case current_post() of
        undefined ->
            ?LOG_ERROR("Trying to unpublish undefined post.", []);
        Id ->
            Post = db_post:get_post(Id),
            db_post:save_post(db_post:set_state(draft, Post)),

            % update ui
            cms_compose_view:set_post_state(draft)
    end.

event_save() ->
    ?LOG_INFO("Saving draft", []),
    with_locale_do(post_dialog_language_drop_down,
        fun(Locale) ->
            try
                % save
                Id = do_save(Locale),
                set_current_post(Id),

                % update ui
                cms_compose_view:set_saved_label()
            catch
                _:_ = Error ->
                    error_logger:error_report([{error, Error},
                                               {st, erlang:get_stacktrace()}]),
                    cms_compose_view:set_save_failed_label()
            end
        end).

do_save(Locale) ->
    do_save(Locale, undefined).
do_save(Locale, State) ->
    Post = case current_post() of
        undefined ->
            empty_post();
        Id ->
            db_post:get_post(Id)
    end,

    Post1 = db_post:set_title(wf:q(post_dialog_subject_input), Locale, Post),
    Post2 = db_post:set_body(wf:q(post_dialog_text_area), Locale, Post1),
    Post3 = db_post:set_state(State, Post2),

    % If we are publishing then update the timestamp
    Post4 = if State == public -> Post3#db_post{timestamp = unix_timestamp()};
               true            -> Post3
            end,

    NewPost = Post4#db_post{edited = unix_timestamp()},

    Doc = db_post:save_post(NewPost),
    db_doc:get_id(Doc).

event_discard() ->
    ?LOG_INFO("Discarding draft", []),
    case current_post() of
        undefined ->
            ?LOG_WARNING("Trying to discard unexisting post.", []),

            % update ui
            cms_admin_view:back();
        Id ->
            try
                % delete from database
                db_controller:delete_doc_by_id(Id),

                % clear cache
                unset_current_post(),

                % update ui
                cms_admin_view:back()
            catch
                _:_ = Error ->
                    ?LOG_ERROR("Could not discard post '~p': ~p", [Id, Error])
            end
    end.

%
% Locale
%

-spec event_language() -> any().
event_language() ->
    with_locale_do(post_dialog_language_drop_down,
        fun(Locale) ->
            case current_post() of
                undefined -> ok;
                Id        -> change_locale(Locale, Id)
            end
        end).

-spec event_copy_language() -> any().
event_copy_language() ->
    Fun = fun(Locale, NewLocale) ->
        case current_post() of
            undefined ->
                ok;
            Id ->
                #db_post{title = Title,
                         body = Body} = Post = db_post:get_post(Id),

                % copy title
                Post1 = case db_post:value_by_locale(Locale, Title) of
                    {_, NewTitle} ->
                        db_post:set_title(NewTitle, NewLocale, Post);
                    nothing ->
                        Post
                end,

                % copy body
                Post2 = case db_post:value_by_locale(Locale, Body) of
                    {_, NewBody} ->
                        db_post:set_body(NewBody, NewLocale, Post1);
                    nothing ->
                        Post1
                end,

                % save new post
                db_post:save_post(Post2)
        end
    end,

    with_locale_do(post_dialog_copy_lang_drop_down,
        fun(NewLocale) ->
            with_locale_do(post_dialog_language_drop_down,
                fun(Locale) -> Fun(Locale, NewLocale) end)
        end).

-spec change_locale(atom(), binary()) -> any().
change_locale(Locale, Id) ->
    #db_post{body = Body, title = Title} = db_post:get_post(Id),

    % update form content with other translation, or empty if none

    % content
    NewSubject = db_post:value_by_locale(Locale, Title),
    NewBody = db_post:value_by_locale(Locale, Body),

    cms_compose_view:set_content(NewSubject, NewBody),

    ok.

%
% Tag manipulations
%

event_tag_alternatives() ->
    ?LOG_INFO("tag_alternatives()", []),
    cms_compose_view:tag_alternatives().

event_remove_tag(ElementId, Tag) ->
    ?LOG_INFO("remove_tag(~p, ~p)", [ElementId, Tag]),
    case current_post() of
        undefined ->
            ?LOG_WARNING("Trying to remove tag '~p' from unknown post.", [Tag]);
        Id ->
            db_post:pop_tag(Tag, Id),

            cms_compose_view:remove_tag(ElementId)
    end.

event_add_tag(Tag) ->
    ?LOG_INFO("add_tag(~p)", [Tag]),
    case current_post() of
        undefined ->
            Id = save_new_post((empty_post())#db_post{tags = [Tag]}),
            set_current_post(Id);
        Id ->
            db_post:push_tag(Tag, Id)
    end,

    cms_compose_view:add_tag(Tag).

