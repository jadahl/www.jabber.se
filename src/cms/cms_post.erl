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

-module(cms_post).
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
    cms_post_view:title().

body() ->
    Post = case current_post() of
        undefined ->
            empty_post();
        Id ->
            db_post:get_post(Id)
    end,

    body(Post).

body(Post) ->
    cms_post_view:body(Post, i18n:get_language()).

body(Post, Locale) ->
    cms_post_view:body(Post, Locale).

hook() ->
    cms_post_view:wire_validators().

%
% Events
%

event({open, Id, Locale, Back}) ->
    ?AUTH(event_open(Id, Locale, Back));

% Compose events

event(post) ->
    ?AUTH(event_post());

event(save) ->
    ?AUTH(event_save());

event(discard) ->
    ?AUTH(event_discard());

% Tag events

event(language) ->
    ?AUTH(event_language());

event(tag_alternatives) ->
    ?AUTH(event_tag_alternatives());

event({remove_tag, Id, Tag}) ->
    ?AUTH(event_remove_tag(Id, Tag));

event(add_tag) ->
    ?AUTH(event_add_tag(wf:q(post_dialog_tag_input))).

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


%
% Database document control
%

empty_post() ->
    #db_post{
        authors = [wf:user()],
        timestamp = unix_timestamp()
    }.

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

-spec with_locale_do(fun((atom()) -> any())) -> any() | none().
with_locale_do(Fun) ->
    Locale = wf:q(post_dialog_language_drop_down),
    case Locale == "undefined" orelse i18n:is_lang(Locale) of
        true ->
            Fun(list_to_atom(Locale));
        _ ->
            throw({invalid_locale, Locale})
    end.

event_post() ->
    ?LOG_INFO("Posting draft", []),
    try
        with_locale_do(
            fun(Locale) ->
                % publish and save
                do_save(Locale, public),

                % clear cache
                unset_current_post(),

                % update ui
                cms_admin_view:back()
            end)
    catch
        {invalid_locale, Locale} ->
            ?LOG_ERROR("Couldn't figure out locale (~p), not posting.", [Locale])
    end.

event_save() ->
    ?LOG_INFO("Saving draft", []),
    try
        with_locale_do(
            fun(Locale) ->
                % save
                do_save(Locale),

                % update ui
                cms_post_view:set_saved_label()
            end)
    catch
        {invalid_locale, Locale} ->
            ?LOG_ERROR("Couldn't figure out locale (~p), not saving.", [Locale])
    end.

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

    NewPost = Post3#db_post{
        timestamp = unix_timestamp()
    },

    db_post:save_post(NewPost),

    ok.

event_discard() ->
    ?LOG_INFO("Discarding draft", []),
    case current_post() of
        undefined ->
            ?LOG_WARNING("Trying to discard unexisting post.", []);
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
    try
        with_locale_do(
            fun(Locale) ->
                case current_post() of
                    undefined ->
                        ?LOG_WARNING("Trying to change unknown post to locale '~p'.", [Locale]);
                    Id ->
                        change_locale(Locale, Id)
                end,

                wf:wire(edit_post_body, language_loading, #hide{}),
                wf:wire(#site_cast{cast = enable_forms, args = [edit_post_body]})
            end)
    catch
        {invalid_locale, InvalidLocale} ->
            ?LOG_WARNING("Trying to set post to invalid locale '~p'.", [InvalidLocale])
    end.

-spec change_locale(atom(), binary()) -> any().
change_locale(Locale, Id) ->
    #db_post{body = Body, title = Title} = db_post:get_post(Id),

    % update form content with other translation, or empty if none

    % subject
    NewSubject = case db_post:value_by_locale(Locale, Title) of
        nothing ->
            "";
        Subject ->
            Subject
    end,

    % body
    NewBody = case db_post:value_by_locale(Locale, Body) of
        nothing ->
            "";
        Body1 ->
            Body1
    end,

    cms_post_view:set_content(NewSubject, NewBody),

    ok.

%
% Tag manipulations
%

event_tag_alternatives() ->
    cms_post_view:tag_alternatives().

event_remove_tag(ElementId, Tag) ->
    ?LOG_INFO("remove_tag(~p, ~p)", [ElementId, Tag]),
    case current_post() of
        undefined ->
            ?LOG_WARNING("Trying to remove tag '~p' from unknown post.", [Tag]);
        Id ->
            db_post:pop_tag(Tag, Id),

            cms_post_view:remove_tag(ElementId)
    end.

event_add_tag(Tag) ->
    ?LOG_INFO("add_tag(~p)", [Tag]),
    case current_post() of
        undefined ->
            save_new_post((empty_post())#db_post{tags = [Tag]});
        Id ->
            db_post:push_tag(Tag, Id)
    end,

    cms_post_view:add_tag(Tag).

