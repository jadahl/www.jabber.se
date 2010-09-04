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
-export([selected/0, left/0, title/0, body/0, hook/0, event/1, new_post/0, open_post/1]).
-behaviour(gen_cms_admin_module).

-include("include/ui.hrl").
-include("include/utils.hrl").
-include("include/db/db.hrl").

%
% Admin control
%

selected() ->
    new_post().

left() ->
    close().

title() ->
    cms_post_view:title().

body() ->
    Post = case get_current_post() of
        undefined ->
            new_post();
        CurrentPost ->
            CurrentPost
    end,

    body(Post).

body(Post) ->
    cms_post_view:body(Post).

hook() ->
    cms_post_view:wire_validators().

%
% Events
%

event({open, Post, Back}) ->
    ?AUTH(open(Post, Back));

%
% Compose events
%

event(post) ->
    ?AUTH(post());

event(save) ->
    ?AUTH(save());

event(discard) ->
    ?AUTH(discard());

%
% Tag events
%

event(tag_alternatives) ->
    ?AUTH(tag_alternatives());

event({remove_tag, Id, Tag}) ->
    ?AUTH(remove_tag(Id, Tag));

event(add_tag) ->
    ?AUTH(add_tag(wf:q(post_dialog_tag_input))).

%
% Open / close
%

open(Post, Back) ->
    Post1 = open_post(Post),
    cms_admin_view:set_body_back(title(), body(Post1), ?MODULE, Back).

close() ->
    set_current_post(undefined).

%close(_Back) ->
%    set_current_post(undefined),
%    cms_admin:back().

%
% Read/Write
%

-define(POST_KEY, {cms_post, current_post}).

get_current_post() ->
    wf:session(?POST_KEY).

set_current_post(Post) ->
    wf:session(?POST_KEY, Post).

empty_post() ->
    #db_post{authors = [wf:user()]}.

new_post() ->
    open_post(empty_post()).

open_post(Post) ->
    set_current_post(Post),

    % new client side post object
    wf:wire(#js_call{fname = "$Site.$new_post"}),

    Post.
    
%
% Document control
%

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.

post() ->
    ?LOG_INFO("Posting draft", []),
    case get_current_post() of
        undefined ->
            ?LOG_WARNING("Trying to post unknown draft.", []);
        Post ->
            try
                % save to db
                do_save(Post#db_post{state = public}),

                % clear cache
                set_current_post(undefined),

                % update ui
                cms_admin_view:back()
            catch
                _:_ = Error ->
                    ?LOG_ERROR("Could not post: ~p", [Error])
            end
    end.

save() ->
    ?LOG_INFO("Saving draft", []),
    case get_current_post() of
        undefined ->
            ?LOG_WARNING("Trying to save unknown draft.", []);
        Post ->
            % update db side
            NewPost = do_save(Post),

            % update cache (need to?)
            set_current_post(NewPost),

            % display saved
            wf:update(post_dialog_saved_message, ?T(msg_id_post_dialog_saved)),

            ok
    end.

do_save(Post) ->
    Post1 = case wf:q(post_dialog_subject_input) of
        undefined -> Post;
        Subject -> Post#db_post{title = Subject}
    end,

    Post2 = case wf:q(post_dialog_text_area) of
        undefined -> Post1;
        Body -> Post1#db_post{body = Body}
    end,

    NewPost = Post2#db_post{
        authors = [wf:user()],
        timestamp = unix_timestamp()
    },

    % update db doc
    db_post:update_post(NewPost),

    NewPost.

discard() ->
    ?LOG_INFO("Discarding draft", []),
    case get_current_post() of
        undefined ->
            ?LOG_WARNING("Trying to discard unexisting post.", []);
        #db_post{id = Id} = Post ->
            try
                % delete from database
                db_controller:delete_doc_by_id(Id),

                % clear cache
                set_current_post(undefined),

                % update ui
                wf:wire(post_dialog, #dialog_hide{})
            catch
                _:_ = Error ->
                    ?LOG_ERROR("Could not discard post '~p': ~p", [Post, Error])
            end
    end.

%
% Tag manipulations
%

tag_alternatives() ->
    wf:wire(#autocomplete{anchor = post_dialog_tag_input, alternatives = ["Foo", "Bar", "Baz"]}),

    ok.

remove_tag(Id, Tag) ->
    case get_current_post() of
        undefined ->
            ?LOG_WARNING("Trying to remove tag '~p' from nonexisting draft.", [Tag]);
        #db_post{tags = Tags} = Post ->
            case lists:member(Tag, Tags) of
                true ->
                    do_remove_tag(Post, Id, Tag);
                _ ->
                    ?LOG_INFO("Trying to remove nonexisting tag '~p' from draft '~p'", [Tag, Post])
            end
    end.

do_remove_tag(Post, ElementId, Tag) ->
    % update database draft
    db_post:pop_tag(Tag, Post#db_post.id),

    % update in-memory draft
    set_current_post(Post#db_post{tags = Post#db_post.tags -- [Tag]}),

    % update client side dom
    wf:remove(ElementId),

    ok.

add_tag(Tag) ->
    % TODO add lock mechanism to avoid collisions
    ?LOG_INFO("add_tag(~p)", [Tag]),
    case get_current_post() of
        undefined ->
            ?LOG_WARNING("Trying to add tag '~p' to nonexisting draft.", [Tag]);
        #db_post{tags = Tags} = Post ->
            case lists:member(Tag, Tags) of
                true ->
                    ?LOG_INFO("duplicate", []),
                    ok;
                _ ->
                    do_add_tag(Post, Tag)
            end
    end.

do_add_tag(Post, Tag) ->
    ?LOG_INFO("Adding tag ~p to draft", [Tag]),

    % add tag
    NewPost = Post#db_post{tags = Post#db_post.tags ++ [Tag]},

    % update database draft
    case NewPost#db_post.id of
        undefined ->
            db_post:save_post(NewPost);
        Id ->
            db_post:push_tag(Tag, Id)
    end,

    % update in-memory draft
    set_current_post(NewPost),

    % update client side dom
    {Element, TagId} = cms_post_view:tag(Tag),
    wf:wire(#js_call{fname = "$Site.current_post.add_tag", args = [Tag, TagId]}),
    wf:insert_bottom(post_dialog_tags, Element),

    ok.

