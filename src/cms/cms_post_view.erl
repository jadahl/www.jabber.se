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

-module(cms_post_view).
-export([
        tag/1, add_tag/1, remove_tag/1, tag_alternatives/0,
        set_saved_label/0, set_save_failed_label/0,
        set_content/2, set_post_state/1,
        body/2, wire_validators/0, title/0]).

-include_lib("nitrogen_core/include/wf.hrl").

-include("include/utils.hrl").
-include("include/ui.hrl").
-include("include/db/db.hrl").

%
% Tags
%

tag(TagName) ->
    Id = wf_render_elements:temp_id(),
    {
        #link{
            id = Id,
            class = post_dialog_tag,
            postback = {remove_tag, Id, TagName},
            delegate = cms_post,
            actions = #event{
                type = click,
                actions = #site_cast{cast = current_post.remove_tag, args = [TagName]}},
            text = TagName},
        Id
    }.

add_tag(Tag) ->
    % update client side dom
    {Element, TagId} = tag(Tag),
    wf:wire(#site_cast{cast = current_post.add_tag, args = [Tag, TagId]}),
    wf:insert_bottom(post_dialog_tags, Element).

remove_tag(ElementId) ->
    wf:remove(ElementId).

tag_alternatives() ->
    % FIXME
    wf:wire(post_dialog_tag_input, #autocomplete{alternatives = ["Foo", "Bar", "Baz"]}).

%
% Saved label
%

set_saved_label() ->
    set_saved_label(?T(msg_id_post_dialog_saved)).

set_save_failed_label() ->
    set_saved_label(?T(msg_id_post_dialog_save_failed)).

set_saved_label(Text) ->
    wf:update(post_dialog_saved_message, Text).

%
% Publish / Unpublish button
%

set_post_state(State) ->
    action_state_panel:set_panel(State, post_dialog_publish_unpublish).

%
% Content
%

drop_locale(nothing) -> "";
drop_locale({_, Content}) -> Content.

set_content(Subject, Body) ->
    wf:set(post_dialog_subject_input, drop_locale(Subject)),
    wf:set(post_dialog_text_area, drop_locale(Body)).

%
% Body
%

body(#db_post{tags = Tags, state = State} = Post, Locale) ->

    % get tags
    {TagElements, TagIds} = lists:unzip(lists:map(fun tag/1, Tags)),
    case TagIds of
        [] ->
            ok;
        _ ->
            wf:wire(wf:f("$Site.$current_post.set_tags(~s);", [couchbeam_mochijson2:encode({lists:zip(Tags, lists:map(fun list_to_binary/1, TagIds))})]))
    end,

    % content
    Subject = drop_locale(db_post:value_by_locale(Locale, Post#db_post.title)),
    Body = drop_locale(db_post:value_by_locale(Locale, Post#db_post.body)),

    EnableSaveButtonEvent = #event{type = textchange, actions = [#enable{target = post_dialog_save_button}, #update{target = post_dialog_saved_message, elements = ""}]},
    #panel{
        id = edit_post_body,
        body = [
            % language
            #label{text = ?T(msg_id_post_dialog_language), style = ?BLOCK},
            #panel{
                style = ?BLOCK,
                body = [
                    #dropdown{
                        id = post_dialog_language_drop_down,
                        style = ?INLINE,
                        postback = language,
                        actions = #event{
                            type = change,
                            actions = [
                                #site_cast{cast = disable_forms, args = [edit_post_body]},
                                #show{target = language_loading}
                            ]
                        },
                        delegate = cms_post,
                        options = [
                            #option{text = Language, value = LocaleTmp, selected = LocaleTmp == Locale} || {LocaleTmp, Language} <- i18n:enabled_languages()
                        ] ++ [#option{text = ?T(msg_id_language_unspecified), value = undefined, selected = Locale == undefined}]
                    },

                    " ",

                    #span{style = ?HIDDEN, text = ?T(msg_id_loading), id = language_loading}
                ]},

            % subject
            #label{text = ?T(msg_id_post_dialog_subject), style = ?BLOCK},
            #textbox{id = post_dialog_subject_input, text = Subject, style = ?BLOCK, actions = EnableSaveButtonEvent},

            % tags
            #label{text = ?T(msg_id_post_dialog_tags), style = ?BLOCK},
            #panel{id = post_dialog_tags, body = TagElements, style = ?INLINE},
            #state_panel{
                id = post_dialog_tag_panel,
                inline = true,
                bodies = [
                    {button, #button{
                            id = post_dialog_add_tag_button,
                            text = ?T(msg_id_post_dialog_add_tag),
                            postback = tag_alternatives,
                            actions = #event{type = click, actions = [
                                    #state_panel_set{target = post_dialog_tag_panel, key = input},
                                    #focus{target = post_dialog_tag_input}
                                ]},
                            delegate = cms_post}},
                    {input, #textbox{
                            id = post_dialog_tag_input,
                            postback = add_tag, 
                            delegate = cms_post,
                            actions = [
                                #event{type = enterkey, actions = [
                                        % remove old autocomplete
                                        #autocomplete{method = destroy},

                                        % display add button
                                        #state_panel_set{target = post_dialog_tag_panel, key = button},

                                        % focus add button
                                        #focus{target = post_dialog_add_tag_button},

                                        % enable save button
                                        #enable{target = post_dialog_save_button}
                                    ]},
                                #event{type = focus, actions = [
                                        #select{}
                                    ]}
                            ]}}
                ],
                visible = true,
                init_state = button
            },#br{},

            % content
            #label{text = ?T(msg_id_post_dialog_content), style = ?BLOCK},
            #textarea{id = post_dialog_text_area, text = Body, actions = EnableSaveButtonEvent},

            % publish/unpublish | save | discard | cancel
            #state_panel{
                id = post_dialog_publish_unpublish,
                inline = true,
                visible = true,
                init_state = State,
                bodies = [
                    {public, 
                        #button{
                            id = post_dialog_unpublish_button,
                            text = ?T(msg_id_post_dialog_unpublish),
                            actions = #event{type = click, actions = #disable{}},
                            postback = unpublish,
                            delegate = cms_post
                        }
                    },
                    {draft,
                        #button{
                            id = post_dialog_publish_button,
                            text = ?T(msg_id_post_dialog_publish),
                            actions = #event{type = click, actions = #disable{}},
                            postback = publish,
                            delegate = cms_post
                        }
                    }
                ]
            },

            #button{
                id = post_dialog_save_button,
                text = ?T(msg_id_post_dialog_save),
                actions = #event{type = click, actions = #disable{}},
                postback = save,
                delegate = cms_post},

            #button{
                id = post_dialog_discard_button,
                text = ?T(msg_id_post_dialog_discard),
                postback = discard,
                delegate = cms_post},

            " ",

            #span{id = post_dialog_saved_message}
        ]
    }.

wire_validators() ->
    % validator to avoid tag duplicates
    wf:wire(post_dialog_tag_input, post_dialog_tag_input,
        #validate{validators = [
                #js_custom{
                    text = " ",
                    function = "$Site.current_post.validate_new_tag"},
                #is_required{text = " "}
            ]}).

title() ->
    ?T(msg_id_post_dialog_title).

