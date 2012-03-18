%
%    Jabber.se Web Application
%    Copyright (C) 2010-2012 Jonas Ådahl
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

-module(cms_compose_view).
-export([
        tag/1, add_tag/1, remove_tag/1, tag_alternatives/0,
        set_saved_label/0, set_save_failed_label/0,
        set_content/2, set_post_state/1,
        body/2, wire_validators/0, title/0,
        event/1,

        set_post_id_failed/0,
        set_post_id_success/0]).

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
            delegate = cms_compose,
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
    set_saved_label(?TXT("Saved...")).

set_save_failed_label() ->
    set_saved_label(?TXT("Saving failed...")).

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
% Language dropdown
%

language_dropdown(Id, Languages, Message, Postback) ->
    LoadingId = list_to_atom(atom_to_list(Id) ++ "_lang_loading"),
    [#dropdown{id = Id,
               style = ?INLINE,
               postback = {cms_compose, Postback, LoadingId},
               delegate = ?MODULE,
               actions = #event{type = change,
                                actions = [#site_cast{cast = disable_forms,
                                                      args = [edit_post_body]},
                                           #show{target = LoadingId}]},
               options = Languages},

     " ",

     #span{style = ?HIDDEN,
           text = Message,
           id = LoadingId}].

%
% Body
%

tags(TagElements) ->
    [#label{text = ?TXT("Tags:"), style = ?BLOCK},
     #panel{id = post_dialog_tags, body = TagElements, style = ?INLINE},
     #state_panel{
         id = post_dialog_tag_panel,
         inline = true,
         bodies = [
             {button, #button{
                     id = post_dialog_add_tag_button,
                     text = ?TXT("Add"),
                     postback = tag_alternatives,
                     actions = #event{type = click, actions = [
                             #state_panel_set{target = post_dialog_tag_panel,
                                              key = input},
                             #focus{target = post_dialog_tag_input}
                         ]},
                     delegate = cms_compose}},
             {input, #textbox{
                     id = post_dialog_tag_input,
                     postback = add_tag,
                     delegate = cms_compose,
                     actions = [
                         #event{type = enterkey, actions = [
                                 % remove old autocomplete
                                 #autocomplete{method = destroy},

                                 % display add button
                                 #state_panel_set{target = post_dialog_tag_panel,
                                                  key = button},

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
     }].

content_type_options(CurrentContentType) ->
    ContentTypes = [{"Markdown", <<"text/markdown">>},
                    {"XHTML", <<"application/xhtml+xml">>}],
    Current = cf_utils:to_binary(CurrentContentType),
    [#option{text = Text, value = Type, selected = Current == Type}
     || {Text, Type} <- ContentTypes].

tools(Locale, Post) ->
    Id = case Post#db_post.id of
        undefined -> "";
        CurrentId -> CurrentId
    end,
    ContentType = Post#db_post.content_type,
    [#label{text = ?TXT("Tools:"), style = ?BLOCK},
     #expandable{categories =
         [

          %
          % Translation
          %

          {translation,
           ?TXT("Translation"),
           [#label{text = ?TXT("Go to translation"),
                   style = ?BLOCK},
            #panel{style = ?BLOCK,
                   body = language_dropdown(
                       post_dialog_language_drop_down,
                       ([#option{text = Lang,
                                value = LocaleTmp,
                                selected = LocaleTmp == Locale}
                        || {LocaleTmp, Lang} <- cf_i18n:enabled_languages()] ++
                        [#option{text = ?TXT("(unspecified)"),
                                 value = undefined,
                                 selected = Locale == undefined}]),
                       ?TXT("Loading..."),
                       language)},

            #hr{},

            #label{text = ?TXT("Copy translation to"),
                   style = ?BLOCK},
            #panel{body = language_dropdown(
                       post_dialog_copy_lang_drop_down,
                       [#option{text = ?TXT("(select language)"),
                                value = none} |
                        ([#option{text = Lang,
                                  value = LocaleTmp}
                          || {LocaleTmp, Lang} <- cf_i18n:enabled_languages()] ++
                         [#option{text = ?TXT("(unspecified)"),
                                 value = undefined}])],
                       ?TXT("Copying..."),
                       copy_lang)}]},

          %
          % Content type
          %

          {content_type,
           ?TXT("Content type"),
           [#label{text = ?TXT("Choose content type"), style = ?BLOCK},
            #panel{style = ?BLOCK,
                   body = #dropdown{id = post_dialog_content_type_dropdown,
                                    postback = set_content_type,
                                    delegate = cms_compose,
                                    options = content_type_options(ContentType)}}]},

          {advanced,
           ?TXT("Advanced"),
           [#label{text = ?TXT("Set post ID value"), style = ?BLOCK},
            #textbox{style = ?INLINE,
                     id = new_post_id,
                     text = Id,
                     actions = #event{type = enterkey,
                                      target = set_post_id_button,
                                      actions = #disable{}},
                     postback = set_post_id,
                     delegate = cms_compose},
            " ",
            #button{style = ?INLINE,
                    text = ?TXT("Set"),
                    id = set_post_id_button,
                    actions = #event{type = click,
                                     actions = #disable{}},
                    postback = set_post_id,
                    delegate = cms_compose},
            " ",
            #span{id = set_post_id_status}]}]}].

body(#db_post{tags = Tags, state = State} = Post, Locale) ->

    % get tags
    {TagElements, TagIds} = lists:unzip(lists:map(fun tag/1, Tags)),
    case TagIds of
        [] ->
            ok;
        _ ->
            JSON = {lists:zip(Tags, lists:map(fun list_to_binary/1, TagIds))},
            Ts = couchbeam_mochijson2:encode(JSON),
            wf:wire(wf:f("$Site.$current_post.set_tags(~s);", [Ts]))
    end,

    % content
    Subject = drop_locale(db_post:value_by_locale(Locale, Post#db_post.title)),
    Body = drop_locale(db_post:value_by_locale(Locale, Post#db_post.body)),

    EnableSaveButtonEvent =
        #event{type = textchange,
               actions = [#enable{target = post_dialog_save_button},
                          #update{target = post_dialog_saved_message,
                                  elements = ""}]},
    #panel{
        id = edit_post_body,
        body = [
            % subject
            #label{text = ?TXT("Subject:"), style = ?BLOCK},
            #textbox{id = post_dialog_subject_input,
                     text = Subject,
                     style = ?BLOCK,
                     actions = EnableSaveButtonEvent},

            % tags
            tags(TagElements),
            #br{},

            % tools
            tools(Locale, Post),
            #br{},

            % content
            #label{text = ?TXT("Content:"), style = ?BLOCK},
            #textarea{id = post_dialog_text_area,
                      text = Body,
                      actions = EnableSaveButtonEvent},

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
                            text = ?TXT("Unpublish"),
                            actions = #event{type = click,
                                             actions = #disable{}},
                            postback = unpublish,
                            delegate = cms_compose
                        }
                    },
                    {draft,
                        #button{
                            id = post_dialog_publish_button,
                            text = ?TXT("Publish"),
                            actions = #event{type = click,
                                             actions = #disable{}},
                            postback = publish,
                            delegate = cms_compose
                        }
                    }
                ]
            },

            #button{
                id = post_dialog_save_button,
                text = ?TXT("Save"),
                actions = #event{type = click, actions = #disable{}},
                postback = save,
                delegate = cms_compose},

            #button{
                id = post_dialog_discard_button,
                text = ?TXT("Discard"),
                postback = discard,
                delegate = cms_compose},

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
    ?TXT("Compose").

%
% Element events
%

event({Module, Postback, LoadingId}) ->
    wf:wire(edit_post_body, LoadingId, #hide{}),
    wf:wire(#site_cast{cast = enable_forms, args = [edit_post_body]}),
    Module:event(Postback).

%
% Advanced edition options
%

set_post_id_failed() ->
    wf:wire(set_post_id_button, #enable{}),
    wf:update(set_post_id_status, ?TXT("Failed")).

set_post_id_success() ->
    wf:wire(set_post_id_button, #enable{}),
    wf:update(set_post_id_status, ?TXT("Done")).
