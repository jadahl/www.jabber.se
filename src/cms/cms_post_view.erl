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

-module(cms_post_view).
-export([tag/1, body/1, wire_validators/0, title/0]).

-include("include/utils.hrl").
-include("include/ui.hrl").
-include("include/db/db.hrl").

tag(TagName) ->
    Id = wf_render_elements:temp_id(),
    {
        #link{
            id = Id,
            class = post_dialog_tag,
            postback = {remove_tag, Id, TagName},
            delegate = cms_post,
            actions = #event{type = click, actions = #js_call{fname = "$Site.current_post.remove_tag", args = [TagName]}},
            text = TagName},
        Id
    }.

body(#db_post{
        title = Title,
        tags = Tags,
        body = Body
    }) ->
    {TagElements, TagIds} = lists:unzip(lists:map(fun tag/1, Tags)),

    case TagIds of
        [] ->
            ok;
        _ ->
            wf:wire(wf:f("$Site.current_post.set_tags(~s);", [couchbeam_mochijson2:encode({lists:zip(Tags, lists:map(fun list_to_binary/1, TagIds))})]))
    end,

    EnableSaveButtonEvent = #event{type = textchange, actions = [#enable{target = post_dialog_save_button}, #update{target = post_dialog_saved_message, elements = ""}]},

    #panel{
        id = edit_post_body,
        body = [
            % subject
            #label{text = ?T(msg_id_post_dialog_subject), style = ?BLOCK},
            #textbox{id = post_dialog_subject_input, text = Title, style = ?BLOCK, actions = EnableSaveButtonEvent},

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

            % post | cancel
            #button{
                id = post_dialog_post_button,
                text = ?T(msg_id_post_dialog_post),
                postback = post,
                delegate = cms_post},

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

            #button{
                id = post_dialog_cancel_button,
                text = ?T(msg_id_cancel),
                actions = #event{type = click, actions = cms_admin_view:back_action()}},

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

%show(Post) ->
%    cms_admin_view:set_body(?T(msg_id_post_dialog_title), body(Post)),
%    wire_validators().
