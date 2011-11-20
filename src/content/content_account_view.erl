%
%    Jabber.se Web Application
%    Copyright (C) 2011 Jonas Ådahl
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

-module(content_account_view).
-export(
    [
        body/1,

        change_password/0,
        recover_password/0,
        change_private_email/0,

        on_validation_failed/0,
        on_change_password_success/2,
        on_change_password_not_allowed/0,

        on_change_private_email_success/3,
        on_change_private_email_not_allowed/0
    ]).

-include("include/content.hrl").
-include("include/utils.hrl").
-include("include/ui.hrl").

-include_lib("nitrogen_core/include/wf.hrl").

-define(FUNCTIONS,
    [{change_password, ?T(msg_id_account_change_password)},
     {recover_password, ?T(msg_id_account_recover_password)},
     {change_private_email, ?T(msg_id_account_change_private_email)}]).

on_validation_failed() ->
    enable_form().

update_account_body(Body) ->
    wf:update(account_body, Body).

on_change_password_success(Username, Hostname) ->
    NewContent = [
        #h3{text = ?T(msg_id_account_password_changed)},
        #p{body = [
                ?T(msg_id_account_password_changed_msg_part1),
                #span{class = code,
                      text = lists:flatten([Username, <<"@">>, Hostname])},
                ?T(msg_id_account_password_changed_msg_part2)
            ]}
    ],
    update_account_body(NewContent).

on_change_password_not_allowed() ->
    NewContent = [
        #h3{text = ?T(msg_id_account_password_change_failed)},
        #p{body = ?T(msg_id_account_password_change_failed_msg)}
    ],
    update_account_body(NewContent).

on_change_private_email_success(Username, Hostname, Email) ->
    NewContent = [
        #h3{text = ?T(msg_id_account_email_changed)},
        #p{body = [
                ?T(msg_id_account_email_changed_msg_part1),
                #span{class = code,
                      text = lists:flatten([Username, <<"@">>, Hostname])},
                ?T(msg_id_account_email_changed_msg_part2),
                #span{class = code, text = Email},
                ?T(msg_id_account_email_changed_msg_part3)
            ]}
    ],
    update_account_body(NewContent).

on_change_private_email_not_allowed() ->
    NewContent = [
        #h3{text = ?T(msg_id_account_email_change_failed)},
        #p{body = ?T(msg_id_account_email_change_failed_msg)}
    ],
    update_account_body(NewContent).

actions(Path, Function) ->
    FullPath = utils:join(Path ++ [atom_to_list(Function)], $/),

    #event{type = click,
           actions = [#show{target = menu_spinner},
                      #js_call{fname = "$Site.$trigger_menu",
                               args = [list_to_binary(FullPath)]}]}.

menu(Path) ->
    #list{class = account_menu,
          body = [#listitem{body = #link{
                            text = Text,
                            actions = actions(Path, Function)}} ||
                  {Function, Text} <- ?FUNCTIONS]}.

wire_validators(Validators) ->
    state_handler:set_state(validators, []),
    [wf:wire(submit_button, Input,
            #validate{validators = InputValidators})
     || {Input, InputValidators} <- Validators].

submit_button(Postback) ->
    #button{text = ?T(msg_id_submit),
            id = submit_button,
            delegate = content_account,
            postback = Postback,
            handle_invalid = true,
            on_invalid = [#hide{target = account_spinner},
                          #enable{target = submit_button},
                          #enable{target = clear_button}],
            actions = #event{type = click,
                             actions = [#show{target = account_spinner},
                                        #disable{target = clear_button},
                                        #disable{target = submit_button}]}}.

button_panel(Postback) ->
    #panel{body = [#reset{id = clear_button,
                          text = ?T(msg_id_clear)},
                   submit_button(Postback),
                   #panel{class = inline_spinner_container,
                          body = [#image{image = ?SMALL_SPINNER_IMAGE,
                                         alt = "spinner",
                                         class = inline_spinner,
                                         style = ?HIDDEN,
                                         id = account_spinner}]}]}.

change_password() ->
    ?LOG_INFO("change password", []),
    Body = #panel{style = ?INLINE,
           body = [
               #form{controls = [
                       #label{text = ?T(msg_id_form_username),
                              style = ?BLOCK},
                       #textbox{id = username},
                       #p{},
                       #label{text = ?T(msg_id_form_current_password),
                              style = ?BLOCK},
                       #password{id = current_password},
                       #p{},
                       #label{text = ?T(msg_id_form_new_password),
                              style = ?BLOCK},
                       #password{id = new_password},
                       #p{},
                       #label{text = ?T(msg_id_form_confirm_password),
                              style = ?BLOCK},
                       #password{id = confirm_password},
                       #p{},
                       button_panel(change_password)]}]},

    Validators = [
        {username,         [#is_required{text = ?T(msg_id_form_required)}]},
        {current_password, [#is_required{text = ?T(msg_id_form_required)}]},
        {new_password,     [#is_required{text = ?T(msg_id_form_required)},
                            #min_length{length = 6,
                                        text = ?T(msg_id_form_at_least)}]},
        {confirm_password, [#is_required{text = ?T(msg_id_form_required)},
                            #confirm_password{password = new_password,
                                              text = ?T(msg_id_form_match)}]}],

    {Body, Validators}.

recover_password() ->
    Body = #panel{style = ?INLINE,
           body = [
               #form{controls = [
                       #label{text = ?T(msg_id_form_username),
                              style = ?BLOCK},
                       #textbox{id = username},
                       #p{},
                       button_panel(recover_password)]}]},

    Validators = [{username, [#is_required{text = ?T(msg_id_form_required)}]}],

    {Body, Validators}.

change_private_email() ->
    Body = #panel{style = ?INLINE,
           body = [
               #form{controls = [
                       #label{text = ?T(msg_id_form_username),
                              style = ?BLOCK},
                       #textbox{id = username},
                       #p{},
                       #label{text = ?T(msg_id_form_password),
                              style = ?BLOCK},
                       #password{id = password},
                       #p{},
                       #label{text = ?T(msg_id_form_new_email),
                              style = ?BLOCK},
                       #textbox{id = new_email},
                       #p{},
                       button_panel(change_private_email)]}]},

    Validators = [
        {username,  [#is_required{text = ?T(msg_id_form_required)}]},
        {password,  [#is_required{text = ?T(msg_id_form_required)}]},
        {new_email, [#is_required{text = ?T(msg_id_form_required)},
                #is_email{text = ?T(msg_id_form_valid_email)}]}],

    {Body, Validators}.

enable_form() ->
    wf:wire(account_spinner, #hide{}),
    wf:wire(submit_button, #enable{}),
    wf:wire(clear_button, #enable{}).

body([_] = Path) ->
    #content{body = [#h2{text = ?T(msg_id_account)},
                     menu(Path)],
             title = ?T(msg_id_account)};

body([_, SubPath]) ->
    case proplists:lookup(list_to_atom(SubPath), ?FUNCTIONS) of
        {Function, Title} ->
            {Body, Validators} = ?MODULE:Function(),
            #content{body = [#h2{text = Title},
                             #panel{id = account_body,
                                    body = Body}],
                     title = Title,
                     post_eval = fun() -> wire_validators(Validators) end};
        _ ->
            web_index:content_error(invalid_account_funtion)
    end;
body(_) ->
    web_index:content_error(invalid_account_funtion).

