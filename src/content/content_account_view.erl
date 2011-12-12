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
        on_recover_password_set/2,

        on_change_private_email_success/3,
        on_change_private_email_not_allowed/0,

        on_recover_mail_sent/2,
        on_recover_mail_already_sent/2,
        on_recover_failed/2,
        on_recovery_set_password_failed/1,

        recover_session_found_body/3,
        recover_session_canceled_body/2,
        recover_session_unknown_body/0
    ]).

-include("include/content.hrl").
-include("include/utils.hrl").
-include("include/ui.hrl").

-include_lib("nitrogen_core/include/wf.hrl").

-define(MIN_PASSWOR_LENGTH, 6).

-define(FUNCTIONS,
    [{change_password, ?TXT("Change password")},
     {recover_password, ?TXT("Lost password")},
     {change_private_email, ?TXT("Change private E-mail")}]).

on_validation_failed() ->
    enable_form().

update_account_body(Body) ->
    wf:update(account_body, Body).

on_change_password_success(Username, Hostname) ->
    NewContent = [
        #h3{text = ?TXT("Password changed")},
        #p{body = ?STXT("A new password for the account "
                        "<span class=\"code\">$user$@$host$</span> "
                        "has been set.",
                        [{user, Username}, {host, Hostname}])}
    ],
    update_account_body(NewContent).

on_change_password_not_allowed() ->
    NewContent = [
        #h3{text = ?TXT("Failed")},
        #p{body = ?TXT("Either the username, the password or both were "
                       "incorrect.")}
    ],
    update_account_body(NewContent).

on_recover_password_set(Username, Hostname) ->
    NewContent = [
        #h3{text = ?TXT("Password set")},
        #p{body = ?STXT("A new password for the account "
                        "<span class=\"code\">$user$@$host$</span> "
                        "has been set.",
                        [{user, Username}, {host, Hostname}])}
    ],
    update_account_body(NewContent).

on_change_private_email_success(Username, Hostname, Email) ->
    NewContent = [
        #h3{text = "Changed"},
        #p{body = ?STXT("E-mail address of the account "
                        "<span class=\"code\">$user$@$host$</span> "
                        "has been changed to "
                        "<span class=\"code\">$email$</span>.",
                        [{user, Username}, {host, Hostname}, {email, Email}])}
    ],
    update_account_body(NewContent).

on_change_private_email_not_allowed() ->
    NewContent = [
        #h3{text = ?TXT("Failed")},
        #p{body = ?TXT("Either the username, the password or both were "
                       "incorrect.")}
    ],
    update_account_body(NewContent).

on_recover_mail_sent(Username, Hostname) ->
    NewContent = [
        #h3{text = ?TXT("E-mail sent")},
        #p{body = ?STXT("An E-mail containing instrucions on how to complete "
                        "the recovery proceduce has been sent to the "
                        "address registered to the account "
                        "<span class=\"code\">$user$@$host$</span>. "
                        "Please check your inbox. It may take some time "
                        "before the E-mail arrives. If after some time you "
                        "still have not received any E-mail, first check your "
                        "Spam directory. If did receive any recovery E-mail, "
                        "please contact the administrator for help.",
                        [{user, Username}, {host, Hostname}])}
    ],
    update_account_body(NewContent).

on_recover_mail_already_sent(Username, Hostname) ->
    NewContent = [
        #h3{text = ?TXT("Recovery E-mail already sent")},
        #p{body = ?STXT("Recovery instructions has already been sent to "
                        "the registered E-mail address of the account "
                        "<span class=\"code\">$user$@$host$</span>. Please "
                        "check your inbox and spam folder. If you have further "
                        "issues, please contact the administrator.",
                        [{user, Username}, {host, Hostname}])}
    ],
    update_account_body(NewContent).


on_recover_failed(Username, Hostname) ->
    NewContent = [
        #h3{text = ?TXT("Failed to recover account")},
        #p{body = ?STXT("Failed to recover the account "
                        "<span class=\"code\">$user$@$host$</span>. "
                        "Please contact the administrator for help.",
                        [{user, Username}, {host, Hostname}])}
    ],
    update_account_body(NewContent).

on_recovery_set_password_failed(_Error) ->
    NewContent = [
        #h3{text = ?TXT("Failed")},
        #p{body = ?TXT("Failed to set new password. Contact the "
                       "admistrator for help.")}
    ],

    update_account_body(NewContent).

actions(Path, Function) ->
    FullPath = cf_utils:join(Path ++ [atom_to_list(Function)], $/),

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
    #button{text = ?TXT("Submit"),
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
                          text = ?TXT("Clear")},
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
                       #label{text = ?TXT("Username:"),
                              style = ?BLOCK},
                       #textbox{id = username},
                       #p{},
                       #label{text = ?TXT("Current password:"),
                              style = ?BLOCK},
                       #password{id = current_password},
                       #p{},
                       #label{text = ?TXT("New password:"),
                              style = ?BLOCK},
                       #password{id = new_password},
                       #p{},
                       #label{text = ?TXT("Confirm password:"),
                              style = ?BLOCK},
                       #password{id = confirm_password},
                       #p{},
                       button_panel(change_password)]}]},

    Validators = [
        {username,         [#is_required{text = ?TXT("Required")}]},
        {current_password, [#is_required{text = ?TXT("Required")}]},
        {new_password,     [#is_required{text = ?TXT("Required")},
                            #min_length{length = ?MIN_PASSWOR_LENGTH,
                                        text = ?STXT("Password must be at "
                                                     "least $min$ characters "
                                                     "long.",
                                                     [{min, ?MIN_PASSWOR_LENGTH}])}]},
        {confirm_password, [#is_required{text = ?TXT("Required")},
                            #confirm_password{password = new_password,
                                              text = ?TXT("Passwords must "
                                                          "match")}]}],

    {Body, Validators}.

recover_password() ->
    Body = #panel{style = ?INLINE,
           body = [
               #form{controls = [
                       #label{text = ?TXT("Username:"),
                              style = ?BLOCK},
                       #textbox{id = username},
                       #p{},
                       button_panel(recover_password)]}]},

    Validators = [{username, [#is_required{text = ?TXT("Required")}]}],

    {Body, Validators}.

change_private_email() ->
    Body = #panel{style = ?INLINE,
           body = [
               #form{controls = [
                       #label{text = ?TXT("Username:"),
                              style = ?BLOCK},
                       #textbox{id = username},
                       #p{},
                       #label{text = ?TXT("Password:"),
                              style = ?BLOCK},
                       #password{id = password},
                       #p{},
                       #label{text = ?TXT("New E-mail:"),
                              style = ?BLOCK},
                       #textbox{id = new_email},
                       #p{},
                       button_panel(change_private_email)]}]},

    Validators = [
        {username,  [#is_required{text = ?TXT("Required")}]},
        {password,  [#is_required{text = ?TXT("Required")}]},
        {new_email, [#is_required{text = ?TXT("Required")},
                     #is_email{text = ?TXT("Enter a valid E-mail "
                                           "address.")}]}],

    {Body, Validators}.

enable_form() ->
    wf:wire(account_spinner, #hide{}),
    wf:wire(submit_button, #enable{}),
    wf:wire(clear_button, #enable{}).

content(Title, Body) ->
    #content{body = [#h2{text = Title},
                     #panel{id = account_body,
                            body = Body}],
             title = Title}.

body([_] = Path) ->
    #content{body = [#h2{text = ?TXT("Account")},
                     menu(Path)],
             title = ?TXT("Account")};

body([_, SubPath | _]) ->
    case proplists:lookup(list_to_atom(SubPath), ?FUNCTIONS) of
        {Function, Title} ->
            {Body, Validators} = ?MODULE:Function(),

            Content = content(Title, Body),
            Content#content{
                     title = Title,
                     post_eval = fun() ->
                                     wire_validators(Validators),
                                     enable_form()
                                 end};
        _ ->
            web_index:content_error(invalid_account_function)
    end;
body(_) ->
    web_index:content_error(invalid_account_function).

recover_session_found_body(Username, Hostname, SessionID) ->
    Body = #panel{style = ?INLINE,
           body = [
               #label{text = ?TXT("Account:"),
                      style = ?BLOCK},
               #span{class = code,
                     text = lists:flatten([Username, <<"@">>, Hostname])},
               #p{},
               #form{controls = [
                       #hidden{id = recover_session_id,
                               text = SessionID},
                       #label{text = ?TXT("New password:"),
                              style = ?BLOCK},
                       #password{id = new_password},
                       #p{},
                       #label{text = ?TXT("Confirm password:"),
                              style = ?BLOCK},
                       #password{id = confirm_password},
                       #p{},
                       button_panel(recover_change_password)]}]},

    Validators = [
        {new_password, [#is_required{text = ?TXT("Required")},
                        #min_length{length = ?MIN_PASSWOR_LENGTH,
                                    text = ?STXT("Password must be at least "
                                                 "$min$ characters long.",
                                                 [{min, ?MIN_PASSWOR_LENGTH}])}]},
        {confirm_password, [#is_required{text = ?TXT("Required")},
                            #confirm_password{password = new_password,
                                              text = ?TXT("Passwords must "
                                                          "match")}]}],
    Title = ?TXT("Account recovery"),
    Content = content(Title, Body),
    Content#content{title = Title,
                    post_eval = fun() -> wire_validators(Validators) end}.

recover_session_canceled_body(Username, Hostname) ->
    content(?TXT("Recovery canceled"),
            ?STXT("The recovery of the account "
                  "<span class=\"code\">$user$@$host$</span> has been "
                  "canceled.",
                  [{user, Username}, {host, Hostname}])).

recover_session_unknown_body() ->
    content(?TXT("Recovery failed"),
            ?TXT("The account recovery session has timed out.")).

