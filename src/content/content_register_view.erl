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

-module(content_register_view).
-export(
    [
        body/0,

        on_validation_failed/0,
        on_back/0,
        on_failed/1,
        on_exists/0,
        on_success/2,
        on_email_not_set/2
    ]).

-include("include/utils.hrl").
-include("include/ui.hrl").
-include("include/content.hrl").

body() ->
    {RegisterForm, Validators} = register_form(),

    Body = #panel{
        class = [register],
        body = [
            #h2{text = ?TXT("Register")},
            #panel{id = register_view, body = RegisterForm}
        ]},

    #content{
        body = Body,
        title = ?TXT("Register"),
        post_eval =
            fun() ->
                wire_validators(Validators)
            end}.

register_form() ->
    Body = [
        #panel{style = ?INLINE, body = [
            #form{controls = [
                #label{text = ?TXT("Username: "),
                       style = ?BLOCK},
                #textbox{id = username},
                #p{},
                #label{text = ?TXT("Password:"),
                       style = ?BLOCK},
                #password{id = password, class = textbox},
                #p{},
                #label{text = ?TXT("Confirm password:"),
                       style = ?BLOCK},
                #password{id = pwd_confirm, class = textbox},
                #p{},
                #panel{body = [
                        #label{text = ?TXT("E-mail ")},
                        #label{text = ?TXT("(optional, not published)"),
                               class = small},
                        #label{text = ": "}
                    ]},
                #textbox{id = email},
                #p{},
                #panel{body = [#reset{id = clear_button,
                                      text = ?TXT("Clear")},
                               #button{text = ?TXT("Register"),
                                       id = create_button,
                                       actions = #event{type = click,
                                                        actions = [
                                                            #show{target = register_spinner},
                                                            #disable{target = create_button},
                                                            #disable{target = clear_button}
                                                        ]},
                                       postback = create,
                                       handle_invalid = true,
                                       on_invalid = [
                                           #hide{target = register_spinner},
                                           #enable{target = create_button},
                                           #enable{target = clear_button}
                                       ],
                                       delegate = content_register},
                               #panel{class = inline_spinner_container,
                                      body = #image{image = ?SMALL_SPINNER_IMAGE,
                                                    alt = "spinner",
                                                    class = inline_spinner,
                                                    style = ?HIDDEN,
                                                    id = register_spinner}}]}
            ]}
        ]}
    ],

    Validators = [
        {username,    [#is_required{text = ?TXT("Required")},
                       #custom{function = fun is_available_validator/2,
                               server_side_only = true,
                               text = ?TXT("Username already taken")}]},
        {email,       [#maybe_email{text = ?TXT("Enter a valid email address")}]},
        {password,    [#is_required{text = ?TXT("Required")},
                       #min_length{length = 6,
                                   text = ?TXT("Password must be at least 6 "
                                               "characters long.")}]},
        {pwd_confirm, [#is_required{text = ?TXT("Required")},
                       #confirm_password{password = password,
                                         text = ?TXT("Passwords must match")}]}
    ],

    {Body, Validators}.

%
% Validator

is_available_validator(_A, Value) ->
    content_register:is_available(Value, cf_config:host()).

%
% Internal
%

wire_validators(Validators) ->
    state_handler:set_state(validators, []),
    [wf:wire(create_button, Input, 
            #validate{validators = InputValidators})
     || {Input, InputValidators} <- Validators].
    
set_body(Body) ->
    wf:update(register_view, Body).

enable_form() ->
    wf:wire(register_spinner, #hide{}),
    wf:wire(clear_button, #enable{}),
    wf:wire(create_button, #enable{}).

%
% API
%

on_failed(Reason) ->
    Message = case Reason of
        exists -> ?TXT("Username already taken");
        input  -> ?TXT("Invalid input");
        _      -> ?TXT("Internal error")
    end,

    Body = [
        #h3{text = ?TXT("Registration failed")},
        #p{body = Message},
        #button{text = ?TXT("Back"),
                delegate = content_register,
                postback = back}
    ],

    set_body(Body).

on_validation_failed() ->
    enable_form().

on_exists() ->
    wf:wire(username, #validation_error{text = ?TXT("Username already taken")}),
    enable_form().

on_back() ->
    {Body, Validators} = register_form(),
    wf:update(register_view, Body),
    wire_validators(Validators).

success_body(Username, Hostname) ->
    [
        #h3{text = ?TXT("Account registered")},
        #p{body = ?STXT("The account <span class=\"code\">$user$@$host$</span> "
                        "was successfully registered. You may start using it "
                        "right away.",
                        [{user, Username}, {host, Hostname}])}
    ].

on_success(Username, Hostname) ->
    set_body(success_body(Username, Hostname)).

on_email_not_set(Username, Hostname) ->
    Body = success_body(Username, Hostname) ++ [
        #span{class = warning, style = ?BLOCK,
              text = ?TXT("Note:")},
        #span{class = warning,
              text = ?TXT("You did not specify an E-mail address. This means "
                          "you cannot request a new password in case you lost "
                          "it. If you want to be able to, you need to register "
                          "it in the future. The E-mail you register will not "
                          "be public.")}
    ],
    set_body(Body).

