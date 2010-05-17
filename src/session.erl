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

-module(session).
-include_lib("nitrogen/include/wf.inc").
-export([event/1, login_panel/0]).

-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/ui.hrl").

%
% Constants
%

-define(LOGIN_DIALOG_ID, login_dialog).

%%
% Events
%%

event(login) ->
    wf:wire(#event{target = login_dialog, actions = #appear{}});

event(do_login) ->
    % authenticate
    Username = wf:q(login_username),
    Password = wf:q(login_password),
    case authenticate(Username, Password) of
        granted ->
            wf:user(Username),
            wf:wire(#state_panel_set{target = login_link, key = authenticated}),
            wf:wire(#state_panel_set{target = login_panel, animate = true, key = success});
        _ ->
            wf:wire(#state_panel_set{target = login_panel, animate = true, key = fail})
    end;

event(do_logout) ->
    wf:clear_session(),
    wf:wire(#state_panel_set{target = login_link, key = anonymous});

event(Event) ->
    ?LOG_WARNING("Unhandled event \"~p\".~n", [Event]).

%
% Panels
%

panel_login() ->
    [
        #h3{text = ?T(msg_id_login)},
        #p{},
        #label{text = ?T(msg_id_login_username)},
        #textbox{id = login_username, class = login_input, next = login_password},
        #p{},
        #label{text = ?T(msg_id_login_password)},
        #password{id = login_password, class = login_input, next = login_login},
        #p{},
        #panel{id = login_status, body = button_panel()}
    ].

panel_progress() ->
    [
        #p{class = center, body = #image{image = ?SPINNER_IMAGE, id = login_spinner}}
    ].

panel_success() ->
    [
        #h3{class = center, text = ?T(msg_id_login_success)},
        #panel{
            class = center,
            body = #button{
                text = ?T(msg_id_close),
                actions = #event{type = click, actions = #state_panel_hide{target = login_panel}}
            }
        }
    ].

panel_fail() ->
    [
        #h3{class = center, text = ?T(msg_id_login_fail)},
        #panel{
            class = center,
            body = #button{
                text = "Close",
                actions = #event{type = click, actions = #state_panel_hide{target = login_panel}}
            }
        }
    ].

button_panel() ->
    [
        #button{
            id = login_login,
            text = ?T(msg_id_login),
            actions = #event{type = click, actions = #state_panel_set{target = login_panel, validate_group = login_login, key = progress}},
            delegate = session,
            postback = do_login
        },
        " ",
        #button{
            id = login_cancel,
            text = ?T(msg_id_cancel),
            actions = #event{type = click, actions = #state_panel_hide{target = login_panel}}
        }
    ].

login_panel() ->
    SubBodies = [
        {login, panel_login()},
        {progress, panel_progress()},
        {success, panel_success()},
        {fail, panel_fail()}
    ],

    LoginLinkBodies = [
        {anonymous, 
            #link{
                class = login_link,
                text = ?T(msg_id_login),
                actions = #event{type = click, actions = #state_panel_show{target = login_panel, key = login}},
                delegate = session,
                postback = login}},
        {authenticated,
            #link{
                class = login_link,
                text = ?T(msg_id_logout),
                delegate = session,
                postback = do_logout}}
        ],
    Body = 
    [
        #ui_state_panel{
            id = login_panel,
            class = login_panel,
            bodies = SubBodies,
            init_state = login
        },
        #ui_state_panel{
            id = login_link,
            bodies = LoginLinkBodies,
            visible = true,
            init_state = ?EITHER(wf:user() == undefined, anonymous, authenticated)
        }
    ],

    % login detail validators
    wf:wire(login_login, login_username, #validate{validators = [
                #is_required{text = "*"}
            ]}),
    wf:wire(login_login, login_password, #validate {validators = [
                #is_required{text = "*"}
            ]}),
    ?UI(Body).

%
% Authentication
%

authenticate(Username, Password) ->
    case authenticate1(Username, Password) of
        granted ->
            granted;
        denied ->
            receive
            after 2000 ->
                    denied
            end
    end.

authenticate1(Username, Password) ->
    case db_user:get_password_hash_for(Username) of
        {ok, StoredHash} ->
            case sha2:hexdigest256(Password) of
                InputHash when InputHash == StoredHash ->
                    granted;
                _ ->
                    denied
            end;
        _ ->
            denied
    end.
