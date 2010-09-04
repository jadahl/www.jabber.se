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

-module(session_view).
-export([login_dialog/0, login_panel/0, admin_panel/0]).

-include_lib("nitrogen/include/wf.hrl").
-include("include/utils.hrl").
-include("include/ui.hrl").

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
                actions = #event{type = click, actions = #state_panel_hide{target = login_dialog}}
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
                actions = #event{type = click, actions = #state_panel_hide{target = login_dialog}}
            }
        }
    ].

button_panel() ->
    [
        #button{
            id = login_login,
            text = ?T(msg_id_login),
            actions = #event{type = click, actions = #state_panel_set{target = login_dialog, validate_group = login_login, key = progress}},
            delegate = session,
            postback = do_login
        },
        " ",
        #button{
            id = login_cancel,
            text = ?T(msg_id_cancel),
            actions = #event{type = click, actions = #state_panel_hide{target = login_dialog}}
        }
    ].

login_dialog() ->
    % login detail validators
    wf:wire(login_login, login_username, #validate{validators = [
                #is_required{text = "*"}
            ]}),
    wf:wire(login_login, login_password, #validate {validators = [
                #is_required{text = "*"}
            ]}),

    #state_panel{
        id = login_dialog,
        class = login_panel,
        bodies = [
            {login, panel_login()},
            {progress, panel_progress()},
            {success, panel_success()},
            {fail, panel_fail()}
        ],
        init_state = login
    }.

%
% Login link
%

login_link() ->
    #state_panel{
        id = login_link,
        bodies = [
            {anonymous, 
                #link{
                    class = login_link,
                    text = ?T(msg_id_login),
                    actions = #event{type = click, actions = #state_panel_show{target = login_dialog, key = login}},
                    delegate = session,
                    postback = login}},
            {authenticated,
                #link{
                    class = login_link,
                    text = ?T(msg_id_logout),
                    delegate = session,
                    postback = do_logout}
            }
        ],
        visible = true,
        init_state = ?EITHER(wf:user() == undefined, anonymous, authenticated)
    }.

%
% Login panel
%

login_panel() ->
    [
        login_dialog(),

        login_link()
    ].

%
% Admin panel
%

admin_panel() ->
    #panel{
        id = admin_panel,
        style = ?HIDDEN,
        body = [
            #link{
                class = admin_panel_button,
                text = ?T(msg_id_admin_content),
                delegate = cms_admin,
                postback = admin
            }

            %" | ",

            %#link{
            %    class = admin_panel_button,
            %    text = ?T(msg_id_admin_account)}
        ]
    }.

