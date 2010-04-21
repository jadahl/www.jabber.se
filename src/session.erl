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
            action_state_panel:set(success, login_panel);
        _ ->
            ok
    end;
event(Event) ->
    ?LOG_WARNING("Unhandled event \"~p\".~n", [Event]).

%%
% Panels
%%

panel_login() ->
    [
        #h3{text = "Login"},
        #p{},
        #label{text = "Username"},
        #textbox{id = login_username, class = login_input, next = login_password},
        #p{},
        #label{text = "Password"},
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
        #h3{class = center, text = "Login Success"},
        #panel{
            class = center,
            body = #button{
                text = "Close",
                actions = #event{type = click, actions = #state_panel_hide{target = login_panel}}
            }
        }
    ].

panel_fail() ->
    [
        #h3{class = warning_title, text = "Login failed"}
    ].

button_panel() ->
    [
        #button{
            id = login_login,
            text = "Login",
            actions = #event{type = click, actions = #state_panel_set{target = login_panel, key = progress}},
            delegate = session,
            postback = do_login
        },
        " ",
        #button{
            id = login_cancel,
            text = "Cancel",
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
    Body = 
    [
        #ui_state_panel{
            id = login_panel,
            class = login_panel,
            bodies = SubBodies,
            init_state = login
        },
        #link{
            id = login_link,
            text = "Login",
            actions = #event{type = click, actions = #state_panel_show{target = login_panel, key = login}},
            delegate = session,
            postback = login
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
    io:format("login: ~p:~p~n", [Username, Password]),
    receive 
    after 2000 ->
            ok
    end,
    granted.
