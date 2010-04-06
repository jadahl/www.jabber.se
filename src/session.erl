-module(session).
-include_lib("nitrogen/include/wf.inc").
-export([event/1, login_panel/0]).

-include("include/config.hrl").
-include("include/utils.hrl").


%%
% Events
%%

event(login) ->
    io:format("login~n"),
    %wf:wire(#event{target = login_username, type = init, actions = #jquery_effect{type = 'focus'}}),

    % update content
    %LoginDialog = wf:render(login_dialog()),
    %wf:update(login_dialog, LoginDialog),
    wf:wire(#event{type = init, target = login_dialog, actions = #appear{}});

event(do_login) ->
    % update to progress animation
    LoginAnimation = #image{image = ?SPINNER_IMAGE},
    wf:update(login_status, LoginAnimation),

    % authenticate
    [Username] = wf:q(login_username),
    [Password] = wf:q(login_password),
    case authenticate(Username, Password) of
        granted ->
            % hide dialog
            CloseButton = #button{
                text = "Close",
                actions = #event{type = click, target = login_dialog, actions=#fade{}}},
            wf:update(login_status, #panel{body = [#span{text = "Login Successful"}, CloseButton]});

        _ ->
            % reset buttons
            wf:update(login_status, button_panel()),

            % show failed
            wf:update(login_title, login_title(failed))
    end.

%%
% Interface
%%

%%
% Panels
%%

login_title() ->
    #h3{text = "Login"}.
login_title(warning) ->
    #h3{class = warning_title, text = "Login failed"};
login_title(_) ->
    #h3{text = "undefined"}.

login_dialog() ->
    utils:dialog("login_dialog",
        [
            login_title(),
            #p{},
            #label{text = "Username"},
            #textbox{id = login_username, next = login_password},
            #p{},
            #label{text = "Password"},
            #password{id = login_password, next = login_login},
            #p{},
            #panel{id = login_status, body = button_panel()}
        ]).

button_panel() ->
    [
        #button{
            id = login_login,
            text = "Login",
            postback = {session, do_login}},
        " ",
        #button{
            id = login_cancel,
            text = "Cancel",
            postback = {session, cancel_login}}
    ].


login_panel() ->
    Body = 
    [
        login_dialog(),
        #link{
            id = login_link,
            text = "Login",
            postback = {session, login}}
    ],

    % login detail validators
    wf:wire(login_login, login_username, #validate{validators = [
                #is_required{text = "*"}
            ]}),
    wf:wire(login_login, login_password, #validate {validators = [
                #is_required{text = "*"}
            ]}),
    Body.

%%
% Authentication
%%

authenticate(Username, Password) ->
    io:format("login: ~p:~p~n", [Username, Password]),
    receive 
    after 1000 ->
            ok
    end,
    granted.
