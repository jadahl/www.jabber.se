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

-module(content_account).
-export(
    [
        default_config/0,
        body/1, event/1, event_invalid/1
    ]).

-include("include/utils.hrl").

-behaviours(gen_cf_config).

default_config() ->
    [{recover_base_path, "/account/recover"}].

body([_, "recover", "cancel", ID]) ->
    SessionID = list_to_binary(ID),
    case cf_recover_password:cancel(SessionID) of
        {ok, Username, Hostname} ->
            content_account_view:recover_session_canceled_body(Username,
                                                               Hostname);
        _ ->
            content_account_view:recover_session_unknown_body()
    end;
body([_, "recover", ID]) ->
    SessionID = list_to_binary(ID),
    case cf_recover_password:recover(SessionID) of
        {ok, Username, Hostname, SessionID} ->
            content_account_view:recover_session_found_body(Username,
                                                            Hostname,
                                                            SessionID);
        _ ->
            content_account_view:recover_session_unknown_body()
    end;
body(Arg) ->
    content_account_view:body(Arg).

change_password() ->
    Username = wf:q(username),
    CurrentPassword = wf:q(current_password),
    NewPassword = wf:q(new_password),

    Hostname = cf_config:domain(),

    case cf_mod_restful:change_password(Username,
                                        Hostname,
                                        CurrentPassword,
                                        NewPassword) of
        ok ->
            content_account_view:on_change_password_success(Username,
                                                            Hostname);
        {error, not_allowed} ->
            content_account_view:on_change_password_not_allowed();
        _R ->
            ?LOG_ERROR("error when changing password: ~p", [_R]),
            web_index:load_error(internal_error, trigger)
    end.

recover_password() ->
    Username = wf:q(username),

    Path = ?CONFM(recover_base_path),
    Lang = i18n:get_language(),
    Hostname = cf_config:domain(),

    case cf_mod_restful:get_private_email(Username, Hostname) of
        {error, _} ->
            content_account_view:on_recover_failed(Username, Hostname);
        Email ->
            case cf_recover_password:send_recovery_email(Username,
                                                         Hostname,
                                                         Email,
                                                         Path,
                                                         Lang) of
                ok ->
                    content_account_view:on_recover_mail_sent(Username,
                                                              Hostname);
                {error, already_sent} ->
                    content_account_view:on_recover_mail_already_sent(Username,
                                                                      Hostname);
                _ ->
                    content_account_view:on_recover_failed(Username,
                                                           Hostname)
            end
    end.

change_private_email() ->
    Username = wf:q(username),
    Password = wf:q(password),
    NewEmail = wf:q(new_email),

    Hostname = cf_config:domain(),

    case cf_mod_restful:change_private_email(Username,
                                             Hostname,
                                             Password,
                                             NewEmail) of
        ok ->
            content_account_view:on_change_private_email_success(Username,
                                                                 Hostname,
                                                                 NewEmail);
        {error, not_allowed} ->
            content_account_view:on_change_private_email_not_allowed();
        _R ->
            ?LOG_ERROR("error when changing private email: ~p", [_R]),
            web_index:load_error(internal_error, trigger)
    end.

recover_change_private_email() ->
    Password = wf:q(new_password),
    ID = list_to_binary(wf:q(recover_session_id)),
    case cf_recover_password:force_change_password(Password, ID) of
        {ok, Username, Hostname} ->
            content_account_view:on_recover_password_set(Username, Hostname);
        {error, Reason} ->
            ?LOG_ERROR("Setting recovery password failed: ~p", [Reason]),
            Error = case Reason of
                not_found -> not_found;
                _         -> internal
            end,
            content_account_view:on_recovery_set_password_failed(Error)
    end.

event(change_password) ->
    session:env(),
    change_password();
event(recover_password) ->
    session:env(),
    recover_password();
event(change_private_email) ->
    session:env(),
    change_private_email();
event(recover_change_password) ->
    session:env(),
    recover_change_private_email();
event(_Event) ->
    ?LOG_ERROR("Invalid event '~p'", [_Event]).

event_invalid(_E) ->
    content_account_view:on_validation_failed().
