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
        body/1, event/1, event_invalid/1
    ]).

-include("include/utils.hrl").

body(Arg) -> content_account_view:body(Arg).

change_password() ->
    Username = wf:q(username),
    CurrentPassword = wf:q(current_password),
    NewPassword = wf:q(new_password),

    Hostname = config:domain(),

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
    ?LOG_INFO("~p", [Username]),
    ok.

change_private_email() ->
    Username = wf:q(username),
    Password = wf:q(password),
    NewEmail = wf:q(new_email),

    Hostname = config:domain(),

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

event(change_password) ->
    session:env(),
    change_password();
event(recover_password) ->
    session:env(),
    recover_password();
event(change_private_email) ->
    session:env(),
    change_private_email();
event(_Event) ->
    ?LOG_ERROR("Invalid event '~p'", [_Event]).

event_invalid(_E) ->
    content_account_view:on_validation_failed().
