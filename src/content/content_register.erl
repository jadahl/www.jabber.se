%
%    Jabber.se Web Application
%    Copyright (C) 2010-2011 Jonas Ådahl
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

-module(content_register).
-export(
    [
        body/1, event/1, event_invalid/1,

        is_available/2
    ]).

-include_lib("nitrogen_core/include/wf.hrl").

-include("include/utils.hrl").

-define(REQUEST_TIMEOUT, 5000).

%
% Content
%

body(_) ->
    content_register_view:body().

%
% RPC
%

is_available(Username, Host) ->
    case cf_mod_restful:is_registered(Username, Host) of
        Result when is_boolean(Result) ->
            not Result;
        _Error ->
            ?LOG_ERROR("Error in username taken validator: ~p", [_Error]),
            session:env(),
            content_register_view:on_failed(error),
            false
    end.

%
% Events
%

event(create) ->
    session:env(),

    [Username, Password] = [
        list_to_binary(wf:q(Param)) || Param <- [username, password]],
    Email = case wf:q(email) of
        undefined -> undefined;
        List -> list_to_binary(List)
    end,
    Hostname = list_to_binary(cf_config:domain()),

    case cf_mod_restful:register(Username, Hostname, Password, Email) of
        exists ->
            content_register_view:on_exists();
        ok ->
            content_register_view:on_success(Username, Hostname);
        email_not_set ->
            content_register_view:on_email_not_set(Username, Hostname);
        {error, Reason} ->
            ?LOG_ERROR("Failed to register ~p: ~p", [Username, Reason]),
            content_register_view:on_failed(Reason)
    end;
event(back) ->
    session:env(),
    content_register_view:on_back().

event_invalid(create) ->
    session:env(),
    content_register_view:on_validation_failed().

