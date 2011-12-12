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

-module(cf_session).
-export([authenticated/0, event/1, env/0, language/1, page_init/0, unauthorized_request/0]).

-include_lib("nitrogen_core/include/wf.hrl").

-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/ui.hrl").

%
% Constants
%

-define(LOGIN_DIALOG_ID, login_dialog).

%
% Query
%

query_params() ->
    (wf_context:request_bridge()):query_params().

query_lang() ->
    Query = query_params(),
    case lists:keysearch("lang", 1, Query) of
        {value, {_, Lang}} ->
            {lang, Lang};
        _ ->
            undefined
    end.

%
% Cookies
%

cookie_lang() ->
    case wf:cookie("lang") of
        [] ->
            wf:cookie("lang", cf_i18n:update_language()),
            cookie_init;
        Lang ->
            {lang, Lang}
    end.


%
% Environment
%

env_language() ->
    case cookie_lang() of
        {lang, Lang} ->
            cf_i18n:set_language(Lang);
        cookie_init ->
            case query_lang() of
                {lang, Lang} ->
                    language(Lang);
                _ ->
                    ok
            end
    end.

env() ->
    % update language
    env_language(),

    ok.

language(Lang) ->
    wf:cookie("lang", Lang),
    cf_i18n:set_language(Lang).

%
% Utils
%

authenticated() ->
    wf:user() /= undefined.

%
% Events
%

event(do_login) ->
    env(),

    case wf:user() of
        User when is_list(User) ->
            ?LOG_WARNING("Trying to login while already authenticated as '~s'", [User]);
        undefined ->
            login()
    end;

event(do_logout) ->
    env(),

    wf:clear_session(),
    cf_session_view:logged_out();

event(Event) ->
    ?LOG_WARNING("Unhandled event \"~p\".~n", [Event]).

%
% Hook
%

page_init() ->
    User = wf:user(),
    cf_session_view:page_init(User).

%
% Login
%

login() ->
    % authenticate
    Username = wf:q(login_username),
    Password = wf:q(login_password),
    case authenticate(Username, Password) of
        granted ->
            % Set session user value
            wf:user(Username),

            % Update view
            cf_session_view:logged_in();
        _ ->
            cf_session_view:login_failed()
    end.



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

%
% Timeout
%

unauthorized_request() ->
    cf_session_view:unauthorized_request().

