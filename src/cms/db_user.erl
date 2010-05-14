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

-module(db_user).
-export([parse_doc/2, render_user/1, get_password_hash_for/1, save_user/2]).

-include("include/config.hrl").
-include("include/utils.hrl").
-include("include/cms/db.hrl").

%
% Parse
%

parse_doc({K, V}, U) ->
    case K of
        '_id' -> U#db_user{username = V};
        password_hash -> U#db_user{password_hash = V};
        full_name -> U#db_user{full_name = V};
        email -> U#db_user{email = V};
        jid -> U#db_user{jid = V};

        type -> U;
        '_rev' -> U;
        _ ->
            ?LOG_INFO("Parser: Unknown entry ~p", [K]),
            U
    end.

%
% Render
%

render_user(#db_user{
        username = Username,
        password_hash = PasswordHash,
        full_name = FullName,
        email = EMail,
        jid = Jid}) ->
    {
        Username,
        user,
        [
            {password_hash, PasswordHash},
            {full_name, FullName},
            {email, EMail},
            {jid, Jid}
        ]
    }.

%
% API
%

get_password_hash_for(User) ->
    Rows = db_utils:view_rows(db_controller:get_view("user_login", [{<<"key">>, list_to_binary(User)}])),
    UserBin = list_to_binary(User),
    case Rows of
        [{Id, _Key, PasswordHash}] when Id == UserBin ->
            {ok, binary_to_list(PasswordHash)};
        [] ->
            ?LOG_INFO("Unknown user '~s'", [User]),
            {error, not_found}
    end.
    
save_user(User, Db) ->
    db_controller:save_doc(fun render_user/1, User, Db).

