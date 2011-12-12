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

-module(cf_mod_restful).
-export(
    [
        is_registered/2,
        register/4,
        change_password/4,
        force_change_password/3,
        change_private_email/4,
        get_private_email/2
    ]).

-include("include/utils.hrl").


%
% Config
%

server_address() -> cf_config:content(?MODULE, server).
server_port() ->    cf_config:content(?MODULE, port).
path() ->           cf_config:content(?MODULE, path).
key() ->            cf_config:content(?MODULE, key).


%
% Utils
%

get_request(Function, Hostname, SubPath, Properties) ->
    Key = list_to_binary(key()),
    Path =
        cf_url:url_path_encode(path() ++ SubPath ++ [atom_to_list(Function)],
                               [{key, Key} | Properties]),

    case rest:request_get(Hostname, server_address(), server_port(), Path) of
        {ok, Value}    -> Value;
        {error, Error} -> {error, Error};
        Error          -> {error, Error}
    end.

post_request(Function, Hostname, SubPath, Properties) ->
    Key = list_to_binary(key()),
    Path =
        cf_url:url_path_encode(path() ++ SubPath ++ [atom_to_list(Function)],
                               []),
    JSON = {struct, [{key, Key} | Properties]},

    case rest:request_post_json(Hostname, server_address(), server_port(),
                                Path, JSON) of
        {ok, Result} ->
            ?LOG_INFO("result = ~p", [Result]),
            case Result of
                {struct, [{K, V}]} when is_binary(K), is_binary(V) ->
                    {list_to_atom(binary_to_list(K)),
                     list_to_atom(binary_to_list(V))};
                Binary when is_binary(Binary) ->
                    list_to_atom(binary_to_list(Binary));
                _ ->
                    {error, invalid}
            end;
        {error, Reason} ->
            ?LOG_INFO("reason = ~p", [Reason]),
            {error, Reason}
    end.

%
% API
%

is_registered(Username, Hostname) ->
    case get_request(is_registered,
                     Hostname,
                     ["register"],
                     [{username, Username},
                      {host, Hostname}]) of
        Value when is_boolean(Value) -> Value;
        {error, Error}               -> {error, Error};
        Error                        -> {error, Error}
    end.

register(Username, Hostname, Password, Email) ->
    case post_request(register,
                      Hostname,
                      ["register"],
                      [{username, Username},
                       {host, Hostname},
                       {password, Password}
                       | case Email of
                             ""        -> [];
                             undefined -> [];
                             _         -> [{email, Email}]
                         end]) of
        ok                       -> ok;
        {private_email, not_set} -> email_not_set;
        exists                   -> exists;
        {error, Error}           -> {error, Error};
        Error                    -> {error, Error}
    end.

change_password(Username, Hostname, OldPassword, NewPassword) ->
    case post_request(change_password,
                      Hostname,
                      ["register"],
                      [{username, Username},
                       {host, Hostname},
                       {old_password, OldPassword},
                       {new_password, NewPassword}]) of
        ok             -> ok;
        {error, Error} -> {error, Error}
    end.

force_change_password(Username, Hostname, NewPassword) ->
    case post_request(force_change_password,
                      Hostname,
                      ["register"],
                      [{username, Username},
                       {host, Hostname},
                       {new_password, NewPassword}]) of
        ok             -> ok;
        {error, Error} -> {error, Error}
    end.

change_private_email(Username, Hostname, Password, NewEmail) ->
    case post_request(change,
                      Hostname,
                      ["private_email"],
                      [{username, Username},
                       {host, Hostname},
                       {password, Password},
                       {new_email, NewEmail}]) of
        ok                 -> ok;
        {error, _} = Error -> Error
    end.

get_private_email(Username, Hostname) ->
    case get_request(get,
                     Hostname,
                     ["private_email"],
                     [{username, Username},
                      {host, Hostname}]) of
        Email when is_binary(Email) ->
            case validator_is_email:validate(tmp_group, Email) of
                true -> Email;
                _    -> {error, invalid}
            end;
        {error, _} = Error ->
            Error;
        _ ->
            {error, error}
    end.
