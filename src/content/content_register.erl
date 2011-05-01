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

        hostname/0,

        url_path_encode/2,
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
% Text utils
%

-spec url_path_encode([string()], [{string(), string()}]) -> string().
url_path_encode(Path, Params) ->
    Buf1 = string:join([escape(P) || P <- Path], "/"),

    Buf2 = if
        Params == [] -> "";
        true         -> [$? | param_encode(Params)]
    end,

    case Buf1 ++ Buf2 of
        [] -> [];
        Result -> [$/ | Result]
    end.

-spec param_encode([{string() | atom(), string()}]) -> string().
param_encode(Params) ->
    string:join([escape(Key) ++ "=" ++ escape(Value)
            || {Key, Value} <- Params], "&").

-spec escape(atom() | string()) -> string().
escape(Atom) when is_atom(Atom) ->
    escape(atom_to_list(Atom));
escape([]) -> [];
escape([C | Cs]) when (C >= $a andalso C =< $z);
                      (C >= $A andalso C =< $Z);
                      (C >= $0 andalso C =< $9);
                      C == $.; C == $_; C == $-, C == $/ ->
    [C | escape(Cs)];
escape([C | Cs]) when C < 256 ->
    [$% | httpd_util:integer_to_hexlist(C)] ++ escape(Cs);
escape([C | Cs]) ->
    escape(binary_to_list(unicode:characters_to_binary([C])) ++ Cs).

%
% Config
%

hostname() ->
    config:content(?MODULE, hostname).

server_address() ->
    config:content(?MODULE, server).

server_port() ->
    config:content(?MODULE, port).

server_key() ->
    config:content(?MODULE, key).

%
% RPC
%

is_available(Username, Host) ->
    Key = server_key(),

    Path = url_path_encode(["api", "register", "is_registered"],
                           [{username, Username}, {host, Host}, {key, Key}]),

    case rest:request_get(hostname(), server_address(), server_port(), Path) of
        {error, _Error} ->
            ?LOG_ERROR("Error in username taken validator: ~p", [_Error]),
            session:env(),
            content_register_view:on_failed(error),
            true;
        {ok, Value} when is_boolean(Value) ->
            not Value
    end.

try_register(Username, Password, Host, Email) ->
    Key = list_to_binary(server_key()),
    Path = url_path_encode(["api", "register", "register"], []),
    JSON = {struct, [
            {key, Key},
            {username, Username},
            {host, Host},
            {password, Password}
            |
            case Email of
                "" -> [];
                _  -> [{email, Email}]
            end
        ]},
    case rest:request_post_json(hostname(), server_address(), server_port(),
                                Path, JSON) of
        {ok, Result} ->
            case Result of
                {struct, [{K, V}]} ->
                    case {K, V} of
                        {<<"error">>, _} ->
                            {error, list_to_atom(binary_to_list(V))};
                        _ ->
                            {error, unkown}
                    end;
                <<"ok">> ->
                    ok;
                <<"email_not_set">> ->
                    email_not_set;
                _ ->
                    {error, invalid}
            end;
        {error, Reason} ->
            {error, Reason}
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
    Hostname = list_to_binary(hostname()),

    case try_register(Username, Password, Hostname, Email) of
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

