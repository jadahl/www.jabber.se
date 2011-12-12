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

-module(cf_url).
-export([path/0, scheme/0, url/1, url/2, content_path_to_module/1,
         content_module_path/1,
         url_path_encode/2]).

path() ->
    [$/ | Path] = (wf_context:request_bridge()):path(),
    Path.

scheme() ->
    case cf_config:read(https_tunneled) of
        true ->
            SSLPort = cf_config:read(https_port),
            case (wf_context:request_bridge()):port() of
                SSLPort -> https;
                _       -> http
            end;
        _ ->
            (wf_context:request_bridge()):scheme()
    end.

%
% Same as url/2 where Scheme is what scheme was used to the server.
%
url(Path) ->
    url(scheme(), Path).

%
% Render an URL given a scheme and a path. The URL will point at the server
% where the function is called from.
%
url(Scheme, Path) ->
    Host = cf_config:host(),
    Port = case Scheme of
        https -> cf_config:read(https_port);
        http  -> cf_config:read(http_port)
    end,

    lists:flatten(
        [
            % scheme, such as https://
            case Scheme of
                http  -> "http://";
                https -> "https://"
            end,

            % hostname, such as localhost
            Host,

            % port, if not hidden, such as :8000
            case cf_config:read(http_https_port_forward) of
                true -> "";
                _    -> [$: | integer_to_list(Port)]
            end,

            % path, such as /foo
            Path
        ]).

content_path_to_module(List) when is_list(List) ->
    list_to_atom("content_" ++ List).

content_module_path(Atom) ->
    case atom_to_list(Atom) of
        "content_" ++ Module -> Module;
        _                    -> undefined
    end.

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
escape(Binary) when is_binary(Binary) ->
    escape(binary_to_list(Binary));
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

