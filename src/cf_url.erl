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
-export([path/0, url/1, url/2, content_path_to_module/1]).

path() ->
    [$/ | Path] = (wf_context:request_bridge()):path(),
    Path.

%
% Same as url/2 where Scheme is what scheme was used to the server.
%
url(Path) ->
    url((wf:request_bridge()):scheme(), Path).

%
% Render an URL given a scheme and a path. The URL will point at the server
% where the function is called from.
%
url(Scheme, Path) ->
    Host = config:host(),
    Port = case Scheme of
        https -> config:read(https_port);
        http  -> config:read(http_port)
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
            case config:read(http_https_port_forward) of
                true -> "";
                _    -> [$: | integer_to_list(Port)]
            end,

            % path, such as /foo
            Path
        ]).

content_path_to_module(List) when is_list(List) ->
    list_to_atom("content_" ++ List).
