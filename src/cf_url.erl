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
-export([url/1, url/2]).

url(Path) ->
    url((wf:request_bridge()):scheme(), Path).

url(Scheme, Path) ->
    Host = config:host(),
    Port = case Scheme of
        https -> config:read(https_port);
        http  -> config:read(http_port)
    end,

    lists:flatten(
        [
            case Scheme of
                http  -> "http://";
                https -> "https://"
            end,

            Host,

            case {Scheme, Port} of
                {http, 80}   -> "";
                {https, 443} -> "";
                {_, _}       -> [$: | integer_to_list(Port)]
            end,

            Path
        ]).

