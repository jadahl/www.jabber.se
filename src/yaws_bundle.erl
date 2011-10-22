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

-module(yaws_bundle).
-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-export([start/0, stop/0]).

-define(SITE_APP, www_jabber_se_app).

start() ->
	% Set up Yaws Configuration...
	{ok, App} = application:get_application(),
	Id = atom_to_list(App),

    Base = [{allowed_scripts, []},
            {appmods, [{"/", ?SITE_APP}]}],

    GL = [{logdir, "./logs"},
          {cache_refresh_secs, 5}],

    HTTPPort = config:read(http_port),
    HTTPSPort = config:read(https_port),

    Normal = [{port, HTTPPort} | Base],

    SSL = case config:read(https_tunneled) of
            true ->
                [];
            _ ->
                [{ssl, #ssl{keyfile = "server.key",
                            certfile = "server.crt"}}]
        end ++ [{port, HTTPSPort} | Base],

    SL = [[{listen, {0, 0, 0, 0, 0, 0, 0, 0}} | Normal],
          [{listen, {0, 0, 0, 0, 0, 0, 0, 0}} | SSL]],

    % Default on Linux and other OSs is to bind IPv6 ports to IPv4 as well,
    % so might not need the following IPv4 configurations.
    % [{listen, {0, 0, 0, 0}} | Normal],
    % [{listen, {0, 0, 0, 0}} | SSL]

    yaws:start_embedded("./wwwroot", SL, GL, Id),

    {ok, spawn_link(fun() -> receive shutdown -> ok end end)}.

stop() -> 
	ok.

