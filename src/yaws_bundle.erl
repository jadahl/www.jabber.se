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
-include("simple_bridge/include/yaws_api.hrl").
-export([start/0, stop/0]).

-define(PORT, 8000).
-define(SITE_APP, www_jabber_se_app).

start() ->
	% Set up Yaws Configuration...
	{ok, App} = application:get_application(),
	Id = atom_to_list(App),

	SC = #sconf {
		docroot = "./wwwroot",
		port=?PORT,
		appmods = [{"/web", ?SITE_APP}]
	},
	DefaultGC = yaws_config:make_default_gconf(false, Id),
	GC = DefaultGC#gconf {
		logdir = "./logs",
		cache_refresh_secs = 5
	},

	% Following code adopted from yaws:start_embedded/4. 
	% This will need to change if Yaws changes!!!
	ok = application:set_env(yaws, embedded, true),
	ok = application:set_env(App, embedded, true),
	ok = application:set_env(App, id, Id),
	{ok, Pid} = yaws_sup:start_link(),
	yaws_config:add_yaws_soap_srv(GC),
	SCs = yaws_config:add_yaws_auth([SC]),
	yaws_api:setconf(GC, [SCs]),
	{ok, Pid}.
	
stop() -> 
	% Stop the Yaws server.
	Pid = application_controller:get_master(yaws),
	exit(Pid, kill),
	ok.

