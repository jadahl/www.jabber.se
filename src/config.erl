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

-module(config).

-behaviour(gen_server).

-include("include/menu.hrl").
-include("include/config.hrl").

-export(
    [
        start/0,
        start_link/0, stop/0,

        % config access functions
        title/0,
        modules/0,
        content/0,
        menu/0,
        languages/0,
        host/0,
        path/0,

        % config helper functions
        content_enabled/1,
        
        % gen_server
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).

-record(state, {
        title = []              :: iolist(),
        modules = []            :: list(module()),
        content = []            :: list(module()),
        menu = []               :: list(module()),
        languages = []          :: list(atom()),
        host = "localhost:8000" :: string(),
        path = "/web"           :: string()

    }).

start() -> start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

title()     -> get_config(title).
modules()   -> get_config(modules).
content()   -> get_config(content).
menu()      -> get_config(menu).
languages() -> get_config(languages).
host()      -> get_config(host).
path()      -> get_config(path).

content_enabled(Module) -> lists:member(Module, get_config(content)).

get_config(Config) ->
    {ok, Value} = gen_server:call(?MODULE, {get, Config}),
    Value.

%
% gen_server callbacks
%

init(_) ->
    {ok, #state{
            title = ?TITLE,
            modules = ?MODULES,
            content = ?ENABLED_CONTENT,
            menu = ?MENU_ELEMENTS,
            languages = ?ENABLED_LOCALES
        }}.

handle_call({get, Configs}, _From, S) when is_list(Configs) ->
    try
        {reply, {ok, [internal_get_config(Config, S) || Config <- Configs], S}}
    catch
        not_found ->
            {reply, {error, not_found}, S}
    end;
handle_call({get, Config}, _From, S) when is_atom(Config) ->
    try
        {reply, {ok, internal_get_config(Config, S)}, S}
    catch
        not_found -> {reply, {error, not_Found}, S}
    end;
handle_call(stop, _From, S) ->
    {stop, normal, S};
handle_call(_Request, _From, S) ->
    {reply, {error, badarg}, S}.

internal_get_config(title, S)     -> S#state.title;
internal_get_config(modules, S)   -> S#state.modules;
internal_get_config(content, S)   -> S#state.content;
internal_get_config(menu, S)      -> S#state.menu;
internal_get_config(languages, S) -> S#state.languages;
internal_get_config(host, S)      -> S#state.host;
internal_get_config(path, S)      -> S#state.path;
internal_get_config(_, _S) -> throw(not_found).

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Reason, _S) ->
    ok.
