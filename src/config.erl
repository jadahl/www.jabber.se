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

-include("include/utils.hrl").
-include("include/menu.hrl").
-include("include/config.hrl").

-export(
    [
        start/0,
        start_link/0, stop/0,

        % config access functions
        title/0,
        modules/0,
        enabled_content/0,
        menu/0,
        languages/0,
        host/0,
        path/0,
        default_content_url/0,

        read/1,
        set/2,

        % content config helper functions
        content/2,

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
        table
    }).

start() -> start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

title()               -> read(title).
modules()             -> read(modules).
enabled_content()     -> read(enabled_content).
menu()                -> read(menu).
languages()           -> read(languages).
host()                -> read(host).
path()                -> read(path).
default_content_url() -> read(default_content_url).

content(Content, Key) -> read({c, Content, Key}).

content_enabled(Module) ->
    lists:member(Module, read(enabled_content)).

read(Config) ->
    case gen_server:call(?MODULE, {get, Config}) of
        {ok, Value} ->
            Value;
        _Error ->
            ?LOG_ERROR("Error when reading config '~w': ~p", [Config, _Error]),
            undefined
    end.

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

%
% gen_server callbacks
%

init(_) ->
    Table = ets:new(config, []),

    % Basic configuration
    ets:insert(Table, [
            {title, ?TITLE},
            {host, ?HOST},
            {path, ?BASE_DIR},
            {http_port, ?HTTP_PORT},
            {https_port, ?HTTPS_PORT},
            {http_https_port_forward, ?HTTP_HTTPS_PORT_FORWARD},
            {modules, ?MODULES},
            {enabled_content, ?ENABLED_CONTENT},
            {default_content_url, ?DEFAULT_CONTENT_URL},
            {menu, ?MENU_ELEMENTS},
            {languages, ?ENABLED_LOCALES}
        ]),

    ets:insert(Table,
        lists:flatten([
            [
                {{c, Content, Key}, Value}
                ||
                {Key, Value} <- ContentConfig
            ]
            ||
            {Content, ContentConfig} <- ?CONTENT_CONFIG
        ])),
    {ok, #state{table = Table}}.

handle_call({get, Config}, _From, S) ->
    try
        if is_list(Config) ->
                {reply, {ok, [lookup(C, S) || C <- Config]}, S};
           true ->
                {reply, {ok, lookup(Config, S)}, S}
        end
    catch
        {not_found, Key} -> {reply, {error, {not_found, Key}}, S}
    end;
handle_call({set, Key, Value}, _From, S) ->
    set(Key, Value, S),
    {reply, ok, S};
handle_call(stop, _From, S) ->
    {stop, normal, S};
handle_call(_Request, _From, S) ->
    {reply, {error, badarg}, S}.

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Reason, _S) ->
    ok.

%
% Internal
%

lookup(Key, State) ->
    case ets:lookup(State#state.table, Key) of
        [{Key, Value}] -> Value;
        _              -> throw({not_found, Key})
    end.

set(Key, Value, State) ->
    ets:insert(State#state.table, {Key, Value}).

