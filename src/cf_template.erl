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

-module(cf_template).
-export(
    [
        % Start/stop
        start/0,
        start_link/0,
        stop/0,

        % gen_cf_config
        default_config/0,

        % API
        parse_file/3,
        parse/2,

        % gen_server
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-behaviour(gen_server).
-behaviour(gen_cf_config).

-include("include/utils.hrl").

%
% gen_cf_config
%

default_config() -> [{dir, ["priv", "templates"]}].

%
% Start/stop
%

start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%
% gen_server
%

init(_) ->
    Cache = ets:new(?MODULE, []),

    {ok, Cache}.

handle_call({get_template, File, Lang}, _From, Cache) ->
    Path = find_template_file(File, Lang),
    case ets:lookup(Cache, Path) of
        [{Path, Template}] ->
            {reply, {ok, Template}, Cache};
        _  ->
            case file:read_file(Path) of
                {ok, Template} ->
                    ets:insert(Cache, {Path, Template}),
                    {reply, {ok, Template}, Cache};
                _ = Error ->
                    {reply, Error, Cache}
            end
    end;
handle_call(stop, _From, Cache) ->
    {stop, normal, Cache}.

handle_cast(_Message, Cache) ->
    {noreply, Cache}.

handle_info(_Info, Cache) ->
    {noreply, Cache}.

code_change(_OldVsn, Cache, _Extra) ->
    {ok, Cache}.

terminate(_Reason, _Cache) ->
    ok.
%
% Internal
%

find_template_file(File, Lang) ->
    Dir = ?CONFM(dir),
    Filenames = [lists:flatten(cf_utils:join(Dir ++ [cf_utils:to_string(Lang), File],
                                          "/")),
                 lists:flatten(cf_utils:join(Dir ++ [cf_i18n:to_lc2(Lang), File],
                                          "/"))],
    try_template_file(Filenames).

try_template_file([Filename | Filenames]) ->
    case file:read_file_info(Filename) of
        {ok, _FileInfo} ->
            Filename;
        _ ->
            try_template_file(Filenames)
    end.

%
% API
%

parse_file(File, Lang, Map) ->
    case gen_server:call(?MODULE, {get_template, File, Lang}) of
        {ok, Template} -> parse(Template, Map);
        _              -> {error, no_template}
    end.

parse(Template, Map) ->
    try
        {ok, list_to_binary(parse1(Template, Map))}
    catch
        _:_ = Exception ->
            error_logger:error_report([{exception_in, {?MODULE, parse}},
                                       {template, Template},
                                       {map, Map},
                                       {exception, Exception},
                                       {stacktrace, erlang:get_stacktrace()}]),
            {error, parse}
    end.

parse1(Template, Map) ->
    Size = size(Template),
    case binary:match(Template, <<"[[[">>) of
        {Start, _Len} ->
            {End, _} = binary:match(Template, <<"]]]">>, 
                                    [{scope, {Start + 3,
                                              Size - (Start + 3)}}]),
            KeyB = binary:part(Template, Start + 3, End - (Start + 3)),
            Value = proplists:get_value(cf_utils:to_atom(KeyB), Map, <<"undefined">>),
            [binary:part(Template, 0, Start),
             Value,
             parse1(binary:part(Template, End + 3, Size - (End + 3)), Map)];
        nomatch ->
            [Template]
    end.

