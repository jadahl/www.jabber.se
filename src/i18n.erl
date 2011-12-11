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

-module(i18n).
-export([
        start/0, stop/0,
        reload/0,
        get_language/0, update_language/0, set_language/1,
        t/1, t/2, alias/1,
        enabled_languages/0,
        read_dir/1, read_translations/0, is_lang/1,
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
    ]).

-behaviour(gen_server).

-include("include/utils.hrl").
-include("include/config.hrl").

%
% API
%

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, close).

get_language() ->
    get(current_language).

update_language() ->
    case get_language() of
        undefined ->
            set_language(?DEFAULT_LOCALE);
        CurrentLang ->
            CurrentLang
    end.

enabled_languages() ->
    {ok, Languages} = gen_server:call(?MODULE, enabled_languages),
    Languages.

reload() ->
    gen_server:call(?MODULE, reload_translations),
    ok.
%
% Returns the new set language
set_language(Lang) when is_list(Lang) ->
    case is_lang(Lang) of
        true ->
            set_language(list_to_atom(Lang));
        false ->
            ?LOG_WARNING("Unknown language '~p'.", [Lang]),
            {error, invalid_language}
    end;
set_language(Lang) when is_atom(Lang) ->
    Lang2 = alias(Lang),
    % Gettext
    put(gettext_language, to_lc2(Lang)),
    % Internal
    put(current_language, Lang2),
    Lang2.

t(Id) ->
    Lang = case get_language() of
        undefined ->
            ?LOG_WARNING("Translation request with no language set for '~p'", [Id]),
            % Uncomment the following line to get origin of call
            %erlang:display(catch erlang:error(no_lang)),
            ?DEFAULT_LOCALE;
        Lang1 ->
            Lang1
    end,
    t(Id, Lang).

t(Id, Lang) ->
    case gen_server:call(?MODULE, {translate, Id, Lang}) of
        {ok, Lang1, Result1} when Lang1 == Lang ->
            Result1;
        {ok, _Lang2, Result2} ->
            ?LOG_WARNING("Missing translation for '~p' in locale ~p", [Id, Lang]),
            Result2;
        Error ->
            ?LOG_ERROR("Could not translate '~p' using locale ~p due to ~p", [Id, Lang, Error]),
            "[" ++ atom_to_list(Id) ++ "]"
    end.

alias(en) -> en_US;
alias(sv) -> sv_SE;
alias(Lang) -> Lang.

%
% Internal
%

to_lc2(Atom) when is_atom(Atom) ->
    to_lc2(atom_to_list(Atom));
to_lc2([C1, C2, $_ | _Rest]) ->
    [C1, C2].

-spec is_lang(atom()|string()) -> boolean().
is_lang(Atom) when is_atom(Atom) ->
    is_lang(atom_to_list(Atom));
is_lang([C1, C2, $_, C3, C4]) when 
    ((C1 >= $a) and (C1 =< $z)) and
    ((C2 >= $a) and (C2 =< $z)) and
    ((C3 >= $A) and (C3 =< $Z)) and
    ((C4 >= $A) and (C4 =< $Z)) ->
    true;
is_lang([C1, C2]) when 
    ((C1 >= $a) and (C1 =< $z)) and
    ((C2 >= $a) and (C2 =< $z)) ->
    true;
is_lang(_) ->
    false.

eval(S) ->
    try
        eval(S, [])
    catch
        E ->
            io:format("E = ~p~n", [E])
    end.

eval(S, Env) ->
    {ok, Scanned, _} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed, Env).

read_scan_parse(Filename, Lang) ->
    try
        {ok, Binary} = file:read_file(Filename),
        {value, Value, _} = eval(binary_to_list(Binary)),
        {Lang, dict:from_list(Value)}
    catch
        error:Error ->
            ?LOG_ERROR("Failed to read translation file '~s' due to '~p'", [Filename, Error]),
            ?LOG_ERROR("Stack trace: ~p", [erlang:get_stacktrace()]),
            []
    end.

read_file(Directory, Filename) ->
    Lang = filename:basename(Filename, ".res"),
    case is_lang(Lang) of
        true ->
            read_scan_parse(Directory ++ Filename, list_to_atom(Lang));
        false ->
            []
    end.

read_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            lists:flatten([read_file(Dir, Filename) || Filename <- Filenames]);
        Error ->
            Error
    end.

read_translations() ->
    read_dir(?TRANSLATIONS_DIRECTORY).

maybe_t(Id, Lang, State) ->
    case lists:keysearch(Lang, 1, State) of
        {value, {_, Dict}} ->
            case dict:find(Id, Dict) of
                {ok, Value} ->
                    {just, Value};
                _ ->
                    nothing
            end;
        _ ->
            nothing
    end.

%
% gen_server
%

init(_Args) ->
    {ok, read_translations()}.

handle_call({translate, Id, Lang}, _From, State) ->
    Reply = case maybe_t(Id, Lang, State) of
        {just, Translated} ->
            {ok, Lang, Translated};
        nothing ->
            case maybe_t(Id, ?DEFAULT_LOCALE, State) of
                {just, Translated2} ->
                    {ok, ?DEFAULT_LOCALE, Translated2};
                nothing ->
                    {error, translation_not_found}
            end
    end,
    {reply, Reply, State};

handle_call(enabled_languages, _From, State) ->
    Reply = try
        {ok,
            [
                case maybe_t(language, Locale, State) of
                    {just, Translated} ->
                        {Locale, Translated};
                    nothing ->
                        throw(translation_not_found)
                end
                || Locale <- ?ENABLED_LOCALES
            ]
        }
    catch
        _:_ = Error ->
            {error, Error}
    end,
    {reply, Reply, State};

handle_call(reload_translations, _From, _State) ->
    {reply, ok, read_translations()};

handle_call(Request, From, State) ->
    ?LOG_WARNING("Unexpected call from ~p. Request = ~p, State = ~p", [From, Request, State]),
    {noreply, State}.

handle_cast(close, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    ?LOG_WARNING("Unexpected cast. Request = ~p, State = ~p", [Request, State]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_WARNING("Unexpected info. Info = ~p, State = ~p", [Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    case Reason of
        normal ->
            ok;
        _ ->
            ?LOG_WARNING("Abnormal shutdown due to '~p', State = ~p", [Reason, State])
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
