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

-module(cf_recover_password).
-export(
    [
        % Start/stop
        start/0,
        start_link/0,
        stop/0,

        % API
        send_recovery_email/5,
        recover/1,
        cancel/1,
        force_change_password/2,

        % gen_cf_config
        default_config/0,

        % gen_server
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-include("include/utils.hrl").

-behaviour(gen_cf_config).
-behaviour(gen_server).

-record(session, {username, hostname, timestamp}).
-record(state, {sessions, session_timeout}).

%
% gen_cf_config
%

default_config() ->
    [{from_address, "noreply@localhost"},
     {from_name, "Localhost"},
     {template, "recover_email_template.txt"},
     {session_timeout, 60 * 60 * 24}]. % 24 hours

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
% API
%

send_recovery_email(Username, Hostname, Email, RecoverPath, Lang) ->
    gen_server:call(?MODULE, {send_recovery_email,
                              Username, Hostname, Email, RecoverPath, Lang}).

recover(ID) ->
    gen_server:call(?MODULE, {recover, ID}).

cancel(ID) ->
    gen_server:call(?MODULE, {cancel, ID}).

force_change_password(Password, ID) ->
    gen_server:call(?MODULE, {force_change_password, Password, ID}).

%
% Internal
%

%
% gen_server
%

init(_) ->
    Sessions = ets:new(?MODULE, []),
    {ok, #state{sessions = Sessions}}.

handle_call({send_recovery_email, Username, Hostname, Email,
             RecoverPath, Lang},
            _From, State) ->
    try
        Reply = case is_recovery_active(Username, Hostname, State) of
            true ->
                {error, already_sent};
            false ->
                send_recovery_email_impl(Username, Hostname, Email,
                                         RecoverPath, Lang, State)
        end,
        Timeout = get_lowest_timeout(State),
        {reply, Reply, State, Timeout}
    catch
        _:_ = Error ->
            error_logger:error_report(["Error when sending recovery email",
                                       {username, Username},
                                       {hostname, Hostname},
                                       {email, Email},
                                       {recover_path, RecoverPath},
                                       {state, State},
                                       {error, Error},
                                       {stacktrace, erlang:get_stacktrace()}]),
            {reply, {error, internal}, State}
    end;
handle_call({recover, ID}, _From, State) ->
    case ets:lookup(State#state.sessions, ID) of
        [{ID, Session}] ->
            Username = Session#session.username,
            Hostname = Session#session.hostname,
            {reply, {ok, Username, Hostname, ID}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({cancel, ID}, _From, State) ->
    case ets:lookup(State#state.sessions, ID) of
        [{ID, Session}] ->
            Username = Session#session.username,
            Hostname = Session#session.hostname,
            ets:delete(State#state.sessions, ID),
            {reply, {ok, Username, Hostname}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({force_change_password, Password, ID}, _From, State) ->
    try
        case ets:lookup(State#state.sessions, ID) of
            [{ID, Session}] ->
                Username = Session#session.username,
                Hostname = Session#session.hostname,
                ok = cf_mod_restful:force_change_password(Username,
                                                          Hostname,
                                                          Password),
                {reply, {ok, Username, Hostname}, State};
            [] ->
                error_logger:info_report(["force_change_password", {id, ID}, {ets2tab, ets:tab2list(State#state.sessions)}]),
                {reply, {error, not_found}, State}
        end
    catch
        _:_ = Error ->
            error_logger:error_report(["Error when setting password",
                                       {id, ID},
                                       {state, State},
                                       {error, Error},
                                       {stacktrace, erlang:get_stacktrace()}]),
            {reply, {error, internal}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    Now = now_s(),
    F = fun({ID, #session{timestamp = TS}}, Acc) ->
            if
                Now > TS -> [ID | Acc];
                true     -> Acc
            end
        end,
    IDs = ets:foldl(F, [], State#state.sessions),
    [ets:delete(State#state.sessions, ID) || ID <- IDs],

    Timeout = get_lowest_timeout(State),
    {noreply, State, Timeout};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

send_recovery_email_impl(Username, Hostname, Email, RecoverPath, Lang, State) ->
    FromAdress = ?CONFM(from_address),
    FromName = cf_config:title(),

    ID = generate_id(),
    URL = get_url(ID, RecoverPath),
    CancelURL = get_cancel_url(ID, RecoverPath),

    Subject = ?TXT2("Recover your account", cf_i18n:to_lc2(Lang)),
    {ok, Message} = cf_template:parse_file(?CONFM(template), Lang,
                                           [{account, [Username, <<"@">>,
                                                       Hostname]},
                                            {service_name, cf_config:title()},
                                            {url, URL},
                                            {cancel, CancelURL}]),

    ok = cf_mail:send(FromAdress, FromName, Email, Subject, Message),

    TS = now_s(),
    Session = #session{timestamp = TS,
                       username = Username,
                       hostname = Hostname},

    ets:insert(State#state.sessions, {ID, Session}),

    ok.

fill_any(0) -> [];
fill_any(N) -> ['_' | fill_any(N-1)].

is_recovery_active(Username, Hostname, State) ->
    T = list_to_tuple(fill_any(tuple_size(#session{}))),
    T1 = setelement(#session.username, T, Username),
    Pattern = {'_', setelement(#session.hostname, T1, Hostname)},
    [] /= ets:match(State#state.sessions, Pattern).

get_lowest_timeout(State) ->
    F = fun({_, #session{timestamp = Timestamp}}, infinity) ->
            Timestamp;
            ({_, #session{timestamp = Timestamp}}, LowestTimestamp) ->
                if
                    Timestamp < LowestTimestamp -> Timestamp;
                    true                        -> LowestTimestamp
                end
        end,

    % Get lowest timestamp (s)
    LowestTimestamp = ets:foldl(F, infinity, State#state.sessions),

    case LowestTimestamp of
        infinity ->
            infinity;
        _ ->
            % Get session timout (s)
            SessionTimeout = ?CONFM(session_timeout),

            % Get current time (s)
            Now = now_s(),
            
            % Timeout
            Timeout = LowestTimestamp + SessionTimeout,

            % Return 0 or diff in milliseconds
            if
                Timeout < Now -> 0;
                true          -> (Timeout - Now) * 1000
            end
    end.

get_url(ID, Path) ->
    cf_url:url(https, [Path, $/, ID]).

get_cancel_url(ID, Path) ->
    cf_url:url(https, [Path, "/cancel/", ID]).

generate_id() ->
    list_to_binary(sha2:hexdigest256(uuid:to_string(uuid:uuid4()))).

now_s() ->
    {MegaSeconds, Seconds, _} = now(),
    MegaSeconds * 1000000000 + Seconds.

