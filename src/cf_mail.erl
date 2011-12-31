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

-module(cf_mail).
-export(
    [
        default_config/0,

        send/5
    ]).

-behaviour(gen_cf_config).

-include("include/utils.hrl").

default_config() ->
    [{server, "localhost"},
     {port, 25}].

send(From, FromName, To, Subject, Message) ->
    Server = ?CONFM(server),
    Port = ?CONFM(port),

    Headers = [{"Subject", Subject},
               {"To", list_to_binary(["<", To, ">"])},
               {"From", list_to_binary([FromName, " <", From, ">"])}],

    Options = [{no_mx_lookups, true},
               {relay, Server},
               {port, Port}],

    Body = [[[Key, <<": ">>, Value, <<"\r\n">>]
             || {Key, Value} <- Headers],
            <<"\r\n">>, Message],

    error_logger:info_report(["Sending E-mail",
                              {from, From},
                              {from_name, FromName},
                              {to, To},
                              {subject, Subject}]),

    case gen_smtp_client:send_blocking({From, [To], Body}, Options) of
        _B when is_binary(_B) ->
            ok;
        {error, Reason, _Msg} ->
            error_logger:error_report([{error_in, {?MODULE, send}},
                                       {args, [From, FromName, To, Subject, Message]},
                                       {reason, Reason},
                                       {message, _Msg}]),
            {error, Reason};
        {error, Reason} ->
            error_logger:error_report([{error_in, {?MODULE, send}},
                                       {args, [From, FromName, To, Subject, Message]},
                                       {reason, Reason}]),
            {error, Reason}
    end.

