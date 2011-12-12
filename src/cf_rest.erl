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

-module(cf_rest).

-export([
        request_get/4,
        request_post_json/5
    ]).

-include("include/utils.hrl").

-define(TIMEOUT, 5000).

-define(CONTENTTYPE_JSON, "application/json").

-spec request_get(Host::string(), Server:: string() | tuple(),
                  Port::1..65535, Path::string()) ->
    {error, Reason::atom()} | {ok, term()}.
request_get(Host, Server, Port, Path) ->
    case ibrowse:send_req(url(Server, Port, Path),
                          [], get, [],
                          options(Host)) of
        {ok, "200", Headers, Data} -> process_response(Headers, Data);
        R -> {error, R}
    end.

-spec request_post_json(Host::string(), Server:: string() | tuple(),
                        Port::1..65535, Path::string(), JSON::term()) ->
    {error, Reason::atom()} | {ok, term()}.
request_post_json(Host, Server, Port, Path, JSON) ->
    Headers = [{"Content-Type", ?CONTENTTYPE_JSON}],
    Data = mochijson2:encode(JSON),
    case ibrowse:send_req(url(Server, Port, Path),
                          Headers,
                          post, Data,
                          options(Host)) of
        {ok, _, ResponseHeaders, ResponseData} ->
            process_response(ResponseHeaders, ResponseData);

        R ->
            {error, R}
    end.

options(Host) ->
    HostS = cf_utils:to_string(Host),
    [{host_header, HostS},
     {connect_timeout, ?TIMEOUT}].

url(Server, Port, Path) ->
    ServerS = if is_tuple(Server) -> ipt_to_list(Server);
                 true             -> Server
              end,

    % only supports http currently:w
    "http://" ++ ServerS ++ ":" ++
    integer_to_list(Port) ++ Path.

ipt_to_list(IPT) when is_tuple(IPT), tuple_size(IPT) == 8 ->
    "[" ++ inet_parse:ntoa(IPT) ++ "]";
ipt_to_list(IPT) when is_tuple(IPT), tuple_size(IPT) == 4 ->
    inet_parse:ntoa(IPT).

process_response(Headers, Data) ->
    case lists:keysearch("Content-Type", 1, Headers) of
        {value, {_, ?CONTENTTYPE_JSON}} ->
            case mochijson2:decode(Data) of
                {ok, JSON1} -> {ok, JSON1};
                JSON2       -> {ok, JSON2}
            end;
        _ ->
            {error, bad_format}
    end.
