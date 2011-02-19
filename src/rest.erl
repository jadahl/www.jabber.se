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

-module(rest).

-export([
        request_get/3,
        request_post_json/4
    ]).

-include("include/utils.hrl").

-define(TIMEOUT, 5000).

-define(CONTENTTYPE_JSON, "application/json").

-spec request_get(string(), Port::1..65535, Path::string()) ->
    {error, Reason::atom()} | {ok, term()}.
request_get(Host, Port, Path) ->
    case catch lhttpc:request(Host, Port, false, Path,
            'GET', [], [], ?TIMEOUT, []) of
        {ok, {{200, _}, Headers, Data}} ->
            process_response(Headers, Data);
        _R ->
            {error, _R}
    end.

-spec request_post_json(string(), Port::1..65535, Path::string(),
                        JSON::term()) ->
    {error, Reason::atom()} | {ok, term()}.
request_post_json(Host, Port, Path, JSON) ->
    Headers = [{"Content-Type", ?CONTENTTYPE_JSON}],
    Data = mochijson2:encode(JSON),
    case catch lhttpc:request(Host, Port, false, Path,
            'POST', Headers, Data, ?TIMEOUT, []) of
        {ok, {_, InHeaders, InData}} ->
            process_response(InHeaders, InData);
        R ->
            {error, R}
    end.

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
