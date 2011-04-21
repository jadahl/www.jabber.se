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

-module(register_tests).

-include_lib("eunit/include/eunit.hrl").

url_encode_test() ->
    Ins = [
        {
            {["foo", "bar"], [{"baz", "biz"}, {"boz", "bez"}]},
            "/foo/bar?baz=biz&boz=bez"
        },
        {
            {["foo", "bar"], []},
            "/foo/bar"
        },
        {
            {[], [{"foo", "bar"}]},
            "/?foo=bar"
        },
        {
            {["灣"], []},
            "/%E7%81%A3"
        },
        {
            {[], [{"臺", "灣"}]},
            "/?%E8%87%BA=%E7%81%A3"
        },
        {
            {[[28771]], []},
            "/%E7%81%A3"
        },
        {
            {[], [{key, "Value"}]},
            "/?key=Value"
        }
    ],

    [?assertEqual(Expect, register:url_path_encode(Path, Params)) || {{Path, Params}, Expect} <- Ins].

