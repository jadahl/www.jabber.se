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

-module(web_feed).
-include("include/utils.hrl").
-include("include/menu.hrl").
-export([main/0]).

%
% Document entry points
%

main() ->
    Path = case wf:path_info() of
        [$/ | Rest] -> Rest;
        Result      -> Result
    end,

    Module = cf_url:content_path_to_module(Path),

    case cf_config:content_enabled(Module) of
        false ->
            web_error:main();
        true ->
            try
                Body = Module:atom("/feed" ++ wf:path_info()),
                wf:content_type("application/atom+xml"),
                Body
            catch
                error:undef ->
                    ?LOG_WARNING("No atom feed for module \"~p\"", [Module]),
                    web_error:main()
            end
    end.

