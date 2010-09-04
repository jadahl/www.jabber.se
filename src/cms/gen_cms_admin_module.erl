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

-module(gen_cms_admin_module).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {selected, 0}, % hook for when admin module was selected
        {left,  0}, % hook for when user navigated away
        {title, 0}, % return title of the module
        {body, 0}, % return the body of the module
        {hook, 0} % hook for when the body has been wired
    ];
behaviour_info(_Other) ->
    undefined.
