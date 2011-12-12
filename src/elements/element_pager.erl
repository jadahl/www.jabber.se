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

-module(element_pager).
-export([render_element/1, num_id/1, prev/3, next/3, num/4, event/1]).

-include("include/ui.hrl").

num_id(Num) ->
    list_to_atom(lists:flatten(io_lib:format("pager_num_~p", [Num]))).

prev(1, _Count, _Adapter)->
    #span{text = "←", id = pager_prev, class = pager_num};
prev(Current, Count, Adapter) ->
    #link{
        text = "←",
        id = pager_prev,
        class = pager_num,
        postback = {page, Current - 1, Count, Adapter},
        delegate = ?MODULE}.

next(Current, Count, _Adapter) when Current == Count ->
    #span{text = "→", id = pager_next, class = pager_num};
next(Current, Count, Adapter) ->
    #link{
        text = "→",
        id = pager_next,
        class = pager_num,
        postback = {page, Current + 1, Count, Adapter},
        delegate = ?MODULE}.

num(Num, Current, _Count, _Adapter) when Num == Current ->
    #span{
        text = integer_to_list(Num),
        id = num_id(Num),
        class = [pager_num, pager_num_current]};
num(Num, _Current, Count, Adapter) ->
    #link{
        text = integer_to_list(Num),
        id = num_id(Num),
        class = pager_num,
        postback = {page, Num, Count, Adapter},
        delegate = ?MODULE}.

render_element(#pager{id = Id, init_page = Init, count = Count, adapter = Adapter}) ->
    #panel{
        id = Id,
        class = pager_container,
        body = if
            Count < 2 -> [];
            true ->
                [
                    % <-
                    prev(Init, Count, Adapter),

                    % 1, 2, 3, ...
                    [num(Num, Init, Count, Adapter) || Num <- cf_utils:range(Count)],

                    % ->
                    next(Init, Count, Adapter)
                ]
        end
    }.

%
% Redirect pager events to adapter
%

event({page, Index, Count, Adapter}) ->
    Adapter:pager_set(Index, Count).
