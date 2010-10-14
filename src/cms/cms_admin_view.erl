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

-module(cms_admin_view).
-export([title/0, body/0, set_body_back/4, set_body_close/2, back_action/0, back/0, show_dialog/0]).

-include("include/utils.hrl").
-include("include/ui.hrl").
-include("include/config.hrl").

-define(DIALOG_ID, admin_dialog).

set_body_back(Title, Body, Current, Module) ->
    action_dialog:set(?DIALOG_ID, Title, Body),
    action_dialog:corner(?DIALOG_ID, back, #event{postback = {back, Current, Module}, delegate = cms_admin}).

set_body_close(Title, Body) ->
    action_dialog:set(?DIALOG_ID, Title, Body),
    action_dialog:corner(?DIALOG_ID, close, #dialog_hide{target = ?DIALOG_ID}).

back_action() ->
    #dyn_dialog_back{target = ?DIALOG_ID}.

back() ->
    action_dialog:back(?DIALOG_ID).

show_dialog() ->
    Dialog = #dialog{id = ?DIALOG_ID},
    dialog:show(Dialog, fun() -> set_body_close(?T(msg_id_admin_title), body()) end).

title() ->
    ?T(msg_id_admin_title).

body() ->
    #list{
        class = admin_modules,
        body =
        [
            #listitem{body = #link{text = Module:title(), delegate = cms_admin, postback = {select, Module}}} 
            || Module <- ?CMS_ADMIN_MODULES
        ]}.

