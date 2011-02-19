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

-ifndef(config_hrl).
-define(config_hrl, true).

%-ifndef(DEBUG).
%-define(DEBUG, true).
%-endif.

-define(ENABLE_LOG_INFO, true).
-define(ENABLE_LOG_WARNING, true).
-define(ENABLE_LOG_ERROR, true).

-define(MODULES, [config, db_controller, i18n]).
-define(HOOKS, [session]).

-define(TITLE, "Jabber.se").

-define(TRANSLATIONS_DIRECTORY, "res/translations/").

-define(ENABLED_LOCALES, [
        sv_SE,
        en_US
    ]).
%-define(DEFAULT_LOCALE, 'en').
-define(DEFAULT_LOCALE, 'en_US').
%-define(DEFAULT_LOCALE, 'sv_SE').

-define(ADMIN_USER_ENTRY,
    #db_user{
        username = "admin",
        password_hash = sha2:hexdigest256("admin"),
        full_name = "Admin",
        email = "admin@localhost",
        jid = "admin@localhost"
    }).

-define(BASE_DIR, "/web/").

%-define(URL_BASE, "http://www.jabber.se").
-define(URL_BASE, "http://localhost:8000" ++ ?BASE_DIR).

-define(DEFAULT_INDEX_MODULE, news).

-define(ENABLED_CONTENT, [about, news, register]).

-define(CONTENT_CONFIG,
    [
        {register, [{hostname, "localhost"},
                    {server, "localhost"},
                    {port, 8088},
                    {key, "abc123"}]}
    ]).

-define(MENU_ELEMENTS,
    [
        #menu_element{
            module = news,
            title = msg_id_news},
        #menu_element{
            module = register,
            title = msg_id_register},
        #menu_element{
            module = about,
            title = msg_id_about}
    ]).

-define(CMS_ADMIN_MODULES,
    [
        cms_post,
        cms_drafts,
        cms_manage
    ]).

-define(SPINNER_IMAGE, "/res/spinner.gif").
-define(SMALL_SPINNER_IMAGE, "/res/spinner-small.gif").

-endif.
