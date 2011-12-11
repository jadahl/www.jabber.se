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

-define(MODULES, [cf_config, db_controller, i18n, yaws_bundle]).
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


%-define(HOST, "www.jabber.se").
-define(HOST, "localhost").

%-define(DOMAIN, "jabber.se").
-define(DOMAIN, "localhost").

-define(BASE_DIR, "/").

-define(HTTP_HTTPS_PORT_FORWARD, false).
-define(HTTPS_TUNNELED, false).
-define(HTTP_PORT, 8000).
-define(HTTPS_PORT, 8008).

-define(DEFAULT_CONTENT_URL, "news").

-define(ENABLED_CONTENT, [content_about,
                          content_news,
                          content_register,
                          content_account]).

-define(CONTENT_CONFIG,
    [
        {cf_mod_restful, [{hostname, ?HOST},
                          {server, {0, 0, 0, 0, 0, 0, 0, 1}},
                          {port, 8088},
                          {path, ["api"]},
                          {key, "secret"}]}
    ]).

-define(MENU_ELEMENTS,
    [
        #menu_element{
            path = "news",
            title = msg_id_news},
        #menu_element{
            path = "register",
            title = msg_id_register},
        #menu_element{
            path = "account",
            title = msg_id_account},
        #menu_element{
            path = "about",
            title = msg_id_about}
    ]).

-define(CMS_ADMIN_MODULES,
    [
        cms_compose,
        cms_drafts,
        cms_manage
    ]).

-define(SPINNER_IMAGE, "/res/spinner.gif").
-define(SMALL_SPINNER_IMAGE, "/res/spinner-small.gif").
-define(SPINNER_IMAGE_MENU, "/res/spinner-menu.gif").

-endif.
