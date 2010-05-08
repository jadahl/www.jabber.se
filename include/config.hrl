-ifndef(config_hrl).
-define(config_hrl, true).

-ifndef(DEBUG).
-define(DEBUG, true).
-endif.

-define(ENABLE_LOG_INFO, true).
-define(ENABLE_LOG_WARNING, true).
-define(ENABLE_LOG_ERROR, true).

-define(MODULES, [db_controller]).

-define(TITLE, "Jabber.se").

%-define(DEFAULT_LOCALE, 'en_US').
-define(DEFAULT_LOCALE, 'sv_SE').

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

-define(MENU_ELEMENTS,
    [
        #menu_element{
            module = news,
            title = "News",
            url = "#news"},
        #menu_element{
            module = about,
            title = "About",
            url = "#about"}
    ]
).

-define(SPINNER_IMAGE, "/res/spinner.gif").

-endif.
