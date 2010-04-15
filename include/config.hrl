-ifndef(config_hrl).
-define(config_hrl, true).

-ifndef(DEBUG).
-define(DEBUG, true).
-endif.

%-define(ENABLE_LOG_INFO, true).
-define(ENABLE_LOG_WARNING, true).
-define(ENABLE_LOG_ERROR, true).

-define(MODULES, [cms_db]).

-define(TITLE, "Jabber.se").

%-define(URL_BASE, "http://www.jabber.se/web/").
-define(URL_BASE, "http://localhost:8000/web/").

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

-define(SPINNER_IMAGE, "/nitrogen/spinner.gif").

-endif.
