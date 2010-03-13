-define(MODULES, [cms_db]).

-define(TITLE, "Jabber.se").

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
