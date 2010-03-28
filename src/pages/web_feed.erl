-module(web_feed).
-include("include/menu.hrl").
-export([main/0]).

main() ->
    Path = wf:get_path_info(),
    case menu:get_element_by_path(menu:get_menu_elements(), Path) of
        none ->
            web_error:main();
        MenuElement ->
            Module = MenuElement#menu_element.module,
            try
                Body = {Module, atom}(),
                wf:set_content_type("application/atom+xml"),
                Body
            catch
                error:undef ->
                    web_error:main()
            end
    end.


