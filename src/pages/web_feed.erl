-module(web_feed).
-include("include/utils.hrl").
-include("include/menu.hrl").
-export([main/0]).

%
% Document entry points
%

main() ->
    Path = wf:path_info(),
    case menu:get_element_by_path(Path, menu:get_menu_elements()) of
        nothing ->
            web_error:main();
        MenuElement ->
            Module = MenuElement#menu_element.module,
            try
                Body = {Module, atom}(),
                wf:content_type("application/atom+xml"),
                Body
            catch
                error:undef ->
                    ?LOG_WARNING("No atom feed for module \"~p\"", [Module]),
                    web_error:main()
            end
    end.

