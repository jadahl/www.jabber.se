-module(web_index).
-include_lib ("nitrogen/include/wf.inc").

-include("include/config.hrl").
-include("include/constants.hrl").
-include("include/menu.hrl").
-include("include/utils.hrl").

-compile(export_all).

%
% Body content manipulation
%

get_default_menu_element() ->
    menu:get_element_by_module(?DEFAULT_INDEX_MODULE).

load_content(Module) ->
    load_content(Module, []).
load_content('', Options) ->
    change_body(get_default_menu_element(), get_default_body(), Options);
load_content(Module, Options) ->
    case menu:get_element_by_module(Module) of
        nothing ->
            ?LOG_WARNING("load_content called for nonexisting module \"~p\",", [Module]);
        MenuElement ->
            try
                Body = {Module, body}(),
                change_body(MenuElement, Body, Options)
            catch
                error:undef ->
                    ?LOG_WARNING("Could not change body for selected menu item ~p.", [Module]),
                    change_body(get_default_menu_element(), get_default_body(), Options)
            end
    end.

%
% Javascript API
%

-define(ENTRY_FUNCTIONS, [init_content, load_content]).

wire_entry_point(Name) when is_atom(Name) ->
    wf:wire(#api{name = Name, tag = Name});
wire_entry_point({Name, Tag}) ->
    wf:wire(#api{name = Name, tag = Tag}).

site_api() ->
    lists:foreach(fun wire_entry_point/1, ?ENTRY_FUNCTIONS).

%
% Feed Icon
%

get_feed_url(Module) ->
    try
        case {Module, atom_url}() of
            {ok, AtomUrl} ->
                #link{
                    class = ?ATOM_ICON_CLASS,
                    url = AtomUrl,
                    id = ?ATOM_ICON_ID};
            _ ->
                none
        end
    catch
        error:undef ->
            none
    end.

set_feed(Module) ->
    case get_feed_url(Module) of
        none ->
            wf:wire(#js_call{fname = "$Site.$clear_atom_feed_icon", args = [?ATOM_ICON_ID]});
        Icon ->
            wf:wire(#js_call{fname = "$Site.$set_atom_feed_icon", args = [?ATOM_ICON_ID, Icon]})
    end.

%
% Document body modification
%

%
% change_body(atom(), content()) -> ok
% change_body(atom(), content(), options()) -> ok
%    options() = [animate]
%
%    animate - Animate transition (default off)
%
change_body(MenuElement, Body) ->
    change_body(MenuElement, Body, []).
change_body(MenuElement, Body, Options) ->
    Animate = lists:any(fun(Option) -> Option == animate end, Options),
    Id = content_body,
    Event = #event{target = Id, type = default},

    % update dom
    ?WHEN(Animate, wf:wire(Event#event{actions=#hide{}})),
    wf:update(Id, Body),
    wf:wire(Event#event{actions = ?EITHER(Animate, #appear{}, #show{})}),

    % set title
    wf:wire(#js_call{fname = "$Site.$set_title", args = [menu:full_title(MenuElement)]}),

    % sed feed icon
    set_feed(MenuElement#menu_element.module),

    % add history entry
    wf:wire(#js_call{fname = "$Site.$history_push", args = [menu:full_title(MenuElement), menu:full_url(MenuElement)]}),

    ok.


get_default_body() ->
    {?DEFAULT_INDEX_MODULE, body}().

%
% Events
%

event({Module, Event}) when is_atom(Module) ->
    try
        ?LOG_WARNING("Module ~p (~p) called with a depricated method.", [Module, Event]),
        Module:event(Event)
    catch
        error:undef ->
            ?LOG_WARNING("Target for event {~p, ~p} not found.", [Module, Event])
    end;
event(Event) ->
    ?LOG_WARNING("Unhandled event \"~p\".~n", [Event]).

%
% API Events
%

api_event(init_content, init_content, [Fragment]) when is_list(Fragment) ->
    ?LOG_INFO("init_content(~p);", [Fragment]),
    load_content(list_to_atom(Fragment));
api_event(load_content, load_content, [Fragment]) when is_list(Fragment) ->
    ?LOG_INFO("load_content(~p);", [Fragment]),
    case menu:get_element_by_url(Fragment) of
        #menu_element{module = Module} ->
            load_content(Module, [animate]);
        _ ->
            ?LOG_WARNING("Unknown url requested: ~p", [Fragment])
    end;
api_event(A, B, C) ->
    ?LOG_WARNING("Unhandled api_event ~p, ~p, ~p.", [A, B, C]).


%
% Document entry points
%

main() ->
    % site api
    site_api(),

    % initial post back
    wf:wire("page.init_content($Site.$get_fragment_path());"),

    % return template
    #template { file="./wwwroot/template.html" }.

%
% Template entry points
%

head() ->
    feed:get_feed_links().

title() ->
    ?TITLE.

body() ->
    #panel{id = content_body}.

foot() ->
    [].

