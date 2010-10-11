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

-module(web_index).
-include("include/ui.hrl").

-include("include/config.hrl").
-include("include/constants.hrl").
-include("include/menu.hrl").
-include("include/utils.hrl").

-compile(export_all).

%
% Body content manipulation
%

get_default_menu_element() ->
    {just, Element} = menu:get_element_by_module(?DEFAULT_INDEX_MODULE),
    Element.

load_content(Module) ->
    load_content(Module, []).
load_content('', Options) ->
    change_body(get_default_menu_element(), get_default_body(), Options);
load_content(Module, Options) ->
    case menu:get_element_by_module(Module) of
        nothing ->
            ?LOG_WARNING("load_content called for nonexisting module \"~p\",", [Module]);
        {just, MenuElement} ->
            try
                Body = Module:body(),
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
        case Module:atom_url() of
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

    % set current menu element
    wf:wire(#js_call{fname = "$Site.$menu_set_current", args = [MenuElement#menu_element.module]}),

    % add history entry
    wf:wire(#js_call{fname = "$Site.$history_push", args = [menu:full_title(MenuElement), menu:full_url(MenuElement)]}),

    ok.


get_default_body() ->
    {?DEFAULT_INDEX_MODULE, body}().

%
% Fragment
%

parse_segment(Segment) ->
    case lists:splitwith(fun(C) -> C /= $= end, Segment) of
        {Key, [$= | Value]} ->
            {list_to_atom(Key), Value};
        {_, []} ->
            {path, Segment};
        _ ->
            ?LOG_WARNING("Odd fragment encountered - '~p'", [Segment]),
            []
    end.

parse_fragment([]) ->
    [];
parse_fragment(Fragment) ->
    case lists:splitwith(fun(C) -> C /= $, end, Fragment) of
        {[], [$, | Rest1]} ->
            parse_fragment(Rest1);
        {Segment, [$, | Rest2]} ->
            [parse_segment(Segment) | parse_fragment(Rest2)];
        {Segment, []} ->
            [parse_segment(Segment)]
    end.

fragment_set_lang(Args) ->
    case lists:keysearch(lang, 1, Args) of
        {value, {lang, Lang}} ->
            i18n:set_language(Lang);
        _ -> 
            ok
    end.

fragment_load_content(Args) ->
    case lists:keysearch(path, 1, Args) of
        {value, {path, Path}} ->
            load_content(list_to_atom(Path));
        _ ->
            ok
    end.

process_fragment(Fragment) ->
    Args = parse_fragment(Fragment),

    % language
    fragment_set_lang(Args),

    % content
    fragment_load_content(Args),

    ok.

%
% Hooks
%

page_init_hooks() ->
    [Module:page_init() || Module <- ?HOOKS].

%
% Events
%

event({language, Lang}) ->
    % update cookie and set process dictionary
    session:language(Lang),

    % reload content
    wf:wire(#js_call{fname = "$Site.$reload_content"}),

    ok;

event(Event) ->
    ?LOG_WARNING("Unhandled event \"~p\".~n", [Event]).

%
% API Events
%

api_event(init_content, init_content, [Fragment]) when is_list(Fragment) ->
    ?LOG_INFO("init_content(~p);", [Fragment]),
    session:env(),
    process_fragment(Fragment),

    page_init_hooks();
api_event(load_content, load_content, [Fragment]) when is_list(Fragment) ->
    ?LOG_INFO("load_content(~p);", [Fragment]),
    session:env(),
    case menu:get_element_by_url(Fragment) of
        {just, #menu_element{module = Module}} ->
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
    session:env(),

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

language() ->
    [
        #link{text = "Svenska", class = "language_link", postback = {language, sv_SE}},
        " | ",
        #link{text = "English", class = "language_link", postback = {language, en_US}}
    ].

body() ->
    #panel{id = content_body}.

foot() ->
    [].

dialogs() ->
    #panel{id = dialogs}.
