%
%    Jabber.se Web Application
%    Copyright (C) 2010-2011 Jonas Ådahl
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
-include("include/content.hrl").
-include("include/constants.hrl").
-include("include/menu.hrl").
-include("include/utils.hrl").

-export([
        % nitrogen entry points
        main/0,
        event/1,
        api_event/3,

        % template entry points
        head/0,
        foot/0,
        dialogs/0,
        body/0,
        title/0,
        language/0
    ]).

-type load_type() :: trigger | init | history.

%
% Content loading
%

-spec content_error(atom(), load_type()) -> any().
content_error(Error, Type) ->
    set_body(io_lib:format(?T(msg_id_error_occured), [Error]),
        ?T(msg_id_error_title), undefined, "error", Type).

-spec load_url(string(), load_type()) -> any().
load_url(URL, Type) ->
    case parse_url(URL) of
        {[ModuleS | SubPath], _Params} ->
            load_content(list_to_atom(ModuleS), SubPath, URL, Type);
        _ ->
            content_error(not_found, Type)
    end.

load_content(Module, SubPath, URL, Type) ->
    ?LOG_INFO("load_content(~p, ~p, ~p, ~p)", [Module, SubPath, URL, Type]),
    case lists:member(Module, config:content()) of
        true  -> do_load_content(Module, SubPath, URL, Type);
        false -> content_error(not_allowed, Type)
    end.

do_load_content(Module, SubPath, URL, Type) ->
    case catch Module:body(SubPath, Type) of
        #content{body = Body, title = Title} ->
            set_body(Body, Title, Module, URL, Type);
        {'EXIT', _Error} ->
            ?LOG_WARNING("An error occurred when loading content: ~p",
                [_Error]),
            content_error(error, Type)
    end.

%
% Document body modification
%

-spec set_body(iolist(), iolist(), module(), string(), load_type()) -> ok.
set_body(Body, Title, Module, URL, Type) ->
    Animate = lists:member(Type, [trigger, history]),
    Event = #event{target = content_body, type = default},

    % update dom
    ?WHEN(Animate, wf:wire(Event#event{actions=#hide{}})),
    wf:update(content_body, Body),
    wf:wire(Event#event{actions = ?EITHER(Animate, #appear{}, #show{})}),

    % set title
    wf:wire(#js_call{fname = "$Site.$set_title",
                     args = [[config:title(), " - ", Title]]}),

    % if there is a menu element for this module, set it
    case menu:get_element_by_module(Module) of
        {just, MenuElement} ->
            MenuElementID = menu:menu_element_id(MenuElement),
            wf:wire(#js_call{fname = "$Site.$menu_set_current",
                             args = [MenuElementID]});
        nothing ->
            ok
    end,

    % if this is an init call, initialize history, otherwise update history
    Dir = utils:maybe_append(config:path(), $/) ++ "index/",
    case Type of
        init ->
            wf:wire(#js_call{fname = "$Site.$history_push_initial",
                             args = [Title, URL, Dir]});
        trigger ->
            wf:wire(#js_call{fname = "$Site.$history_push",
                             args = [Title, URL, Dir]});
        _ ->
            ok
    end,

    ok.

%
% URL parsing
%

reverse(Binary) ->
    S = size(Binary) * 8,
    <<L:S/integer-little>> = Binary,
    <<L:S/integer-big>>.

finalize_value(Binary) ->
    lists:flatten(io_lib:format("~ts", [reverse(Binary)])).

-spec parse_path(string()) -> {list(string()), string()}.
parse_path(Cs) ->
    parse_path(Cs, [<<>>]).

-spec parse_path(string(), list(binary())) -> {list(string()), string()}.
parse_path(Cs, [PathR | PathsR]) when [] == Cs orelse $? == hd(Cs) ->
    Path = lists:reverse([finalize_value(PathR) | PathsR]),
    {lists:filter(fun([]) -> false; (_) -> true end, Path),
     if Cs /= [] -> tl(Cs); true -> [] end};
parse_path([$/ | Cs], [PathR | PathsR]) ->
    parse_path(Cs, [<<>>, finalize_value(PathR) | PathsR]);
parse_path([$%, N1, N2 | Cs], [PathR | PathsR]) ->
    C = url_num_to_char(N1, N2),
    parse_path(Cs, [<<C:8, PathR/binary>> | PathsR]);
parse_path([C | Cs], [PathR | PathsR]) ->
    parse_path(Cs, [<<C:8,PathR/binary>> | PathsR]).

-spec parse_params(string()) -> [{string(), string()}].
parse_params(Cs) ->
    parse_params(Cs, key, <<>>, <<>>, []).

-spec parse_params(string(), key | value, binary(), binary(),
                   [{string(), string()}]) -> [{string(), string()}].
parse_params([], _, <<>>, <<>>, Params) ->
    lists:reverse(Params);
parse_params([], _, KeyR, ValueR, Params) when KeyR /= [] ->
    lists:reverse([{finalize_value(KeyR), finalize_value(ValueR)} | Params]);
parse_params([$& | Cs], _, KeyR, ValueR, Params) ->
    parse_params(Cs, key, <<>>, <<>>,
                 [{finalize_value(KeyR), finalize_value(ValueR)} | Params]);
parse_params([$%, N1, N2 | Cs], KeyOrValue, KeyR, ValueR, Params) ->
    Char = url_num_to_char(N1, N2),
    case KeyOrValue of
        key ->
            NewKeyR = <<Char:8, KeyR/binary>>,
            NewValueR = ValueR;
        value ->
            NewKeyR = KeyR,
            NewValueR = <<Char:8, ValueR/binary>>
    end,
    parse_params(Cs, KeyOrValue, NewKeyR, NewValueR, Params);

parse_params([$= | Cs], key, KeyR, <<>>, Params) ->
    parse_params(Cs, value, KeyR, <<>>, Params);
parse_params([C | Cs], key, KeyR, ValueR, Params) ->
    parse_params(Cs, key, <<C:8, KeyR/binary>>, ValueR, Params);

parse_params([C | Cs], value, KeyR, ValueR, Params) ->
    parse_params(Cs, value, KeyR, <<C:8, ValueR/binary>>, Params).

hex(N) when is_integer(N), N >= $0, N =< $9 -> N - $0;
hex(N) when is_integer(N), N >= $a, N =< $f -> N - $a + 10;
hex(N) when is_integer(N), N >= $A, N =< $F -> N - $A + 10.

url_num_to_char(N1, N2) -> (hex(N1) * 16) + hex(N2).

-spec parse_url(string()) -> {list(string()), [{string(), string()}]}.
parse_url(URL) ->
    {Path, Rest} = parse_path(URL),
    {Path, parse_params(Rest)}.

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
% Javascript API
%

site_api() ->
    APIs = [
        #api{name = init_content,   tag = content},
        #api{name = menu_triggered, tag = content},
        #api{name = history_load,   tag = content}
    ],
    [wf:wire(API) || API <- APIs],
    ok.

api_event(Name, content, Args) ->
    ?LOG_INFO("~s(~p)", [Name, Args]),
    session:env(),

    case Args of
        []                      -> URL = undefined;
        [URL] when is_list(URL) -> ok
    end,

    case Name of
        init_content ->
            InitURL = case URL of
                undefined -> wf:path_info();
                _         -> URL
            end,
            load_url(InitURL, init),
            page_init_hooks();
        menu_triggered ->
            load_url(URL, trigger);
        history_load ->
            load_url(URL, history)
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
    wf:wire("$Site.$boot();"),

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
        #link{text = "Svenska", 
              class = "language_link",
              postback = {language, sv_SE}},
        " | ",
        #link{text = "English",
              class = "language_link",
              postback = {language, en_US}}
    ].

body() ->
    #panel{id = content_body}.

foot() ->
    [].

dialogs() ->
    #panel{id = dialogs}.
