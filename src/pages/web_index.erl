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
        description/0,
        head/0,
        foot/0,
        dialogs/0,
        body/0,
        page_title/0,
        title/0,
        language/0,

        load_error/2,
        content_error/1
    ]).

-type load_type() :: trigger | init | history.

%
% Content loading
%

-spec load_error(atom() | iolist(), load_type()) -> any().
load_error(Error, Type) when is_atom(Error) ->
    load_error(?TXT("An error occurred"), Type);
load_error(Message, Type) ->
    set_body(Message, ?TXT("Error"), error, Type).

content_error(Error) ->
    content_error(Error, ?TXT("An error occurred")).
content_error(Error, Message) ->
    Stacktrace = erlang:get_stacktrace(),
    error_logger:error_report([{message, lists:flatten(Message)},
                               {error, Error},
                               {stacktrace, Stacktrace}]),
    #content{body = Message,
             title = ?TXT("Error")}.

get_content(Module, Path) ->
    case cf_config:content_enabled(Module) of
        true  -> do_get_content(Module, Path);
        false -> content_error(not_allowed)
    end.

do_get_content(Module, Path) ->
    try
        Module:body(Path)
    catch
        _:_ = Error -> content_error(Error)
    end.

load_content(URL, Type) ->
    case local_path(URL) of
        [ContentPath | _] = Path ->
            Module = cf_url:content_path_to_module(ContentPath),
            #content{body = Body,
                     title = Title,
                     post_eval = PostEval} = get_content(Module, Path),
            set_body(Body, Title, URL, Type),
            if is_function(PostEval) -> PostEval();
               true -> ok
            end;
        _ ->
            load_error(not_found, Type)
    end.

cached_content() ->
    get(content).

cache_content() ->
    case cf_url:path() of
        "" -> URL = cf_config:default_content_url();
        URL -> ok
    end,

    {BasePath, []} = parse_path(cf_config:path()),

    Content = case local_path(URL) of
        [ContentPath | _] = Path ->
            ?LOG_INFO("base-path=~p, path=~p", [BasePath, Path]),
            Module = cf_url:content_path_to_module(ContentPath),
            get_content(Module, Path);
        _ ->
            content_error('404')
    end,

    % Store the content in the process dictionary
    put(content, Content).

%
% Document body modification
%

-spec set_body(iolist(), iolist(), string(), load_type()) -> ok.
set_body(Body, Title, URL, Type) ->
    Animate = lists:member(Type, [trigger, history]),
    Event = #event{target = content_body, type = default},

    % update dom
    ?WHEN(Animate, wf:wire(Event#event{actions=#hide{}})),
    wf:update(content_body, Body),
    wf:wire(Event#event{actions = ?EITHER(Animate, #appear{}, #show{})}),

    % set title
    wf:wire(#js_call{fname = "$Site.$set_title",
                     args = [[cf_config:title(), " - ", Title]]}),

    % if there is a menu element for this module, set it
    case cf_menu:element_by_path(URL) of
        undefined ->
            ok;
        MenuElement ->
            MenuElementID = cf_menu:menu_element_id(MenuElement),
            wf:wire(#js_call{fname = "$Site.$menu_set_current",
                             args = [MenuElementID]})
    end,

    % if this is an init call, initialize history, otherwise update history
    Dir = cf_utils:maybe_append(cf_config:path(), $/),
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

local_path(URL) ->
    {FullPath, _Params} = parse_url(URL),
    {BasePath, []} = parse_path(cf_config:path()),
    cf_utils:drop_prefix(BasePath, FullPath).

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
    cf_session:language(Lang),

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

api_event(init_content, content, Args) ->
    ?LOG_INFO("init_content(~p)", [Args]),
    cf_session:env(),
    page_init_hooks();
api_event(Name, content, Args) ->
    ?LOG_INFO("~s(~p)", [Name, Args]),
    cf_session:env(),

    case Args of
        [] ->
            ok;
        [URL] ->
            case Name of
                menu_triggered ->
                    load_content(URL, trigger),
                    cf_menu:hide_spinner();
                history_load ->
                    load_content(URL, history)
            end
    end;

api_event(A, B, C) ->
    ?LOG_WARNING("Unhandled api_event ~p, ~p, ~p.", [A, B, C]).

%
% Document entry points
%

main() ->
    case cf_url:scheme() of
        http ->
            wf:status_code(302),
            wf:header("Location", cf_url:url(https, cf_config:path() ++ cf_url:path())),
            "";
        https ->
            % HSTS, set to 7 days
            wf:header("Strict-Transport-Security", "max-age=604800"),

            cf_session:env(),

            % site api
            site_api(),

            % initial post back
            wf:wire("$Site.$boot();"),

            % store content data in process dict
            cache_content(),

            % return template
            #template{file = "./wwwroot/template.html"}
    end.

%
% Template entry points
%

description() ->
    cf_session:env(),
    ?TXT("Jabber.se is a not-for-profit public Jabber service in Sweden. It's "
         "open for anyone and completely free.").

head() ->
    cf_feed:get_feed_links().

page_title() ->
    ?TITLE.

title() ->
    #content{title = Title} = cached_content(),
    ?TITLE ++ " - " ++ Title.

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
    % Get the content cached by the main function.
    #content{body = Body,
             post_eval = PostEval} = cached_content(),

    % Since wired actions will be called later by the template
    % we can evaluate the function now.
    if is_function(PostEval) -> PostEval();
       true                  -> ok
    end,

    #panel{id = content_body,
           body = Body}.

foot() ->
    [].

dialogs() ->
    #panel{id = dialogs}.
