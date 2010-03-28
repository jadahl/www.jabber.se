-module(cms_db).
-include("include/cms/cms_db.hrl").
-include("src/cms/old_content.hrl").
-behaviour(gen_server).
-compile(export_all).
-export([start/0, info/0, get_view/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

start() ->
    InitialState = #cms_db_state{},
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitialState, []).

info() ->
    gen_server:call(?MODULE, info).

setup_db(Db) ->
    couchbeam_db:save_doc(Db, ?DB_NEWS_DOC),
    lists:map(fun(Post) -> post_content(Db, Post) end, get_all_old()).

get_all_old() ->
    ?OLD_CONTENT.

connect_database() ->
    Connection = couchbeam_server:start_connection_link(),
    {Connection, case couchbeam_db:open(Connection, ?COUCHDB_NAME) of
        not_found ->
            NewDb = couchbeam_db:create(Connection, ?COUCHDB_NAME),
            setup_db(NewDb),
            NewDb;
        ExistingDb ->
            ExistingDb
    end}.

disconnect_database({Connection, Db}) ->
    couchbeam_db:close(Db),
    couchbeam_server:close(Connection).

get_view(Type) ->
    case gen_server:call(?MODULE, {get_view, Type}) of
        {view, Result} ->
            Result;
        _ ->
            []
    end.

internal_get_view(Db, Type) ->
    ViewPid = couchbeam_db:query_view(Db, {"jabber_se", Type}, []),
    View = couchbeam_view:parse_view(ViewPid),
    couchbeam_view:close_view(ViewPid),
    View.

get_posts(Type) ->
    View = get_view(Type),
    case View of
        {error, _Reason} ->
            [];
        _ ->
            {_NumRows, _Offset,
                _Params,
                Rows
            } = View,
            rows_to_contents(Rows)
    end.

rows_to_contents(Rows) ->
    lists:map(fun (Row) -> row_to_content(Row) end, Rows).

row_to_content(Row) ->
    {_, _, {Contents}} = Row,
    json_to_contents(Contents, #content{}, none).

json_to_contents([], Content, _ContentFun) -> Content;
json_to_contents([JContent | JContents], Content, ContentFun) ->
    NewContentState = json_to_content(JContent, Content, ContentFun),
    case NewContentState of
        none ->
            json_to_contents(JContents, Content, ContentFun);
        {NewContent, NewContentFun} ->
            json_to_contents(JContents, NewContent, NewContentFun)
    end.

json_to_content({<<"id">>, Id}, Content, ContentFun) ->
    {Content#content{id = Id}, ContentFun};
json_to_content({<<"ts">>, Ts}, Content, ContentFun) ->
    {Content#content{timestamp = Ts}, ContentFun};
json_to_content({<<"type">>, <<"blog_post">>}, Content, _ContentFun) ->
    {Content, fun blog_post_to_content/1};
json_to_content({<<"type">>, _}, _Content, _ContentFun) ->
    none;
json_to_content({<<"authors">>, Authors}, Content, ContentFun) ->
    {Content#content{authors = lists:map(fun binary_to_list/1, Authors)}, ContentFun};
json_to_content({<<"content">>, _JContent}, _Content, none) ->
    none;
json_to_content({<<"content">>, JContent}, Content, ContentFun) ->
    {Content#content{content = ContentFun(JContent)}, ContentFun};
json_to_content(_, _Content, _ContentFun) ->
    none.

blog_post_to_content({Contents}) when is_list(Contents) ->
    blog_contents_to_content(Contents, #blog_post{}).

blog_contents_to_content([], BP) -> BP;
blog_contents_to_content([Content | Contents], BP) ->
    Buffer = blog_content_to_content(Content, BP),
    case Buffer of
        none ->
            blog_contents_to_content(Contents, BP);
        _ ->
            blog_contents_to_content(Contents, Buffer)
    end.

blog_content_to_content({<<"title">>, Title}, BP) ->
    BP#blog_post{title = binary_to_list(Title)};
blog_content_to_content({<<"body">>, Body}, BP) ->
    BP#blog_post{body = binary_to_list(Body)};
blog_content_to_content({<<"tags">>, Tags}, BP) ->
    BP#blog_post{tags = lists:map(fun binary_to_list/1, Tags)};
blog_content_to_content({<<"lang">>, Lang}, BP) ->
    BP#blog_post{lang = binary_to_list(Lang)};
blog_content_to_content(_, BP) ->
    BP.

post_content(Db, #content{
        id = Id,
        timestamp = TimeStamp,

        authors = Authors,
        content = Content
    }) ->
    Doc = {[
            {<<"id">>, Id},
            {<<"ts">>, TimeStamp},
            {<<"authors">>, lists:map(fun(Author) ->
                            list_to_binary(Author)
                    end, Authors)},
            {<<"type">>, list_to_binary(content_type(Content))},
            {<<"content">>, content(Content)}
        ]},
    couchbeam_db:save_doc(Db, Doc),
    ok.

content_type(#blog_post{}) -> "blog_post";
content_type(_) -> "unknown".

content(#blog_post{
        title = Title,
        body = Body,
        tags = Tags,
        lang = Lang
    }) ->
    Language = if
        is_list(Lang) ->
            "en";
        true ->
            Lang
    end,
    Content = {[
            {<<"title">>, list_to_binary(Title)},
            {<<"lang">>, list_to_binary(Language)},
            {<<"body">>, list_to_binary(Body)},
            {<<"tags">>, lists:map(fun(Tag) ->
                            list_to_binary(Tag)
                    end, Tags)}
        ]},
    Content;
content(_) ->
    {<<"unknown">>}.

post(_Post) ->
    ok.


get_all() ->
    get_posts(all).

get_news() ->
    get_posts("news").

%%------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------

init(#cms_db_state{} = InitialState) ->
    process_flag(trap_exit, true),
    couchbeam:start(),
    case connect_database() of
        {Connection, Db} ->
            State = InitialState#cms_db_state{
                connection = Connection,
                db = Db
            },
            {ok, State};
        _ ->
            {fail, InitialState}
    end.

handle_call(info, _From, State) ->
    io:format("hande_call(info, _, _)~n", []),
    {reply, active, State};

handle_call({get_view, View}, _From, #cms_db_state{db = Db} = State) ->
    {reply, {view, internal_get_view(Db, View)}, State};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    io:format("handle_info - EXIT ~p~n", [Reason]),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
