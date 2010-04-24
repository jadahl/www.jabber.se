-module(cms_db).
-behaviour(gen_server).
-export([
        start/0, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2,
        get_all_old/0, get_view/1, get_all/0, get_news/0, get_posts/1,
        parse_entry/1, parse_content/2,
        to_simple_entry/1
    ]).

-include("include/utils.hrl").
-include("include/cms/cms_db.hrl").
-include("src/cms/old_content.hrl").

%
% Control
%

start() ->
    InitialState = #cms_db_state{},
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitialState, []).

%
% API
%

get_all() ->
    get_posts(all).

get_news() ->
    get_posts("news").

%
% Utilities
%

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).

atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%
% DB functionality
%

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

retrieve_view(Type, Db) ->
    ViewPid = couchbeam_db:query_view(Db, {"jabber_se", Type}, []),
    View = case couchbeam_view:parse_view(ViewPid) of
        {error, Reason} -> ?THROW(Reason);
        V -> V
    end,
    couchbeam_view:close_view(ViewPid),
    View.

%
% Parsing
%

to_simple_rows(Rows) ->
    lists:map(fun to_simple_entries/1, Rows).

to_simple_entries({_, _, {Rows}}) ->
    lists:map(fun to_simple_entry/1, Rows).

to_simple_entry({Key, {Values}}) when is_list(Values) ->
    {binary_to_atom(Key), lists:map(fun to_simple_entry/1, Values)};
to_simple_entry({Key, Value}) ->
    {binary_to_atom(Key), Value}.

parse(Rows) ->
    lists:map(fun parse_row/1, Rows).

parse_row(Data) when is_list(Data) ->
    lists:foldl(fun parse_entry/2, #content{}, Data).

parse_entry({Key, Value}) ->
    {binary_to_atom(Key), Value}.

parse_entry({K, V}, C) ->
    case K of
        id -> C#content{id = V};
        ts -> C#content{timestamp = V};
        type -> C#content{type = binary_to_atom(V)};
        authors -> C#content{authors = lists:map(fun binary_to_list/1, V)};
        content -> C#content{content = parse_content(V, C)};
        _ ->
            ?LOG_INFO("Parser: Unknown entry ~p", [K]),
            C
    end.

parse_content(V, #content{type = Type}) ->
    case lists:keyfind(Type, 1, ?DB_CONTENT_TYPES) of
        false ->
            ?LOG_WARNING("Parser: Unknown content type ~p", [Type]),
            undefined;
        {_, Module} ->
            Module:parse_content(V)
    end.

%
% Get
%

get_posts(Type) ->
    {_NumRows, _Offset, _Params, Rows} = get_view(Type),
    parse(to_simple_rows(Rows)).

%
% Post
%

post_content(Content, Db) ->
    couchbeam_db:save_doc(Db, render_content(Content)).

render_content(Content) ->
    post_render(render_entry(Content)).

post_render(Doc) ->
    {lists:map(fun finalize_entry/1, Doc)}.

finalize_entry({K, V}) ->
    {atom_to_binary(K), finalize_value(V)}.

finalize_value(Num) when is_integer(Num) ->
    list_to_binary(integer_to_list(Num));
finalize_value(Num) when is_float(Num) ->
    list_to_binary(float_to_list(Num));
finalize_value(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
finalize_value(Bin) when is_binary(Bin) ->
    Bin;
finalize_value([C | S] = V) when is_number(C) and is_list(S) ->
    list_to_binary(V);
finalize_value(L) when is_list(L) ->
    lists:map(fun finalize_value/1, L);
finalize_value({L}) ->
    {lists:map(fun finalize_entry/1, L)}.

render_entry(#content{
        id = Id,
        timestamp = TimeStamp,
        authors = Authors,
        type = Type,
        content = Content}) ->
    [
        {id, Id},
        {ts, TimeStamp},
        {authors, Authors},
        {type, Type},
        {content, render_content_by_type(Type, Content)}
    ].

render_content_by_type(Type, Content) ->
    case lists:keyfind(Type, 1, ?DB_CONTENT_TYPES) of
        false ->
            ?LOG_WARNING("Parser: Unknown content type '~p'.", [Type]),
            undefined;
        {_, Module} ->
            Module:render_content(Content)
    end.

%
% Setup
%

setup_db(Db) ->
    couchbeam_db:save_doc(Db, ?DB_NEWS_DOC),
    lists:map(fun(Post) -> post_content(Post, Db) end, get_all_old()).

get_all_old() ->
    ?OLD_CONTENT.

%
% gen_server wrappers
%

get_view(Type) ->
    case gen_server:call(?MODULE, {get_view, Type}) of
        {view, Result} ->
            Result;
        _ ->
            ?THROW(no_view)
    end.

%
% gen_server callbacks
%

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
    {reply, ?CATCH_AND_WARN({view, retrieve_view(View, Db)}), State};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    io:format("handle_info - EXIT ~p~n", [Reason]),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #cms_db_state{connection = Connection, db = Db}) ->
    disconnect_database({Connection, Db}).

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

