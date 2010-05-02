-module(db_controller).
-behaviour(gen_server).
-export([
        start/0, stop/0, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2,
        get_view/2, get_view/3, get_database/0, save_doc/2, save_doc/3, save_docs/2, save_docs/3
    ]).

-include("include/utils.hrl").
-include("include/cms/db.hrl").
-include("src/cms/old_content.hrl").

%
% Control
%

start() ->
    InitialState = #db_state{},
    {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, InitialState, []),
    init_database().

stop() ->
    gen_server:cast(?MODULE, terminate).

-define(DB_PID_KEY, stk_db_pid).

get_database() ->
    case get(?DB_PID_KEY) of
        undefined ->
            Pid1 = gen_call(get_db_pid),
            put(?DB_PID_KEY, Pid1),
            Pid1;
        Pid2 ->
            Pid2
    end.

init_database() ->
    ok.

%
% DB functionality
%

open_or_create_database(State) ->
    ?LOG_INFO("Opening database", []),
    Connection = couchbeam_server:start_connection_link(),
    {Method, Database} = case couchbeam_db:open(Connection, ?COUCHDB_NAME) of
        not_found ->
            {created, couchbeam_db:create(Connection, ?COUCHDB_NAME)};
        ExistingDb ->
            {opened, ExistingDb}
    end,
    put(?DB_PID_KEY, Database),
    {Method, State#db_state{connection = Connection, db =Database}}.

disconnect_database({Connection, Db}) ->
    ?LOG_INFO("Closing down couchbream database", []),
    ?WHEN(Db, couchbeam_db:close(Connection, Db)),
    ?WHEN(Connection, couchbeam_server:close(Connection)).

get_view(ViewName, Params) ->
    get_view(ViewName, Params, get_database()).
get_view(ViewName, Params, Db) ->
    ViewPid = couchbeam_db:query_view(Db, {?DESIGN_NAME, ViewName}, Params),
    Res = couchbeam_view:parse_view(ViewPid),
    couchbeam_view:close_view(ViewPid),
    case Res of
        {error, Reason} -> ?THROW({Reason, ViewName});
        _ -> Res
    end.

save_doc(RenderFun, Input) ->
    save_doc(RenderFun, Input, get_database()).
save_doc(RenderFun, Input, Db) ->
    couchbeam_db:save_doc(Db, db_utils:render(RenderFun, Input)).

save_docs(RenderFun, Inputs) ->
    save_docs(RenderFun, Inputs, get_database()).
save_docs(RenderFun, Inputs, Db) ->
    [save_doc(RenderFun, Input, Db) || Input <- Inputs],
    ok.

%
% Setup
%

setup_db(#db_state{db = Db} = DbState) ->
    ?LOG_INFO("Setting up db ('~p')", [DbState]),
    couchbeam_db:save_doc(Db, ?DB_DESIGN_DOC),
    db_post:save_posts(get_all_old(), Db),
    db_user:save_user(?ADMIN_USER_ENTRY, Db),
    DbState.

get_all_old() ->
    ?OLD_CONTENT.

%
% gen_server helpers
%

gen_call(Call) ->
    case gen_server:call(?MODULE, Call) of
        A when is_atom(A) ->
            A;
        {ok, Result} ->
            Result;
        {error, E} ->
            ?THROW(E)
    end.

%
% gen_server callbacks
%

init(#db_state{} = InitialState) ->
    process_flag(trap_exit, true),
    couchbeam:start(),
    case open_or_create_database(InitialState) of
        {created, CreatedDb} ->
            {ok, setup_db(CreatedDb)};
        {opened, OpenedDb} ->
            {ok, OpenedDb};
        Other ->
            ?LOG_ERROR("Failed to open or create database with the result '~p'", [Other]),
            {fail, InitialState}
    end.

handle_call(info, _From, State) ->
    {reply, active, State};
handle_call(get_db_pid, _From, #db_state{db = Db} = State) ->
    {reply, {ok, Db}, State};
handle_call(Call, _From, State) ->
    ?LOG_WARNING("Unhandled call '~p'", [Call]),
    {reply, ok, State}.

handle_cast(terminate, State) ->
    {stop, normal, State};
handle_cast(Cast, State) ->
    ?LOG_WARNING("Unhandled cast '~p'", [Cast]),
    {noreply, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    ?LOG_WARNING("handle_info - EXIT ~p~n", [Reason]),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #db_state{connection = Connection, db = Db}) ->
    disconnect_database({Connection, Db}).

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

