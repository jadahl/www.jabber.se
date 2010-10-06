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

-module(db_controller).
-behaviour(gen_server).
-export([
        start/0, stop/0, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2,
        get_database/0,
        get_view/2, get_view/3, 
        open_doc/1, open_doc/2,
        save_doc/1, save_doc/2,
        render_and_save/2, render_and_save/3,
        render_and_save_many/2, render_and_save_many/3,
        delete_doc/1,  delete_doc/2, delete_doc_by_id/1, delete_doc_by_id/2
    ]).

-include("include/utils.hrl").
-include("include/db/db.hrl").
-include("src/db/old_content.hrl").

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

% dialyzer inferrs the none()
%-spec open_or_create_database(#db_state{}) -> {created|opened, pid()}.
open_or_create_database(State) ->
    ?LOG_INFO("Opening database connection", []),
    Connection = couchbeam_server:start_connection_link(),
    {Method, Database} = case couchbeam_db:open(Connection, ?COUCHDB_NAME) of
        not_found ->
            {created, couchbeam_db:create(Connection, ?COUCHDB_NAME)};
        ExistingDb ->
            {opened, ExistingDb}
    end,
    put(?DB_PID_KEY, Database),
    {Method, State#db_state{connection = Connection, db = Database}}.

disconnect_database({Connection, Db}) ->
    ?WHEN(Db, couchbeam_db:close(Connection, Db)),
    ?WHEN(Connection, couchbeam_server:close(Connection)).

%
% Views
%

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

%
% Opening
%

open_doc(DocId) ->
    open_doc(DocId, get_database()).
open_doc(DocId, Db) when is_binary(DocId) ->
    open_doc(binary_to_list(DocId), Db);
open_doc(DocId, Db) when is_atom(DocId) ->
    open_doc(atom_to_list(DocId), Db);
open_doc(DocId, Db) when is_list(DocId) ->
    ?DB_HANDLE_RESULT(couchbeam_db:open_doc(Db, DocId)).

%
% Saving
%

render_and_save_many(RenderFun, Inputs) ->
    render_and_save_many(RenderFun, Inputs, get_database()).
render_and_save_many(RenderFun, Inputs, Db) ->
    lists:foreach(fun(Input) -> render_and_save(RenderFun, Input, Db) end, Inputs).

render_and_save(RenderFun, Input) ->
    render_and_save(RenderFun, Input, get_database()).
render_and_save(RenderFun, Input, Db) ->
    ?DB_HANDLE_RESULT(couchbeam_db:save_doc(Db, db_doc:render(RenderFun, Input))).

save_doc(Doc) ->
    save_doc(Doc, get_database()).
save_doc(Doc, Db) ->
    ?DB_HANDLE_RESULT(couchbeam_db:save_doc(Db, Doc)).

%
% Deleting
%

delete_doc(Doc) ->
    delete_doc(Doc, get_database()).
delete_doc(Doc, Db) ->
    ?DB_HANDLE_RESULT(couchbeam_db:delete_doc(Db, Doc)).

delete_doc_by_id(DocId) ->
    delete_doc_by_id(DocId, get_database()).
delete_doc_by_id(DocId, Db) ->
    Doc = open_doc(DocId, Db),
    delete_doc(Doc, Db).

%
% Setup
%

setup_db(#db_state{db = Db} = DbState) ->
    ?LOG_INFO("Setting up db ('~p')", [DbState]),
    ?DB_HANDLE_RESULT(couchbeam_db:save_doc(Db, ?DB_DESIGN_DOC)),
    ?LOG_INFO("Saving old posts", []),
    db_post:save_posts(get_all_old(), Db),
    ?LOG_INFO("Saving admin entry", []),
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

init(InitialState) ->
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

