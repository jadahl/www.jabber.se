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

-ifndef(db_hrl).
-define(db_hrl, true).

-define(COUCHDB_NAME, "jabber_se-cms").
-define(DESIGN_NAME, "jabber_se").

-define(DB_VIEWS,
    {[
            {<<"posts">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\") emit(0 - doc.ts, doc); }">>}]}},
            {<<"news">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\" && doc.tags.indexOf(\"news\") >= 0) emit(0 - doc.ts, doc); }">>}]}},
            {<<"posts_by_id">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\") emit(doc._id, doc); }">>}]}},
            {<<"user_login">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"user\") emit(doc._id, doc.password_hash); }">>}]}}
    ]}).

-define(DB_DESIGN_DOC,
    {[
            {<<"_id">>, <<"_design/jabber_se">>},
            {<<"language">>,<<"javascript">>},
            {<<"views">>, ?DB_VIEWS}
    ]}).

-record(db_state, {
        connection,
        db}).

-record(db_post, {
        id,
        title,
        timestamp,
        tags,
        authors,
        body
    }).

-record(db_user, {
        username,
        password_hash,
        full_name,
        email,
        jid
    }).

-endif.
