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
            {<<"posts_pub_count">>, {[
                        {<<"map">>, <<"function (doc) { if (doc.type == \"post\" && doc.state == \"public\") { for (author in doc.authors) { emit(doc.authors[author], 1); }}}">>},
                        {<<"reduce">>, <<"function (keys, values) { return sum(values); }">>}
                    ]}},

            {<<"posts_draft_count">>, {[
                        {<<"map">>, <<"function (doc) { if (doc.type == \"post\" && doc.state == \"draft\") { for (author in doc.authors) { emit(doc.authors[author], 1); }}}">>},
                        {<<"reduce">>, <<"function (keys, values) { return sum(values); }">>}
                    ]}},

            {<<"posts">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\") emit(0 - doc.ts, doc); }">>}]}},
            {<<"posts_by">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\") { for(author in doc.authors) { emit([doc.authors[author], 0 - doc.ts], doc); }}}">>}]}},
            {<<"drafts">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\" && doc.state == \"draft\") { for (author in doc.authors) { emit([doc.authors[author], 0 - doc.ts], doc); }}}">>}]}},
            {<<"news">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\" && doc.state == \"public\" && doc.tags.indexOf(\"news\") >= 0) emit(0 - doc.ts, doc); }">>}]}},
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
        rev,
        state = draft,
        title = "",
        timestamp,
        tags = [],
        authors = [],
        body = ""
    }).

-record(db_user, {
        username,
        password_hash,
        full_name,
        email,
        jid
    }).

%
% Configuration
%

-define(DEFAULT_POSTS_PER_PAGE, 20).

%
% Helper macros
%

-define(DB_HANDLE_RESULT(Expr), db_utils:throw_error_if_not_doc(Expr)).

-endif.
