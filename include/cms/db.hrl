-ifndef(db_hrl).
-define(db_hrl, true).

-define(COUCHDB_NAME, "jabber_se-cms").
-define(DESIGN_NAME, "jabber_se").

-define(DB_VIEWS,
    {[
            {<<"posts">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\") emit(0 - doc.ts, doc); }">>}]}},
            {<<"news">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\" && doc.tags.indexOf(\"news\") >= 0) emit(0 - doc.ts, doc); }">>}]}},
            {<<"posts_by_id">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"post\") emit(doc._id, doc); }">>}]}},
            {<<"user_login">>, {[{<<"map">>, <<"function (doc) { if (doc.type == \"user\") emit(doc.username, doc.password_hash); }">>}]}}
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
