-ifndef(cms_db_hrl).
-define(cms_db_hrl, true).

-define(COUCHDB_NAME, "jabber_se-cms").

-define(DB_CONTENT_TYPES, [
        % {type, module}
        {blog_post, cms_blog}
    ]).

-define(DB_NEWS_DOC,
  {[
      {<<"_id">>, <<"_design/jabber_se">>},
      {<<"language">>,<<"javascript">>},
      {<<"views">>,
        {[{<<"news">>,
              {[{<<"map">>,
                    <<"function (doc) {\n if (doc.type == \"blog_post\") {\n emit(0 - doc.ts, doc);\n}\n}">>
                  }]}
            }]}
      }]}.

-record(cms_db_state, {
        connection,
        db}).

-record(content, {
        id,
        path,
        timestamp,
        authors,
        permission,
        type,
        content}).

-record(blog_post, 
        {title,
        body,
        tags,
        lang = "en"}).

-endif.
