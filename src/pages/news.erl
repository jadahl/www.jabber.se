-module(news).
-include("include/config.hrl").
-export([body/0, atom/0, atom_url/0]).

body() ->
    cms:body().

atom() ->
    cms:atom("#news", "News"). % FIXME better configuration

atom_url() ->
    {ok, ?URL_BASE ++ "feed/news"}.
