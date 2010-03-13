-module(news).
-export([body/0]).

body() ->
    cms:body("news").
