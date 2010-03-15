-module(utils).
-include_lib("nitrogen/include/wf.inc").
-export([t_to_ht/1, ts_to_ht/1, text_to_hyper_text/1, texts_to_hyper_text/1, text_to_ht/1]).

t_to_ht(T) -> text_to_hyper_text(T).
ts_to_ht(Ts) -> texts_to_hyper_text(Ts).

text_to_hyper_text([E | Es]) when is_integer(E) ->
    #p{body = [E | Es]};
text_to_hyper_text(Es) when is_list(Es) ->
    texts_to_hyper_text(Es);
text_to_hyper_text(E) ->
    text_to_ht(E).

texts_to_hyper_text(Texts) when is_list(Texts) ->
    lists:map(fun text_to_ht/1, Texts);
texts_to_hyper_text(_) ->
    [].

text_to_ht(T) when is_list(T) ->
    T;
text_to_ht({email, EMail}) ->
    #link{url = "mailto:" ++ EMail, text = EMail};
text_to_ht({url, URL}) ->
    #link{url = URL, text = URL};
text_to_ht({link, URL, Text}) ->
    #link{url = URL, text = Text};
text_to_ht({jid, JID}) ->
    #link{url = "xmpp:" ++ JID, text = JID};
text_to_ht({muc, MUC}) ->
    #link{url = "xmpp:" ++ MUC ++ "?join", text = MUC};
text_to_ht(T) ->
    io:format("~p.erl:~p Warning: format ~p not recognized.", [?MODULE, ?LINE, T]),
    [].
