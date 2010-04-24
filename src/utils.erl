-module(utils).
-include_lib("nitrogen/include/wf.inc").

-include("include/utils.hrl").
-export([time_to_iso8601/1, t_to_ht/1, ts_to_ht/1, text_to_hyper_text/1, texts_to_hyper_text/1, text_to_ht/1, log/5, find_with/3]).

%
% Converters
%

time_to_iso8601(Time) ->
    Ms = trunc(Time / 1000000), 
    Now = {Ms, Time - (Ms * 1000000), 0},
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time(Now),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
            [Year, Month, Day, Hour, Min, Sec])).

%
% Hyper text
%

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

%
% Logging
%

log(Level, Module, Line, Format, Args) ->
    LogLine = io_lib:format(Format, Args),
    io:format("~s:~p:~p: ~s~n", [Level, Module, Line, LogLine]).

%
% Utility functions
%

%
% maybe_foldl(MaybeFun, Acc0, List) -> Acc1
%  MaybeFun = fun(Elem, AccIn) -> {just, AccOut} | nothing
%  Elem = term()
%  Acc0 = Acc1 = AccOut = AccIn = term()
%  List = [term()]
%
find_with(MaybeFun, AccIn, [Elem| Elements]) ->
    case MaybeFun(Elem, AccIn) of
        {just, AccOut} ->
            AccOut;
        nothing ->
            find_with(MaybeFun, AccIn, Elements)
    end;
find_with(_, _, []) ->
    nothing.

