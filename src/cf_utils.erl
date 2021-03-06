%
%    Jabber.se Web Application
%    Copyright (C) 2010-2011 Jonas Ådahl
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

-module(cf_utils).
-include_lib("nitrogen_core/include/wf.hrl").

-include("include/utils.hrl").
-export([
        ts_to_date_s/1, ts_to_date/1, ts_to_hour_min/1, time_to_iso8601/1,
        to_binary/1, to_string/1, to_atom/1,
        sub_id/2,
        to_xml/1,
        log/5,
        drop_prefix/2,
        join/2, range/1, range/2, just/1, find_with/3, forall/2,
        keyreplacewith/4, keyreplaceoraddwith/4,
        maybe_append/2
    ]).

%
% Converters
%

ts_to_date_s(undefined) ->
    "undefined";
ts_to_date_s(TS) when is_integer(TS) ->
    {{Year, Month, Day}, _} = ts_to_date(TS),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day]).

ts_to_date(TS) ->
    Ms = trunc(TS / 1000000),
    Now = {Ms, TS - (Ms * 1000000), 0},
    calendar:now_to_universal_time(Now).


ts_to_hour_min(TS) ->
    Minute = (TS div 60) rem 60,
    Hour = (TS div (60*60)) rem 24,
    io_lib:format("~2.10.0B:~2.10.0B", [Hour, Minute]).

time_to_iso8601(Time) ->
    Ms = trunc(Time / 1000000),
    Now = {Ms, Time - (Ms * 1000000), 0},
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time(Now),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
            [Year, Month, Day, Hour, Min, Sec])).

to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
to_binary(Float) when is_float(Float) ->
    list_to_binary(float_to_list(Float));
to_binary(Integer) when is_integer(Integer) ->
    list_to_binary(integer_to_list(Integer));
to_binary(Binary) when is_binary(Binary) ->
    Binary.

to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Float) when is_float(Float) ->
    float_to_list(Float);
to_string(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_string(List) when is_list(List) ->
    List.

to_atom(Atom) when is_atom(Atom) ->
    Atom;
to_atom(List) when is_list(List) ->
    list_to_atom(List);
to_atom(Binary) when is_binary(Binary) ->
    list_to_atom(binary_to_list(Binary)).


%
% Ids
%

sub_id(Parent, Child) when is_atom(Parent) and is_atom(Child) ->
    list_to_atom(atom_to_list(Parent) ++ "." ++ atom_to_list(Child));
sub_id(Parent, Child) ->
    [wf_render_actions:normalize_path(Parent), " > ", wf_render_actions:normalize_path(Child)].

%
% XML
%

to_xml({List}) when is_list(List) ->
    lists:map(fun to_xml/1, List);
to_xml(List) when is_list(List) ->
    List;
to_xml(Bin) when is_binary(Bin) ->
    Bin;
to_xml({Name, Attrs, Content}) ->
    wf_tags:emit_tag(Name, to_xml(Content), Attrs);
to_xml({Name, Content}) ->
    wf_tags:emit_tag(Name, to_xml(Content), []).

%
% Logging
%

log(Level, Module, Line, Format, Args) ->
    LogLine = io_lib:format(Format, Args),
    io:format("~s:~p:~p:~p: ~s~n", [Level, Module, Line, self(), LogLine]).

%
% Utility functions
%

drop_prefix([], List)               -> List;
drop_prefix([I | Rest], [I | List]) -> drop_prefix(Rest, List);
drop_prefix(_, _List)               -> no_match.

join([], _) ->
    [];
join([X1 | Xs], D) ->
    [X1 | [[D, X] || X <- Xs]].

range(Num) ->
    range(1, Num).

range(Start, Num) ->
    range1(Start, Start + Num).

range1(I, Num) when I < Num ->
    [I | range1(I + 1, Num)];
range1(_, _) ->
    [].

-spec just({just, Value} | nothing) -> Value | nothing.
just({just, Value}) -> Value;
just(nothing) -> nothing.

%
% find_with(MaybeFun, Acc0, List) -> Acc1 | nothing
%  MaybeFun = fun(Elem, AccIn) -> {just, AccOut} | nothing
%  Elem = term()
%  Acc0 = Acc1 = AccOut = AccIn = term()
%  List = [term()]
%
-spec find_with(fun((Elem, Elem) -> {just, Elem} | nothing), Elem, [Elem]) -> {just, Elem} | nothing.
find_with(MaybeFun, AccIn, [Elem| Elements]) ->
    case MaybeFun(Elem, AccIn) of
        {just, AccOut} ->
            {just, AccOut};
        nothing ->
            find_with(MaybeFun, AccIn, Elements)
    end;
find_with(_, _, []) ->
    nothing.


forall(Fun, [Item | Items]) ->
    Fun(Item),
    forall(Fun, Items);
forall(_Fun, []) ->
    ok.

keyreplacewith(Key, N, Fun, [Tuple | TupleList]) ->
    if
        element(N, Tuple) =:=  Key ->
            [Fun(Tuple) | TupleList];
        true ->
            [Tuple | keyreplacewith(Key, N, Fun, TupleList)]
    end;
keyreplacewith(_, _, _, []) ->
    [].

keyreplaceoraddwith(Key, N, Fun, [Tuple | TupleList]) ->
    if
        element(N, Tuple) =:=  Key ->
            [Fun(Tuple) | TupleList];
        true ->
            [Tuple | keyreplaceoraddwith(Key, N, Fun, TupleList)]
    end;
keyreplaceoraddwith(_, _, Fun, []) ->
    [Fun(none)].

maybe_append([ToAppend], ToAppend) -> [ToAppend];
maybe_append([], ToAppend)         -> [ToAppend];
maybe_append([X | XS], ToAppend)   -> [X | maybe_append(XS, ToAppend)].

