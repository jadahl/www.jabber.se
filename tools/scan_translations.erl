#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname scan_translations -pa ebin/

-define(THROW(Reason), throw({exception, Reason, ?MODULE, ?LINE})).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(DBG(Str, Args), io:format(Str, Args)).
-else.
-define(DBG(_Str, _Args), ok).
-endif.

print_usage() ->
    io:format("Usage: scan_translations [-d] [-h] -t translation directory -i include directory -s source directory~n").

usage() ->
    print_usage(),
    halt(1).

main(Args) ->
    Options = try
        options(Args)
    catch
        {exception, {need_value, Option}, _, _} ->
            ?DBG("option requires an argument - '~p'~n", [Option]),
            usage();
        {exception, Option, _, _} ->
            ?DBG("parsing failed - '~p'~n", [Option]),
            usage()
    end,

    case options_valid(Options) of
        true ->
            ok;
        false ->
            io:format("Missing options.~n"),
            usage()
    end,

    put(options, Options),

    scan_directories(),

    ok.

%
% Scanning
%

scan_directories() ->
    Options = get(options),

    IncludeDirs = include_dirs(Options),
    SourceDirs = source_dirs(Options),
    % FIXME ensure only one -t is called
    [TranslationsDir] = translations_dir(Options),

    Ids = scan_dirs(SourceDirs, IncludeDirs),

    add_missing_ids(Ids, TranslationsDir),

    ok.

scan_dirs(Dirs, IDirs) ->
    lists:flatten([scan_dir(Dir, IDirs) || Dir <- Dirs]).

scan_dir(Dir, IDirs) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            [scan_file(Dir ++ Filename, IDirs) || Filename <- Filenames];
        Error ->
            ?THROW(Error)
    end.

scan_file(Filename, IDirs) ->
    case lists:suffix(".erl", Filename) of
        true ->
            scan_erl(Filename, IDirs);
        false ->
            []
    end.

scan_erl(Filename, IDirs) ->
    case epp:parse_file(Filename, IDirs, []) of
        {ok, Parsed} ->
            traverse(Parsed, []);
        Error ->
            io:format("~s: Parsing failed: ~p~n", [Filename, Error]),
            halt(1)
    end.

traverse({atom, _, Atom}, State) ->
    case atom_to_list(Atom) of
        [$m, $s, $g, $_, $i, $d, $_ | _Rest] ->
            [Atom | State];
        _ ->
            State
    end;
traverse(Parsed, State) ->
    Exprs = case Parsed of
        _ when is_list(Parsed) ->
            Parsed;
        _ ->
            erl_syntax:subtrees(Parsed)
    end,
    lists:foldl(fun(Expr, InState) -> traverse(Expr, InState) end, State, Exprs).

%
% Add missing translation ids
%

render_entry({MsgId, Translation}, IsLast) ->
    End = if
        not IsLast -> ",";
        true -> ""
    end,
    list_to_binary(io_lib:format("    {~p, ~p}~s % not translated~n", [MsgId, Translation, End])).

%
% Merge new entries into file. Return new file content.
%

next_is_tail(CurrentLine, [{Line, tail, _} | _Entries]) when CurrentLine + 1 == Line -> true;
next_is_tail(_, _) -> false.

insert_comma(Binary) ->
    list_to_binary(insert_comma_s(binary_to_list(Binary))).

insert_comma_s([$} | Rest]) ->
    [$}, $, | Rest];
insert_comma_s([C | Rest]) ->
    [C | insert_comma_s(Rest)];
insert_comma_s([]) ->
    [].

merge_file(CurrentLine, F, []) ->
    case read_line(F) of
        {ok, Line} ->
            [Line, merge_file(CurrentLine + 1, F, [])];
        eof ->
            [];
        Error ->
            ?THROW(Error)
    end;

merge_file(CurrentLine, F, [{EntryLine, _Mode, NewEntry} | NewEntries]) when EntryLine == CurrentLine ->
    [render_entry(NewEntry, NewEntries == []) | merge_file(CurrentLine + 1, F, NewEntries)];

merge_file(CurrentLine, F, NewEntries) ->
    case read_line(F) of
        {ok, Line} ->
            NewLine = case next_is_tail(CurrentLine, NewEntries) of
                true -> insert_comma(Line);
                false -> Line
            end,
            [NewLine | merge_file(CurrentLine + 1, F, NewEntries)];
        eof -> ?THROW(unexpected_eof);
        Error -> ?THROW(Error)
    end.

%
% Overwrite file (if not dry-run) with new entries
%
rewrite_file(NewEntries, Filename) ->
    case file:open(Filename, [write, read, binary]) of
        {ok, F} ->
            Merged = merge_file(1, F, NewEntries),

            case is_dry_run() of
                true ->
                    io:format("~s would be:~n~s~n", [Filename, Merged]);
                false ->
                    io:format("Rewriting ~s~n", [Filename]),
                    case file:copy(F, Filename ++ ".bak") of
                        {ok, _} ->
                            file:close(F),
                            case file:write_file(Filename, Merged) of ok -> ok; Error2 -> ?THROW(Error2) end,
                            ok;
                        Error ->
                            io:format("Could not create backup."),
                            ?THROW(Error)
                    end
            end,

            ok;
        Error ->
            ?THROW(Error)
    end.

tree_insert({NewId, _} = NewEntry,
    {cons, Line, {tuple, _, [{atom, _, Id}, _]}, Rest}, Inserted) ->
    if
        NewId < Id -> 
            {Line + Inserted, mangle, NewEntry};
        true ->
            tree_insert(NewEntry, Rest, Inserted)
    end;
tree_insert(NewId, {nil, Line}, Inserted) ->
    {Line + Inserted, tail, NewId};
tree_insert(_, Tree, _) ->
    io:format("Syntax error."),
    ?THROW({syntax_error, Tree}).

insert_ids_to_tree(Ids, Tree) ->
    insert_ids_to_tree(Ids, Tree, 0).

insert_ids_to_tree([Id | Ids], Tree, Inserted) ->
    [tree_insert(Id, Tree, Inserted) | insert_ids_to_tree(Ids, Tree, Inserted + 1)];
insert_ids_to_tree([], _, _) ->
    [].

insert_ids_to_file(Ids, Filename, Parsed) ->
    [Tree] = Parsed, %% real error if more than one

    Inserts = insert_ids_to_tree(Ids, Tree),

    rewrite_file(Inserts, Filename),

    ok.

add_missing_ids(Ids, TDir) ->
    case file:list_dir(TDir) of
        {ok, Filenames} ->
            utils:forall(fun(Filename) -> add_missing_ids_to_file(Ids, TDir ++ Filename) end, Filenames);
        _ ->
            ok
    end.

add_missing_ids_to_file(Ids, Filename) ->
    case read_translation_file(Filename) of
        {ok, Parsed} ->
            Missing = missing(Parsed, Ids),
            case Missing of
                [] ->
                    ok;
                _ ->
                    insert_ids_to_file(Missing, Filename, Parsed),
                    
                    ok
            end;
        skip ->
            ok;
        Error ->
            ?DBG("Error while reading file '~s': ~p~n", [Filename, Error]),
            io:format("Failed to read translation file '~s'~n", [Filename]),
            halt(1)
    end.

read_translation_file(Filename) ->
    case filename:basename(Filename) of
        [C1, C2, $_, C3, C4 | ".res"] when
        ((C1 >= $a) and (C1 =< $z)) and
        ((C2 >= $a) and (C2 =< $z)) and
        ((C3 >= $A) and (C3 =< $Z)) and
        ((C4 >= $A) and (C4 =< $Z)) ->
            {ok, Binary} = file:read_file(Filename),
            {ok, Scanned, _} = erl_scan:string(binary_to_list(Binary)),
            erl_parse:parse_exprs(Scanned);
        _ ->
            skip
    end.

%
% Comparing
%

% TODO: only traverse once
has_msgid(Id1, {tuple, _, [{atom, _, Id2}, _]}) when Id1 == Id2 ->
    true;
has_msgid(_, []) ->
    false;
has_msgid(Id, [E | Es]) ->
    case has_msgid(Id, E) of
        true ->
            true;
        false ->
            has_msgid(Id, Es)
    end;
has_msgid(Id, Expr) ->
    has_msgid(Id, erl_syntax:subtrees(Expr)).

missing(Exprs, Ids) ->
    lists:usort(missing1(Exprs, Ids)).

missing1(Exprs, [Id | Ids]) ->
    case has_msgid(Id, Exprs) of
        false ->
            [{Id, "[" ++ atom_to_list(Id) ++ "]"} | missing1(Exprs, Ids)];
        true ->
            missing1(Exprs, Ids)
    end;
missing1(_, []) ->
    [].

%
% Options
%

is_dry_run() ->
    case lists:keysearch(d, 1, get(options)) of
        {value, {d, true}} ->
            true;
        _ ->
            false
    end.

source_dirs(Options) ->
    gather_args(Options, s).

translations_dir(Options) ->
    gather_args(Options, t).

include_dirs(Options) ->
    gather_args(Options, i).

gather_args(Options, Opt) ->
    lists:foldl(fun(Option, OptValues) ->
                case Option of
                    {Opt, OptValue} -> [OptValue | OptValues];
                    _ -> OptValues
                end
        end, [], Options).

options_valid(Options) ->
    case options_valid(Options, [t, i, s]) of
        [] ->
            true;
        _ ->
            false
    end.

options_valid([{Option, _} | Options], Needed) ->
    options_valid(Options, Needed -- [Option]);
options_valid([], Needed) ->
    Needed.

option(Value, {need_value, Type}) ->
    {out, {Type, Value}};
option("-d", none) ->
    {out, {d, true}};
option("-h", none) ->
    print_usage(),
    io:format("   -d       - dry run~n   -s <dir> - source directory~n   -t <dir> - translation directory~n   -i <dir> - include directory~n   -h       - display help~n"),
    halt(1);
option("-t", none) ->
    {need_value, t};
option("-i", none) ->
    {need_value, i};
option("-s", none) ->
    {need_value, s};
option(Option, _) ->
    ?THROW({unknown_option, Option}).

options(Args) ->
    options(Args, none).
options([Arg | Args], State) ->
    case option(Arg, State) of
        {out, Option} ->
            [Option | options(Args, none)];
        NewState ->
            options(Args, NewState)
    end;
options([], none) ->
    [];
options([], {need_value, Type}) ->
    ?THROW({missing_value, Type});
options([], State) ->
    ?THROW({invalid_state, State}).

%
% Utils
%

% TODO: replace this function with file:read_line if OTP > R13B02
read_line(IoDevice) ->
    try
        {ok, read_line1(IoDevice)}
    catch
        eof -> eof;
        {error, Error} -> {error, Error}
    end.

read_line1(IoDevice) ->
    case file:read(IoDevice, 1) of
        {ok, <<"\n">>} ->
            <<"\n">>;
        {ok, Data} ->
            L = read_line1(IoDevice),
            <<Data/binary, L/binary>>;
        Error ->
            throw(Error)
    end.

